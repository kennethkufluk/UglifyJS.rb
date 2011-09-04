=begin

  A JavaScript tokenizer / parser / beautifier / compressor.

  This version is suitable for RUBY.

  This file contains the tokenizer/parser.  It is a port to Ruby
  of Uglify-JS [2] which is a JavaScript port
  of parse-js [1], a JavaScript parser library written in Common Lisp
  by Marijn Haverbeke.  Thank you Marijn!  Thank you Mihai!

  [1] http://marijn.haverbeke.nl/parse-js/
  [2] https://github.com/mishoo/UglifyJS

  Exported functions:

    - tokenizer(code) -- returns a function.  Call the returned
      function to fetch the next token.

    - parse(code) -- returns an AST of the given JavaScript code.

  Distributed under the BSD license:

    Copyright 2011 (c) Kenneth Kufluk <kenneth@kufluk.com>
    Based on parse-js.js (https://github.com/mishoo/UglifyJS).

    parse-js.js Copyright 2010 (c) Mihai Bazon <mihai.bazon@gmail.com>
    Based on parse-js (http://marijn.haverbeke.nl/parse-js/).

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

        * Redistributions of source code must retain the above
          copyright notice, this list of conditions and the following
          disclaimer.

        * Redistributions in binary form must reproduce the above
          copyright notice, this list of conditions and the following
          disclaimer in the documentation and/or other materials
          provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER “AS IS” AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
    OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
    TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
    THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGE.

 ***********************************************************************/

=end

require 'parsejs/tokenizer'
require 'parsejs/util'

module Parsejs

  include Tokenizer

  # /* -----[ Parser (constants) ]----- */

  UNARY_PREFIX = [
    "typeof",
    "void",
    "delete",
    "--",
    "++",
    "!",
    "~",
    "-",
    "+"
  ]

  UNARY_POSTFIX = [ "--", "++" ]

  ASSIGNMENT = {
    '=' => true,
    '+=' => '+',
    '-=' => '-',
    '/=' => '/',
    '*=' => '*',
    '%=' => '%',
    '>>=' => '>>',
    '<<=' => '<<',
    '>>>=' => '>>>',
    '|=' => '|',
    '^=' => '^',
    '&=' => '&'
  }

  STATEMENTS_WITH_LABELS = [ "for", "do", "while", "switch" ]

  ATOMIC_START_TOKEN = [ "atom", "num", "string", "regexp", "name" ]

  # /* -----[ Parser ]----- */

  class NodeWithToken
    def initializer(str, start, _end)
      @name = str
      @start = start
      @end = _end
    end
    def to_s
      @name
    end
  end

  class Parse

    include Util

    def initialize(text_arg, exigent_mode=nil, embed_tokens=nil)
      @syntax = {
        :input       => text_arg.is_a?(String) ? Tokenizer::Tokenizer.new(text_arg) : text_arg,
        :token       => nil,
        :prev        => nil,
        :peeked      => nil,
        :in_function => 0,
        :in_loop     => 0,
        :labels      => []
      }
      @exigent_mode = exigent_mode
      @embed_tokens = embed_tokens

      @syntax[:token] = _next()

    end

    def parse
      a = []
      while !is("eof")
        a.push(statement.call())
      end
      return ["toplevel", a]
    end

    protected

    def is(type, value=nil)
      is_token?(@syntax[:token], type, value)
    end

    def peek
      @syntax[:peeked] || (@syntax[:peeked] = @syntax[:input].next_token())
    end

    def _next
      @syntax[:prev] = @syntax[:token]
      if @syntax[:peeked]
        @syntax[:token] = @syntax[:peeked]
        @syntax[:peeked] = nil
      else
        @syntax[:token] = @syntax[:input].next_token()
      end
      @syntax[:token]
    end

    def get_next
      lambda do
        _next()
      end
    end

    def prev
      @syntax[:prev]
    end

    def croak(msg, line=nil, col=nil, pos=nil)
      ctx = @syntax[:input].get_context()
      js_error(msg,
         line != nil ? line : ctx[:tokline],
         col != nil ? col : ctx[:tokcol],
         pos != nil ? pos : ctx[:tokpos])
    end

    def token_error(token, msg)
      croak(msg, token[:line], token[:col])
    end

    def unexpected(token=nil)
      if token == nil
        token = @syntax[:token]
      end
      token_error(token, "Unexpected token: " + token[:type] + " (" + token[:value] + ")")
    end

    def expect_token(type, val)
      if is(type, val)
        return _next()
      end
      token_error(@syntax[:token], "Unexpected token " + @syntax[:token][:type] + ", expected " + type)
    end

    def expect(punc)
      expect_token("punc", punc)
    end

    def can_insert_semicolon
      return !@exigent_mode && (
        @syntax[:token][:nlb] || is("eof") || is("punc", "}")
      )
    end

    def semicolon
      lambda do
        if is("punc", ";")
          _next()
        elsif !can_insert_semicolon()
          unexpected()
        end
      end
    end

    def as(*args)
      args
    end

    def parenthesised
      lambda do
        expect("(")
        ex = expression.call()
        expect(")")
        ex
      end
    end

    def add_tokens(str, start, _end)
      str.is_a?(NodeWithToken) ? str : NodeWithToken(str, start, _end)
    end

    def maybe_embed_tokens(parser)
      if @embed_tokens
        return lambda do
          start = @syntax[:token]
#kpk - this is a problem

          ast = parser.apply(this, arguments)
          ast[0] = add_tokens(ast[0], start, prev())
          return ast
        end
      else
        return parser
      end
    end

    #kpk: meh
    def statement
      maybe_embed_tokens(lambda do
        if is("operator", "/")
          @syntax[:peeked] = nil
          @syntax[:token] = @syntax[:input].next_token(true) # force regexp
        end
        case @syntax[:token][:type]
          when "num", "string", "regexp", "operator", "atom"
            return simple_statement()
          when "name"
            return is_token?(peek(), "punc", ":") ?
              labeled_statement(prog1(@syntax[:token][:value], get_next, get_next)) :
              simple_statement()
          when "punc"
            case @syntax[:token][:value]
              when "{"
                return as("block", block_())
              when "[", "("
                return simple_statement()
              when ";"
                _next()
                return as("block")
              else
                unexpected()
            end

          when "keyword"
            case prog1(@syntax[:token][:value], get_next)
              when "break"
                return break_cont("break")
              when "continue"
                return break_cont("continue")
              when "debugger"
                semicolon.call()
                return as("debugger")
              when "do"
                ils = in_loop.call(statement)
                expect_token("keyword", "while")
                return as("do", prog1(parenthesised, semicolon), ils)
              when "for"
                return for_()
              when "function"
                return function_.call(true)
              when "if"
                return if_()
              when "return"
                if @syntax[:in_function] == 0
                  croak("'return' outside of function")
                end
                return as("return",
                    if is("punc", ";")
                      _next()
                      nil
                    else
                      if can_insert_semicolon()
                        nil
                      else
                        prog1(expression, semicolon)
                      end
                    end
                  )
              when "switch"
                return as("switch", parenthesised.call(), switch_block_())
              when "throw"
                return as("throw", prog1(expression, semicolon))
              when "try"
                return try_()
              when "var"
                return prog1(var_, semicolon)
              when "const"
                return prog1(const_, semicolon)
              when "while"
                return as("while", parenthesised.call(), in_loop.call(statement))
              when "with"
                return as("with", parenthesised.call(), statement.call())
              else
                unexpected()
            end
        end
      end)
    end

    def labeled_statement(label)
      @syntax[:labels].push(label)
      start = @syntax[:token]
      stat = statement.call()
      if @exigent_mode && !HOP(STATEMENTS_WITH_LABELS, stat[0])
        unexpected(start)
      end
      @syntax[:labels].pop()
      return as("label", label, stat)
    end

    def simple_statement
      return as("stat", prog1(expression, semicolon))
    end

    def break_cont(type)
      if !can_insert_semicolon()
        name = is("name") ? @syntax[:token][:value] : nil
      end
      if name != nil
        _next()
        if !member(name, @syntax[:labels])
          croak("Label " + name + " without matching loop or statement")
        end
      elsif @syntax[:in_loop] == 0
        croak(type + " not inside a loop or switch")
      end
      semicolon.call()
      return as(type, name)
    end

    def for_
      expect("(")
      init = nil
      if !is("punc", ";")
        init = if is("keyword", "var")
          _next()
          var_.call(true)
        else
          expression.call(true, true)
        end
        if is("operator", "in")
          return for_in(init)
        end
      end
      return regular_for(init)
    end

    def regular_for(init)
      expect(";")
      test = is("punc", ";") ? nil : expression.call()
      expect(";")
      step = is("punc", ")") ? nil : expression.call()
      expect(")")
      return as("for", init, test, step, in_loop.call(statement))
    end

    def for_in(init)
      lhs = init[0] == "var" ? as("name", init[1][0]) : init
      _next()
      obj = expression.call()
      expect(")")
      return as("for-in", init, lhs, obj, in_loop.call(statement))
    end

    # classy
    def function_
      maybe_embed_tokens(lambda do |in_statement|
        name = is("name") ? prog1(@syntax[:token][:value], get_next) : nil
        if in_statement && !name
          unexpected()
        end
        expect("(")

        # arguments
        first = true
        args = []
        while !is("punc", ")")
          if first
            first = false
          else
            expect(",")
          end
          if !is("name")
            unexpected()
          end
          args.push(@syntax[:token][:value])
          _next()
        end
        _next()

        # body
        @syntax[:in_function]+=1
        loop = @syntax[:in_loop]
        @syntax[:in_loop] = 0
        body = block_()
        @syntax[:in_function]-=1
        @syntax[:in_loop] = loop

        return as(in_statement ? "defun" : "function",
            name,
            args,
            body
            )
      end)
    end

    def if_
      cond = parenthesised.call()
      body = statement.call()
      if is("keyword", "else")
        _next()
        belse = statement.call()
      end
      return as("if", cond, body, belse)
    end

    def block_
      expect("{")
      a = []
      while !is("punc", "}")
        if is("eof")
          unexpected()
        end
        a.push(statement.call())
      end
      _next()
      return a
    end

    #kpk
    def switch_block_
      in_loop.call(lambda do
        expect("{")
        a = []
        cur = nil
        while !is("punc", "}")
          if is("eof")
            unexpected()
          end
          if is("keyword", "case")
            _next()
            cur = []
            a.push([ expression.call(), cur ])
            expect(":")
          elsif is("keyword", "default")
            _next()
            expect(":")
            cur = []
            a.push([ nil, cur ])
          else
            if !cur
              unexpected()
            end
            cur.push(statement.call())
          end
        end
        _next()
        return a
      end)
    end

    def try_()
      body = block_()
      if is("keyword", "catch")
        _next()
        expect("(")
        if !is("name")
          croak("Name expected")
        end
        name = @syntax[:token][:value]
        _next()
        expect(")")
        bcatch = [ name, block_() ]
      end
      if is("keyword", "finally")
        _next()
        bfinally = block_()
      end
      if !bcatch && !bfinally
        croak("Missing catch/finally blocks")
      end
      return as("try", body, bcatch, bfinally)
    end

    def vardefs(no_in=nil)
      a = []
      while true
        if !is("name")
          unexpected()
        end
        name = @syntax[:token][:value]
        _next()
        if is("operator", "=")
          _next()
          a.push([ name, expression.call(false, no_in) ])
        else
          a.push([ name ]);
        end
        if (!is("punc", ","))
          break
        end
        _next()
      end
      return a
    end

    def var_
      lambda do |*args|
        no_in = args[0]
        as("var", vardefs(no_in))
      end
    end

    def const_()
      as("const", vardefs())
    end

    def new_()
      newexp = expr_atom.call(false)
      if (is("punc", "("))
        _next()
        args = expr_list(")")
      else
        args = []
      end
      return subscripts(as("new", newexp, args), true)
    end

    def expr_atom
      maybe_embed_tokens(lambda do |allow_calls|
        if is("operator", "new")
          _next()
          return new_()
        end
        if is("punc")
          case @syntax[:token][:value]
            when "("
              _next()
              return subscripts(prog1(expression, lambda { expect(")") } ), allow_calls)
            when "["
              _next()
              return subscripts(array_(), allow_calls)
            when "{"
              _next()
              return subscripts(object_(), allow_calls)
          end
          unexpected()
        end
        if is("keyword", "function")
          _next()
          return subscripts(function_.call(false), allow_calls)
        end
        if HOP(ATOMIC_START_TOKEN, @syntax[:token][:type])
          atom = @syntax[:token][:type] == "regexp" ?
            as("regexp", @syntax[:token][:value][0], @syntax[:token][:value][1]) :
            as(@syntax[:token][:type], @syntax[:token][:value])
          return subscripts(prog1(atom, get_next), allow_calls)
        end
        unexpected()
      end)
    end

    def expr_list(closing, allow_trailing_comma=false, allow_empty=false)
      first = true
      a = []
      while !is("punc", closing)
        if first
          first = false
        else
          expect(",")
        end
        if allow_trailing_comma && is("punc", closing)
          break
        end
        if is("punc", ",") && allow_empty
          a.push([ "atom", "undefined" ])
        else
          a.push(expression.call(false))
        end
      end
      _next()
      return a
    end

    def array_
      as("array", expr_list("]", !@exigent_mode, true))
    end

    def object_
      first = true
      a = []
      while !is("punc", "}")
        if (first)
          first = false
        else
          expect(",")
        end
        if !@exigent_mode && is("punc", "}")
          # allow trailing comma
          break
        end
        type = @syntax[:token][:type]
        name = as_property_name()
        if type == "name" && (name == "get" || name == "set") && !is("punc", ":")
          a.push([ as_name(), function_.call(false), name ])
        else
          expect(":")
          a.push([ name, expression.call(false) ])
        end
      end
      _next()
      return as("object", a)
    end

    def as_property_name
      case @syntax[:token][:type]
        when "num", "string"
          return prog1(@syntax[:token][:value], get_next)
      end
      return as_name()
    end

    def as_name
      case @syntax[:token][:type]
        when "name", "operator", "keyword", "atom"
          return prog1(@syntax[:token][:value], get_next)
        else
          unexpected()
      end
    end

    def subscripts(expr, allow_calls)
      if is("punc", ".")
        _next()
        return subscripts(as("dot", expr, as_name()), allow_calls)
      end
      if is("punc", "[")
        _next()
        return subscripts(as("sub", expr, prog1(expression, lambda { expect("]") })), allow_calls)
      end
      if allow_calls && is("punc", "(")
        _next()
        return subscripts(as("call", expr, expr_list(")")), true)
      end
      return expr
    end

    def maybe_unary(allow_calls)
      if is("operator") && HOP(UNARY_PREFIX, @syntax[:token][:value])
        return make_unary("unary-prefix",
              prog1(@syntax[:token][:value], get_next),
              maybe_unary(allow_calls));
      end
      val = expr_atom.call(allow_calls)
      while is("operator") && HOP(UNARY_POSTFIX, @syntax[:token][:value]) && !@syntax[:token][:nlb]
        val = make_unary("unary-postfix", @syntax[:token][:value], val)
        _next()
      end
      return val
    end

    def make_unary(tag, op, expr)
      if (op == "++" || op == "--") && !is_assignable(expr)
        croak("Invalid use of " + op + " operator")
      end
      return as(tag, op, expr);
    end

    def expr_op(left, min_prec, no_in)
      op = is("operator") ? @syntax[:token][:value] : nil
      if op && op == "in" && no_in
        op = nil
      end
      prec = op != nil ? PRECEDENCE[op] : nil
      if prec != nil && prec > min_prec
        _next()
        right = expr_op(maybe_unary(true), prec, no_in)
        return expr_op(as("binary", op, left, right), min_prec, no_in)
      end
      return left
    end

    def expr_ops(no_in)
      expr_op(maybe_unary(true), 0, no_in)
    end

    def maybe_conditional(no_in)
      expr = expr_ops(no_in)
      if is("operator", "?")
        _next()
        yes = expression.call(false)
        expect(":")
        return as("conditional", expr, yes, expression.call(false, no_in))
      end
      return expr
    end

    def is_assignable(expr)
      if !@exigent_mode
        return true
      end
      case expr[0]
        when "dot", "sub", "new", "call"
          return true
        when "name"
          return expr[1] != "this"
      end
    end

    def maybe_assign(no_in)
      left = maybe_conditional(no_in)
      val = @syntax[:token][:value]
      if is("operator") && !ASSIGNMENT[val].nil?
        if is_assignable(left)
          _next()
          return as("assign", ASSIGNMENT[val], left, maybe_assign(no_in))
        end
        croak("Invalid assignment")
      end
      return left
    end

    #kpk
    def expression
      maybe_embed_tokens(lambda do |*args|
        commas, no_in = args
        if commas.nil?
          commas = true
        end
        expr = maybe_assign(no_in)
        if commas && is("punc", ",")
          _next()
          return as("seq", expr, expression.call(true, no_in))
        end
        return expr
      end)
    end

    def in_loop
      lambda do |cont|
        begin
          @syntax[:in_loop]+=1
          return cont.call()
        rescue Exception=>ex
        end
        @syntax[:in_loop]-=1
      end
    end

    def is_token?(token, type, val)
      token[:type] == type && (val == nil || token[:value] == val)
    end

    def js_error(message, line='', col='', pos='')
      raise Tokenizer::JS_Parse_Error, [message, line, col, pos]
    end

    def HOP(obj, prop)
      # in Javascript, you convert your arrays to hashes, then check the hash
      # crazy eh.
      # in Ruby, we'll just check the array
      obj.include?(prop)
    end

    def prog1(ret, *args)
      if ret.is_a? Proc
        ret = ret.call
      end
      for arg in args
        arg.call
      end
      ret
    end

    def member(name, array)
      array.include?(name)
    end

  end

end