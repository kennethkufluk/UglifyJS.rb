# /* -----[ re-generate code from the AST ]----- */

require 'parsejs/util'
require 'uglifyjs/ast_walker'

module GenCode

  DOT_CALL_NO_PARENS = [
    "name",
    "array",
    "object",
    "string",
    "dot",
    "sub",
    "call",
    "regexp"
  ]

  class Generator

    include Util

    def initialize(ast, options = {})
      @ast = ast
      @options = {
        :indent_start => 0,
        :indent_level => 4,
        :quote_keys   => false,
        :space_colon  => false,
        :beautify     => false,
        :ascii_only   => false,
        :inline_script=> false
      }.merge(options)
      @beautify = options[:beautify]
      @indentation = 0
      @newline = @beautify ? "\n" : ""
      @space = @beautify ? " " : ""

      @stack = []

    end

    def go
      return make.call(@ast)
    end



    def make_string(str, ascii_only)
      dq = 0
      sq = 0
      str = str.gsub(/[\\\f\b\n\r\t\x22\x270x20280x2029]/) do |s|
        case s
          when "\\"
            next "\\\\"
          when "\b"
            next "\\b"
          when "\f"
            next "\\f"
          when "\n"
            next '\\n'
          when "\r"
            next "\\r"
          when "\t"
            next "\\t"
          when "\u2028"
            next "\\u2028"
          when "\u2029"
            next "\\u2029"
          when '"'
            dq+=1
            next '"'
          when "'"
            sq+=1
            next "'"
        end
        next s
      end
      str = to_ascii(str) if (ascii_only)
      return (if dq > sq
        "'" + str.gsub(/\x27/, "\\'") + "'"
      else
        '"' + str.gsub(/\x22/, '\\"') + '"'
      end)
    end

    ASCII = (0x0080..0xffff).to_a.flatten.pack('U*').freeze
    def to_ascii(str)
      return str.gsub(/[#{ASCII}]/, lambda do |ch|
        code = ch[0].to_s(16)
        while (code.length < 4)
          code = "0" + code
        end
        return "\\u" + code
      end)
    end

    SPLICE_NEEDS_BRACKETS = [ "if", "while", "do", "for", "for-in", "with" ]


    def encode_string(str)
      ret = make_string(str, @options[:ascii_only])
      if @options[:inline_script]
        ret = ret.gsub(/<\x2fscript([>\/\t\n\f\r ])/i, "<\\/script$1")
      end
      return ret
    end

    def make_name
      lambda do |name, *args|
        name = name.to_s
        if (@options[:ascii_only])
          name = to_ascii(name)
        end
        return name
      end
    end

    def indent
      lambda do |*lines|
        line = lines[0]
        if line.nil?
          line = ""
        end
        if @beautify
          line = repeat_string(" ", @options[:indent_start] + @indentation * @options[:indent_level]) + line
        end
        return line
      end
    end

    def with_indent(cont, incr=nil, *args)
      if incr.nil?
        incr = 1
      end
      @indentation += incr
      begin
        return cont.call(args)
      ensure
        @indentation -= incr
      end
    end

    def add_spaces(a)
      if (@beautify)
        return a.join(" ")
      end
      b = []
      for i in (0...a.length)
        nextch = a[i + 1]
        b.push(a[i])
        if (nextch &&
            ((/[a-z0-9_\x24]$/i =~ a[i].to_s && /^[a-z0-9_\x24]/i =~ nextch.to_s) ||
             (/[\+\-]$/ =~ a[i].to_s && /^[\+\-]/ =~ nextch.to_s)))
          b.push(" ")
        end
      end
      return b.join("")
    end

    def add_commas(a)
      return a.join("," + @space)
    end

    def parenthesize(expr, *args)
      gen = make.call(expr)
      for i in (0...args.length)
        el = args[i]
        if ((el.is_a?(Proc) && el.call(expr)) || expr[0] == el)
          return "(" + gen + ")"
        end
      end
      return gen
    end

    def best_of(a)
      if (a.length == 1)
        return a[0]
      end
      if (a.length == 2)
        b = a[1]
        a = a[0]
        return (a.length <= b.length) ? a : b
      end
      return best_of([ a[0], best_of(a.slice(1..-1)) ])
    end

    def needs_parens(expr)
      if (expr[0] == "function" || expr[0] == "object")
        # dot/call on a literal def requires the
        # def literal itself to be parenthesized
        # only if it's the first "thing" in a
        # statement.  This means that the parent is
        # "stat", but it could also be a "seq" and
        # we're the first in this "seq" and the
        # parent is "stat", and so on.  Messy stuff,
        # but it's worth the trouble.
        a = Array.new(@stack)
        _self = a.pop()
        p = a.pop()
        while (p)
          return true if p[0] == "stat"
          if (((p[0] == "seq" || p[0] == "call" || p[0] == "dot" || p[0] == "sub" || p[0] == "conditional") && p[1] === _self) ||
              ((p[0] == "binary" || p[0] == "assign" || p[0] == "unary-postfix") && p[2] === _self))
            _self = p
            p = a.pop()
          else
            return false
          end
        end
      end
      return !HOP(DOT_CALL_NO_PARENS, expr[0])
    end

    def make_num(num)
      str = num.to_s
      a = [ str.gsub(/^0\./, ".") ]
      if num.floor === num
        num = num.to_i
        a.push("0x" + num.to_s(16).downcase, # probably pointless
               "0" + num.to_s(8)) # same.
        if m = /^(.*?)(0+)$/.match(num.to_s)
          a.push(m[1] + "e" + m[2].length.to_s)
        end
      elsif m = /^0?\.(0+)(.*)$/.match(str)
        a.push(m[2] + "e-" + (m[1].length + m[2].length),
             str.substr(str.indexOf(".")))
      end
      return best_of(a)
    end

    # GENERATORS
    def generate_string(str)
      encode_string(str)
    end
    def generate_num(num)
      make_num(num)
    end
    def generate_name(*args)
      make_name.call(*args)
    end
    def generate_toplevel(statements)
      return make_block_statements(statements).join(@newline + @newline)
    end
    def generate_splice(statements)
      parent = @stack[@stack.length - 2][0]
      if (HOP(SPLICE_NEEDS_BRACKETS, parent))
        # we need block brackets in this case
        return make_block.apply(this, arguments)
      else
        return MAP(make_block_statements(statements, true),
             lambda do |line, i|
               # the first line is already indented
               return i > 0 ? indent.call(line) : line
             end).join(@newline)
      end
    end
    def generate_block(*args)
      make_block(*args)
    end
    def generate_var(defs)
      return "var " + add_commas(MAP(defs, make_1vardef)) + ";"
    end
    def generate_const(defs)
      return "const " + add_commas(MAP(defs, make_1vardef)) + ";"
    end
    def generate_try(tr, ca, fi)
      out = [ "try", make_block(tr) ]
      if ca
        out.push("catch", "(" + ca[0] + ")", make_block(ca[1]))
      end
      if fi
        out.push("finally", make_block(fi))
      end
      return add_spaces(out)
    end
    def generate_throw(expr)
      return add_spaces([ "throw", make.call(expr) ]) + ";"
    end
    def generate_new(ctor, args)
      args = args.length > 0 ? "(" + add_commas(MAP(args, make)) + ")" : ""
      return add_spaces([ "new", parenthesize(ctor, "seq", "binary", "conditional", "assign", lambda do |expr|
        w = ASTWalker.new(@ast)
        has_call = false
        begin
          w.with_walkers({
            "call" => lambda { puts "weird exception" and raise has_call=true },
            "function" => lambda { puts "weird function" and return self }
          }, lambda do
            w.walk.call(expr)
          end)
          # kpk: I've added a return false here, because uglify assumes
          # that a no return will yield undefined==false
          # whereas ruby will return the result of the last statement
          return false
        rescue Exception=>ex
          if (has_call)
            puts "weird exception used for control #{ex.inspect}"
            return true
          end
          raise ex
        end
      end) + args ])
    end
    def generate_switch(expr, body)
      return add_spaces([ "switch", "(" + make.call(expr) + ")", make_switch_block(body) ])
    end
    def generate_break(label=nil)
      out = "break"
      if !label.nil?
        out += " " + make_name.call(label)
      end
      return out + ";"
    end
    def generate_continue(label=nil)
      out = "continue"
      if !label.nil?
        out += " " + make_name.call(label)
      end
      return out + ";"
    end
    def generate_conditional(co, th, el)
      return add_spaces([ parenthesize(co, "assign", "seq", "conditional"), "?",
              parenthesize(th, "seq"), ":",
              parenthesize(el, "seq") ])
    end
    def generate_assign(op, lvalue, rvalue)
      if (op && op != true)
        op += "="
      else
        op = "="
      end
      return add_spaces([ make.call(lvalue), op, parenthesize(rvalue, "seq") ])
    end
    def generate_dot(expr, *args)
      out = make.call(expr)
      i = 0
      if expr[0] == "num"
        if !(/\./ =~ expr[1])
          out += "."
        end
      elsif needs_parens(expr)
        out = "(" + out + ")"
      end
      while i < args.length
        out += "." + make_name.call(args[i])
        i+=1
      end
      return out
    end
    def generate_call(func, args)
      f = make.call(func)
      if (needs_parens(func))
        f = "(" + f + ")"
      end
      return f + "(" + add_commas(MAP(args, lambda do |expr, *args|
        return parenthesize(expr, "seq")
      end)) + ")"
    end
    def generate_function(*args)
      make_function(*args)
    end
    def generate_defun(*args)
      make_function(*args)
    end
    def generate_if(co, th, el)
      out = [ "if", "(" + make.call(co) + ")", el ? make_then(th) : make.call(th) ]
      if (el)
        out.push("else", make.call(el))
      end
      return add_spaces(out)
    end
    def generate_for(init, cond, step, block)
      out = [ "for" ]
      init = (!init.nil? ? make.call(init) : "").sub(/;*\s*$/, ";" + @space)
      cond = (!cond.nil? ? make.call(cond) : "").sub(/;*\s*$/, ";" + @space)
      step = (!step.nil? ? make.call(step) : "").sub(/;*\s*$/, "")
      args = init + cond + step
      if (args == "; ; ")
        args = ";;"
      end
      out.push("(" + args + ")", make.call(block))
      return add_spaces(out)
    end
    def generate_for_in(vvar, key, hash, block)
      return add_spaces([ "for", "(" +
              (vvar ? make.call(vvar).sub(/;+$/, "") : make.call(key)),
              "in",
              make.call(hash) + ")", make.call(block) ])
    end
    def generate_while(condition, block)
      return add_spaces([ "while", "(" + make.call(condition) + ")", make.call(block) ])
    end
    def generate_do(condition, block)
      return add_spaces([ "do", make.call(block), "while", "(" + make.call(condition) + ")" ]) + ";"
    end
    def generate_return(expr=nil)
      out = [ "return" ]
      if !expr.nil?
        out.push(make.call(expr))
      end
      return add_spaces(out) + ";"
    end
    def generate_binary(operator, lvalue, rvalue)
      left = make.call(lvalue)
      right = make.call(rvalue)
      # # XXX: I'm pretty sure other cases will bite here.
      # #      we need to be smarter.
      # #      adding parens all the time is the safest bet.
      if (member(lvalue[0], [ "assign", "conditional", "seq" ]) ||
          lvalue[0] == "binary" && PRECEDENCE[operator] > PRECEDENCE[lvalue[1]])
        left = "(" + left + ")"
      end
      if (member(rvalue[0], [ "assign", "conditional", "seq" ]) ||
          rvalue[0] == "binary" && PRECEDENCE[operator] >= PRECEDENCE[rvalue[1]] &&
          !(rvalue[1] == operator && member(operator, [ "&&", "||", "*" ])))
        right = "(" + right + ")"
      elsif (!@beautify && @options[:inline_script] && (operator == "<" || operator == "<<") &&
         rvalue[0] == "regexp" && /^script/i =~ rvalue[1])
        right = " " + right
      end
      return add_spaces([ left, operator, right ])
    end
    def generate_unary_prefix(operator, expr)
      val = make.call(expr)
      if (!(expr[0] == "num" || (expr[0] == "unary-prefix" && !HOP(OPERATORS, operator + expr[1])) || !needs_parens(expr)))
        val = "(" + val + ")"
      end
      return operator + ((is_alphanumeric_char?(operator[0].chr)) ? " " : "") + val
    end
    def generate_unary_postfix(operator, expr)
      val = make.call(expr)
      if (!(expr[0] == "num" || (expr[0] == "unary-postfix" && !HOP(OPERATORS, operator + expr[1])) || !needs_parens(expr)))
        val = "(" + val + ")"
      end
      return val + operator
    end
    def generate_sub(expr, subscript)
      hash = make.call(expr)
      if (needs_parens(expr))
        hash = "(" + hash + ")"
      end
      return hash + "[" + make.call(subscript) + "]"
    end
    def generate_object(props)
      if (props.length == 0)
        return "{}"
      end
      return "{" + @newline + with_indent(lambda do
        return MAP(props, lambda do |p, *args|
          if (p.length == 3)
            # getter/setter.  The name is in p[0], the arg.list in p[1][2], the
            # body in p[1][3] and type ("get" / "set") in p[2].
            return indent.call(make_function(p[0], p[1][2], p[1][3], p[2]))
          end
          key = p[0]
          val = parenthesize(p[1], "seq")
          if @options[:quote_keys]
            key = encode_string(key)
          elsif ((key.is_a?(Fixnum)) &&
               parseFloat(key) >= 0)
            key = make_num(key)
          elsif (!is_identifier(key))
            key = encode_string(key)
          end
          return indent.call(add_spaces(@beautify && @options[:space_colon] ?
                 [ key, ":", val ] :
                 [ key + ":", val ]))
        end).join("," + @newline)
      end) + @newline + indent.call("}")
    end
    def generate_regexp(rx, mods)
      return "/" + rx + "/" + mods
    end
    def generate_array(elements)
      if (elements.length == 0)
        return "[]"
      end
      return add_spaces([ "[", add_commas(MAP(elements, lambda do |el, *args|
        if (!@beautify && el[0] == "atom" && el[1] == "undefined")
          return ""
        end
        return parenthesize(el, "seq")
      end)), "]" ])
    end
    def generate_stat(stmt)
      return make.call(stmt).sub(/;*\s*$/, ";")
    end
    def generate_seq(*args)
      return add_commas(MAP(args, make))
    end
    def generate_label(name, block)
      return add_spaces([ make_name.call(name), ":", make.call(block) ])
    end
    def generate_with(expr, block)
      return add_spaces([ "with", "(" + make.call(expr) + ")", make.call(block) ])
    end
    def generate_atom(name)
      return make_name.call(name)
    end

    # # The squeezer replaces "block"-s that contain only a single
    # # statement with the statement itself; technically, the AST
    # # is correct, but this can create problems when we output an
    # # IF having an ELSE clause where the THEN clause ends in an
    # # IF *without* an ELSE block (then the outer ELSE would refer
    # # to the inner IF).  This def checks for this case and
    # # adds the block brackets if needed.
    def make_then(th)
      if (th[0] == "do")
        # https:#github.com/mishoo/UglifyJS/issues/#issue/57
        # IE croaks with "syntax error" on code like this:
        #     if (foo) do ... while(cond); else ...
        # we need block brackets around do/while
        return make.call([ "block", [ th ]])
      end
      b = th
      while (true)
        type = b[0]
        if (type == "if")
          if (!b[3])
            # no else, we must add the block
            return make.call([ "block", [ th ]])
          end
          b = b[3]
        elsif (type == "while" || type == "do")
          b = b[2]
        elsif (type == "for" || type == "for-in")
          b = b[4]
        else
          break
        end
      end
      return make.call(th)
    end

    def make_function(name, args=nil, body=nil, keyword=nil)
      out = keyword || "function"
      if (name)
        out += " " + make_name.call(name)
      end
      out += "(" + add_commas(MAP(args, make_name)) + ")"
      return add_spaces([ out, make_block(body) ])
    end

    def make_block_statements(statements, noindent=false)
      a = []
      last = statements.length - 1
      for i in (0..last)
        stat = statements[i]
        code = make.call(stat)
        if (code != ";")
          if (!@beautify && i == last)
            if ((stat[0] == "while" && empty(stat[2])) ||
                (member(stat[0], [ "for", "for-in"] ) && empty(stat[4])) ||
                (stat[0] == "if" && empty(stat[2]) && !stat[3]) ||
                (stat[0] == "if" && stat[3] && empty(stat[3])))
              code = code.sub(/;*\s*$/, ";")
            else
              code = code.sub(/;+\s*$/, "")
            end
          end
          a.push(code)
        end
      end
      return noindent ? a : MAP(a, indent)
    end

    def make_switch_block(body)
      n = body.length
      if (n == 0)
        return "{}"
      end
      return "{" + @newline + MAP(body, lambda do |branch, i|
        has_body = branch[1].length > 0
        code = with_indent(lambda do
            return indent.call(branch[0] ?
                  add_spaces([ "case", make.call(branch[0]) + ":" ]) :
                  "default:")
          end, 0.5) + (has_body ? @newline + with_indent(lambda do
            return make_block_statements(branch[1]).join(@newline)
          end) : "")
        if (!@beautify && has_body && i < n - 1)
          code += ";"
        end
        return code
      end).join(@newline) + @newline + indent.call("}")
    end

    def make_block(statements)
      return ";" if (!statements)
      return "{}" if (statements.length == 0)
      return "{" + @newline + with_indent(lambda do
        return make_block_statements(statements).join(@newline)
      end) + @newline + indent.call("}")
    end

    def make_1vardef
      lambda do |vardef, *args|
        name = vardef[0]
        val = vardef[1]
        if !val.nil?
          name = add_spaces([ make_name.call(name), "=", parenthesize(val, "seq") ])
        end
        return name
      end
    end


    def make
      lambda do |node, *args|
        type = node[0]
        #gen = generators[type]
        #if (!gen)
        #  throw new Error("Can't find generator for \"" + type + "\"")
        @stack.push(node)
        ret = self.send(("generate_"+type.gsub(/-/, '_')).to_sym, *(node.slice(1..-1)))
        @stack.pop()
        return ret
      end
    end

  end


  # /* -----[ Utilities ]----- */

  def repeat_string(str, i)
    return "" if (i <= 0)
    return str if (i == 1)
    d = repeat_string(str, i >> 1)
    d += d
    d += str if (i & 1)
    return d
  end
end