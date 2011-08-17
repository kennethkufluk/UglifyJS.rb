# /* -----[
#    - compress foo["bar"] into foo.bar,
#    - remove block brackets {} where possible
#    - join consecutive var declarations
#    - various optimizations for IFs:
#      - if (cond) foo(); else bar();  ==>  cond?foo():bar();
#      - if (cond) foo();  ==>  cond&&foo();
#      - if (foo) return bar(); else return baz();  ==> return foo?bar():baz(); # also for throw
#      - if (foo) return bar(); else something();  ==> {if(foo)return bar();something()}
#    ]----- */

require "uglifyjs/ast_walker"
require "uglifyjs/scope"

module Squeeze

  def warn(msg)
    puts msg
  end

  def best_of(ast1, ast2)
    return gen_code(ast1).length > gen_code(ast2[0] == "stat" ? ast2[1] : ast2).length ? ast2 : ast1
  end

  def last_stat(b)
    if b[0] == "block" && b[1] && b[1].length > 0
      return b[1][b[1].length - 1]
    end
    return b
  end

  def aborts(t=nil)
    if t
      t = last_stat(t)
      if t[0] == "return" || t[0] == "break" || t[0] == "continue" || t[0] == "throw"
        return true
      end
    end
  end

  def boolean_expr(expr)
    return ( (expr[0] == "unary-prefix" &&
        member(expr[1], [ "!", "delete" ])) ||

       (expr[0] == "binary" &&
        member(expr[1], [ "in", "instanceof", "==", "!=", "===", "!==", "<", "<=", ">=", ">" ])) ||

       (expr[0] == "binary" &&
        member(expr[1], [ "&&", "||" ]) &&
        boolean_expr(expr[2]) &&
        boolean_expr(expr[3])) ||

       (expr[0] == "conditional" &&
        boolean_expr(expr[2]) &&
        boolean_expr(expr[3])) ||

       (expr[0] == "assign" &&
        expr[1] === true &&
        boolean_expr(expr[3])) ||

       (expr[0] == "seq" &&
        boolean_expr(expr[expr.length - 1]))
           )
  end

  def make_conditional(c, t, e)
    make_real_conditional = lambda do
      if c[0] == "unary-prefix" && c[1] == "!"
        return e ? [ "conditional", c[2], e, t ] : [ "binary", "||", c[2], t ]
      else
        return e ? [ "conditional", c, t, e ] : [ "binary", "&&", c, t ]
      end
    end
    # shortcut the conditional if the expression has a constant value
    return when_constant.call(c, lambda do |ast, val|
      warn_unreachable(val ? e : t)
      return (val ? t : e)
    end, make_real_conditional)
  end

  def is_string(node)
    return (node[0] == "string" ||
      node[0] == "unary-prefix" && node[1] == "typeof" ||
      node[0] == "binary" && node[1] == "+" &&
      (is_string(node[2]) || is_string(node[3])))
  end

  def when_constant

    # # this can only evaluate constant expressions.  If it finds anything
    # # not constant, it throws $NOT_CONSTANT.
    evaluate = lambda do |expr|
      case expr[0]
        when "string", "num"
          return expr[1]
        when "name", "atom"
          case expr[1]
            when "true"
              return true
            when "false"
              return false
          end
        when "unary-prefix"
          case expr[1]
            when "!"
              return !evaluate(expr[2])
            when "typeof"
              return typeof evaluate(expr[2])
            when "~"
              return ~evaluate(expr[2])
            when "-"
              return -evaluate(expr[2])
            when "+"
              return +evaluate(expr[2])
          end
        when "binary"
          left = expr[2]
          right = expr[3]
          case expr[1]
            when "&&"
              return evaluate(left) &&   evaluate(right)
            when "||"
              return evaluate(left) ||   evaluate(right)
            when "|"
              return evaluate(left) |    evaluate(right)
            when "&"
              return evaluate(left) &    evaluate(right)
            when "^"
              return evaluate(left) ^    evaluate(right)
            when "+"
              return evaluate(left) +    evaluate(right)
            when "*"
              return evaluate(left) *    evaluate(right)
            when "/"
              return evaluate(left) /    evaluate(right)
            when "-"
              return evaluate(left) -    evaluate(right)
            when "<<"
              return evaluate(left) <<   evaluate(right)
            when ">>"
              return evaluate(left) >>   evaluate(right)
            when ">>>"
              warn "Zero-fill right shift operator (>>>) not available."
              return evaluate(left) >>  evaluate(right)
            when "=="
              return evaluate(left) ==   evaluate(right)
            when "==="
              return evaluate(left) ===  evaluate(right)
            when "!="
              return evaluate(left) !=   evaluate(right)
            when "!=="
              return evaluate(left) !=  evaluate(right)
            when "<"
              return evaluate(left) <    evaluate(right)
            when "<="
              return evaluate(left) <=   evaluate(right)
            when ">"
              return evaluate(left) >    evaluate(right)
            when ">="
              return evaluate(left) >=   evaluate(right)
            when "in"
              return evaluate(right).include? evaluate(left)
            when "instanceof"
              warn "instanceof not yet implemented."
              return false
              #return evaluate(left) instanceof evaluate(right)
          end
      end
      raise 'NOT CONSTANT'
    end

    return lambda do |expr, yes, no|
      begin
        val = evaluate(expr)
        case typeof(val)
          when "string"
            ast =  [ "string", val ]
          when "number"
            ast =  [ "num", val ]
          when "boolean"
            ast =  [ "name", String(val) ]
          else
            raise "Can't handle constant of type: " + (typeof val)
        end
        return yes.call(expr, ast, val)
      rescue Exception=>ex
        if (ex === "NOT_CONSTANT")
          if (expr[0] == "binary" &&
              (expr[1] == "===" || expr[1] == "!==") &&
              ((is_string(expr[2]) && is_string(expr[3])) ||
              (boolean_expr(expr[2]) && boolean_expr(expr[3]))))
            expr[1] = expr[1].substr(0, 2)
          elsif (no && expr[0] == "binary" &&
             (expr[1] == "||" || expr[1] == "&&"))
              # the whole expression is not constant but the lval may be...
              begin
                lval = evaluate(expr[2])
                expr = ((expr[1] == "&&" && (lval ? expr[3] : lval))    ||
                  (expr[1] == "||" && (lval ? lval    : expr[3])) ||
                  expr)
              rescue Exception => ex2
                # IGNORE... lval is not constant
              end
          end
          return no ? no.call(expr, expr) : nil
        else
          raise ex
        end
      end
    end
  end

  def warn_unreachable(ast)
    if !empty(ast)
      warn("Dropping unreachable code: " + gen_code(ast, true))
    end
  end

  def prepare_ifs(ast)
    w = ASTWalker.new(ast)
    walk = w.walk
    # In this first pass, we rewrite ifs which abort with no else with an
    # if-else.  For example:
    #
    # if (x) {
    #     blah();
    #     return y;
    # }
    # foobar();
    #
    # is rewritten into:
    #
    # if (x) {
    #     blah();
    #     return y;
    # } else {
    #     foobar();
    # }
    redo_if = lambda do |statements|
      statements = MAP(statements, walk)

      for i in 0...statements.length
        fi = statements[i]
        next if fi[0] != "if"

        next if fi[3] && walk(fi[3])

        t = walk(fi[2])
        next if !aborts(t)

        conditional = walk(fi[1])

        e_body = statements.slice(i + 1)
        if e_body.length == 1
          e = e_body[0]
        else
          e = [ "block", e_body ]
        end

        ret = statements.slice(0, i).concat([ [
          fi[0],    # "if"
          conditional,    # conditional
          t,        # then
          e         # else
        ] ])

        return redo_if(ret)
      end

      return statements
    end

    redo_if_lambda = lambda do |ast, name, args, body|
      body = redo_if(body)
      return [ ast[0], name, args.slice(), body ]
    end

    redo_if_block = lambda do |ast, statements|
      out = [ ast[0] ]
      if statements != null
        out.push(redo_if(statements))
      end
      return out
    end

    return w.with_walkers({
      "defun"=> redo_if_lambda,
      "function"=> redo_if_lambda,
      "block"=> redo_if_block,
      "splice"=> redo_if_block,
      "toplevel"=> lambda do |ast, statements|
        return [ ast[0], redo_if(statements) ]
      end,
      "try"=> lambda do |ast, t, c, f|
        return [
          ast[0],
          redo_if(t),
          c != null ? [ c[0], redo_if(c[1]) ] : nil,
          f != null ? redo_if(f) : bil
        ]
      end
    }, lambda do
      return walk(ast)
    end)
  end

  class Squeeze

    include Util

    def initialize(ast, options={})
      @options = defaults(options, {
        :make_seqs   => true,
        :dead_code   => true,
        :keep_comps  => true,
        :no_warnings => false
      })

      @w = ASTWalker.new(ast)
      @walk = @w.walk
      @scope = nil
      @ast = ast
      @ast = prepare_ifs(@ast)
      @ast = AddScope.new(@ast).go
    end

    def go
      return @w.with_walkers({
        "sub"=> lambda do |ast, expr, subscript|
          if subscript[0] == "string"
            name = subscript[1]
            if is_identifier(name)
              return [ "dot", @walk.call(expr), name ]
            elsif /^[1-9][0-9]*$/=~name || name === "0"
              return [ "sub", @walk.call(expr), [ "num", parseInt(name, 10) ] ]
            end
          end
        end,
        "if"=> make_if,
        "toplevel"=> lambda do |ast, body|
          return [ "toplevel", with_scope(ast.scope, lambda do
            return tighten(MAP(body, @walk))
          end) ]
        end,
        "switch"=> lambda do |ast, expr, body|
          last = body.length - 1
          return [ "switch", @walk.call(expr), MAP(body, lambda do |branch, i|
            var block = tighten(MAP(branch[1], @walk))
            if i == last && block.length > 0
              node = block[block.length - 1]
              if node[0] == "break" && !node[1]
                block.pop()
              end
            end
            return [ branch[0] ? @walk.call(branch[0]) : null, block ];
          end) ]
        end,
        "function"=> _lambda,
        "defun"=> _lambda,
        "block"=> lambda do |ast, body|
          if body
            return rmblock([ "block", tighten(MAP(body, @walk)) ])
          end
        end,
        "binary"=> lambda do |ast, op, left, right|
          return when_constant.call([ "binary", op, @walk.call(left), @walk.call(right) ], lambda do |c|
            #yes
            return best_of(@walk.call(c), ast)
          end, lambda do
            #no
            return ast
          end)
        end,
        "conditional"=> lambda do |ast, c, t, e|
          return make_conditional(@walk.call(c), @walk.call(t), @walk.call(e))
        end,
        "try"=> lambda do |ast, t, c, f|
          return [
            "try",
            tighten(MAP(t, @walk)),
            c ? [ c[0], tighten(MAP(c[1], @walk)) ] : nil,
            f ? tighten(MAP(f, @walk)) : nil
          ];
        end,
        "unary-prefix"=> lambda do |ast, op, expr|
          expr = @walk.call(expr)
          ret = [ "unary-prefix", op, expr ]
          if op == "!"
            ret = best_of(ret, negate(expr))
          end
          return when_constant.call(ret, lambda do |ast, val|
            return @walk.call(ast) # it's either true or false, so minifies to !0 or !1
          end, lambda { return ret } )
        end,
        "name"=> lambda do |ast, name|
          case name
            when "true"
              return [ "unary-prefix", "!", [ "num", 0 ]]
            when "false"
              return [ "unary-prefix", "!", [ "num", 1 ]]
          end
        end,
        "new"=> lambda do |ast, ctor, args|
          if ctor[0] == "name" && ctor[1] == "Array" && !scope.has("Array")
            if args.length != 1
              return [ "array", args ]
            else
              return [ "call", [ "name", "Array" ], args ]
            end
          end
        end,
        "call"=> lambda do |ast, expr, args|
          if expr[0] == "name" && expr[1] == "Array" && args.length != 1 && !scope.has("Array")
            return [ "array", args ]
          end
        end,
        "while"=> _do_while
      }, lambda do
        return @walk.call(@ast)
      end)
    end

    def negate(c)
      not_c = [ "unary-prefix", "!", c ]
      case c[0]
        when "unary-prefix"
          return c[1] == "!" && boolean_expr(c[2]) ? c[2] : not_c
        when "seq"
          c = slice(c)
          c[c.length - 1] = negate(c[c.length - 1])
          return c
        when "conditional"
          return best_of(not_c, [ "conditional", c[1], negate(c[2]), negate(c[3]) ])
        when "binary"
          op = c[1]
          left = c[2]
          right = c[3]
          if !@options[:keep_comps]
            case (op)
              when "<="
                return [ "binary", ">", left, right ]
              when "<"
                return [ "binary", ">=", left, right ]
              when ">="
                return [ "binary", "<", left, right ]
              when ">"
                return [ "binary", "<=", left, right ]
            end
          end
          case op
            when "=="
              return [ "binary", "!=", left, right ]
            when "!="
              return [ "binary", "==", left, right ]
            when "==="
              return [ "binary", "!==", left, right ]
            when "!=="
              return [ "binary", "===", left, right ]
            when "&&"
              return best_of(not_c, [ "binary", "||", negate(left), negate(right) ])
            when "||"
              return best_of(not_c, [ "binary", "&&", negate(left), negate(right) ])
          end
      end
      return not_c
    end

    def with_scope(s, cont)
      _scope = @scope
      @scope = s
      ret = cont.call()
      ret.scope = s
      @scope = _scope
      return ret
    end

    def rmblock(block)
      if block != null && block[0] == "block" && block[1]
        if block[1].length == 1
          block = block[1][0]
        elsif block[1].length == 0
          block = [ "block" ]
        end
      end
      return block
    end

    def _lambda
      lambda do |ast, name, args, body|
        is_defun = ast[0] == "defun"
        body = with_scope(body.scope, lambda do
          ret = tighten(MAP(body, @walk), "lambda")
          if !is_defun && name && !HOP(scope.refs, name)
            name = nil
          end
          return ret
        end)
        return [ ast[0], name, args, body ]
      end
    end

    # we get here for blocks that have been already transformed.
    # this def does a few things:
    # 1. discard useless blocks
    # 2. join consecutive var declarations
    # 3. remove obviously dead code
    # 4. transform consecutive statements using the comma operator
    # 5. if block_type == "lambda" and it detects constructs like if(foo) return ... - rewrite like if (!foo) { ... }
    def tighten(statements, block_type=nil)
      statements = statements.reduce(lambda do |a, stat|
        if stat[0] == "block"
          if stat[1]
            a.push.apply(a, stat[1])
          end
        else
          a.push(stat)
        end
        return a
      end, [])

      statements = (lambda do |a, prev|
        statements.each do |cur|
          if (prev && ((cur[0] == "var" && prev[0] == "var") ||
                 (cur[0] == "const" && prev[0] == "const")))
            prev[1] = prev[1].concat(cur[1])
          else
            a.push(cur)
            prev = cur
          end
        end
        return a
      end).call([])

      if @options[:dead_code]
        statements = (lambda do |a, has_quit|
          statements.each do |st|
            if has_quit
              if member(st[0], [ "function", "defun" , "var", "const" ])
                a.push(st)
              elsif !@options[:no_warnings]
                warn_unreachable(st)
              end
            else
              a.push(st)
              if member(st[0], [ "return", "throw", "break", "continue" ])
                has_quit = true
              end
            end
          end
          return a
        end).call([])
      end

      if @options[:make_seqs]
        statements = (lambda do |a, prev|
          statements.each do |cur|
            if prev && prev[0] == "stat" && cur[0] == "stat"
              prev[1] = [ "seq", prev[1], cur[1] ]
            else
              a.push(cur)
              prev = cur
            end
          end
          return a
        end).call([])
      end

      if block_type == "lambda"
        statements = (lambda do |i, a, stat|
          while i < statements.length
            i = i+1
            stat = statements[i]
            if stat[0] == "if" && !stat[3]
              if stat[2][0] == "return" && stat[2][1].nil?
                a.push(make_if.call(negate(stat[1]), [ "block", statements.slice(i) ]))
                break
              end
              last = last_stat(stat[2])
              if (last[0] == "return" && last[1].nil?)
                a.push(make_if.call(stat[1], [ "block", stat[2][1].slice(0, -1) ], [ "block", statements.slice(i) ]))
                break
              end
            end
            a.push(stat)
          end
          return a
        end).call(0, [])
      end

      return statements
    end

    def make_if
      lambda do |ast, c, t, e|
        return when_constant.call(c, lambda do |ast, val|
          if val
            warn_unreachable(e)
            return t
          else
            warn_unreachable(t)
            return e
          end
        end, lambda do
          return make_real_if(c, t, e)
        end)
      end
    end

    def make_real_if(c, t, e)
      c = @walk.call(c)
      t = @walk.call(t)
      e = @walk.call(e)

      if empty(t)
        c = negate(c)
        t = e
        e = nil
      elsif empty(e)
        e = nil
      else
        # if we have both else and then, maybe it makes sense to switch them?
        (lambda do
          a = gen_code(c)
          n = negate(c)
          b = gen_code(n)
          if b.length < a.length
            tmp = t
            t = e
            e = tmp
            c = n
          end
        end).call()
      end
      if empty(e) && empty(t)
        return [ "stat", c ]
      end
      ret = [ "if", c, t, e ]
      if t[0] == "if" && empty(t[3]) && empty(e)
        ret = best_of(ret, @walk.call([ "if", [ "binary", "&&", c, t[1] ], t[2] ]))
      elsif t[0] == "stat"
        if e
          if e[0] == "stat"
            ret = best_of(ret, [ "stat", make_conditional(c, t[1], e[1]) ])
          end
        else
          ret = best_of(ret, [ "stat", make_conditional(c, t[1]) ])
        end
      elsif e && t[0] == e[0] && (t[0] == "return" || t[0] == "throw") && t[1] && e[1]
        ret = best_of(ret, [ t[0], make_conditional(c, t[1], e[1] ) ])
      elsif e && aborts(t)
        ret = [ [ "if", c, t ] ]
        if e[0] == "block"
          if e[1]
            ret = ret.concat(e[1])
          end
        else
          ret.push(e)
        end
        ret = @walk.call([ "block", ret ])
      elsif t && aborts(e)
        ret = [ [ "if", negate(c), e ] ]
        if t[0] == "block"
          if t[1]
            ret = ret.concat(t[1])
          end
        else
          ret.push(t)
        end
        ret = @walk.call([ "block", ret ])
      end
      return ret
    end

    def _do_while
      lambda do |ast, cond, body|
        return when_constant.call(cond, lambda do |cond, val|
          if !val
            warn_unreachable(body)
            return [ "block" ]
          else
            return [ "for", nil, nil, nil, @walk.call(body) ]
          end
        end)
      end
    end

  end

end