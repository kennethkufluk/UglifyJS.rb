# /* -----[ helper for AST traversal ]----- */

require 'uglifyjs/util'

class ASTWalker

  include Util

  def initialize(ast)
    @user = {}
    @stack = []
    @ast = ast
  end

  def walk
    lambda do |ast|
      if ast.nil?
        return nil
      end
      begin
        @stack.push(ast)
        type = ast[0]
        gen = @user[type]
        if gen
          ret = gen.call(ast[0], *(ast.slice(1..-1)))
          if ret != nil
            return ret
          end
        end
        gen = walkers[type]
        return gen.call(ast[0], *(ast.slice(1..-1)))
      ensure
        @stack.pop()
      end
    end
  end

  def with_walkers(walkers, cont)
    save = {}
    for i in walkers
      if HOP(walkers, i)
        save[i] = @user[i]
        @user[i] = walkers[i]
      end
    end
    ret = cont.call()
    for i in save
      if HOP(save, i)
        if !save[i]
          delete @user[i]
        else
          @user[i] = save[i]
        end
      end
    end
    return ret
  end

  def parent
    @stack[@stack.length - 2]  # last one is current node
  end

  def stack
    @stack
  end

  protected

  def _vardefs
    lambda do |ast0, defs|
      return [ ast0, map(defs, lambda do |mydef|
        a = [ mydef[0] ]
        if mydef.length > 1
          a[1] = walk.call(mydef[1])
        end
        return a
      end ) ]
    end
  end
  def _block
    lambda do |ast0, statements|
      out = [ ast0 ]
      if statements != null
        out.push(map(statements, walk))
      end
      return out
    end
  end
  def walkers
    @walkers ||= {
      #kpk make into lamdas?
      # or just make a new class?
      "string" => lambda do |ast0, str|
        return [ ast0, str ]
      end,
      "num" => lambda do |ast0, num|
        return [ ast0, num ]
      end,
      "name" => lambda do |ast0, name|
        return [ ast0, name ]
      end,
      "toplevel" => lambda do |ast0, statements|
        return [ ast0, MAP(statements, walk) ]
      end,
      "block" => _block,
      "splice" => _block,
      "var" => _vardefs,
      "const" => _vardefs,
      "try" => lambda do |t, c, f|
        return [
          ast0,
          MAP(t, walk),
          c != null ? [ c[0], MAP(c[1], walk) ]  : null,
          f != null ? MAP(f, walk)  : null
        ];
      end,
      "throw" => lambda do |ast0, expr|
        return [ ast0, walk.call(expr) ]
      end,
      "new" => lambda do |ast0, ctor, args|
        return [ ast0, walk.call(ctor), MAP(args, walk) ]
      end,
      "switch" => lambda do |ast0, expr, body|
        return [ ast0, walk.call(expr), MAP(body, lambda do |branch|
          return [ branch[0] ? walk.call(branch[0])  : null,
             MAP(branch[1], walk) ]
        end) ]
      end,
      "break" => lambda do |ast0, label|
        return [ ast0, label ]
      end,
      "continue" => lambda do |ast0, label|
        return [ ast0, label ]
      end,
      "conditional" => lambda do |ast0, cond, t, e|
        return [ ast0, walk.call(cond), walk.call(t), walk.call(e) ]
      end,
      "assign" => lambda do |ast0, op, lvalue, rvalue|
        return [ ast0, op, walk.call(lvalue), walk.call(rvalue) ]
      end,
      "dot" => lambda do |ast0, expr, *args|
        return [ ast0, walk.call(expr) ].concat(args)
      end,
      "call" => lambda do |ast0, expr, args|
        return [ ast0, walk.call(expr), MAP(args, walk) ]
      end,
      "function" => lambda do |ast0, name, args, body|
        return [ ast0, name, args.slice(), MAP(body, walk) ]
      end,
      "defun" => lambda do |ast0, name, args, body|
        return [ ast0, name, args.slice(), MAP(body, walk) ]
      end,
      "if" => lambda do |ast0, conditional, t, e|
        return [ ast0, walk.call(conditional), walk.call(t), walk.call(e) ]
      end,
      "for" => lambda do |ast0, init, cond, step, block|
        return [ ast0, walk.call(init), walk.call(cond), walk.call(step), walk.call(block) ]
      end,
      "for-in" => lambda do |ast0, vvar, key, hash, block|
        return [ ast0, walk.call(vvar), walk.call(key), walk.call(hash), walk.call(block) ]
      end,
      "while" => lambda do |ast0, cond, block|
        return [ ast0, walk.call(cond), walk.call(block) ]
      end,
      "do" => lambda do |ast0, cond, block|
        return [ ast0, walk.call(cond), walk.call(block) ]
      end,
      "return" => lambda do |ast0, expr|
        return [ ast0, walk.call(expr) ]
      end,
      "binary" => lambda do |ast0, op, left, right|
        return [ ast0, op, walk.call(left), walk.call(right) ]
      end,
      "unary-prefix" => lambda do |ast0, op, expr|
        return [ ast0, op, walk.call(expr) ]
      end,
      "unary-postfix" => lambda do |ast0, op, expr|
        return [ ast0, op, walk.call(expr) ]
      end,
      "sub" => lambda do |ast0, expr, subscript|
        return [ ast0, walk.call(expr), walk.call(subscript) ]
      end,
      "object" => lambda do |ast0, props|
        return [ ast0, MAP(props, lambda do |p|
          return p.length == 2 ?
              [ p[0], walk.call(p[1]) ] :
              [ p[0], walk.call(p[1]), p[2] ] # get/set-ter
        end) ]
      end,
      "regexp" => lambda do |ast0, rx, mods|
        return [ ast0, rx, mods ]
      end,
      "array" => lambda do |ast0, elements|
        return [ ast0, MAP(elements, walk) ]
      end,
      "stat" => lambda do |ast0, stat|
        return [ ast0, walk.call(stat) ]
      end,
      "seq" => lambda do |ast0, *args|
        return [ ast0 ].concat(MAP(args, walk))
      end,
      "label" => lambda do |ast0, name, block|
        return [ ast0, name, walk.call(block) ]
      end,
      "with" => lambda do |ast0, expr, block|
        return [ ast0, walk.call(expr), walk.call(block) ]
      end,
      "atom" => lambda do |ast0, name|
        return [ ast0, name ]
      end
    }
  end

end

