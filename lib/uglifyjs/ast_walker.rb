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
    lambda do |ast, *args|
      if ast.nil?
        return nil
      end
      begin
        @stack.push(ast)
        type = ast[0]
        gen = @user[type]
        if gen
          puts "USER MATCH #{type}, #{ast.class}"
          ret = gen.call(ast, *(ast.slice(1..-1)))
          if ret != nil
            return ret
          end
        end
        gen = walkers[type]
        puts "WALKER MATCH: #{ast.inspect}, #{type}"
        return gen.call(ast, *(ast.slice(1..-1)))
      ensure
        @stack.pop()
      end
    end
  end

  def with_walkers(walkers, cont)
    save = {}
    for i,j in walkers
      if HOP(walkers, i)
        save[i] = @user[i]
        @user[i] = walkers[i]
      end
    end
    ret = cont.call()
    for i,j in save
      if HOP(save, i)
        if !save[i]
          @user.delete(i)
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
    lambda do |ast, defs|
      return [ ast[0], MAP(defs, lambda do |mydef, *args|
        a = [ mydef[0] ]
        if mydef.length > 1
          #puts "mydef: #{mydef.inspect} "
          a[1] = walk.call(mydef[1])
        end
        return a
      end ) ]
    end
  end
  def _block
    lambda do |ast, statements|
      out = [ ast[0] ]
      if !statements.nil?
        out.push(MAP(statements, walk))
      end
      return out
    end
  end
  def walkers
    @walkers ||= {
      #kpk make into lamdas?
      # or just make a new class?
      "string" => lambda do |ast, str|
        return [ ast[0], str ]
      end,
      "num" => lambda do |ast, num|
        return [ ast[0], num ]
      end,
      "name" => lambda do |ast, name|
        return [ ast[0], name ]
      end,
      "toplevel" => lambda do |ast, statements, *args|
        return [ ast[0], MAP(statements, walk) ]
      end,
      "block" => _block,
      "splice" => _block,
      "var" => _vardefs,
      "const" => _vardefs,
      "try" => lambda do |ast, t, c, f|
        return [
          ast[0],
          MAP(t, walk),
          c != null ? [ c[0], MAP(c[1], walk) ]  : null,
          f != null ? MAP(f, walk)  : null
        ];
      end,
      "throw" => lambda do |ast, expr|
        return [ ast[0], walk.call(expr) ]
      end,
      "new" => lambda do |ast, ctor, args|
        return [ ast[0], walk.call(ctor), MAP(args, walk) ]
      end,
      "switch" => lambda do |ast, expr, body|
        return [ ast[0], walk.call(expr), MAP(body, lambda do |branch, *args|
          return [ branch[0] ? walk.call(branch[0])  : null,
             MAP(branch[1], walk) ]
        end) ]
      end,
      "break" => lambda do |ast, label|
        return [ ast[0], label ]
      end,
      "continue" => lambda do |ast, label|
        return [ ast[0], label ]
      end,
      "conditional" => lambda do |ast, cond, t, e|
        return [ ast[0], walk.call(cond), walk.call(t), walk.call(e) ]
      end,
      "assign" => lambda do |ast, op, lvalue, rvalue|
        return [ ast[0], op, walk.call(lvalue), walk.call(rvalue) ]
      end,
      "dot" => lambda do |ast, expr, *args|
        return [ ast[0], walk.call(expr) ].concat(args)
      end,
      "call" => lambda do |ast, expr, args|
        return [ ast[0], walk.call(expr), MAP(args, walk) ]
      end,
      "function" => lambda do |ast, name, args, body|
        return [ ast[0], name, Array.new(args), MAP(body, walk) ]
      end,
      "defun" => lambda do |ast, name, args, body|
        return [ ast[0], name, Array.new(args), MAP(body, walk) ]
      end,
      "if" => lambda do |ast, conditional, t, e|
        return [ ast[0], walk.call(conditional), walk.call(t), walk.call(e) ]
      end,
      "for" => lambda do |ast, init, cond, step, block|
        return [ ast[0], walk.call(init), walk.call(cond), walk.call(step), walk.call(block) ]
      end,
      "for-in" => lambda do |ast, vvar, key, hash, block|
        return [ ast[0], walk.call(vvar), walk.call(key), walk.call(hash), walk.call(block) ]
      end,
      "while" => lambda do |ast, cond, block|
        return [ ast[0], walk.call(cond), walk.call(block) ]
      end,
      "do" => lambda do |ast, cond, block|
        return [ ast[0], walk.call(cond), walk.call(block) ]
      end,
      "return" => lambda do |ast, expr|
        return [ ast[0], walk.call(expr) ]
      end,
      "binary" => lambda do |ast, op, left, right|
        return [ ast[0], op, walk.call(left), walk.call(right) ]
      end,
      "unary-prefix" => lambda do |ast, op, expr|
        return [ ast[0], op, walk.call(expr) ]
      end,
      "unary-postfix" => lambda do |ast, op, expr|
        return [ ast[0], op, walk.call(expr) ]
      end,
      "sub" => lambda do |ast, expr, subscript|
        return [ ast[0], walk.call(expr), walk.call(subscript) ]
      end,
      "object" => lambda do |ast, props|
        return [ ast[0], MAP(props, lambda do |p, *args|
          return p.length == 2 ?
              [ p[0], walk.call(p[1]) ] :
              [ p[0], walk.call(p[1]), p[2] ] # get/set-ter
        end) ]
      end,
      "regexp" => lambda do |ast, rx, mods|
        return [ ast[0], rx, mods ]
      end,
      "array" => lambda do |ast, elements|
        return [ ast[0], MAP(elements, walk) ]
      end,
      "stat" => lambda do |ast, stat|
        return [ ast[0], walk.call(stat) ]
      end,
      "seq" => lambda do |ast, *args|
        return [ ast[0] ].concat(MAP(args, walk))
      end,
      "label" => lambda do |ast, name, block|
        return [ ast[0], name, walk.call(block) ]
      end,
      "with" => lambda do |ast, expr, block|
        return [ ast[0], walk.call(expr), walk.call(block) ]
      end,
      "atom" => lambda do |ast, name|
        return [ ast[0], name ]
      end
    }
  end

end

