# /* -----[ Scope and mangling ]----- */

module Scope

  class Scope

    include Util

    DIGITS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$_"

    attr_accessor :names
    attr_accessor :mangled
    attr_accessor :rev_mangled
    attr_accessor :body
    attr_accessor :refs
    attr_accessor :uses_with
    attr_accessor :uses_eval
    attr_accessor :parent
    attr_accessor :children
    attr_accessor :level

    def initialize(parent=nil)
      @names = []  # names defined in this scope
      @mangled = {}      # mangled names (orig.name => mangled)
      @rev_mangled = {}  # reverse lookup (mangled => orig.name)
      @cname = -1  # current mangled name
      @refs = {}   # names referenced from this scope
      @uses_with = false # will become TRUE if with() is detected in this or any subscopes
      @uses_eval = false # will become TRUE if eval() is detected in this or any subscopes
      @parent = parent   # parent scope
      @children = []     # sub-scopes
      if parent
        @level = parent.level + 1
        @parent.children.push(self)
      else
        @level = 0
      end
    end

    def has(name)
      s = self
      begin
        if s.names.include?(name)
          return s
        end
      end while (s = s.parent)
    end
    def has_mangled(mname)
      s = self
      begin
        if (HOP(s.rev_mangled, mname))
          return s
        end
      end while s = s.parent
    end
    def toJSON
      return {
        :names => @names,
        :uses_eval => @uses_eval,
        :uses_with => @uses_with
      }
    end

    def next_mangled
      # we must be careful that the new mangled name:
      #
      # 1. doesn't shadow a mangled name from a parent
      #    scope, unless we don't reference the original
      #    name from this scope OR from any sub-scopes!
      #    This will get slow.
      #
      # 2. doesn't shadow an original name from a parent
      #    scope, in the event that the name is not mangled
      #    in the parent scope and we reference that name
      #    here OR IN ANY SUBSCOPES!
      #
      # 3. doesn't shadow a name that is referenced but not
      #    defined (possibly global defined elsewhere).
      begin
        m = base54(@cname+=1)

        # # case 1.
        prior = has_mangled(m)
        if (prior && @refs[prior.rev_mangled[m]] === prior)
          next
        end

        # # case 2.
        prior = has(m)
        if (prior && prior != self && @refs[m] === prior && !prior.has_mangled(m))
          next
        end

        # # case 3.
        if (HOP(@refs, m) && @refs[m].nil?)
          next
        end

        # # I got "do" once. :-/
        if (!is_identifier(m))
          next
        end

        return m
      end while true
    end
    def set_mangle(name, m)
      @rev_mangled[m] = name
      return @mangled[name] = m
    end
    def get_mangled(name, newMangle)
      return name if (@uses_eval || @uses_with) # no mangle if eval or with is in use
      s = has(name)
      return name if (!s) ## not in visible scope, no mangle
      return s.mangled[name] if HOP(s.mangled, name)  # already mangled in this scope
      return name if (!newMangle)           # not found and no mangling requested
      return s.set_mangle(name, s.next_mangled())
    end
    def define(name=nil)
      if !name.nil?
        @names.push(name)
        return name
      end
    end

    protected

    def base54(num)
      ret = ""
      begin
        ret = DIGITS[num % 54].chr + ret
        num = (num / 54).floor
      end while (num > 0)
      return ret
    end


  end

  class ScopedArray < Array

    attr_accessor :scope

  end

  class AddScope

    include Util

    def initialize(ast)
      @current_scope = nil
      @w = ASTWalker.new(ast)
      @walk = @w.walk
      @ast = ast
      @having_eval = []
    end

    def go
      return with_new_scope(lambda do
        # process AST
        ret = @w.with_walkers({
          "function" => _lambda,
          "defun" => _lambda,
          "with" => lambda do |ast, expr, block|
            s = @current_scope
            begin
              s.uses_with = true
            end while s = @current_scope
            nil
          end,
          "var" => lambda do |ast, defs, *args|
            MAP(defs, lambda { |d, *args| define.call(d[0]) })
            nil
          end,
          "const" => lambda do |ast, defs|
            MAP(defs, lambda { |d| define.call(d[0]) })
            nil
          end,
          "try" => lambda do |ast, t, c, f|
            if !c.nil?
              return [
                ast[0],
                MAP(t, @walk),
                [ define.call(c[0]), MAP(c[1], @walk) ],
                f ? MAP(f, @walk) : nil
              ]
            end
            nil
          end,
          "name" => lambda do |ast, name|
            if (name == "eval")
              #puts "eval"+@current_scope.class.to_s
              @having_eval.push(@current_scope)
            end
            reference(name)
            nil
          end
        }, lambda do
          #puts "Walking this ast: #{@ast.inspect}"
          return @walk.call(@ast)
        end)

        # the reason why we need an additional pass here is
        # that names can be used prior to their definition.

        # scopes where eval was detected and their parents
        # are marked with uses_eval, unless they define the
        # "eval" name.
        #puts "having #{@having_eval.inspect} "
        MAP(@having_eval, lambda do |scope, *args|
          if (!(scope.has("eval")))
            while (scope)
              scope.uses_eval = true
              scope = scope.parent
            end
          end
        end)

        #puts "running fixrefs for current scope: #{@current_scope.children.length}"
        fixrefs(@current_scope)

        return ret
      end)
    end

    protected

    # for referenced names it might be useful to know
    # their origin scope.  current_scope here is the
    # toplevel one.
    def fixrefs(scope)
      # # do children first; order shouldn't matter
      scope.children.reverse.each do |child|
        fixrefs(child)
      end
      for i,j in scope.refs
        if (HOP(scope.refs, i))
          # find origin scope and propagate the reference to origin
          origin = scope.has(i)
          s = scope
          begin
            s.refs[i] = origin
            break if (s === origin)
          end while s = s.parent
        end
      end
    end

    def with_new_scope(cont)
      @current_scope = Scope.new(@current_scope)
      #puts "Scope: #{@current_scope}"
      ret = @current_scope.body = cont.call()
      #puts "is ret an array? #{ret.class}  #{ret.inspect}"
      ret = ScopedArray.new(ret)
      #puts "is ret an array? #{ret.class}  #{ret.inspect}"
      ret.scope = @current_scope
      #puts "is ret an array? #{ret.class}  #{ret.inspect}"
      @current_scope = @current_scope.parent
      return ret
    end

    def define
      lambda do |name, *args|
        return @current_scope.define(name)
      end
    end

    def reference(name)
      @current_scope.refs[name] = true
    end

    def _lambda
      lambda do |ast, name, args, *body|
        body = body[0]
        is_defun = ast[0] == "defun"
        ret = [ ast[0], is_defun ? define.call(name) : name, args, with_new_scope(lambda do
          if (!is_defun)
            define.call(name)
          end
          MAP(args, define)
          return MAP(body, @walk)
        end)]
        return ret
      end
    end

  end

end