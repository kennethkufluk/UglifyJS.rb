
# /* -----[ mangle names ]----- */

require "uglifyjs/ast_walker"
require "uglifyjs/scope"

class Mangler

  include Scope
  include Util

  def initialize(ast, options=nil)
    @ast = ast
    @w = ASTWalker.new(@ast)
    @walk = @w.walk
    @options = options || {}
    @scope = nil
  end

  def getAST
    return @w.with_walkers({
      "function" => _lambda,
      "defun" => lambda do |ast, *args|
        # move def declarations to the top when
        # they are not in some block.
        ast = _lambda.call(ast, *args)
        case @w.parent()[0]
          when "toplevel", "function", "defun"
            return AtTop.new(ast)
        end
        return ast
      end,
      "var" => _vardefs,
      "const" => _vardefs,
      "name" => lambda do |ast, name|
        return get_define(name) || [ ast[0], get_mangled(name) ]
      end,
      "try" => lambda do |ast, t, c, f|
        return [ ast[0],
           MAP(t, @walk),
           !c.nil? ? [ get_mangled(c[0]), MAP(c[1], @walk) ] : nil,
           !f.nil? ? MAP(f, @walk) : nil ]
      end,
      "toplevel" => lambda do |ast, body|
        return with_scope(ast.scope, lambda do
          return [ ast[0], MAP(body, @walk) ]
        end)
      end
    }, lambda do
      ast_with_scope = AddScope.new(@ast).go
      return @walk.call(ast_with_scope)
    end)
  end

  protected

  def get_mangled(name, newMangle=nil)
    if (!@options[:toplevel] && !@scope.parent)
      return name ## don't mangle toplevel
    end
    if @options[:except] && member(name, @options[:except])
      return name
    end
    return @scope.get_mangled(name, newMangle)
  end

  def get_define(name)
    if @options[:defines]
      # we always lookup a defined symbol for the current scope FIRST, so declared
      # vars trump a DEFINE symbol, but if no such var is found, then match a DEFINE value
      if !@scope.has(name)
        if HOP(@options[:defines], name)
          return @options[:defines][name]
        end
      end
      return nil
    end
  end

  def _lambda
    lambda do |ast, *others|
      is_defun = ast[0] == "defun"
      name = others[0] || nil
      args = others[1] || nil
      body = others[2] || nil
      if name
        if is_defun
          name = get_mangled(name)
        else
          extra = {}
          if !(@scope.uses_eval || @scope.uses_with)
            name = extra[name] = @scope.next_mangled()
          else
            extra[name] = name
          end
        end
      end
      if !body.nil?
        body = with_scope(body.scope, lambda do
          args = MAP(args, lambda do |name2, *args2|
            return get_mangled(name2)
          end)
          return MAP(body, @walk)
        end, extra)
      end
      return [ ast[0], name, args, body ]
    end
  end

  def with_scope(s, cont, extra=nil)
    _scope = @scope
    @scope = s
    if extra
      for i,j in extra
        if HOP(extra, i)
          s.set_mangle(i, extra[i])
        end
      end
    end
    for i in s.names
      if s.names.include?(i)
        get_mangled(i, true)
      end
    end
    ret = ScopedArray.new(cont.call())
    ret.scope = s
    @scope = _scope
    return ret
  end

  def _vardefs
    lambda do |ast, defs|
      return [ ast[0], MAP(defs, lambda do |d, *args|
        return [ get_mangled(d[0]), @walk.call(d[1]) ]
      end) ]
    end
  end

end
