# /* -----[ Scope and mangling ]----- */

module Scope

  class Scope

    def initialize(parent)
      @names = {}  # names defined in this scope
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
        parent.children.push(this) #kpk fix this
      else
        @level = 0
      end
    end

    def has(name)
      for (var s = this; s; s = s.parent)
        if HOP(s.names, name)
          return s
        end
      end
    end
    def has_mangled(mname)
      for (var s = this; s; s = s.parent)
        if (HOP(s.rev_mangled, mname))
          return s
        end
      end
    end
    def toJSON
      return {
        :names => this.names,
        :uses_eval => this.uses_eval,
        :uses_with => this.uses_with
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
      for (;;) {
        m = base54(++this.cname)

        # # case 1.
        prior = this.has_mangled(m)
        if (prior && this.refs[prior.rev_mangled[m]] === prior)
          continue
        end

        # # case 2.
        prior = this.has(m)
        if (prior && prior !== this && this.refs[m] === prior && !prior.has_mangled(m))
          continue
        end

        # # case 3.
        if (HOP(this.refs, m) && this.refs[m] == null)
          continue
        end

        # # I got "do" once. :-/
        if (!is_identifier(m))
          continue
        end

        return m
      end
    end
    def set_mangle(name, m)
      this.rev_mangled[m] = name
      return this.mangled[name] = m
    end
    def get_mangled(name, newMangle)
      return name if (this.uses_eval || this.uses_with) # no mangle if eval or with is in use
      s = this.has(name)
      return name if (!s) ## not in visible scope, no mangle
      return s.mangled[name] if HOP(s.mangled, name)  # already mangled in this scope
      if (!newMangle) return name          # not found and no mangling requested
      return s.set_mangle(name, s.next_mangled())
    end
    def define(name)
      if name != null
        return this.names[name] = name
      end
    end

    protected

    DIGITS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$_"
    def base54
      lambda do |num|
        ret = ""
        begin
          ret = DIGITS[num % 54] + ret
          num = Math.floor(num / 54)
        end while (num > 0)
        return ret
      end
    end


  end

  class ast_add_scope

    def initialize(ast)
      @current_scope = null
      @w = ast_walker()
      @walk = w.walk
      @having_eval = []
    end

    def with_scope
      return with_new_scope(lambda do
        # process AST
        var ret = w.with_walkers({
          "function": _lambda,
          "defun": _lambda,
          "with": function(expr, block) {
            for (var s = current_scope; s; s = s.parent)
              s.uses_with = true;
          },
          "var": function(defs) {
            MAP(defs, function(d){ define(d[0]) });
          },
          "const": function(defs) {
            MAP(defs, function(d){ define(d[0]) });
          },
          "try": function(t, c, f) {
            if (c != null) return [
              this[0],
              MAP(t, walk),
              [ define(c[0]), MAP(c[1], walk) ],
              f != null ? MAP(f, walk) : null
            ];
          },
          "name": function(name) {
            if (name == "eval")
              having_eval.push(current_scope);
            reference(name);
          }
        }, function(){
          return walk(ast);
        });

        # the reason why we need an additional pass here is
        # that names can be used prior to their definition.

        # scopes where eval was detected and their parents
        # are marked with uses_eval, unless they define the
        # "eval" name.
        MAP(having_eval, function(scope){
          if (!scope.has("eval")) while (scope) {
            scope.uses_eval = true;
            scope = scope.parent;
          }
        });

        fixrefs(current_scope);

        return ret;
      end)
    end

    protected

    # for referenced names it might be useful to know
    # their origin scope.  current_scope here is the
    # toplevel one.
    def fixrefs(scope, i) {
      # # do children first; order shouldn't matter
      for (i = scope.children.length; --i >= 0;)
        fixrefs(scope.children[i]);
      for (i in scope.refs) if (HOP(scope.refs, i)) {
        # find origin scope and propagate the reference to origin
        for (var origin = scope.has(i), s = scope; s; s = s.parent) {
          s.refs[i] = origin;
          if (s === origin) break;
        }
      }
    end

    def with_new_scope(cont) {
      current_scope = new Scope(current_scope);
      var ret = current_scope.body = cont();
      ret.scope = current_scope;
      current_scope = current_scope.parent;
      return ret;
    end

    def define(name) {
      return current_scope.define(name);
    end

    def reference(name) {
      current_scope.refs[name] = true;
    end

    def _lambda(name, args, body) {
      var is_defun = this[0] == "defun";
      return [ this[0], is_defun ? define(name) : name, args, with_new_scope(function(){
        if (!is_defun) define(name);
        MAP(args, define);
        return MAP(body, walk);
      })];
    end

  end

end