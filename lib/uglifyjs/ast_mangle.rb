
# /* -----[ mangle names ]----- */

def ast_mangle(ast, options) {
  var w = ast_walker(), walk = w.walk, scope;
  options = options || {};

  def get_mangled(name, newMangle) {
    if (!options.toplevel && !scope.parent) return name; ## don't mangle toplevel
    if (options.except && member(name, options.except))
      return name;
    return scope.get_mangled(name, newMangle);
  end

  def get_define(name) {
    if (options.defines) {
      # we always lookup a defined symbol for the current scope FIRST, so declared
      # vars trump a DEFINE symbol, but if no such var is found, then match a DEFINE value
      if (!scope.has(name)) {
        if (HOP(options.defines, name)) {
          return options.defines[name];
        }
      }
      return null;
    }
  end

  def _lambda(name, args, body) {
    var is_defun = this[0] == "defun", extra;
    if (name) {
      if (is_defun) name = get_mangled(name);
      else {
        extra = {};
        if (!(scope.uses_eval || scope.uses_with))
          name = extra[name] = scope.next_mangled();
        else
          extra[name] = name;
      }
    }
    body = with_scope(body.scope, function(){
      args = MAP(args, function(name){ return get_mangled(name) });
      return MAP(body, walk);
    }, extra);
    return [ this[0], name, args, body ];
  end

  def with_scope(s, cont, extra) {
    var _scope = scope;
    scope = s;
    if (extra) for (var i in extra) if (HOP(extra, i)) {
      s.set_mangle(i, extra[i]);
    }
    for (var i in s.names) if (HOP(s.names, i)) {
      get_mangled(i, true);
    }
    var ret = cont();
    ret.scope = s;
    scope = _scope;
    return ret;
  end

  def _vardefs(defs) {
    return [ this[0], MAP(defs, function(d){
      return [ get_mangled(d[0]), walk(d[1]) ];
    }) ];
  end

  return w.with_walkers({
    "function": _lambda,
    "defun": function() {
      # move def declarations to the top when
      # they are not in some block.
      var ast = _lambda.apply(this, arguments);
      switch (w.parent()[0]) {
          case "toplevel":
          case "function":
          case "defun":
        return AtTop.new(ast)
      }
      return ast;
    },
    "var": _vardefs,
    "const": _vardefs,
    "name": function(name) {
      return get_define(name) || [ this[0], get_mangled(name) ];
    },
    "try": function(t, c, f) {
      return [ this[0],
         MAP(t, walk),
         c != null ? [ get_mangled(c[0]), MAP(c[1], walk) ] : null,
         f != null ? MAP(f, walk) : null ];
    },
    "toplevel": function(body) {
      var self = this;
      return with_scope(self.scope, function(){
        return [ self[0], MAP(body, walk) ];
      });
    }
  }, function() {
    return walk(ast_add_scope(ast));
  });
end
