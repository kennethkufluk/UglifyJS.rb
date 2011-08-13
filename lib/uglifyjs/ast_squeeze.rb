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

var warn = function(){};

def best_of(ast1, ast2)
  return gen_code(ast1).length > gen_code(ast2[0] == "stat" ? ast2[1] : ast2).length ? ast2 : ast1;
end

def last_stat(b)
  if (b[0] == "block" && b[1] && b[1].length > 0)
    return b[1][b[1].length - 1];
  return b;
}

def aborts(t)
  if (t) {
    t = last_stat(t);
    if (t[0] == "return" || t[0] == "break" || t[0] == "continue" || t[0] == "throw")
      return true;
  }
end

def boolean_expr(expr)
  return ( (expr[0] == "unary-prefix"
      && member(expr[1], [ "!", "delete" ])) ||

     (expr[0] == "binary"
      && member(expr[1], [ "in", "instanceof", "==", "!=", "===", "!==", "<", "<=", ">=", ">" ])) ||

     (expr[0] == "binary"
      && member(expr[1], [ "&&", "||" ])
      && boolean_expr(expr[2])
      && boolean_expr(expr[3])) ||

     (expr[0] == "conditional"
      && boolean_expr(expr[2])
      && boolean_expr(expr[3])) ||

     (expr[0] == "assign"
      && expr[1] === true
      && boolean_expr(expr[3])) ||

     (expr[0] == "seq"
      && boolean_expr(expr[expr.length - 1]))
         );
end

def make_conditional(c, t, e)
    var make_real_conditional = function() {
  if (c[0] == "unary-prefix" && c[1] == "!") {
      return e ? [ "conditional", c[2], e, t ] : [ "binary", "||", c[2], t ];
  } else {
      return e ? [ "conditional", c, t, e ] : [ "binary", "&&", c, t ];
  }
    end
    # shortcut the conditional if the expression has a constant value
    return when_constant(c, function(ast, val){
  warn_unreachable(val ? e :t);
  return    (val ? t : e);
    }, make_real_conditional);
end

def is_string(node)
  return (node[0] == "string" ||
    node[0] == "unary-prefix" && node[1] == "typeof" ||
    node[0] == "binary" && node[1] == "+" &&
    (is_string(node[2]) || is_string(node[3])));
end

var when_constant = (function(){

  var $NOT_CONSTANT = {};

  # # this can only evaluate constant expressions.  If it finds anything
  # # not constant, it throws $NOT_CONSTANT.
  def evaluate(expr)
    switch (expr[0]) {
        case "string":
        case "num":
      return expr[1];
        case "name":
        case "atom":
      switch (expr[1]) {
          case "true": return true;
          case "false": return false;
      }
      break;
        case "unary-prefix":
      switch (expr[1]) {
          case "!": return !evaluate(expr[2]);
          case "typeof": return typeof evaluate(expr[2]);
          case "~": return ~evaluate(expr[2]);
          case "-": return -evaluate(expr[2]);
          case "+": return +evaluate(expr[2]);
      }
      break;
        case "binary":
      var left = expr[2], right = expr[3];
      switch (expr[1]) {
          case "&&"   : return evaluate(left) &&   evaluate(right);
          case "||"   : return evaluate(left) ||   evaluate(right);
          case "|"    : return evaluate(left) |    evaluate(right);
          case "&"    : return evaluate(left) &    evaluate(right);
          case "^"    : return evaluate(left) ^    evaluate(right);
          case "+"    : return evaluate(left) +    evaluate(right);
          case "*"    : return evaluate(left) *    evaluate(right);
          case "/"    : return evaluate(left) /    evaluate(right);
          case "-"    : return evaluate(left) -    evaluate(right);
          case "<<"   : return evaluate(left) <<   evaluate(right);
          case ">>"   : return evaluate(left) >>   evaluate(right);
          case ">>>"  : return evaluate(left) >>>  evaluate(right);
          case "=="   : return evaluate(left) ==   evaluate(right);
          case "==="  : return evaluate(left) ===  evaluate(right);
          case "!="   : return evaluate(left) !=   evaluate(right);
          case "!=="  : return evaluate(left) !==  evaluate(right);
          case "<"    : return evaluate(left) <    evaluate(right);
          case "<="   : return evaluate(left) <=   evaluate(right);
          case ">"    : return evaluate(left) >    evaluate(right);
          case ">="   : return evaluate(left) >=   evaluate(right);
          case "in"   : return evaluate(left) in   evaluate(right);
          case "instanceof" : return evaluate(left) instanceof evaluate(right);
      }
    }
    throw $NOT_CONSTANT;
  end

  return function(expr, yes, no)
    try {
      var val = evaluate(expr), ast;
      switch (typeof val) {
          case "string": ast =  [ "string", val ]; break;
          case "number": ast =  [ "num", val ]; break;
          case "boolean": ast =  [ "name", String(val) ]; break;
          default: throw new Error("Can't handle constant of type: " + (typeof val));
      }
      return yes.call(expr, ast, val);
    } catch(ex) {
      if (ex === $NOT_CONSTANT) {
        if (expr[0] == "binary"
            && (expr[1] == "===" || expr[1] == "!==")
            && ((is_string(expr[2]) && is_string(expr[3]))
          || (boolean_expr(expr[2]) && boolean_expr(expr[3])))) {
          expr[1] = expr[1].substr(0, 2);
        }
        else if (no && expr[0] == "binary"
           && (expr[1] == "||" || expr[1] == "&&")) {
            # the whole expression is not constant but the lval may be...
            try {
          var lval = evaluate(expr[2]);
          expr = ((expr[1] == "&&" && (lval ? expr[3] : lval))    ||
            (expr[1] == "||" && (lval ? lval    : expr[3])) ||
            expr);
            } catch(ex2) {
          # IGNORE... lval is not constant
            }
        }
        return no ? no.call(expr, expr) : null;
      }
      else throw ex;
    }
  end

})();

def warn_unreachable(ast)
  if (!empty(ast))
    warn("Dropping unreachable code: " + gen_code(ast, true));
end

def prepare_ifs(ast)
  var w = ast_walker(), walk = w.walk;
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
  def redo_if(statements)
    statements = MAP(statements, walk);

    for (var i = 0; i < statements.length; ++i) {
      var fi = statements[i];
      if (fi[0] != "if") continue;

      if (fi[3] && walk(fi[3])) continue;

      var t = walk(fi[2]);
      if (!aborts(t)) continue;

      var conditional = walk(fi[1]);

      var e_body = statements.slice(i + 1);
      var e;
      if (e_body.length == 1) e = e_body[0];
      else e = [ "block", e_body ];

      var ret = statements.slice(0, i).concat([ [
        fi[0],    # "if"
        conditional,    # conditional
        t,        # then
        e         # else
      ] ]);

      return redo_if(ret);
    }

    return statements;
  end

  def redo_if_lambda(name, args, body)
    body = redo_if(body);
    return [ this[0], name, args.slice(), body ];
  end

  def redo_if_block(statements)
    var out = [ this[0] ];
    if (statements != null)
      out.push(redo_if(statements));
    return out;
  end

  return w.with_walkers({
    "defun": redo_if_lambda,
    "function": redo_if_lambda,
    "block": redo_if_block,
    "splice": redo_if_block,
    "toplevel": function(statements) {
      return [ this[0], redo_if(statements) ];
    },
    "try": function(t, c, f) {
      return [
        this[0],
        redo_if(t),
        c != null ? [ c[0], redo_if(c[1]) ] : null,
        f != null ? redo_if(f) : null
      ];
    }
  }, function() {
    return walk(ast);
  });
end

def ast_squeeze(ast, options) {
  options = defaults(options, {
    make_seqs   : true,
    dead_code   : true,
    keep_comps  : true,
    no_warnings : false
  });

  var w = ast_walker(), walk = w.walk, scope;

  def negate(c)
    var not_c = [ "unary-prefix", "!", c ];
    switch (c[0]) {
        case "unary-prefix":
      return c[1] == "!" && boolean_expr(c[2]) ? c[2] : not_c;
        case "seq":
      c = slice(c);
      c[c.length - 1] = negate(c[c.length - 1]);
      return c;
        case "conditional":
      return best_of(not_c, [ "conditional", c[1], negate(c[2]), negate(c[3]) ]);
        case "binary":
      var op = c[1], left = c[2], right = c[3];
      if (!options.keep_comps) switch (op) {
          case "<="  : return [ "binary", ">", left, right ];
          case "<"   : return [ "binary", ">=", left, right ];
          case ">="  : return [ "binary", "<", left, right ];
          case ">"   : return [ "binary", "<=", left, right ];
      }
      switch (op) {
          case "=="  : return [ "binary", "!=", left, right ];
          case "!="  : return [ "binary", "==", left, right ];
          case "===" : return [ "binary", "!==", left, right ];
          case "!==" : return [ "binary", "===", left, right ];
          case "&&"  : return best_of(not_c, [ "binary", "||", negate(left), negate(right) ]);
          case "||"  : return best_of(not_c, [ "binary", "&&", negate(left), negate(right) ]);
      }
      break;
    }
    return not_c;
  end

  def with_scope(s, cont)
    var _scope = scope;
    scope = s;
    var ret = cont();
    ret.scope = s;
    scope = _scope;
    return ret;
  end

  def rmblock(block)
    if (block != null && block[0] == "block" && block[1]) {
      if (block[1].length == 1)
        block = block[1][0];
      else if (block[1].length == 0)
        block = [ "block" ];
    }
    return block;
  end

  def _lambda(name, args, body)
    var is_defun = this[0] == "defun";
    body = with_scope(body.scope, function(){
      var ret = tighten(MAP(body, walk), "lambda");
      if (!is_defun && name && !HOP(scope.refs, name))
        name = null;
      return ret;
    });
    return [ this[0], name, args, body ];
  end

  # we get here for blocks that have been already transformed.
  # this def does a few things:
  # 1. discard useless blocks
  # 2. join consecutive var declarations
  # 3. remove obviously dead code
  # 4. transform consecutive statements using the comma operator
  # 5. if block_type == "lambda" and it detects constructs like if(foo) return ... - rewrite like if (!foo) { ... }
  def tighten(statements, block_type)
    statements = statements.reduce(function(a, stat){
      if (stat[0] == "block") {
        if (stat[1]) {
          a.push.apply(a, stat[1]);
        }
      } else {
        a.push(stat);
      }
      return a;
    }, []);

    statements = (function(a, prev){
      statements.forEach(function(cur){
        if (prev && ((cur[0] == "var" && prev[0] == "var") ||
               (cur[0] == "const" && prev[0] == "const"))) {
          prev[1] = prev[1].concat(cur[1]);
        } else {
          a.push(cur);
          prev = cur;
        }
      });
      return a;
    })([]);

    if (options.dead_code) statements = (function(a, has_quit){
      statements.forEach(function(st){
        if (has_quit) {
          if (member(st[0], [ "function", "defun" , "var", "const" ])) {
            a.push(st);
          }
          else if (!options.no_warnings)
            warn_unreachable(st);
        }
        else {
          a.push(st);
          if (member(st[0], [ "return", "throw", "break", "continue" ]))
            has_quit = true;
        }
      });
      return a;
    })([]);

    if (options.make_seqs) statements = (function(a, prev) {
      statements.forEach(function(cur){
        if (prev && prev[0] == "stat" && cur[0] == "stat") {
          prev[1] = [ "seq", prev[1], cur[1] ];
        } else {
          a.push(cur);
          prev = cur;
        }
      });
      return a;
    })([]);

    if (block_type == "lambda") statements = (function(i, a, stat){
      while (i < statements.length) {
        stat = statements[i++];
        if (stat[0] == "if" && !stat[3]) {
          if (stat[2][0] == "return" && stat[2][1] == null) {
            a.push(make_if(negate(stat[1]), [ "block", statements.slice(i) ]));
            break;
          }
          var last = last_stat(stat[2]);
          if (last[0] == "return" && last[1] == null) {
            a.push(make_if(stat[1], [ "block", stat[2][1].slice(0, -1) ], [ "block", statements.slice(i) ]));
            break;
          }
        }
        a.push(stat);
      }
      return a;
    })(0, []);

    return statements;
  end

  def make_if(c, t, e)
    return when_constant(c, function(ast, val){
      if (val) {
        warn_unreachable(e);
        return t;
      } else {
        warn_unreachable(t);
        return e;
      }
    }, function() {
      return make_real_if(c, t, e);
    });
  end

  def make_real_if(c, t, e)
    c = walk(c);
    t = walk(t);
    e = walk(e);

    if (empty(t)) {
      c = negate(c);
      t = e;
      e = null;
    } else if (empty(e)) {
      e = null;
    } else {
      # if we have both else and then, maybe it makes sense to switch them?
      (function(){
        var a = gen_code(c);
        var n = negate(c);
        var b = gen_code(n);
        if (b.length < a.length) {
          var tmp = t;
          t = e;
          e = tmp;
          c = n;
        }
      })();
    }
    if (empty(e) && empty(t))
      return [ "stat", c ];
    var ret = [ "if", c, t, e ];
    if (t[0] == "if" && empty(t[3]) && empty(e)) {
      ret = best_of(ret, walk([ "if", [ "binary", "&&", c, t[1] ], t[2] ]));
    }
    else if (t[0] == "stat") {
      if (e) {
        if (e[0] == "stat") {
          ret = best_of(ret, [ "stat", make_conditional(c, t[1], e[1]) ]);
        }
      }
      else {
        ret = best_of(ret, [ "stat", make_conditional(c, t[1]) ]);
      }
    }
    else if (e && t[0] == e[0] && (t[0] == "return" || t[0] == "throw") && t[1] && e[1]) {
      ret = best_of(ret, [ t[0], make_conditional(c, t[1], e[1] ) ]);
    }
    else if (e && aborts(t)) {
      ret = [ [ "if", c, t ] ];
      if (e[0] == "block") {
        if (e[1]) ret = ret.concat(e[1]);
      }
      else {
        ret.push(e);
      }
      ret = walk([ "block", ret ]);
    }
    else if (t && aborts(e)) {
      ret = [ [ "if", negate(c), e ] ];
      if (t[0] == "block") {
        if (t[1]) ret = ret.concat(t[1]);
      } else {
        ret.push(t);
      }
      ret = walk([ "block", ret ]);
    }
    return ret;
  end

  def _do_while(cond, body) {
    return when_constant(cond, function(cond, val){
      if (!val) {
        warn_unreachable(body);
        return [ "block" ];
      } else {
        return [ "for", null, null, null, walk(body) ];
      }
    });
  end

  ast = prepare_ifs(ast);
  ast = ast_add_scope(ast);

  return w.with_walkers({
    "sub": function(expr, subscript) {
      if (subscript[0] == "string") {
        var name = subscript[1];
        if (is_identifier(name))
          return [ "dot", walk(expr), name ];
        else if (/^[1-9][0-9]*$/.test(name) || name === "0")
          return [ "sub", walk(expr), [ "num", parseInt(name, 10) ] ];
      }
    },
    "if": make_if,
    "toplevel": function(body) {
      return [ "toplevel", with_scope(this.scope, function(){
        return tighten(MAP(body, walk));
      }) ];
    },
    "switch": function(expr, body) {
      var last = body.length - 1;
      return [ "switch", walk(expr), MAP(body, function(branch, i){
        var block = tighten(MAP(branch[1], walk));
        if (i == last && block.length > 0) {
          var node = block[block.length - 1];
          if (node[0] == "break" && !node[1])
            block.pop();
        }
        return [ branch[0] ? walk(branch[0]) : null, block ];
      }) ];
    },
    "function": _lambda,
    "defun": _lambda,
    "block": function(body) {
      if (body) return rmblock([ "block", tighten(MAP(body, walk)) ]);
    },
    "binary": function(op, left, right) {
      return when_constant([ "binary", op, walk(left), walk(right) ], def yes(c){
        return best_of(walk(c), this);
      }, def no() {
        return this;
      });
    },
    "conditional": function(c, t, e) {
      return make_conditional(walk(c), walk(t), walk(e));
    },
    "try": function(t, c, f) {
      return [
        "try",
        tighten(MAP(t, walk)),
        c != null ? [ c[0], tighten(MAP(c[1], walk)) ] : null,
        f != null ? tighten(MAP(f, walk)) : null
      ];
    },
    "unary-prefix": function(op, expr) {
      expr = walk(expr);
      var ret = [ "unary-prefix", op, expr ];
      if (op == "!")
        ret = best_of(ret, negate(expr));
      return when_constant(ret, function(ast, val){
        return walk(ast); # it's either true or false, so minifies to !0 or !1
      }, function() { return ret });
    },
    "name": function(name) {
      switch (name) {
          case "true": return [ "unary-prefix", "!", [ "num", 0 ]];
          case "false": return [ "unary-prefix", "!", [ "num", 1 ]];
      }
    },
    "new": function(ctor, args) {
      if (ctor[0] == "name" && ctor[1] == "Array" && !scope.has("Array")) {
        if (args.length != 1) {
          return [ "array", args ];
        } else {
          return [ "call", [ "name", "Array" ], args ];
        }
      }
    },
    "call": function(expr, args) {
      if (expr[0] == "name" && expr[1] == "Array" && args.length != 1 && !scope.has("Array")) {
        return [ "array", args ];
      }
    },
    "while": _do_while
  }, function() {
    return walk(ast);
  });
end

