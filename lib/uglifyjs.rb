=begin
/***********************************************************************

  A JavaScript compressor.

  This version is suitable for Ruby.

  This file implements some AST processors.  They work on data built
  by parsejs.rb.

  Exported functions:

    - ast_mangle(ast, options) -- mangles the variable/def names
      in the AST.  Returns an AST.

    - ast_squeeze(ast) -- employs various optimizations to make the
      final generated code even smaller.  Returns an AST.

    - gen_code(ast, options) -- generates JS code from the AST.  Pass
      true (or an object, see the code for some options) as second
      argument to get "pretty" (indented) code.


 ***********************************************************************/
=end

require 'parsejs'

# require 'uglifyjs/ast_walker'
# require 'uglifyjs/scope'
# require 'uglifyjs/ast_squeeze'
require 'uglifyjs/util'
require 'uglifyjs/ast_mangle'
require 'uglifyjs/ast_squeeze'
require 'uglifyjs/gen_code'

module Uglifyjs

  include Util
  # include ASTWalker
  # include Scope
  # include ASTMangle
  # include ASTSqueeze
  include GenCode

  class AST

    def initialize(ast)
      @ast = ast
    end

    def mangle
      @ast = Mangle.new(@ast).go
    end

    def squeeze
      @ast = Squeeze::Squeeze.new(@ast).go
    end

    def gen_code
      mangle
      squeeze
      #puts "MANGLED AST: #{@ast.inspect}"
      GenCode::Generator.new(@ast).go
    end

    protected

  end
end

#var jsp = require("./parse-js"),
#    slice = jsp.slice,
#    member = jsp.member,
#    PRECEDENCE = jsp.PRECEDENCE,
#    OPERATORS = jsp.OPERATORS;
#
#
## /* -----[ Exports ]----- */
#
#exports.ast_walker = ast_walker;
#exports.ast_mangle = ast_mangle;
#exports.ast_squeeze = ast_squeeze;
#exports.gen_code = gen_code;
#exports.ast_add_scope = ast_add_scope;
#exports.set_logger = function(logger) { warn = logger };
#exports.make_string = make_string;
#exports.split_lines = split_lines;
#exports.MAP = MAP;
#
## # keep this last!
#exports.ast_squeeze_more = require("./squeeze-more").ast_squeeze_more;