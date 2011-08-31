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

require 'uglifyjs/util'
require 'uglifyjs/ast_mangle'
require 'uglifyjs/ast_squeeze'
require 'uglifyjs/gen_code'

module Uglifyjs

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
      puts "MANGLED AST: #{@ast.inspect}"
      squeeze
      puts "SQUEEZED AST: #{@ast.inspect}"
      GenCode::Generator.new(@ast).go
    end

    protected

  end
end