=begin
/***********************************************************************

  A JavaScript compressor.

  This version is suitable for Ruby.

  This file implements some AST processors.  They work on data built
  by parsejs.rb.

  Exported functions:

    - mangle -- mangles the variable/def names
      in the AST.

    - squeeze -- employs various optimizations to make the
      final generated code even smaller.

    - gen_code -- generates JS code from the AST.  Pass
      true (or an object, see the code for some options) as second
      argument to get "pretty" (indented) code.

 ***********************************************************************/

  Distributed under the BSD license:

    UglifyJS.rb and the libraries within
    Copyright 2011 (c) Kenneth Kufluk <kenneth@kufluk.com>

    Based on UglifyJS,
    Copyright 2010 (c) Mihai Bazon <mihai.bazon@gmail.com>

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

        * Redistributions of source code must retain the above
          copyright notice, this list of conditions and the following
          disclaimer.

        * Redistributions in binary form must reproduce the above
          copyright notice, this list of conditions and the following
          disclaimer in the documentation and/or other materials
          provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER “AS IS” AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
    OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
    TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
    THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGE.

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
      @ast = Mangler.new(@ast).getAST
      # puts "MANGLED AST: #{@ast.inspect}"
      self
    end

    def squeeze
      @ast = Squeezer.new(@ast).getAST
      # puts "SQUEEZED AST: #{@ast.inspect}"
      self
    end

    def gen_code
      ASTCodeGenerator.new(@ast).generate
    end

  end
end