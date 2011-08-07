$:.unshift(File.join(File.dirname(__FILE__), '..', '..', 'lib'))

require 'test/unit'
require 'uglifyjs'

class UglifyjsTest < Test::Unit::TestCase
  TEST_DIR = File.join(File.dirname(__FILE__), "compress", "test", '*.js')
  EXPECTED_DIR = File.join(File.dirname(__FILE__), "compress", "expected")

  def test_all_js
    Dir[TEST_DIR].each do |filename|
      file_test(filename)
    end
  end

  def teardown
    # remove the ast files
  end

  protected

  def compress(code)
    ast = Parsejs::Parse.new(code).parse
    #ast = jsp.parse(code)
    #ast = pro.ast_mangle(ast)
    #ast = pro.ast_squeeze(ast, {no_warnings: true, extra: true})
    #ast = pro.ast_squeeze_more(ast)
    return Uglifyjs::AST.new(ast).gen_code
    #return pro.gen_code(ast)
  end

  def file_test(filename)
    content = File.open(filename, 'rb') { |f| f.read }
    expected = File.open(File.join(EXPECTED_DIR, File.basename(filename)), 'rb') { |f| f.read }

    outputCompress = compress(content)

    # Check if the noncompressdata is larger or same size as the compressed data
    assert(content.length >= outputCompress.length, 'content is shortened')

    # Check that a recompress gives the same result
    outputReCompress = compress(outputCompress)
    assert_equal(outputReCompress, outputCompress, 'compressing twice gives same result')

    # Check if the compressed output is what is expected
    assert_equal(expected.gsub(/(\r?\n)+$/, ""), outputCompress, 'compressed output matches expected result')

  end

end
