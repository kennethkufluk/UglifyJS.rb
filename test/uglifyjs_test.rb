$:.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))

require 'test/unit'
require 'uglifyjs'

class UglifyjsTest < Test::Unit::TestCase
  FIXTURES_DIR = File.join(File.dirname(__FILE__), 'fixtures', 'json2.js')
  NODE_TESTS = File.join(File.dirname(__FILE__), 'node', 'test.js')

  def test_all_js
    Dir[FIXTURES_DIR].each do |filename|
      file_test(filename)
    end
  end

  def teardown
    # remove the ast files
  end

  protected

  def file_test(filename)
    js = File.open(filename, 'rb') { |f| f.read }
    # node
    parse_node = ''
    parse_node = `#{NODE_TESTS} #{filename}`
    puts parse_node
    nodeFile = File.new("output_node.jstest", "w")
    nodeFile.write(parse_node.gsub(/\}/,"}\n"))
    nodeFile.close

    # ruby
    parse_rb = Parsejs::Parse.new(js).parse
    # puts parse_rb.inspect
    genned_js = Uglifyjs::AST.new(parse_rb).gen_code + "\n"
    puts genned_js
    aFile = File.new("output_rb.jstest", "w")
    aFile.write(genned_js.gsub(/\}/,"}\n"))
    aFile.close

    if parse_node!=parse_rb
      #puts `diff output_node.jstest output_rb.jstest`
      # puts `diffmerge output_node.jstest output_rb.jstest`
    end
    assert (parse_node==genned_js), "#{filename} failed"
  end

end




