$:.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))

require 'test/unit'
require 'uglifyjs'

class UglifyjsTest < Test::Unit::TestCase
  FIXTURES_DIR = File.join(File.dirname(__FILE__), 'fixtures')
  NODE_TESTS = File.join(File.dirname(__FILE__), 'node', 'test.js')

  def test_jquery
    file_test(File.join(FIXTURES_DIR, 'jquery.js'))
  end
  def test_json2
    file_test(File.join(FIXTURES_DIR, 'json2.js'))
  end
  def test_simple
    file_test(File.join(FIXTURES_DIR, 'simple.js'))
  end
  def test_quick
    file_test(File.join(FIXTURES_DIR, 'quicktest.js'))
  end

  protected

  def file_test(filename)
    js = File.open(filename, 'rb') { |f| f.read }
    # node
    parse_node = ''
    time_js1 = Time.now
    parse_node = `#{NODE_TESTS} #{filename}`
    time_js2 = Time.now
    puts parse_node
    nodeFile = File.new("output_node.jstest", "w")
    nodeFile.write(parse_node.gsub(/\}/,"}\n")) #add some line breaks for easier debugging
    nodeFile.close

    # ruby
    time_rb1 = Time.now
    parse_rb = Parsejs::Parse.new(js).parse
    # puts parse_rb.inspect
    genned_js = Uglifyjs::AST.new(parse_rb).mangle.squeeze.gen_code + "\n"
    time_rb2 = Time.now
    puts genned_js
    aFile = File.new("output_rb.jstest", "w")
    aFile.write(genned_js.gsub(/\}/,"}\n")) #add some line breaks for easier debugging
    aFile.close

    if parse_node!=parse_rb
      # puts `diff output_node.jstest output_rb.jstest`
      # puts `diffmerge output_node.jsmin output_rb.jsmin`
    end
    puts "JS #{time_js2 - time_js1}"
    puts "RUBY #{time_rb2 - time_rb1}"
    assert (parse_node==genned_js), "#{filename} failed"
  end

end




