Gem::Specification.new do |s|
  s.name        = "uglifyjs"
  s.version     = '0.0.1'
  s.platform    = Gem::Platform::RUBY
  s.authors     = ["Kenneth Kufluk"]
  s.email       = ["kenneth@twitter.com"]
  s.homepage    = "http://twitter.com"
  s.summary     = "JavaScript Minifier, ported from UglifyJS"
  s.files        = Dir.glob("{lib}/**/*")
  s.require_path = 'lib'
  s.add_dependency('parsejs', '>= 0.0.1')
end