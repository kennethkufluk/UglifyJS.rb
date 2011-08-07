require 'ParseJS'
bob = ParseJS::Parser::Parse.new("(function(){var script=document.createElement('script');script.src='http://mir.aculo.us/dom-monster/dommonster.js?'+Math.floor((+new Date)/(864e5));document.body.appendChild(script);}())").parse
puts bob.inspect.gsub(/, /, ',')
