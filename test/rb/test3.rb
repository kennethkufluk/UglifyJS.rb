require 'Tokenizer'
bob = Tokenizer::Tokenizer.new("864e5")
puts bob.next_token().inspect
