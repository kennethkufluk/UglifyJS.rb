# /* -----[ Tokenizer ]----- */
require 'parsejs/util'
module Tokenizer


  # # /* -----[ Tokenizer (constants) ]----- */

  KEYWORDS_BEFORE_EXPRESSION = [
          "return",
          "new",
          "delete",
          "throw",
          "else",
          "case"
  ]


  OPERATOR_CHARS = "+-*&%=<>!?|~^".chars.to_a

  RE_HEX_NUMBER = /^0x[0-9a-f]+$/i
  RE_OCT_NUMBER = /^0[0-7]+$/
  RE_DEC_NUMBER = /^\d*\.?\d*(?:e[+-]?\d*(?:\d\.?|\.?\d)\d*)?$/i


  WHITESPACE_CHARS = (" \n\r\t\f\v"+[0x00a0,0x200b].flatten.pack('U*')).chars.to_a

  PUNC_BEFORE_EXPRESSION = "[{}(,.;:".chars.to_a

  PUNC_CHARS = "[]{}(),;:".chars.to_a

  UNARY_POSTFIX = [ "--", "++" ]

  REGEXP_MODIFIERS = "gmsiy".chars.to_a


  NEWLINE = [
    0x2028,
    0x2029
  ].flatten.pack('U*').freeze
  BOM = [
    0xFEFF
  ].flatten.pack('U*').freeze

  class JS_Parse_Error < StandardError
    def initialize(args)
      @message = args[0]
      @line = args[1]
      @col = args[2]
      @pos = args[3]
    end
    def to_s
      "#{@message} (line: #{@line}, col: #{@col}, pos: #{@pos})\n\n"
    end
  end

  class EX_EOF < StandardError
  end

  class Tokenizer

    include Util

    def initialize(text_arg)
      @syntax = {
        :text            => text_arg.gsub(/\r\n?|[\n#{NEWLINE}]/o, "\n").gsub(/^#{BOM}/o, ''),
        :pos             => 0,
        :tokpos          => 0,
        :line            => 0,
        :tokline         => 0,
        :col             => 0,
        :tokcol          => 0,
        :newline_before  => false,
        :regex_allowed   => false,
        :comments_before => []
      }
    end

    def next_token(force_regexp=false)
      return read_regexp() if force_regexp
      skip_whitespace()
      start_token()
      ch = peek()
      return token("eof") if ch.nil?
      return read_num() if (is_digit?(ch))
      return read_string() if (ch == '"' || ch == "'")
      return token("punc", next_ch()) if (HOP(PUNC_CHARS, ch))
      return handle_dot() if (ch == ".")
      return handle_slash() if (ch == "/")
      return read_operator() if (HOP(OPERATOR_CHARS, ch))
      return read_word() if (ch == "\\" || is_identifier_start?(ch))
      parse_error("Unexpected character '" + ch.to_s + "'")
    end

    def get_context(nc=nil)
      if nc
        @syntax = nc
      end
      @syntax
    end

    protected

    def peek
      #puts 'peeking'
      #puts "POS: #{@syntax[:text]} -1 #{@syntax[:pos]} -2 #{@syntax[:text][@syntax[:pos]]} -3 #{@syntax[:text][@syntax[:pos]]}"
      code = @syntax[:text][@syntax[:pos]]
      if !code.nil?
        code.chr
      else
        nil
      end
    end

    def next_ch(signal_eof=false)
      ch = peek()
      @syntax[:pos]+=1
      if signal_eof && !ch
        raise EX_EOF
      end
      if ch == "\n"
        @syntax[:newline_before] = true
        @syntax[:line]+=1
        @syntax[:col] = 0
      else
        @syntax[:col]+=1
      end
      ch
    end

    def eof?
      !@syntax.peek
    end

    def find(what, signal_eof=nil)
      pos = @syntax[:text].index(what, @syntax[:pos])
      if signal_eof && pos.nil?
        raise EX_EOF
      end
      pos
    end

    def start_token
      @syntax[:tokline] = @syntax[:line]
      @syntax[:tokcol] = @syntax[:col]
      @syntax[:tokpos] = @syntax[:pos]
    end

    def token(type, value=nil, is_comment=nil)
      @syntax[:regex_allowed] = ((type == "operator" && !HOP(UNARY_POSTFIX, value)) ||
             (type == "keyword" && HOP(KEYWORDS_BEFORE_EXPRESSION, value)) ||
             (type == "punc" && HOP(PUNC_BEFORE_EXPRESSION, value)))
      ret = {
        :type  => type,
        :value => value,
        :line  => @syntax[:tokline],
        :col   => @syntax[:tokcol],
        :pos   => @syntax[:tokpos],
        :nlb   => @syntax[:newline_before]
      }
      if !is_comment.nil?
        ret[:comments_before] = @syntax[:comments_before]
        @syntax[:comments_before] = []
      end
      @syntax[:newline_before] = false
      ret
    end

    def skip_whitespace
      while HOP(WHITESPACE_CHARS, peek())
        next_ch()
      end
    end

    def read_while(pred)
      ret = ""
      ch = peek()
      i = 0
      while ch && pred.call(ch, i)
        i+=1
        ret += next_ch()
        ch = peek()
      end
      ret
    end

    def parse_error(err)
      js_error(err, @syntax[:tokline], @syntax[:tokcol], @syntax[:tokpos])
    end

    def read_num(prefix=nil)
      @read_num = {
        :has_e => false,
        :after_e =>false,
        :has_x=>false,
        :has_dot=>(prefix == ".")
      }
      num = read_while(lambda do |ch, i|
        if ch == "x" || ch == "X"
          return false if @read_num[:has_x]
          return @read_num[:has_x] = true
        end
        if @read_num[:has_x] && (ch == "E" || ch == "e")
          return false if @read_num[:has_e]
          return @read_num[:has_e] = @read_num[:after_e] = true
        end
        if ch == "-"
          return true if (@read_num[:after_e] || (i == 0 && !prefix))
          return false
        end
        return @read_num[:after_e] if ch == "+"
        @read_num[:after_e] = false
        if ch == "."
          if !@read_num[:has_dot] && !@read_num[:has_x]
            return @read_num[:has_dot] = true
          end
          return false
        end
        is_alphanumeric_char?(ch)
      end)
      if prefix
        num = prefix + num
      end
      valid = parse_js_number(num)
      if !valid.nil?
        return token("num", valid)
      else
        parse_error("Invalid @syntax: " + num)
      end
    end

    def read_escaped_char
      ch = next_ch(true)
      case ch
        when "n"
          "\n"
        when "r"
          "\r"
        when "t"
          "\t"
        when "b"
          "\b"
        when "v"
          "\v"
        when "f"
          "\f"
        when "0"
          "\0"
        when "x"
          hex_bytes(2).chr
        when "u"
          "x" #kpk fixme! hex_bytes(4).chr
        when "\n"
          ""
        else
          ch
      end
    end

    def hex_bytes(n)
      num = 0
      while n > 0
        digit = next_ch(true)
        if !digit.nil?
           digit = digit.to_i(16)
           if digit.nil?
             parse_error("Invalid hex-character pattern in string")
           end
           num = (num << 4) | digit
           n -= 1
        end
      end
      num
    end

    def read_string
      return with_eof_error("Unterminated string constant", lambda do
        quote = next_ch()
        ret = ""
        while true
          ch = next_ch(true)
          if ch == "\\"
            # read OctalEscapeSequence (XXX: deprecated if "strict mode")
            # https://github.com/mishoo/UglifyJS/issues/178
            @read_string = {
              :octal_len => 0,
              :first => nil
            }
            ch = read_while(lambda do |*args|
              ch = args[0]
              if ch >= "0" && ch <= "7"
                if !@read_string[:first]
                  @read_string[:first] = ch
                  return @read_string[:octal_len]+=1
                elsif (@read_string[:first] <= "3" && @read_string[:octal_len] <= 2)
                  return @read_string[:octal_len]+=1
                elsif (@read_string[:first] >= "4" && @read_string[:octal_len] <= 1)
                  return @read_string[:octal_len]+=1
                end
              end
              return false
            end)
            if @read_string[:octal_len] > 0
              ch = ch.to_i(8).chr
            else
              ch = read_escaped_char()
            end
          elsif (ch == quote)
            break
          end
          ret += ch
        end
        return token("string", ret)
      end)
    end

    def read_line_comment
      next_ch()
      i = find("\n")
      if i == -1
        ret = @syntax[:text][@syntax[:pos]..-1]
        @syntax[:pos] = @syntax[:text].length
      else
        ret = @syntax[:text][@syntax[:pos],i]
        @syntax[:pos] = i
      end
      return token("comment1", ret, true)
    end

    def read_multiline_comment
      next_ch()
      return with_eof_error("Unterminated multiline comment", lambda do
        i = find("*/", true)
        text = @syntax[:text].slice(@syntax[:pos], i)
        tok = token("comment2", text, true)
        @syntax[:pos] = i + 2
        @syntax[:line] += text.split("\n").length - 1
        @syntax[:newline_before] = !text.index("\n").nil?

        # https://github.com/mishoo/UglifyJS/issues/#issue/100
        if !(/^@cc_on/i =~ text).nil?
          warn("WARNING: at line " + @syntax[:line])
          warn("*** Found \"conditional comment\": " + text)
          warn("*** UglifyJS DISCARDS ALL COMMENTSYNTAX.  This means your code might no longer work properly in Internet Explorer.")
        end

        return tok
      end)
    end

    def read_name
      backslash = false
      name = ""
      while (ch = peek()) != nil
        if !backslash
          if ch == "\\"
            backslash = true
            next_ch()
          elsif is_identifier_char?(ch)
            name += next_ch()
          else
            break
          end
        else
          if ch != "u"
            parse_error("Expecting UnicodeEscapeSequence -- uXXXX")
          end
          ch = read_escaped_char()
          if !is_identifier_char?(ch)
            parse_error("Unicode char: " + ch[0].ord + " is not valid in identifier")
          end
          name += ch
          backslash = false
        end
      end
      name
    end

    def read_regexp
      return with_eof_error("Unterminated regular expression", lambda do
        prev_backslash = false
        regexp = ""
        in_class = false
        while ch = next_ch(true)
          if prev_backslash
            regexp += "\\" + ch
            prev_backslash = false
          elsif ch == "["
            in_class = true
            regexp += ch
          elsif ch == "]" && in_class
            in_class = false
            regexp += ch
          elsif ch == "/" && !in_class
            break
          elsif ch == "\\"
            prev_backslash = true
          else
            regexp += ch
          end
        end
        mods = read_name()
        return token("regexp", [ regexp, mods ])
      end)
    end

    def grow(op)
      return op if !peek()
      bigger = op + peek()
      if HOP(OPERATORS, bigger)
        next_ch()
        return grow(bigger)
      else
        return op
      end
    end

    def read_operator(prefix=nil)
      return token("operator", grow(prefix || next_ch()))
    end

    def handle_slash
      next_ch()
      regex_allowed = @syntax[:regex_allowed]
      case peek()
        when "/"
          @syntax[:comments_before].push(read_line_comment())
          @syntax[:regex_allowed] = regex_allowed
          return next_token()
        when "*"
          @syntax[:comments_before].push(read_multiline_comment())
          @syntax[:regex_allowed] = regex_allowed
          return next_token()
      end
      return @syntax[:regex_allowed] ? read_regexp() : read_operator("/")
    end

    def handle_dot
      next_ch()
      return is_digit?(peek()) ? read_num(".") : token("punc", ".")
    end

    def read_word
      word = read_name()
      (
        !HOP(KEYWORDS, word) ?
        token("name", word) :
        HOP(OPERATORS, word) ?
        token("operator", word) :
        HOP(KEYWORDS_ATOM, word) ?
        token("atom", word) :
        token("keyword", word)
      )
    end

    def with_eof_error(eof_error, cont)
      begin
        eof_error
        return cont.call
      rescue Exception=>ex
        if ex === EX_EOF
          parse_error(eof_error)
        else
          raise ex
        end
      end
    end

    def parse_js_number(num)
      if RE_HEX_NUMBER =~ num
        num[0..1].to_i(16)
      elsif RE_OCT_NUMBER =~ num
        num[0...1].to_i(8)
      elsif RE_DEC_NUMBER =~ num
        (num.to_i == num.to_f) ? num.to_i : num.to_f
      end
    end

    def js_error(message, line='', col='', pos='')
      raise JS_Parse_Error, [message, line, col, pos]
    end



  end

end
