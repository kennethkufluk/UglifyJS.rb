module Util


  ## /* -----[ Utilities ]----- */

  def defaults(args, defs)
   ret = {}
   if args === true
     args = {}
   end
   for i in defs
     if (HOP(defs, i))
       ret[i] = (args && HOP(args, i)) ? args[i] : defs[i]
     end
   end
   return ret
  end

  def empty(b)
    return !b || (b[0] == "block" && (!b[1] || b[1].length == 0))
  end

  def is_identifier(name)
   return /^[a-z_$][a-z0-9_$]*$/i =~ name &&
     name != "this" &&
     !HOP(KEYWORDS_ATOM, name) &&
     !HOP(RESERVED_WORDS, name) &&
     !HOP(KEYWORDS, name)
  end

  def HOP(obj, prop)
    # in Javascript, you convert your arrays to hashes, then check the hash
    # crazy eh.
    # in Ruby, we'll just check the array
    obj.include?(prop)
  end

  def MAP(a, f)
    ret = []
    for i in (0...a.length)
      val = f.call(a[i], i)
      if val.is_a?(AtTop)
        ret.unshift(val.v)
      else
        ret.push(val)
      end
    end
    return ret
  end

  class AtTop
    def initialize(val)
      @v = val
    end
  end

  def member(name, array)
    array.include?(name)
  end

end