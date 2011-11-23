#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-

str = ARGF.read

def make_sorted_suffix_array(str)
  a = Array.new(str.size){|i| i}
  b = a.sort{ |i,j|
    str[i .. -1] <=> str[j .. -1]
  }
  sa = []
  b.each { |i|
    sa << str[i .. -1]
  }
  sa
end

def commmon_prefix_length(a, b)
  (0..a.size).each { |i|
    return i  if b.size < i or a[i] != b[i]
  }

end

def longest_substring(sa)
  maxlen = 0
  index  = 0
  (0 .. sa.size - 2).each { |i|
    pl = commmon_prefix_length(sa[i], sa[i + 1])
    if pl > maxlen
      maxlen = pl
      index  = i
    end
  }
  sa[index][0, maxlen]
end


sa = make_sorted_suffix_array(str)
p longest_substring(sa)
