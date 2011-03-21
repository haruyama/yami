#!ruby -Ku
require 'jcode'
require "rexml/document"

def node(doc,level)
	doc.elements.each('node') { |element|
			if level < 3
			   print '*'*(level+1)
			elsif level >5
			next
			else
   			   print '-'*(level-2)

			end 
		
          	print ' '

		text = element.attributes['TEXT']
		text = text.gsub(/\n/,'')
		puts text
		node(element,level+1)
	}
end

file = File.new( ARGV[0] )
doc = REXML::Document.new file

node(doc.elements["/map"],0)
