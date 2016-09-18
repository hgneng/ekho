
dtddir := ../../dtds/current
dtds := $(wildcard $(dtddir)/[abcdilnpst]*.mod)
elts  := elements.txt
eltname := '[-a-zA-Z0-9_.:]*'

all : $(elts) elements.h typedefs.h factory.cpp

elements : $(elts)

$(elts) : $(dtds)
	grep  \<\!ELEMENT $(dtds) | \
	sed -e 's/\(..*\.mod\):<\!ELEMENT[ 	]*\($(eltname)\)[ 	]*.*/\2	\/\/ \1/' > $(elts)

elements.h : templates/elements.txt $(elts)
	templates/elements $(elts) templates constants > elements.h

factory.cpp : templates/factory.txt $(elts)
	templates/elements $(elts) templates map > factory.cpp

typedefs.h :  templates/typedefs.txt $(elts)
	templates/elements $(elts) templates types > typedefs.h

clean :
	rm -f $(elts) elements.h factory.cpp typedefs.h
