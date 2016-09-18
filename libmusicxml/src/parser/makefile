
APPL = xml
OUT  = xmllex.cpp xmlparse.cpp xmlparse.hpp
OBJ  = xmllex.o xmlparse.o
CXXFLAGS = -I.. -DMAIN
YACC = bison
LEX = flex
PREFIX := libmxml

code : $(OUT)

xml : $(OBJ)
	gcc $(DEBUG) $(OBJ) -lstdc++ -o $(APPL)

xmlparse.cpp : xml.y
	$(YACC) -d -o xmlparse.cpp  -p $(PREFIX) xml.y

xmlparse.hpp : xml.y
	$(YACC) -d -o xmlparse.cpp  -p $(PREFIX) xml.y

xmllex.cpp : xml.l xmlparse.hpp
	$(LEX)  -oxmllex.c++ -I -P$(PREFIX) xml.l

clean:
	rm -f $(APPL) $(OUT) $(OBJ)
