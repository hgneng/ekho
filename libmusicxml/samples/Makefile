subprojects := elements interface files lib parser visitors guido operations

SRC = $(wildcard *.cpp) $(wildcard */*.cpp)
OBJ = $(SRC:.cpp=.o)
LIB = ../libmusicxml2.a
CXXFLAGS := -g -O3 -Wall -Wuninitialized $(addprefix -I../src/, $(subprojects))

applications := countnotes xmlread xmlclone xmliter xml2guido xml2midi readunrolled randomMusic

all : $(applications)


countnotes: countnotes.cpp 
	gcc $(CXXFLAGS) countnotes.cpp $(LIB) -lstdc++ -o countnotes

xmlread: xmlread.cpp 
	gcc $(CXXFLAGS) xmlread.cpp $(LIB) -lstdc++ -o xmlread

xmlclone: xmlclone.cpp 
	gcc $(CXXFLAGS) xmlclone.cpp $(LIB) -lstdc++ -o xmlclone

xmliter : xmliter.cpp
	gcc $(CXXFLAGS) xmliter.cpp $(LIB) -lstdc++ -o xmliter

xml2guido: xml2guido.cpp 
	gcc $(CXXFLAGS) xml2guido.cpp $(LIB) -lstdc++ -lm -o xml2guido

xml2midi: xml2midi.cpp 
	gcc $(CXXFLAGS) xml2midi.cpp $(LIB) -lstdc++ -lm -o xml2midi

readunrolled: readunrolled.cpp 
	gcc $(CXXFLAGS) readunrolled.cpp $(LIB) -lstdc++ -o readunrolled

randomMusic: RandomMusic.cpp 
	gcc $(CXXFLAGS) RandomMusic.cpp $(LIB) -lstdc++ -o randomMusic

transpose: transpose.cpp 
	gcc $(CXXFLAGS) transpose.cpp $(LIB) -lstdc++ -o transpose

partsummary: partsummary.cpp 
	gcc $(CXXFLAGS) partsummary.cpp $(LIB) -lstdc++ -o partsummary

clean :
	rm -f $(applications) $(OBJ)
	rm -rf *.dSYM

dep:
	makedepend -- $(CXXFLAGS) -- $(SRC)

# DO NOT DELETE
