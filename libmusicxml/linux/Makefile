subprojects := ../src/elements ../src/guido ../src/interface ../src/files ../src/lib ../src/parser ../src/operations ../src/visitors

SRC = $(wildcard ../src/*.cpp) $(wildcard ../src/*/*.cpp)
APPSRC = $(wildcard ../samples/*.cpp) 
OBJ = $(SRC:.cpp=.o)

VPATH = $(subprojects)

CXXFLAGS := -g -O3 -Wall -Wuninitialized $(addprefix -I, $(subprojects)) -I../src/factory

lib := ../libmusicxml2.a

all : $(lib) samples

samples : 
	make -C ../samples

$(lib): $(OBJ)
	ar cru $(lib) $(OBJ) 
	ranlib $(lib)

clean :
	rm -f $(lib) $(OBJ)

dep:
	makedepend -- $(CXXFLAGS) -- $(SRC)


