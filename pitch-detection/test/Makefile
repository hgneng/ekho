CXX		?= gcc
CXX_FLAGS 	:= -ansi -pedantic -Werror -Wall -O3 -std=c++17 -flto -lffts -L$(CURDIR)/../lib -lpitch_detection -I$(CURDIR)/../include -Wl,-R$(CURDIR)/../lib -larmadillo -lmlpack
CXX_DEBUG_FLAGS := -ansi -pedantic -Werror -Wall -O0 -std=c++17 -flto -lffts -L$(CURDIR)/../lib -lpitch_detection -I$(CURDIR)/../include -Wl,-R$(CURDIR)/../lib -ggdb -larmadillo -lmlpack

all: test bench

test:
	$(CXX) ./$@*.cpp ./util.cpp -o $@ $(CXX_FLAGS) -lpthread -lgtest -lgtest_main

bench:
	$(CXX) ./$@.cpp ./util.cpp -o $@ $(CXX_FLAGS) -lpthread -lbenchmark

debug: CXX_FLAGS=$(CXX_DEBUG_FLAGS)
debug: test bench

clean:
	-rm bench test

.PHONY: clean
