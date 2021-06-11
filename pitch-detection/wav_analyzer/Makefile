CXX		?= gcc
CXX_FLAGS 	:= -ansi -pedantic -Werror -Wall -O3 -std=c++17 -flto -L$(CURDIR)/../lib -lpitch_detection -lffts -llibnyquist -llibwavpack -llibopus -I$(CURDIR)/../include -Wl,-R$(CURDIR)/../lib -lgflags -lmlpack -larmadillo
CXX_DEBUG_FLAGS := -std=c++17 -L$(CURDIR)/../lib -lpitch_detection -lffts -llibnyquist -llibwavpack -llibopus -I$(CURDIR)/../include -Wl,-R$(CURDIR)/../lib -ggdb -fno-omit-frame-pointer -lgflags -lmlpack -larmadillo

all: wav_analyzer

debug: CXX_FLAGS=$(CXX_DEBUG_FLAGS)
debug: wav_analyzer

wav_analyzer:
	$(CXX) ./*.cpp -o $@ $(CXX_FLAGS)

clean:
	-rm wav_analyzer

.PHONY: clean
