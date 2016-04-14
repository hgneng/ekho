
GPP = g++
SCHEME = gsi
STDOPTS = -Wall -O2
LIBS = -s

all : sr-convert.exe sr-convert8.exe

sr-convert.exe : sr-convert.o dsp.o
	$(GPP) $^ -o $@ $(LIBS)

sr-convert8.exe : sr-convert8.o dsp.o
	$(GPP) $^ -o $@ $(LIBS)

sr-convert.o : sr-convert.cpp edeque.h dsp.h skips.out
	$(GPP) -c $< -o $@ -DOUTPUT16BIT $(STDOPTS)

sr-convert8.o : sr-convert.cpp edeque.h dsp.h skips.out
	$(GPP) -c $< -o $@ -DOUTPUT8BIT $(STDOPTS)

skips.out : make-tables.scm
	$(SCHEME) $<

dsp.o : dsp.cpp dsp.h edeque.h
	$(GPP) -c $< -o $@ $(STDOPTS)

