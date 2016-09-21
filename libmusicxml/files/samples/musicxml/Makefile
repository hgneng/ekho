
files  = $(wildcard *.xml)
gmn := $(patsubst %.xml, %.gmn, $(files))

all : $(gmn)

clean :
	rm -f $(gmn)

%.gmn: %.xml
	xml2guido $< > $@

ActorPreludeSample.gmn : ActorPreludeSample.xml
BeetAnGeSample.gmn : BeetAnGeSample.xml
Binchois20.gmn : Binchois20.xml
BrahWiMeSample.gmn : BrahWiMeSample.xml
BrookeWestSample.gmn : BrookeWestSample.xml
Chant.gmn : Chant.xml
DebuMandSample.gmn : DebuMandSample.xml
Dichterliebe01.gmn : Dichterliebe01.xml
Echigo-Jishi.gmn : Echigo-Jishi.xml
FaurReveSample.gmn : FaurReveSample.xml
MahlFaGe4Sample.gmn : MahlFaGe4Sample.xml
MozaChloSample.gmn : MozaChloSample.xml
MozaVeilSample.gmn : MozaVeilSample.xml
MozartPianoSonata.gmn : MozartPianoSonata.xml
MozartTrio.gmn : MozartTrio.xml
Saltarello.gmn : Saltarello.xml
SchbAvMaSample.gmn : SchbAvMaSample.xml
Telemann.gmn : Telemann.xml
