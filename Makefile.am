AM_CXXFLAGS = -Wall -DEKHO_DATA_PATH='"$(datadir)/ekho-data"'
CXXFLAGS += -O0

FESTIVAL_H = @FESTIVAL_HEADER@
SPEECH_TOOLS_H = @SPEECH_TOOLS_HEADER@

host_cpu=`uname -p`

noinst_LIBRARIES = libekho.a
noinst_HEADERS = src/stdafx.h 
include_HEADERS = src/ekho.h src/ekho_dict.h src/character.h src/phonetic_symbol.h src/zhy_symbol_map.h src/zh_symbol_map.h src/ekho_typedef.h src/ekho_impl.h utfcpp/source/utf8.h

libekho_a_SOURCES = src/libekho.cpp src/libekho_impl.cpp src/ekho_dict.cpp src/number.cpp src/musicxml.cpp src/zhy_symbol_map.cpp src/zh_symbol_map.cpp src/symbol_array.cpp sonic/sonic.c
libekho_a_CXXFLAGS = -I$(srcdir)/utfcpp/source -I$(srcdir)/sonic @CFLAG_PULSEAUDIO@ @CFLAG_FESTIVAL@ -pthread @CFLAG_FRISO@ -std=c++11 -I$(srcdir)/libmusicxml/src/elements -I$(srcdir)/libmusicxml/src/lib -I$(srcdir)/libmusicxml/src/visitors -I$(srcdir)/libmusicxml/src/files -I$(srcdir)/libmusicxml/src/parser
LIBEKHO_LIB = @LIB_PULSEAUDIO@ @LIB_FESTIVAL@ @LIB_MP3LAME@ -lsndfile -lespeak @LIB_FRISO@

if ENABLE_FRISO
noinst_LIBRARIES += libfriso.a
libfriso_a_SOURCES = friso-src/friso_array.c friso-src/friso.c friso-src/friso_hash.c friso-src/friso_lexicon.c friso-src/friso_link.c friso-src/friso_string.c friso-src/friso_GBK.c friso-src/friso_UTF8.c friso-src/friso_ctype.c
libfriso_a_CFLAGS = @CFLAG_FRISO@
endif

bin_PROGRAMS = test_ekho ekho

ekho_SOURCES = src/ekho.cpp
ekho_LDADD = libekho.a $(LIBEKHO_LIB) @LIB_GTK2@ $(srcdir)/libmusicxml/libmusicxml2.a
ekho_CXXFLAGS = @CFLAG_GTK2@ -I$(srcdir)/utfcpp/source -I$(srcdir)/sonic -pthread @CFLAG_FRISO@ -lstdc++
	
test_ekho_SOURCES = src/test_ekho.cpp
test_ekho_CXXFLAGS = -I$(srcdir)/utfcpp/source -I$(srcdir)/sonic @CFLAG_FRISO@
test_ekho_LDADD = libekho.a $(LIBEKHO_LIB) $(srcdir)/libmusicxml/libmusicxml2.a

DATA_DIR = $(srcdir)/ekho-data
DATA_FILES = $(DATA_DIR)/zhy_list $(DATA_DIR)/zh_list $(DATA_DIR)/zh_listx $(DATA_DIR)/zh_patch $(DATA_DIR)/zhy.dict $(DATA_DIR)/zh.dict $(DATA_DIR)/ka_list $(DATA_DIR)/bo_list $(DATA_DIR)/ko_list $(DATA_DIR)/ngangien_list $(DATA_DIR)/pinyin.index $(DATA_DIR)/pinyin.voice $(DATA_DIR)/jyutping.index $(DATA_DIR)/jyutping.voice $(DATA_DIR)/toisanese_list
DATA_SUB_DIRS = $(DATA_DIR)/alphabet $(DATA_DIR)/hakka $(DATA_DIR)/tibetan $(DATA_DIR)/ngangien $(DATA_DIR)/hangul $(DATA_DIR)/toisanese

COPYRIGHT_FILES = $(DATA_DIR)/COPYING-jyutping-wong $(DATA_DIR)/COPYING-pinyin-yali $(DATA_DIR)/COPYING-hakka-weicheng $(DATA_DIR)/COPYING-hangul-haesung $(DATA_DIR)/COPYING-ngangien-qianshan $(DATA_DIR)/COPYING-tibetan-trinley
EXTRA_DIST = $(COPTRIGHT_FILES) # this line seems no longer work. don't know why

install-data-hook:
	mkdir -p $(DESTDIR)$(datadir)/ekho-data
if DIST_VOICE_DATA
	cp -RLf $(DATA_SUB_DIRS) $(DESTDIR)$(datadir)/ekho-data/
endif
	install $(DATA_FILES) $(DESTDIR)$(datadir)/ekho-data/
	cp -Rf $(DATA_DIR)/festival $(DESTDIR)$(datadir)/ekho-data/

uninstall-hook:
	rm -rf $(DESTDIR)$(datadir)/ekho-data

dist-hook:
	mkdir -p $(distdir)/ekho-data
if DIST_VOICE_DATA
	cp -RL $(DATA_SUB_DIRS) $(distdir)/ekho-data/
endif
	cp $(srcdir)/collect_ekho_logs.sh $(distdir)/
	cp $(DATA_FILES) $(distdir)/ekho-data/
	cp $(COPYRIGHT_FILES) $(distdir)/ekho-data/
	if [ -e $(srcdir)/ekho-data/festival/lib/voices/spanish ]; then mv $(srcdir)/ekho-data/festival/lib/voices/spanish $(srcdir)/ekho-data/; fi
	cp -R $(srcdir)/ekho-data/festival $(distdir)/ekho-data/
	if [ -e $(srcdir)/ekho-data/spanish ]; then mv $(srcdir)/ekho-data/spanish $(srcdir)/ekho-data/festival/lib/voices/; fi
	cp -R $(srcdir)/utfcpp $(distdir)/
	cp -R $(srcdir)/sonic $(distdir)/
	cp -R $(srcdir)/speechd-api $(distdir)/
	cp -R $(srcdir)/libmusicxml $(distdir)/
	cp $(srcdir)/install.pl $(distdir)/
	cp $(srcdir)/deploy.sh $(distdir)/
	find $(distdir) -name ".svn" | xargs rm -rf
	find $(distdir) -name ".git" | xargs rm -rf

clean-hook:
	cd $(srcdir)/libmusicxml/linux && make clean
if ENABLE_SPEECHD
	cd $(srcdir)/speechd-api && make clean
endif

all: all-local

all-local:
	cd $(srcdir)/libmusicxml/linux && make
if ENABLE_SPEECHD
	cd $(srcdir)/speechd-api && ./build.sh && ./configure --libdir=/usr && make
endif

if ENABLE_SPEECHD
install: install-am
	cp $(srcdir)/speechd-api/src/modules/.libs/sd_ekho /usr/lib/speech-dispatcher-modules/
	cp $(srcdir)/speechd-api/config/modules/ekho.conf /etc/speech-dispatcher/modules/
	mkdir -p ~/.config/speech-dispatcher/modules/
	cp $(srcdir)/speechd-api/config/modules/ekho.conf ~/.config/speech-dispatcher/modules/
endif
