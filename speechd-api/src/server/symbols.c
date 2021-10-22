
/*
 * symbols.c -- Implements functions handling symbols conversion,
 *              including punctuation, for Speech Dispatcher
 *
 * Copyright (C) 2001,2002,2003, 2007, 2017 Brailcom, o.p.s
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

/* Based off NVDA's symbols replacement code (GPLv2+):
 * https://github.com/nvaccess/nvda/blob/master/source/characterProcessing.py
 *
 * OVERVIEW
 *
 * This file contains all of the logic related to reading, processing and
 * using NVDA symbols replacement files.  It should be 100% compatible with
 * NVDA's equivalent.
 *
 * The files are read by the speech_symbols_load() family of functions.
 * Each symbol is loaded into a SpeechSymbol structure, and the symbols of
 * a file (both simple and complex) are loaded into a SpeechSymbols (note the
 * plural form) structure.
 *
 * The loaded symbols are compiled into GLib PCRE regular expressions
 * (originally a Python one, but they are compatible enough) and converted to
 * a fully usable form into a list of SpeechSymbolProcessor.  These processors
 * are then usable to pre-process an input text with
 * speech_symbols_processor_process_text().
 *
 * The loading steps are automatically handled when calling
 * speech_symbols_processor_new().  To avoid re-processing files more than
 * once even if they are used by different SpeechSymbolProcessor, the loaded
 * files are cached as SpeechSymbols into the G_symbols_dicts global variable.
 * Similarly, lists of SpeechSymbolProcessor are cached into the
 * G_processors global variable.
 *
 * The caches are automatically loaded when looking up an entry with either
 * get_locale_speech_symbols() (for SpeechSymbols lists) or
 * get_locale_speech_symbols_processor() (for SpeechSymbolProcessor lists).
 * This loading is aware of locale strings syntax and will fallback on the
 * language code alone if the language-country combo isn't found.
 *
 * WARNING: this module is NOT thread-safe.  Most notably, the caches are not
 * thread-safe, so the public API insert_symbols() shouldn't be balled from
 * several threads at once.  This should not be an issue, as it is supposed to
 * be called from the speak thread only.
 *
 * This file is mostly a 1:1 translation of NVDA's python code doing the same
 * thing, with slight simplifications or adaptations for C, and removal of
 * unused features like loading user-specific symbols files.
 */

/*
 * TODO:
 * - support NUL byte representation.  However, they aren't properly handled
 *   in the rest of SPD, so it's not so important.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "symbols.h"

/* This denotes the position of some SSML tags */
struct tags {
	gsize pos;		/* Its position in the text */
	gssize shift;		/* How much its position is shifted by the current replacements */
	gint deferrable;	/* Whether it is fine to defer the tag (e.g. a mark or comment) */
	gchar *tags;		/* The content of the tags */
};

/* Speech symbol preserve modes */
typedef enum {
	SYMPRES_INVALID = -1,
	SYMPRES_NEVER = 0,	/* Never preserve the symbol */
	SYMPRES_ALWAYS = 1,	/* Always preserve the symbol */
	SYMPRES_NOREP = 2,	/* Only preserve the symbol if it is not being
				   replaced; i.e. the user has set symbol level
				   lower than the level of this symbol */
	SYMPRES_LITERAL = 3	/* Replace literally, without any spacing
				   addition */
} SymPresMode;

/* Represents a single symbol, and how it should be handled. */
typedef struct {
	char *identifier;
	char *pattern;
	char *replacement;
	SymLvl level;
	SymPresMode preserve;
	char *display_name;
} SpeechSymbol;

/* Represents all symbols in a symbols file.
 * This is roughly an internal representation of the symbols files. */
typedef struct {
	gchar *source;
	/* Ordered list of [identifier(string), pattern(string)] */
	GSList *complex_symbols;
	/* table of identifier(string):symbol(SpeechSymbol) */
	GHashTable *symbols;
} SpeechSymbols;

/* Describes a name->value translation for a field that should be loaded
 * as an integer. */
typedef struct {
	const char *name;
	int value;
} IntFieldDesc;

/* Represents a loaded and cached set of symbols in a usable form */
typedef struct {
	gchar *source;

	struct tags *tags; /* tags attached to the text */
	gint ntags; /* number of elements in tags array */

	GRegex *regex; /* compiled regular expression for parsing input */
	/* Table of identifier(string):symbol(SpeechSymbol).
	 * Indexes are pointers to symbol->identifier. */
	GHashTable *symbols;
	/* list of SpeechSymbol (weak pointers to entries in @c symbols) */
	GSList *complex_list;

	/* Level requested by user */
	SymLvl level;
	/* Level to be supported */
	SymLvl support_level;
} SpeechSymbolProcessor;

/* Map of locale code to arbitrary data. */
typedef GHashTable LocaleMap;
typedef gpointer (*LocaleMapCreateDataFunc) (const gchar *locale, const gchar *file);

/* globals for caching */

/* Map of SpeechSymbols, indexed by their locale and file */
static LocaleMap *G_symbols_dicts = NULL;
/* Map of SpeechSymbolProcessor lists, indexed by their locale */
static LocaleMap *G_processors = NULL;

/* List of files to load */
static GSList *symbols_files;

SymLvl str2SymLvl(const char *str)
{
	SymLvl punct;

	if (!strcmp(str, "no"))
		punct = SYMLVL_NO;
	else if (!strcmp(str, "none"))
		punct = SYMLVL_NONE;
	else if (!strcmp(str, "all"))
		punct = SYMLVL_ALL;
	else if (!strcmp(str, "char"))
		punct = SYMLVL_CHAR;
	else
		punct = SYMLVL_INVALID;

	return punct;
}

/*----------------------------- Locale data map -----------------------------*/

static LocaleMap *locale_map_new(GDestroyNotify value_destroy)
{
	return g_hash_table_new_full(g_str_hash, g_str_equal, g_free, value_destroy);
}

static gpointer locale_map_lookup(LocaleMap *map, const gchar *locale, const gchar *file)
{
	if (file) {
		gchar *str = g_strdup_printf("%s %s", locale, file);
		gpointer res = g_hash_table_lookup(map, str);
		g_free(str);
		return res;
	} else {
		return g_hash_table_lookup(map, locale);
	}
}

/* Fetches or creates a locale item for the map.
 * If @c locale contains a country and data for the whole locale is not found,
 * tries to load the data for the language alone. */
static gpointer locale_map_fetch(LocaleMap *map, const gchar *locale, const gchar *file,
				 LocaleMapCreateDataFunc create)
{
	guint i;

	for (i = 0; i < 2; i++) {
		gpointer value;
		gchar *l;

		if (i == 0) {
			value = locale_map_lookup(map, locale, file);
			l = g_strdup(locale);
		} else {
			gchar **parts = g_strsplit_set(locale, "_-", 2);
			if (!parts[0] || !parts[1]) {
				/* no delimiters, no need to try again */
				g_strfreev(parts);
				continue;
			}
			l = g_strdup(parts[0]);
			value = locale_map_lookup(map, l, file);
			g_strfreev(parts);
		}
		if (value) {
			g_free(l);
			return value;
		}
		/* try to create */
		value = create(l, file);
		if (value) {
			g_hash_table_insert(map, l, value);
			return value;
		}
		g_free(l);
	}

	return NULL;
}

/*--------------------- Escaping xml tags in ssml text ----------------------*/

/*
 * We need not ever speak the SSML syntax, so we need to skip the tags.
 *
 * For lookbehind and lookahead rules to be able to run, we have to really
 * remove the tags from the text, but we want to remember where they were.
 *
 * We thus build an array of the positions of the tags, that the replacement
 * function will update, so we know where to put back the tags.
 *
 * Alongside, we also have to untranslate/translate the xml entities for tag characters.
 */

/* Move tags off from the text */
static gchar *escape_ssml_text(const gchar *text, struct tags **tags_ret, gint *ntags_ret)
{
	const gchar *cur, *curtag = NULL;
	struct tags *tags;
	GString *str;
	gchar *result;
	gchar name[7];		/* Current tag name, only need to recognize against "mark", "/mark", "!--" for now */
	gsize namepos = 0;

	int filling_tag;	/* Whether we are stack tags, or text */
	int in_tag;		/* Whether we are within a tag */
	int in_tag_name;	/* Whether we are within the name part of a tag */
	int in_apos;		/* Whether we are within a '' string in a tag */
	int in_quote;		/* Whether we are within a "" string in a tag */
	gint ntags;

	/* First count how many blocks of tags we will have */
	filling_tag = 0;
	in_tag = 0;
	in_tag_name = 0;
	in_apos = 0;
	in_quote = 0;
	ntags = 0;

	for (cur = text; *cur; cur++) {
		guchar c = *cur;
	
		if (!in_tag) {
			if (c == '<') {
				in_tag = 1;
				if (!filling_tag) {
					ntags++;
					filling_tag = 1;
				}
			} else {
				/* Some text, switch to text */
				filling_tag = 0;
			}
		} else {
			if (in_apos) {
				if (c == '\'')
					in_apos = 0;
			} else if (in_quote) {
				if (c == '"')
					in_quote = 0;
			} else if (c == '\'') {
				in_apos = 1;
			} else if (c == '"') {
				in_quote = 1;
			} else if (c == '>') {
				in_tag = 0;
			}
		}
	}

	/* We can now allocate the array of blocks of tags and restart over, this time filling text and tags */
	tags = malloc(ntags * sizeof(*tags));

	filling_tag = 0;
	in_tag = 0;
	in_apos = 0;
	in_quote = 0;
	ntags = 0;

	str = g_string_sized_new(strlen(text));

	for (cur = text; *cur; cur++) {
		guchar c = *cur;

		if (!in_tag) {
			if (c == '<') {
				in_tag = 1;
				in_tag_name = 1;
				namepos = 0;
				if (!filling_tag) {
					/* Note the tags position in the text */
					tags[ntags].pos = str->len;
					/* A priori only deferrable tags */
					tags[ntags].deferrable = 1;
					curtag = cur;
					filling_tag = 1;
				}
			} else {
				if (filling_tag) {
					/* Some text, dump the tags and switch to text */
					tags[ntags].tags = g_strndup(curtag, cur - curtag);
					ntags++;
					filling_tag = 0;
				}

				if (c == '&') {
					/* Unescape ssml character sequences */
					if (!strncmp(cur, "&quot;", 6)) {
						cur += 5;
						g_string_append_c(str, '"');
					} else if (!strncmp(cur, "&apos;", 6)) {
						cur += 5;
						g_string_append_c(str, '\'');
					} else if (!strncmp(cur, "&lt;", 4)) {
						cur += 3;
						g_string_append_c(str, '<');
					} else if (!strncmp(cur, "&gt;", 4)) {
						cur += 3;
						g_string_append_c(str, '>');
					} else if (!strncmp(cur, "&amp;", 5)) {
						cur += 4;
						g_string_append_c(str, '&');
					} else
						g_string_append_c(str, c);
				} else {
					/* Pure text, append as such */
					g_string_append_c(str, c);
				}
			}
		} else {
			if (in_apos) {
				if (c == '\'')
					in_apos = 0;
			} else if (in_quote) {
				if (c == '"')
					in_quote = 0;
			} else if (c == '\'') {
				in_apos = 1;
			} else if (c == '"') {
				in_quote = 1;
			} else {
				if (in_tag_name) {
					if (c == '>' || c == ' ' || c == '\t' || c == '\r' || c == '\n') {
						in_tag_name = 0;
						name[namepos] = '\0';
						if (strcmp(name, "mark")
						 && strcmp(name, "/mark")
						 && strcmp(name, "mark/")
						 && strcmp(name, "!--")) {
							/* This is a non-deferrable tag */
							tags[ntags].deferrable = 0;
						}
					} else {
						if (namepos < sizeof(name) - 1) {
							name[namepos++] = c;
						}
					}
				}
				if (c == '>')
					in_tag = 0;
			}
		}
	}
	/* Trailing tags content */
	if (filling_tag) {
		tags[ntags].tags = g_strndup(curtag, cur - curtag);
		ntags++;
	}

	*tags_ret = tags;
	*ntags_ret = ntags;

	result = str->str;
	g_string_free(str, FALSE);

	return result;
}

/* Put back tags into the text */
static gchar *unescape_ssml_text(const gchar *text, struct tags *tags, gint ntags)
{
	GString *str;
	const gchar *cur;
	gchar *result;
	struct tags *curtags = tags;

	str = g_string_sized_new(strlen(text));

	for (cur = text; *cur; cur++) {
		guchar c;

		while (ntags && cur - text == curtags->pos) {
			/* We reached the position of a block of tags, put them back */
			g_string_append(str, curtags->tags);
			curtags++;
			ntags--;
		}

		c = *cur;

		/* Re-escape ssml character sequences */
		if (c == '"')
			g_string_append(str, "&quot;");
		else if (c == '\'')
			g_string_append(str, "&apos;");
		else if (c == '<')
			g_string_append(str, "&lt;");
		else if (c == '>')
			g_string_append(str, "&gt;");
		else if (c == '&')
			g_string_append(str, "&amp;");
		else
			g_string_append_c(str, c);
	}

	while (ntags) {
		/* Trailing tags */
		g_string_append(str, curtags->tags);
		curtags++;
		ntags--;
	}

	free(tags);

	result = str->str;
	g_string_free(str, FALSE);

	return result;
}

/*----------------- Speech symbol representation and loading ----------------*/

static SpeechSymbol *speech_symbol_new(void)
{
	SpeechSymbol *sym = g_slice_alloc(sizeof *sym);

	sym->identifier = NULL;
	sym->pattern = NULL;
	sym->replacement = NULL;
	sym->level = SYMLVL_INVALID;
	sym->preserve = SYMPRES_INVALID;
	sym->display_name = NULL;

	return sym;
}

static void speech_symbol_free(SpeechSymbol *sym)
{
	/* sym->identifier is the key, thus freed by hash table */
	g_free(sym->pattern);
	g_free(sym->replacement);
	g_free(sym->display_name);
	g_slice_free1(sizeof *sym, sym);
}

/* checks whether the line should be skipped: either blank or commented */
static int skip_line(const char *line)
{
	if (*line == '#')
		return 1;
	while (g_ascii_isspace(*line))
		line++;
	return *line == 0;
}

/* strips \r and \n at the end of a single line buffer */
static void strip_newline(char *line)
{
	while (*line && *line != '\r' && *line != '\n')
		line++;
	*line = 0;
}

/* Loads an "identifier\tpattern" line into complex_symbols */
static int speech_symbols_load_complex_symbol(SpeechSymbols *ss, const char *line)
{
	char **parts = g_strsplit(line, "\t", 2);

	if (g_strv_length(parts) != 2) {
		g_strfreev(parts);
		return -1;
	}

	ss->complex_symbols = g_slist_prepend(ss->complex_symbols, parts);

	return 0;
}

/* Finds the entry in @p map that corresponds to @p name, and put its value
 * into the integer pointer to by @p value */
static int speech_symbols_load_int_field(IntFieldDesc *map, guint map_len,
					 const char *name, int *value)
{
	guint i;

	for (i = 0; i < map_len; i++) {
		if (strcmp(map[i].name, name) == 0) {
			*value = map[i].value;
			return 0;
		}
	}

	return -1;
}

/* Loads a symbol line into symbols
 * syntax is:
 *   identifier "\t" replacement [ "\t" level [ "\t" preserve ] [ "\t#" comment ] */
static int speech_symbols_load_symbol(SpeechSymbols *ss, const char *line)
{
	char **parts = g_strsplit(line, "\t", -1);
	guint len = g_strv_length(parts);
	char *display_name = NULL;
	char *identifier = NULL;
	char *replacement = NULL;
	int level = SYMLVL_INVALID;
	int pres_mode = SYMPRES_INVALID;
	SpeechSymbol *sym;

	/* last field, if commented: display name */
	if (len > 0 && parts[len - 1][0] == '#') {
		/* Regardless of how many fields there are,
		 * if the last field is a comment, it is the display name. */
		const char *p;

		display_name = parts[len - 1];
		parts[--len] = NULL;

		p = display_name + 1;
		while (g_ascii_isspace(*p))
			p++;
		memmove(display_name, p, strlen(p) + 1);
	}

	/* 4th field (optional): preserve */
	if (len > 3) {
		IntFieldDesc map[] = {
			{ "-",		SYMPRES_NEVER },
			{ "never",	SYMPRES_NEVER },
			{ "always",	SYMPRES_ALWAYS },
			{ "norep",	SYMPRES_NOREP },
			{ "literal",	SYMPRES_LITERAL },
		};

		if (speech_symbols_load_int_field(map, G_N_ELEMENTS(map),
						  parts[3], &pres_mode) < 0)
			goto err;
	}

	/* 3rd field (optional): level */
	if (len > 2) {
		IntFieldDesc map[] = {
			{ "-",		SYMLVL_NONE },
			{ "none",	SYMLVL_NONE },
			{ "some",	SYMLVL_SOME },
			{ "most",	SYMLVL_MOST },
			{ "all",	SYMLVL_ALL },
			{ "char",	SYMLVL_CHAR },
		};

		if (speech_symbols_load_int_field(map, G_N_ELEMENTS(map),
						  parts[2], &level) < 0)
			goto err;
	}

	/* missing required fields */
	if (len < 2 || !parts[0] || !parts[0][0])
		goto err;

	/* 2nd field: replacement */
	if (strcmp(parts[1], "-") == 0)
		replacement = NULL;
	else
		replacement = g_strdup(parts[1]);

	/* 1st field: identifier */
	if (parts[0][0] == '\\' && parts[0][1]) {
		identifier = g_strdup(parts[0] + 1);
		switch (identifier[0]) {
		case '0':
			identifier[0] = '\0';
			/* FIXME: support this */
			MSG2(1, "symbols", "Loading NUL byte entry is not yet supported");
			goto err;
			break;
		case 't': identifier[0] = '\t'; break;
		case 'n': identifier[0] = '\n'; break;
		case 'r': identifier[0] = '\r'; break;
		case 'f': identifier[0] = '\f'; break;
		case 'v': identifier[0] = '\v'; break;
		case '#':
		case '\\':
			/* nothing to do */
			break;
		}
	} else
		identifier = g_strdup(parts[0]);

	sym = speech_symbol_new();
	sym->identifier = identifier;
	sym->replacement = replacement;
	sym->level = level;
	sym->preserve = pres_mode;
	sym->display_name = display_name;

	g_hash_table_insert(ss->symbols, sym->identifier, sym);

	g_strfreev(parts);

	return 0;

err:
	g_free(display_name);
	g_free(identifier);
	g_free(replacement);
	g_strfreev(parts);

	return -1;
}

/* Loads a symbols.dic file into @p ss */
static int speech_symbols_load(SpeechSymbols *ss, const char *filename, gboolean allow_complex)
{
	FILE *fp;
	char *line = NULL;
	size_t n = 0;
	unsigned char bom[3];
	/* line parsing callback for the current section */
	int (*handler) (SpeechSymbols *, const char *) = NULL;

	fp = fopen(filename, "r");
	if (!fp) {
		MSG2(1, "symbols", "Failed to open file '%s': %s", filename, g_strerror(errno));
		return -1;
	}

	/* skip UTF-8 BOM if present */
	if (fread(bom, sizeof *bom, sizeof bom, fp) != sizeof bom ||
	    bom[0] != 0xEF || bom[1] != 0xBB || bom[2] != 0xBF)
		fseek(fp, 0, SEEK_SET);

	while (getline(&line, &n, fp) >= 0) {
		if (skip_line(line))
			continue;
		strip_newline(line);

		if (allow_complex && strcmp(line, "complexSymbols:") == 0) {
			handler = speech_symbols_load_complex_symbol;
		} else if (strcmp(line, "symbols:") == 0) {
			handler = speech_symbols_load_symbol;
		} else if (!handler || handler(ss, line) < 0) {
			MSG2(1, "symbols", "Invalid line in file %s: %s",
			     filename, line);
		}
	}

	free(line);
	fclose(fp);

	return 0;
}

static void speech_symbols_free(SpeechSymbols *ss)
{
	g_slist_free_full(ss->complex_symbols, (GDestroyNotify) g_strfreev);
	g_hash_table_destroy(ss->symbols);
	g_free(ss->source);
	g_free(ss);
}

/* Loads a symbols file for @p locale.
 * Returns a SpeechSymbols*, or NULL on error. */
static gpointer speech_symbols_new(const gchar *locale, const gchar *file)
{
	SpeechSymbols *ss = g_malloc(sizeof *ss);
	gchar *path;

	ss->complex_symbols = NULL;
	ss->source = NULL;
	ss->symbols = g_hash_table_new_full(g_str_hash, g_str_equal,
					    g_free,
					    (GDestroyNotify) speech_symbol_free);

	path = g_build_filename(LOCALE_DATA, locale, file, NULL);
	MSG2(5, "symbols", "Trying to load %s for '%s' from '%s'", file, locale, path);
	if (speech_symbols_load(ss, path, TRUE) >= 0) {
		MSG2(5, "symbols", "Successful");
		/* The elements are added to the start of the list in
		 * speech_symbols_load_complex_symbol() for better speed (as adding to
		 * the end requires walking the whole list), but we want them in the
		 * order they are in the file, so reverse the list. */
		ss->complex_symbols = g_slist_reverse(ss->complex_symbols);
		ss->source = g_strdup(file);
	} else {
		/* Nothing loaded in the end */
		MSG2(5, "symbols", "Failed");
		speech_symbols_free(ss);
		ss = NULL;
	}
	g_free(path);

	return ss;
}

static SpeechSymbols *get_locale_speech_symbols(const gchar *locale, const gchar *file)
{
	if (!G_symbols_dicts) {
		G_symbols_dicts = locale_map_new((GDestroyNotify) speech_symbols_free);
	}

	return locale_map_fetch(G_symbols_dicts, locale, file, speech_symbols_new);
}

void symbols_preprocessing_add_file(const char *name)
{
	MSG2(5, "symbols", "Will load symbol file %s", name);
	symbols_files = g_slist_append(symbols_files, g_strdup(name));
}

/*------------------ Speech symbol compilation & processing -----------------*/

/* sort function sorting strings by length, longest first */
static gint list_sort_string_longest_first(gconstpointer a, gconstpointer b)
{
	return strlen(b) - strlen(a);
}

static void speech_symbols_processor_free(SpeechSymbolProcessor *ssp)
{
	if (ssp->regex)
		g_regex_unref(ssp->regex);
	g_slist_free(ssp->complex_list);
	if (ssp->symbols)
		g_hash_table_unref(ssp->symbols);
	g_free(ssp->source);
	g_free(ssp);
}

static void speech_symbols_processor_list_free(GSList *sspl)
{
	GSList *e;
	for (e = sspl; e; e = e->next)
		speech_symbols_processor_free(e->data);
}

/* Loads and compiles speech symbols conversions for @p locale.
 * Returns a SpeechSymbolProcessor*, or NULL on error */
static SpeechSymbolProcessor *speech_symbols_processor_new(const char *locale, SpeechSymbols *syms)
{
	SpeechSymbolProcessor *ssp = NULL;
	SpeechSymbols *ssbase;
	GHashTableIter iter;
	gpointer key, value;
	GString *characters;
	GSList *multi_chars_list = NULL;
	gchar *escaped;
	GString *escaped_multi;
	GString *pattern;
	GError *error = NULL;
	GSList *sources = NULL;
	GSList *node;
	int has_dash = 0;
	int has_rbracket = 0;
	int has_circum = 0;

	sources = g_slist_append(sources, syms);
	/* Always use the base. */
	ssbase = get_locale_speech_symbols("base", syms->source);
	if (ssbase)
		sources = g_slist_append(sources, ssbase);

	ssp = g_malloc(sizeof *ssp);
	ssp->source = g_strdup(syms->source);
	/* The computed symbol information from all sources. */
	ssp->symbols = g_hash_table_new_full(g_str_hash, g_str_equal,
					     g_free,
					     (GDestroyNotify) speech_symbol_free);
	/* An indexable list of complex symbols for use in building/executing the regexp. */
	ssp->complex_list = NULL;

	/* Add all complex symbols first, as they take priority. */
	for (node = sources; node; node = node->next) {
		SpeechSymbols *syms = node->data;
		GSList *node2;

		for (node2 = syms->complex_symbols; node2; node2 = node2->next) {
			SpeechSymbol *sym;
			gchar **key_val = node2->data;

			if (g_hash_table_contains(ssp->symbols, key_val[0])) {
				/* Already defined */
				continue;
			}

			sym = speech_symbol_new();
			sym->identifier = g_strdup(key_val[0]);
			sym->pattern = g_strdup(key_val[1]);
			g_hash_table_insert(ssp->symbols, sym->identifier, sym);
			ssp->complex_list = g_slist_prepend(ssp->complex_list, sym);
		}
	}
	/* Elements are added at the start for performance, but we want them in the original order */
	ssp->complex_list = g_slist_reverse(ssp->complex_list);

	/* Supplement the data for complex symbols and add all simple symbols. */
	characters = g_string_new(NULL);
	for (node = sources; node; node = node->next) {
		SpeechSymbols *syms = node->data;

		g_hash_table_iter_init(&iter, syms->symbols);
		while (g_hash_table_iter_next(&iter, &key, &value)) {
			const SpeechSymbol *source_sym = value;
			SpeechSymbol *sym;

			sym = g_hash_table_lookup(ssp->symbols, key);
			if (!sym) {
				/* This is a new simple symbol.
				 * (All complex symbols have already been added.) */
				sym = speech_symbol_new();
				sym->identifier = g_strdup(key);
				g_hash_table_insert(ssp->symbols, sym->identifier, sym);
				if (strlen(sym->identifier) == 1) {
					switch (sym->identifier[0]) {
						case '-':
							has_dash = 1;
							break;
						case ']':
							has_rbracket = 1;
							break;
						case '^':
							has_circum = 1;
							break;
						default:
						g_string_append_c(characters, sym->identifier[0]);
					}
				} else {
					multi_chars_list = g_slist_prepend(multi_chars_list, sym->identifier);
				}
			}
			/* If fields weren't explicitly specified, inherit the value from later sources. */
			if (sym->replacement == NULL)
				sym->replacement = g_strdup(source_sym->replacement);
			if (sym->level == SYMLVL_INVALID)
				sym->level = source_sym->level;
			if (sym->preserve == SYMPRES_INVALID)
				sym->preserve = source_sym->preserve;
			if (sym->display_name == NULL)
				sym->display_name = g_strdup(source_sym->display_name);
		}
	}

	/* Set defaults for any fields not explicitly set. */
	g_hash_table_iter_init(&iter, ssp->symbols);
	while (g_hash_table_iter_next(&iter, &key, &value)) {
		SpeechSymbol *sym = value;

		if (!sym->replacement) {
			/* Symbols without a replacement specified are useless. */
			MSG2(2, "symbols", "Replacement not defined "
					   "in locale %s for symbol: %s",
					   locale, sym->identifier);
			ssp->complex_list = g_slist_remove(ssp->complex_list, sym);
			g_hash_table_iter_remove(&iter);
			continue;
		}
		if (sym->level == SYMLVL_INVALID)
			sym->level = SYMLVL_ALL;
		if (sym->preserve == SYMPRES_INVALID)
			sym->preserve = SYMPRES_NEVER;
		if (sym->display_name == NULL)
			sym->display_name = g_strdup(sym->identifier);
	}

	/* build the regex. */

	/* Make characters into a regexp character set. */
	escaped = g_regex_escape_string(characters->str, characters->len);
	g_string_truncate(characters, 0);
	if (*escaped || has_dash || has_rbracket || has_circum) {
		g_string_append_printf(characters, "[%s", escaped);
		if (has_dash)
			g_string_append_printf(characters, "\\-");
		if (has_rbracket)
			g_string_append_printf(characters, "\\]");
		if (has_circum)
			g_string_append_printf(characters, "\\^");
		g_string_append_c(characters, ']');
	}
	g_free(escaped);

	/* The simple symbols must be ordered longest first so that the longer symbols will match.*/
	multi_chars_list = g_slist_sort(multi_chars_list, list_sort_string_longest_first);

	/* TODO: check the syntax is compatible with GLib */
	pattern = g_string_new(NULL);
	/* Strip repeated spaces from the end of the line to stop them from being picked up by repeated. */
	g_string_append(pattern, "(?P<rstripSpace>  +$)");
	/* Repeated characters: more than 3 repeats. */
	if (characters->len) {
		g_string_append_c(pattern, '|');
		g_string_append_printf(pattern, "(?P<repeated>(?P<repTmp>%s)(?P=repTmp){3,})", characters->str);
	}
	/* Complex symbols.
	 * Each complex symbol has its own named group so we know which symbol matched. */
	guint i = 0;
	for (node = ssp->complex_list; node; node = node->next, i++) {
		SpeechSymbol *sym = node->data;
		g_string_append_c(pattern, '|');
		g_string_append_printf(pattern, "(?P<c%u>%s)", i, sym->pattern);
	}
	/* Simple symbols.
	 * These are all handled in one named group.
	 * Because the symbols are just text, we know which symbol matched just by looking at the matched text. */
	escaped_multi = g_string_new(NULL);
	for (node = multi_chars_list; node; node = node->next) {
		escaped = g_regex_escape_string(node->data, -1);
		if (escaped_multi->len > 0)
			g_string_append_c(escaped_multi, '|');
		g_string_append(escaped_multi, escaped);
		g_free(escaped);
	}
	if (escaped_multi->len || characters->len) {
		g_string_append_c(pattern, '|');
		g_string_append_printf(pattern, "(?P<simple>");
		if (escaped_multi->len)
			g_string_append_printf(pattern, "%s", escaped_multi->str);
		if (escaped_multi->len && characters->len)
			g_string_append_printf(pattern, "|");
		if (characters->len)
			g_string_append_printf(pattern, "%s", characters->str);
		g_string_append_printf(pattern, ")");
	}
	g_string_free(escaped_multi, TRUE);

	MSG2(5, "symbols", "building regex: %s", pattern->str);
	ssp->regex = g_regex_new(pattern->str, G_REGEX_OPTIMIZE, 0, &error);
	if (!ssp->regex) {
		/* if regex compilation failed, bail out */
		MSG2(1, "symbols", "ERROR compiling regular expression: %s. "
				   "This is likely due to an invalid complex "
				   "symbol regular expression in locale %s.",
				   error->message, locale);
		g_error_free(error);
		speech_symbols_processor_free(ssp);
		ssp = NULL;
	}

	g_string_free(pattern, TRUE);
	g_string_free(characters, TRUE);
	g_slist_free(multi_chars_list);
	g_slist_free(sources);

	return ssp;
}

/* Loads and compiles speech symbols conversions for @p locale.
 * Returns a SpeechSymbolProcessor*, or NULL on error */
static gpointer speech_symbols_processor_list_new(const char *locale, const char *file)
{
	SpeechSymbolProcessor *ssp;
	SpeechSymbols *ss;
	GSList *sspl = NULL;
	GSList *node;

	/* TODO: load user custom symbols? */

	for (node = symbols_files; node; node = node->next) {
		ss = get_locale_speech_symbols(locale, node->data);
		if (!ss) {
			MSG2(1, "symbols", "Failed to load symbols '%s' for locale '%s'",
					   (char*) node->data, locale);
		} else {
			ssp = speech_symbols_processor_new(locale, ss);
			if (ssp)
				sspl = g_slist_prepend(sspl, ssp);
		}
	}

	/* The elements are added to the start of the list for better speed (as
	 * adding to the end requires walking the whole list), but we want them
	 * in the order they are in the config, so reverse the list. */
	sspl = g_slist_reverse(sspl);

	return sspl;
}

/* Fetch a named group that matched.
 * FIXME: handle empty groups? (e.g. with only lookaheads/lookbehinds) */
static gchar *fetch_named_matching(const GMatchInfo *match_info, const gchar *name)
{
	gchar *capture = g_match_info_fetch_named(match_info, name);

	if (capture && !*capture) {
		g_free(capture);
		capture = NULL;
	}

	return capture;
}

enum group {
	RSTRIPSPACE,
	REPEATED,
	SIMPLE,
	COMPLEX,
};

/* Look for the first block of tags strictly after pos, among tags between firsttag and lasttag */
static gint find_nexttag(struct tags *tags, gint pos, gint firsttag, gint endtag)
{
	gint middletag;

	if (endtag == firsttag)
		/* None here */
		return endtag;

	if (tags[firsttag].pos > pos)
		/* That's it already */
		return firsttag;

	middletag = (firsttag + 1 + endtag) / 2;
	if (middletag == endtag)
		/* None */
		return endtag;

	if (tags[middletag].pos > pos)
		return find_nexttag(tags, pos, firsttag, middletag);
	else
		return find_nexttag(tags, pos, middletag, endtag);
}

static int replace_groups(const GMatchInfo *match_info, GString *result, char *replacement, gint pos)
{
	int in_escape = 0;
	char c;

	while ((c = *replacement++)) {
		if (!in_escape) {
			if (c == '\\')
				in_escape = 1;
			else
				g_string_append_c(result, c);
		} else {
			if (c == '\\')
				g_string_append_c(result, '\\');
			else if (c >= '0' && c <= '9') {
				gchar *res = g_match_info_fetch(match_info, pos + (c - '0'));
				if (res)
					g_string_append(result, res);
				else
					MSG2(1, "symbols", "Unmatched reference \\%c", c);
			} else {
				MSG2(1, "symbols", "Invalid reference \\%c", c);
				g_string_append_c(result, c);
			}
			in_escape = 0;
		}
	}
	if (in_escape)
		MSG2(1, "symbols", "Unterminated backslash");

	return 1;
}

/* Regular expression callback for applying replacements */
static gboolean regex_eval(const GMatchInfo *match_info, GString *result, gpointer user_data)
{
	SpeechSymbolProcessor *ssp = user_data;
	gchar *capture;
	enum group captured_group;
	gchar *group_0;
	gint start = -1, end = -1;
	gint prevlen = result->len, shift;
	gint nexttag, curtag, deferrable;
	guint i = 0;
	SpeechSymbol *sym = NULL;
	gint pos = 0;

	/* First see what we captured */

	/* FIXME: Python regex API allows to find the name of the group that
	 *        matched.  As GRegex doesn't have that, what we do here is try
	 *        and fetch the groups we know, and see if they matched.
	 *        This is not very optimal, but how can we avoid that? */

	if ((capture = fetch_named_matching(match_info, "rstripSpace"))) {
		captured_group = RSTRIPSPACE;
	} else if ((capture = fetch_named_matching(match_info, "repeated"))) {
		captured_group = REPEATED;
	} else if ((capture = fetch_named_matching(match_info, "simple"))) {
		captured_group = SIMPLE;
	} else {
		/* Complex symbol. */
		GSList *node;

		for (node = ssp->complex_list; !sym && node; node = node->next, i++) {
			gchar *group_name = g_strdup_printf("c%u", i);

			if ((capture = fetch_named_matching(match_info, group_name))) {
				gchar **all = g_match_info_fetch_all(match_info);
				gint i;

				pos = -1;
				/* Find out the index of the match */
				for (i = 1; all[i]; i++) {
					if (all[i][0]) {
						pos = i;
						break;
					}
				}
				g_strfreev(all);

				if (pos != -1)
					sym = node->data;
			}
			g_free(group_name);

			if (sym)
				break;
		}

		captured_group = COMPLEX;
	}

	/* Now check where that lies among tags */

	g_match_info_fetch_pos(match_info, 0, &start, &end);

	nexttag = find_nexttag(ssp->tags, start, 0, ssp->ntags);

	/* Check whether the contained tags are deferrable */
	deferrable = 1;
	for (curtag = nexttag; curtag < ssp->ntags; curtag++) {
		if (ssp->tags[curtag].pos >= end)
			/* Don't care about the rest */
			break;
		/* This block of tags is within the group */
		if (!ssp->tags[curtag].deferrable) {
			/* Oops, these tags can't be deferred */
			deferrable = 0;
			break;
		}
	}

	if (!deferrable) {
		group_0 = g_match_info_fetch(match_info, 0);
		MSG2(1, "symbols", "tags '%s' within group |%s| (at %d..%d), not replacing group :/",
				   ssp->tags[curtag].tags, group_0, start, end);
		g_free(group_0);

		g_string_append(result, capture);
		g_free(capture);

		return FALSE;
	}

	/* Defer these tags */
	for (curtag = nexttag; curtag < ssp->ntags; curtag++) {
		if (ssp->tags[curtag].pos >= end)
			/* Don't care about the rest */
			break;
		/* This block of tags is within the group, defer it after the group */
		MSG2(5, "symbols", "deferring tags '%s' to %d", ssp->tags[curtag].tags, end);
		ssp->tags[curtag].pos = end;
	}

	/* Ok, now replace */
	if (captured_group == RSTRIPSPACE) {
		MSG2(5, "symbols", "replacing <rstripSpace>");
		/* nothing to do, just don't add it in the result */
	} else if (captured_group == REPEATED) {
		/* Repeated character. */
		char ch[2] = { capture[0], 0 };
		SpeechSymbol *sym = g_hash_table_lookup(ssp->symbols, ch);

		MSG2(5, "symbols", "replacing <repeated>");

		/* this should never happen, but be on the safe side and check it */
		if (!sym)
			goto symbol_error;

		if (ssp->level >= sym->level) {
			g_string_append_printf(result, " %lu %s ", (unsigned long) strlen(capture), sym->replacement);
		} else {
			g_string_append_c(result, ' ');
		}
	} else {
		const gchar *prefix, *suffix;

		/* One of the defined symbols. **/
		if (captured_group == SIMPLE) {
			/* Simple symbol. */
			sym = g_hash_table_lookup(ssp->symbols, capture);
			MSG2(5, "symbols", "replacing <simple>");
		} else {
			g_assert(captured_group == COMPLEX);
			/* Complex symbol, sym and i already set */
			MSG2(5, "symbols", "replacing <c%u> (complex symbol)", i);
		}

		/* this should never happen, but be on the safe side and check it */
		if (!sym)
			goto symbol_error;

		MSG2(5, "symbols", "replacing sym |%s| (lvl=%d, preserve=%d)",
		     sym->identifier, sym->level, sym->preserve);

		if (sym->preserve == SYMPRES_LITERAL)
			prefix = "";
		else
			prefix = " ";

		if (sym->preserve == SYMPRES_ALWAYS ||
		    (sym->preserve == SYMPRES_NOREP && ssp->level < sym->level))
			suffix = capture;
		else if (sym->preserve == SYMPRES_LITERAL)
			suffix = "";
		else
			suffix = " ";

		if (sym->level > ssp->support_level) {
			/* Leave it to the module */
			g_string_append(result, capture);
		} else if (ssp->level >= sym->level && sym->replacement) {
			g_string_append(result, prefix);
			MSG2(5, "symbols", "replacing with %s", sym->replacement);
			replace_groups(match_info, result, sym->replacement, pos);
			g_string_append(result, suffix);
		} else {
			g_string_append(result, suffix);
		}
	}

	goto out;

symbol_error:
	group_0 = g_match_info_fetch(match_info, 0);
	MSG2(1, "symbols", "WARNING: no symbol for match |%s| (at %d..%d), this shouldn't happen.",
	     group_0, start, end);
	g_free(group_0);

out:
	/* content has grown (or shrunk) by this amount */
	shift = (result->len - prevlen) - strlen(capture);

	if (nexttag < ssp->ntags)
		/* Update positions of tags beyond this */
		ssp->tags[nexttag].shift += shift;

	g_free(capture);

	return FALSE;
}

/* Processes some input and converts symbols in it */
static gchar *speech_symbols_processor_process_text(GSList *sspl, const gchar *input, SymLvl level, SymLvl support_level, SPDDataMode ssml_mode)
{
	gchar *text;
	gchar *processed;
	struct tags *tags = NULL;
	gint ntags = 0, i;
	GError *error = NULL;

	if (ssml_mode == SPD_DATA_SSML) {
		text = escape_ssml_text(input, &tags, &ntags);
		MSG2(5, "symbols", "escaped ssml '%s' to '%s'", input, text);
	} else {
		text = g_strdup(input);
	}

	for ( ; sspl; sspl = sspl->next) {
		SpeechSymbolProcessor *ssp = sspl->data;

		if (ssml_mode == SPD_DATA_SSML) {
			for (i = 0; i < ntags; i++)
				tags[i].shift = 0;
			ssp->tags = tags;
			ssp->ntags = ntags;
		} else
			ssp->ntags = 0;

		ssp->level = level;
		ssp->support_level = support_level;
		processed = g_regex_replace_eval(ssp->regex, text, -1, 0, 0, regex_eval, ssp, &error);
		if (!processed) {
			MSG2(1, "symbols", "ERROR applying regex: %s", error->message);
			g_error_free(error);
		} else {
			MSG2(5, "symbols", "'%s' translated '%s' to '%s'", ssp->source, text, processed);
			g_free(text);
			text = processed;

			if (ssml_mode == SPD_DATA_SSML) {
				/* This accumulates the shifts of all previous replacements */
				gssize shift = 0;

				/* Apply new tags positions */
				for (i = 0; i < ntags; i++) {
					shift += tags[i].shift;
					tags[i].pos += shift;
				}
			}

			if (level == SYMLVL_CHAR && g_utf8_strlen(processed, -1) > 1)
				/* This translated it, avoid letting other rules continue expanding! */
				break;
		}
	}

	if (ssml_mode == SPD_DATA_SSML) {
		processed = unescape_ssml_text(text, tags, ntags);
		MSG2(5, "symbols", "unescaped ssml '%s' to '%s'", text, processed);
		g_free(text);
	} else
		processed = text;

	return processed;
}

/* Gets a possibly cached processor for the given locale */
static GSList *get_locale_speech_symbols_processor(const gchar *locale)
{
	if (!G_processors) {
		G_processors = locale_map_new((GDestroyNotify) speech_symbols_processor_list_free);
	}

	return locale_map_fetch(G_processors, locale, NULL, speech_symbols_processor_list_new);
}

/*----------------------------------- API -----------------------------------*/

/* Process some text, converting symbols according to desired pronunciation. */
static gchar *process_speech_symbols(const gchar *locale, const gchar *text, SymLvl level, SymLvl support_level, SPDDataMode ssml_mode)
{
	GSList *sspl;

	sspl = get_locale_speech_symbols_processor(locale);
	/* fallback to English if there's no processor for the locale */
	if (!sspl && g_str_has_prefix(locale, "en") && strchr("_-", locale[2]))
		sspl = get_locale_speech_symbols_processor("en");
	if (!sspl)
		return NULL;

	return speech_symbols_processor_process_text(sspl, text, level, support_level, ssml_mode);
}

void insert_symbols(TSpeechDMessage *msg, int punct_missing)
{
	gchar *processed;
	SymLvl level = SYMLVL_NONE;
	SymLvl support_level = msg->settings.symbols_preprocessing;

	if (punct_missing && support_level < SYMLVL_ALL)
		/* The user preferred to let some modules handle some punctuation,
		 * but this module doesn't support it, so force handling it ourself. */
		support_level = SYMLVL_ALL;

	switch (msg->settings.msg_settings.punctuation_mode) {
	case SPD_PUNCT_ALL: level = SYMLVL_ALL; break;
	case SPD_PUNCT_MOST: level = SYMLVL_MOST; break;
	case SPD_PUNCT_SOME: level = SYMLVL_SOME; break;
	case SPD_PUNCT_NONE: level = SYMLVL_NONE; break;
	}

	if (msg->settings.type == SPD_MSGTYPE_CHAR)
		level = SYMLVL_CHAR;

	MSG2(5, "symbols", "processing at level %d, supporting level %d", level, support_level);
	processed = process_speech_symbols(msg->settings.msg_settings.voice.language,
		msg->buf, level, support_level, msg->settings.ssml_mode);
	if (processed) {
		MSG2(5, "symbols", "before: |%s|", msg->buf);
		g_free(msg->buf);
		msg->buf = processed;
		MSG2(5, "symbols", "after: |%s|", msg->buf);
		if (support_level >= level)
			/* if we performed the replacement, don't let the module speak it again */
			msg->settings.msg_settings.punctuation_mode = SPD_PUNCT_NONE;

		/* if we provide a character description file, don't let the module spell it */
		if (msg->settings.type == SPD_MSGTYPE_CHAR)
			if (g_utf8_strlen(processed, -1) > 1)
				msg->settings.type = SPD_MSGTYPE_TEXT;
	}
}
