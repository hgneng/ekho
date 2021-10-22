/*
 * module_utils_addvoice.c - Functionality for the DotConf AddVoice feature
 *
 * Copyright (C) 2001, 2002, 2003, 2006, 2007 Brailcom, o.p.s.
 *
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2, or (at your option) any later
 * version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * $Id: module_utils_addvoice.c,v 1.5 2007-07-29 23:43:33 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>

#include <glib.h>
#include <dotconf.h>

#include "module_utils.h"

GHashTable *module_voice_table = NULL;

static int nbvoices=0;
static int nbpaths=0;
static SPDVoice *generic_voices;
static SPDVoice **generic_voices_list;
static char *default_voice;
static char **dependency_paths;
typedef struct {
	char *male1;
	char *male2;
	char *male3;
	char *female1;
	char *female2;
	char *female3;
	char *child_male;
	char *child_female;
} SPDVoiceDef;

DOTCONF_CB(VoiceFileDependency_cb)
{
	dependency_paths = (char **)g_realloc(dependency_paths, (nbpaths + 1) * sizeof *dependency_paths);
	dependency_paths[nbpaths] = g_strdup(cmd->data.str);
	++nbpaths;
	return NULL;
}

DOTCONF_CB(AddVoice_cb)
{
	int i=0;
	GRegex *regex;
	SPDVoiceDef *voices;
	char *language = cmd->data.list[0];
	char *symbolic;
	char *voicename = cmd->data.list[2];
	char *key;
	SPDVoiceDef *value;

	if (language == NULL) {
		DBG("Missing language.\n");
		return NULL;
	}

	if (cmd->data.list[1] == NULL) {
		DBG("Missing symbolic name.\n");
		return NULL;
	}

	if (voicename == NULL) {
		DBG("Missing voice name for %s\n", cmd->data.list[0]);
		return NULL;
	}

	if (module_voice_table == NULL) {
		DBG("No voice table\n");
		return NULL;
	}

	symbolic =
	    (char *)g_ascii_strup(cmd->data.list[1], strlen(cmd->data.list[1]));

	voices = g_hash_table_lookup(module_voice_table, language);
	if (voices == NULL) {
		key = (char *)g_strdup(language);
		value = (SPDVoiceDef *) g_malloc(sizeof(SPDVoiceDef));

		value->male1 = NULL;
		value->male2 = NULL;
		value->male3 = NULL;
		value->female1 = NULL;
		value->female2 = NULL;
		value->female3 = NULL;
		value->child_male = NULL;
		value->child_female = NULL;

		g_hash_table_insert(module_voice_table, key, value);
		voices = value;
	}

	regex = g_regex_new("[$]VOICE", 0, 0, NULL);
	for (i = 0; i < nbpaths; i++)
	{
		char *new_dependency_path = g_regex_replace_literal(regex, dependency_paths[i], -1, 0, cmd->data.list[2], 0, NULL);
		if (! g_file_test(new_dependency_path, G_FILE_TEST_EXISTS)) {
			DBG("Missing dependency %s\n", new_dependency_path);
			g_free(new_dependency_path);
			g_regex_unref(regex);
			return NULL;
		}
		g_free(new_dependency_path);
	}
	g_regex_unref(regex);

	if (!strcmp(symbolic, "MALE1"))
		voices->male1 = g_strdup(voicename);
	else if (!strcmp(symbolic, "MALE2"))
		voices->male2 = g_strdup(voicename);
	else if (!strcmp(symbolic, "MALE3"))
		voices->male3 = g_strdup(voicename);
	else if (!strcmp(symbolic, "FEMALE1"))
		voices->female1 = g_strdup(voicename);
	else if (!strcmp(symbolic, "FEMALE2"))
		voices->female2 = g_strdup(voicename);
	else if (!strcmp(symbolic, "FEMALE3"))
		voices->female3 = g_strdup(voicename);
	else if (!strcmp(symbolic, "CHILD_MALE"))
		voices->child_male = g_strdup(voicename);
	else if (!strcmp(symbolic, "CHILD_FEMALE"))
		voices->child_female = g_strdup(voicename);
	else {
		DBG("Unrecognized voice name in configuration\n");
		return NULL;
	}

	generic_voices = (SPDVoice *)g_realloc(generic_voices, (nbvoices+1) * sizeof(SPDVoice));
	generic_voices_list = (SPDVoice **)g_realloc(generic_voices_list, (nbvoices+2) * sizeof(SPDVoice *));
	generic_voices[nbvoices].name = g_strdup(cmd->data.list[2]);
	generic_voices[nbvoices].language = g_strdup(cmd->data.list[0]);
	generic_voices[nbvoices].variant = g_strdup(cmd->data.list[1]);;
	for (i = 0; i < nbvoices+1; i++)
		generic_voices_list[i] = &generic_voices[i];
	generic_voices_list[nbvoices+1] = NULL;
	DBG("Added voice %s\n", generic_voices[nbvoices].name);
	++nbvoices;
	return NULL;
}

DOTCONF_CB(DefaultVoice_cb)
{
	char *voicename = cmd->data.list[0];
	if (voicename == NULL) {
		DBG("Missing default voice name\n");
		return NULL;
	}

	if (default_voice)
		free(default_voice);
	default_voice = strdup(voicename);

	return NULL;
}

void module_register_available_voices(void)
{
	module_dc_options = module_add_config_option(module_dc_options,
						     &module_num_dc_options,
						     "VoiceFileDependency", ARG_STR,
						     VoiceFileDependency_cb, NULL, 0);
}

void module_register_settings_voices(void)
{
	module_voice_table = g_hash_table_new(g_str_hash, g_str_equal);
	module_dc_options = module_add_config_option(module_dc_options,
						     &module_num_dc_options,
						     "AddVoice", ARG_LIST,
						     AddVoice_cb, NULL, 0);
	module_dc_options = module_add_config_option(module_dc_options,
						     &module_num_dc_options,
						     "DefaultVoice", ARG_STR,
						     DefaultVoice_cb, NULL, 0);
}

gboolean module_existsvoice(const char *voicename)
{
	int i;
	if (!nbvoices)
		return FALSE;
	for (i = 0; generic_voices_list[i] != NULL; i++) {
	if (strcasecmp(generic_voices[i].name, voicename) == 0)
		return TRUE;
	}
	return FALSE;
}

SPDVoice **module_list_registered_voices(void) {
	return (SPDVoice **)generic_voices_list;
}

char *module_getvoice(const char *language, SPDVoiceType voice)
{
	SPDVoiceDef *voices;
	char *ret;

	if (module_voice_table == NULL) {
		DBG("Can't get voice because voicetable is NULL\n");
		return NULL;
	}

	voices = g_hash_table_lookup(module_voice_table, language);
	if (voices == NULL) {
		DBG("There are no voices in the table for language=%s\n",
		    language);
		return NULL;
	}

	switch (voice) {
	case SPD_UNSPECIFIED:
	case SPD_MALE1:
		ret = voices->male1;
		break;
	case SPD_MALE2:
		ret = voices->male2;
		break;
	case SPD_MALE3:
		ret = voices->male3;
		break;
	case SPD_FEMALE1:
		ret = voices->female1;
		break;
	case SPD_FEMALE2:
		ret = voices->female2;
		break;
	case SPD_FEMALE3:
		ret = voices->female3;
		break;
	case SPD_CHILD_MALE:
		ret = voices->child_male;
		break;
	case SPD_CHILD_FEMALE:
		ret = voices->child_female;
		break;
	default:
		fprintf(stderr, "Unknown voice %d", voice);
		return NULL;
	}

	if (ret == NULL)
		ret = voices->male1;
	if (ret == NULL)
		ret = voices->female1;
	if (ret == NULL)
		ret = voices->male2;
	if (ret == NULL)
		ret = voices->female2;
	if (ret == NULL)
		ret = voices->male3;
	if (ret == NULL)
		ret = voices->female3;
	if (ret == NULL)
		ret = voices->child_male;
	if (ret == NULL)
		ret = voices->child_female;
	if (ret == NULL)
		fprintf(stderr, "No voice available for this output module!\n");

	return ret;
}

char *module_getdefaultvoice(void)
{
	return default_voice;
}
