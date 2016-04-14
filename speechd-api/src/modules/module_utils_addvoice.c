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
 * along with this package; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 * $Id: module_utils_addvoice.c,v 1.5 2007-07-29 23:43:33 hanke Exp $
 */

#include <glib.h>

GHashTable *module_voice_table = NULL;

typedef struct
{
    char *male1;
    char *male2;
    char *male3;
    char *female1;
    char *female2;
    char *female3;
    char *child_male;
    char *child_female;
}SPDVoiceDef;

DOTCONF_CB(AddVoice_cb)
{
    SPDVoiceDef *voices;
    char *language = cmd->data.list[0];
    char *symbolic;
    char *voicename = cmd->data.list[2];
    char *key;
    SPDVoiceDef *value;

    if (language == NULL){
        DBG("Missing language.\n");
        return NULL;
    }

    if (cmd->data.list[1] == NULL){
        DBG("Missing symbolic name.\n");
        return NULL;
    }   

    if (voicename == NULL){
        DBG("Missing voice name for %s\n", cmd->data.list[0]);
        return NULL;
    }

    if (module_voice_table == NULL){
        return NULL;
    }

    symbolic = (char*) g_ascii_strup(cmd->data.list[1], strlen(cmd->data.list[1]));

    voices = g_hash_table_lookup(module_voice_table, language);
    if (voices == NULL){
        key = (char*) g_strdup(language);
        value = (SPDVoiceDef*) xmalloc(sizeof(SPDVoiceDef));

        value->male1=NULL; value->male2=NULL; value->male3=NULL;
        value->female1=NULL; value->female2=NULL; value->female3=NULL;
        value->child_male=NULL; value->child_female=NULL;

        g_hash_table_insert(module_voice_table, key, value);
        voices = value;
    }
    
    if (!strcmp(symbolic, "MALE1")) voices->male1 = g_strdup(voicename);
    else if (!strcmp(symbolic, "MALE2")) voices->male2 = g_strdup(voicename);
    else if (!strcmp(symbolic, "MALE3")) voices->male3 = g_strdup(voicename);
    else if (!strcmp(symbolic, "FEMALE1")) voices->female1 = g_strdup(voicename);
    else if (!strcmp(symbolic, "FEMALE2")) voices->female2 = g_strdup(voicename);
    else if (!strcmp(symbolic, "FEMALE3")) voices->female3 = g_strdup(voicename);
    else if (!strcmp(symbolic, "CHILD_MALE")) voices->child_male = g_strdup(voicename);
    else if (!strcmp(symbolic, "CHILD_FEMALE")) voices->child_female = g_strdup(voicename);
    else{
        DBG("Unrecognized voice name in configuration\n");
        return NULL;
    }

    return NULL;
}

void
module_register_settings_voices(void)
{
    module_voice_table = g_hash_table_new(g_str_hash, g_str_equal);
    module_dc_options = module_add_config_option(module_dc_options,
                                                 &module_num_dc_options, "AddVoice",
                                                 ARG_LIST, AddVoice_cb, NULL, 0);
}

char*
module_getvoice(char* language, EVoiceType voice)
{
    SPDVoiceDef *voices;
    char *ret;

    if (module_voice_table == NULL){
        DBG("Can't get voice because voicetable is NULL\n");
        return NULL;
     }

    voices = g_hash_table_lookup(module_voice_table, language);
    if (voices == NULL){
        DBG("There are no voices in the table for language=%s\n", language);
        return NULL;
    }

    switch(voice){
    case MALE1: 
        ret = voices->male1; break;
    case MALE2: 
        ret = voices->male2; break;
    case MALE3: 
        ret = voices->male3; break;
    case FEMALE1: 
        ret = voices->female1; break;
    case FEMALE2: 
        ret = voices->female2; break;
    case FEMALE3: 
        ret = voices->female3; break;
    case CHILD_MALE: 
        ret = voices->child_male; break;
    case CHILD_FEMALE: 
        ret = voices->child_female; break;
    default:
        printf("Unknown voice");
        return NULL;
    }

    if (ret == NULL) ret = voices->male1;
    if (ret == NULL) fprintf(stderr, "No voice available for this output module!");

    return ret;
}
