
/*
 * fdsetconv.c - Conversion of types for Speech Dispatcher
 *
 * Copyright (C) 2001, 2002, 2003 Brailcom, o.p.s.
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
 *
 * $Id: fdsetconv.c,v 1.5 2007-06-21 20:09:45 hanke Exp $
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <glib.h>
#include "fdsetconv.h"

char *EVoice2str(SPDVoiceType voice)
{
	char *str;

	switch (voice) {
	case SPD_MALE1:
		str = g_strdup("male1");
		break;
	case SPD_MALE2:
		str = g_strdup("male2");
		break;
	case SPD_MALE3:
		str = g_strdup("male3");
		break;
	case SPD_FEMALE1:
		str = g_strdup("female1");
		break;
	case SPD_FEMALE2:
		str = g_strdup("female2");
		break;
	case SPD_FEMALE3:
		str = g_strdup("female3");
		break;
	case SPD_CHILD_MALE:
		str = g_strdup("child_male");
		break;
	case SPD_CHILD_FEMALE:
		str = g_strdup("child_female");
		break;
	default:
		str = NULL;
	}

	return str;
}

SPDVoiceType str2EVoice(const char *str)
{
	SPDVoiceType voice;

	if (!strcmp(str, "male1"))
		voice = SPD_MALE1;
	else if (!strcmp(str, "male2"))
		voice = SPD_MALE2;
	else if (!strcmp(str, "male3"))
		voice = SPD_MALE3;
	else if (!strcmp(str, "female1"))
		voice = SPD_FEMALE1;
	else if (!strcmp(str, "female2"))
		voice = SPD_FEMALE2;
	else if (!strcmp(str, "female3"))
		voice = SPD_FEMALE3;
	else if (!strcmp(str, "child_male"))
		voice = SPD_CHILD_MALE;
	else if (!strcmp(str, "child_female"))
		voice = SPD_CHILD_FEMALE;
	else
		voice = -1;

	return voice;
}

char *EPunctMode2str(SPDPunctuation punct)
{
	char *str;

	switch (punct) {
	case SPD_PUNCT_NONE:
		str = g_strdup("none");
		break;
	case SPD_PUNCT_ALL:
		str = g_strdup("all");
		break;
	case SPD_PUNCT_SOME:
		str = g_strdup("some");
		break;
	case SPD_PUNCT_MOST:
		str = g_strdup("most");
		break;
	default:
		str = NULL;
	}

	return str;
}

SPDPunctuation str2EPunctMode(const char *str)
{
	SPDPunctuation punct;

	if (!strcmp(str, "none"))
		punct = SPD_PUNCT_NONE;
	else if (!strcmp(str, "all"))
		punct = SPD_PUNCT_ALL;
	else if (!strcmp(str, "some"))
		punct = SPD_PUNCT_SOME;
	else if (!strcmp(str, "most"))
		punct = SPD_PUNCT_MOST;
	else
		punct = -1;

	return punct;
}

char *ESpellMode2str(SPDSpelling spell)
{
	char *str;

	switch (spell) {
	case SPD_SPELL_ON:
		str = g_strdup("on");
		break;
	case SPD_SPELL_OFF:
		str = g_strdup("off");
		break;
	default:
		str = NULL;
	}

	return str;
}

SPDSpelling str2ESpellMode(const char *str)
{
	SPDSpelling spell;

	if (!strcmp(str, "on"))
		spell = SPD_SPELL_ON;
	else if (!strcmp(str, "off"))
		spell = SPD_SPELL_OFF;
	else
		spell = -1;

	return spell;
}

char *ECapLetRecogn2str(SPDCapitalLetters recogn)
{
	char *str;

	switch (recogn) {
	case SPD_CAP_NONE:
		str = g_strdup("none");
		break;
	case SPD_CAP_SPELL:
		str = g_strdup("spell");
		break;
	case SPD_CAP_ICON:
		str = g_strdup("icon");
		break;
	default:
		str = NULL;
	}

	return str;
}

SPDCapitalLetters str2ECapLetRecogn(const char *str)
{
	SPDCapitalLetters recogn;

	if (!strcmp(str, "none"))
		recogn = SPD_CAP_NONE;
	else if (!strcmp(str, "spell"))
		recogn = SPD_CAP_SPELL;
	else if (!strcmp(str, "icon"))
		recogn = SPD_CAP_ICON;
	else
		recogn = -1;

	return recogn;
}

SPDPriority str2intpriority(const char *str)
{
	SPDPriority priority;

	if (!strcmp(str, "important"))
		priority = SPD_IMPORTANT;
	else if (!strcmp(str, "message"))
		priority = SPD_MESSAGE;
	else if (!strcmp(str, "text"))
		priority = SPD_TEXT;
	else if (!strcmp(str, "notification"))
		priority = SPD_NOTIFICATION;
	else if (!strcmp(str, "progress"))
		priority = SPD_PROGRESS;
	else
		priority = -1;

	return priority;
}
