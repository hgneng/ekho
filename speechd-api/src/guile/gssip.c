/* SSIP Guile interface */

/* Copyright (C) 2004 Brailcom, o.p.s.
   
   Author: Milan Zamazal <pdm@brailcom.org>

   COPYRIGHT NOTICE

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.
*/


#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>

#include <libguile.h>
#include <libspeechd.h>

#include "gssip.h"


#define ASSIGN_VAR(var, pos, checker, setter)                        \
  SCM_ASSERT (SCM_##checker##P (var), var, SCM_ARG##pos, FUNC_NAME); \
  c_##var = SCM_##setter (var)
#define ASSIGN_INT(var, pos) ASSIGN_VAR (var, pos, INUM, INUM)
#define ASSIGN_STRING(var, pos) ASSIGN_VAR (var, pos, STRING, STRING_CHARS)
#define ASSIGN_SYMBOL(var, pos) ASSIGN_VAR (var, pos, SYMBOL, SYMBOL_CHARS)
#define ASSIGN_CONNECTION() ASSIGN_INT (connection, 1);
#define ASSIGN_PRIORITY() c_priority = assign_priority (priority, FUNC_NAME)

#define RETURN_SUCCESS(value) return ((value) ? SCM_BOOL_F : SCM_BOOL_T)

static SPDPriority assign_priority (SCM priority, const char *func_name)
{
  char *c_priority;
  SCM_ASSERT (SCM_SYMBOLP (priority), priority, SCM_ARG3, func_name);
  c_priority = SCM_SYMBOL_CHARS (priority);
  
  {
    const int invalid_priority = -1000;
    int int_priority =
      ((! strcmp (c_priority, "important")) ? SPD_IMPORTANT
       : (! strcmp (c_priority, "message")) ? SPD_MESSAGE
       : (! strcmp (c_priority, "text")) ? SPD_TEXT
       : (! strcmp (c_priority, "notification")) ? SPD_NOTIFICATION
       : (! strcmp (c_priority, "progress")) ? SPD_PROGRESS
       : invalid_priority);
    if (int_priority == invalid_priority)
      scm_wrong_type_arg (func_name, SCM_ARG3, priority);
    return int_priority;
  }
}

/* SSIP connection opening/closing functions */

SCM_DEFINE (ssip_open, "%ssip-open", 3, 0, 0,
            (SCM user, SCM client, SCM component),
            "Open new SSIP connection and return its identifier.")
#define FUNC_NAME s_ssip_open
{
  char *c_user, *c_client, *c_component;
  ASSIGN_STRING (user, 1);
  ASSIGN_STRING (client, 2);
  ASSIGN_STRING (component, 3);

  {
    int connection = spd_open (c_client, c_component, c_user);
    return (connection ? SCM_MAKINUM (connection) : SCM_EOL);
  }
}
#undef FUNC_NAME

SCM_DEFINE (ssip_close, "%ssip-close", 1, 0, 0, (SCM connection),
            "Close the given SSIP connection.")
#define FUNC_NAME s_ssip_close
{
  int c_connection;
  ASSIGN_CONNECTION ();

  spd_close (c_connection);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* Speech output functions */

#define SSIP_OUTPUT_DECL(name, sname)                           \
  SCM_DEFINE (ssip_say_##name, "%ssip-say-" sname, 3, 0, 0,     \
              (SCM connection, SCM text, SCM priority),         \
              "Say " sname " on CONNECTION with PRIORITY and return whether it succeeded.")
#define SSIP_OUTPUT_BODY(spd_func)                                      \
  {                                                                     \
    int c_connection;                                                   \
    char *c_text;                                                       \
    SPDPriority c_priority;                                             \
    ASSIGN_CONNECTION ();                                               \
    ASSIGN_STRING (text, 2);                                            \
    ASSIGN_PRIORITY ();                                                 \
    RETURN_SUCCESS (spd_func (c_connection, c_priority, c_text));       \
  }

#define FUNC_NAME s_ssip_say_text
SSIP_OUTPUT_DECL (text, "text")
SSIP_OUTPUT_BODY (spd_say)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_say_character
SSIP_OUTPUT_DECL (character, "character")
SSIP_OUTPUT_BODY (spd_char)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_say_key
SSIP_OUTPUT_DECL (key, "key")
SSIP_OUTPUT_BODY (spd_key)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_say_icon
SSIP_OUTPUT_DECL (icon, "icon")
SSIP_OUTPUT_BODY (spd_sound_icon)
#undef FUNC_NAME

/* Speech output management functions */

#define SSIP_CONTROL_DECL(name, sname)                  \
  SCM_DEFINE (ssip_##name, "%ssip-" sname, 2, 0, 0,     \
              (SCM connection, SCM id),                 \
              sname "speaking and return whether it succeeded.")
#define SSIP_CONTROL_BODY(name)                                         \
  {                                                                     \
    int c_connection;                                                   \
    int result_code;                                                    \
    ASSIGN_CONNECTION ();                                               \
    if (SCM_INUMP (id))                                                 \
      result_code = spd_stop_uid (c_connection, SCM_INUM (id));         \
    else if (id == SCM_EOL)                                             \
      result_code = spd_stop (c_connection);                            \
    else if (SCM_SYMBOLP (id)                                           \
             && (! strcmp (SCM_SYMBOL_CHARS (id), "t")))                \
      result_code = spd_stop_all (c_connection);                        \
    else                                                                \
      scm_wrong_type_arg (FUNC_NAME, SCM_ARG2, id);                     \
    RETURN_SUCCESS (result_code);                                       \
  }
  
#define FUNC_NAME s_ssip_stop
SSIP_CONTROL_DECL (stop, "stop")
SSIP_CONTROL_BODY (stop)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_cancel
SSIP_CONTROL_DECL (cancel, "cancel")
SSIP_CONTROL_BODY (cancel)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_pause
SSIP_CONTROL_DECL (pause, "pause")
SSIP_CONTROL_BODY (pause)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_resume
SSIP_CONTROL_DECL (resume, "resume")
SSIP_CONTROL_BODY (resume)
#undef FUNC_NAME

/* Speech parameters functions */

#define SSIP_SET_DECL(name, sname)                              \
  SCM_DEFINE (ssip_set_##name, "%ssip-set-" sname, 2, 0, 0,     \
              (SCM connection, SCM value),                      \
              "Set " sname " for CONNECTION.")

#define SSIP_PROCESS_SET_ARGS(type, type_f)     \
  int c_connection;                             \
  type c_value;                                 \
  ASSIGN_CONNECTION ();                         \
  ASSIGN_##type_f (value, 2);

#define SSIP_SET_STRING_BODY(name)                                      \
  {                                                                     \
    SSIP_PROCESS_SET_ARGS (char *, STRING);                             \
    RETURN_SUCCESS (spd_set_##name (c_connection, c_value));            \
  }

#define SSIP_SET_INT_BODY(name)                                         \
  {                                                                     \
    SSIP_PROCESS_SET_ARGS (int, INT);                                   \
    SCM_ASSERT ((-100 <= c_value && c_value <= 100), value, SCM_ARG2,   \
                FUNC_NAME);                                             \
    RETURN_SUCCESS (spd_set_##name (c_connection, c_value));            \
  }

#define FUNC_NAME s_ssip_set_language
SSIP_SET_DECL (language, "language")
SSIP_SET_STRING_BODY (language)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_output_module
SSIP_SET_DECL (output_module, "output-module")
SSIP_SET_STRING_BODY (output_module)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_rate
SSIP_SET_DECL (rate, "rate")
SSIP_SET_INT_BODY (voice_rate)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_pitch
SSIP_SET_DECL (pitch, "pitch")
SSIP_SET_INT_BODY (voice_pitch)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_volume
SSIP_SET_DECL (volume, "volume")
SSIP_SET_INT_BODY (volume)
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_voice
SSIP_SET_DECL (voice, "voice")
{
  SSIP_PROCESS_SET_ARGS (char *, STRING);
  {
    char *command;
    int result_code;

    if (asprintf (&command, "SET self VOICE %s", c_value) < 0)
      scm_memory_error (FUNC_NAME);
    result_code = spd_execute_command (c_connection, command);
    free (command);
    RETURN_SUCCESS (result_code);
  }
}
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_punctuation_mode
SSIP_SET_DECL (punctuation_mode, "punctuation-mode")
{
  SSIP_PROCESS_SET_ARGS (char *, SYMBOL);
  {
    SPDPunctuation mode;
    if (! strcmp (c_value, "none")) mode = SPD_PUNCT_NONE;
    else if (! strcmp (c_value, "some")) mode = SPD_PUNCT_SOME;
    else if (! strcmp (c_value, "all")) mode = SPD_PUNCT_ALL;
    else scm_wrong_type_arg (FUNC_NAME, SCM_ARG2, value);
    RETURN_SUCCESS (spd_set_punctuation (c_connection, mode));
  }
}
#undef FUNC_NAME

#define FUNC_NAME s_ssip_set_spelling_mode
SSIP_SET_DECL (spelling_mode, "spelling-mode")
{
  SSIP_PROCESS_SET_ARGS (char *, SYMBOL);
  {
    SPDSpelling mode;
    if (! strcmp (c_value, "on")) mode = SPD_SPELL_ON;
    else if (! strcmp (c_value, "off")) mode = SPD_SPELL_OFF;
    else scm_wrong_type_arg (FUNC_NAME, SCM_ARG2, value);
    RETURN_SUCCESS (spd_set_spelling (c_connection, mode));
  }
}
#undef FUNC_NAME

/* Raw SSIP commands */

SCM_DEFINE (ssip_raw_command, "%ssip-raw-command", 2, 0, 0,
            (SCM connection, SCM command),
            "Send raw COMMAND to CONNECTION and return whether it succeeded.")
#define FUNC_NAME s_ssip_raw_command
{
  int c_connection;
  char *c_command;
  ASSIGN_CONNECTION ();
  ASSIGN_STRING (command, 2);

  RETURN_SUCCESS (spd_execute_command (c_connection, c_command));
}
#undef FUNC_NAME

/* Define the Scheme bindings */

void init_gssip ()
{
#include "gssip.x"
}
