#ifdef HAVE_GTK2
#include <gtk/gtk.h>
static GtkTextBuffer *buffer_g = NULL;
static GtkClipboard *clipboard_g = NULL;
static bool is_pause_g = false;
static GtkWidget *window_g = NULL;
static GtkTextIter text_offset_g;

static void continue_speak(void *para);

void on_window_destroy(GtkWidget *widget, gpointer data) { gtk_main_quit(); }

/* File functions */
static void export_wav(GtkWidget *w, gpointer data) {
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new(
      "Save File", (GtkWindow *)window_g, GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE,
      GTK_RESPONSE_ACCEPT, NULL);
  gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog),
                                                 TRUE);

  //    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog),
  //    default_folder_for_saving);
  gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "output.wav");

  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    GtkTextIter start;
    GtkTextIter end;
    gchar *text;
    gtk_text_buffer_get_start_iter(buffer_g, &start);
    gtk_text_buffer_get_end_iter(buffer_g, &end);
    text = gtk_text_buffer_get_text(buffer_g, &start, &end, FALSE);

    char *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

    ekho_g->saveWav(text, filename);
    g_free(filename);
  }

  gtk_widget_destroy(dialog);
}

static void export_ogg(GtkWidget *w, gpointer data) {
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new(
      "Save File", (GtkWindow *)window_g, GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE,
      GTK_RESPONSE_ACCEPT, NULL);
  gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog),
                                                 TRUE);

  //    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog),
  //    default_folder_for_saving);
  gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "output.ogg");

  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    GtkTextIter start;
    GtkTextIter end;
    gchar *text;
    gtk_text_buffer_get_start_iter(buffer_g, &start);
    gtk_text_buffer_get_end_iter(buffer_g, &end);
    text = gtk_text_buffer_get_text(buffer_g, &start, &end, FALSE);

    char *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

    ekho_g->saveOgg(text, filename);
    g_free(filename);
  }

  gtk_widget_destroy(dialog);
}

#ifdef HAVE_MP3LAME
static void export_mp3(GtkWidget *w, gpointer data) {
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new(
      "Save File", (GtkWindow *)window_g, GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE,
      GTK_RESPONSE_ACCEPT, NULL);
  gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog),
                                                 TRUE);

  //    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog),
  //    default_folder_for_saving);
  gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(dialog), "output.mp3");

  if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
    GtkTextIter start;
    GtkTextIter end;
    gchar *text;
    gtk_text_buffer_get_start_iter(buffer_g, &start);
    gtk_text_buffer_get_end_iter(buffer_g, &end);
    text = gtk_text_buffer_get_text(buffer_g, &start, &end, FALSE);

    char *filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

    ekho_g->saveMp3(text, filename);
    g_free(filename);
  }

  gtk_widget_destroy(dialog);
}
#endif

/* edit functions */
static void cut(GtkWidget *w, gpointer data) {
  gtk_text_buffer_cut_clipboard(buffer_g, clipboard_g, true);
}

static void mycopy(GtkWidget *w, gpointer data) {
  gtk_text_buffer_copy_clipboard(buffer_g, clipboard_g);
}

static void paste(GtkWidget *w, gpointer data) {
  gtk_text_buffer_paste_clipboard(buffer_g, clipboard_g, NULL, true);
}

static void select_all(GtkWidget *w, gpointer data) {
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_start_iter(buffer_g, &start);
  gtk_text_buffer_get_end_iter(buffer_g, &end);
  gtk_text_buffer_select_range(buffer_g, &end, &start);
}

/* select language */
static void set_voice(gchar *voice) { ekho_g->setVoice(voice); }

static void set_cantonese(GtkWidget *w, gpointer data) {
  ekho_g->setVoice("Cantonese");
}

static void set_mandarin(GtkWidget *w, gpointer data) {
  ekho_g->setVoice("Mandarin");
}

static void set_korean(GtkWidget *w, gpointer data) {
  ekho_g->setVoice("Korean");
}

static void set_english(GtkWidget *w, gpointer data) {
  ekho_g->setVoice("English");
}

/* speech functions */
static void resume(GtkWidget *w, gpointer data) {
  if (is_pause_g) {
    is_pause_g = false;
    ekho_g->resume();
  }
}

static void continue_speak(void *arg) {
  GtkTextIter start, end, start_match, end_match;

  if (not gtk_text_iter_forward_search(
          &text_offset_g, "\n",
          (GtkTextSearchFlags)(GTK_TEXT_SEARCH_TEXT_ONLY |
                               GTK_TEXT_SEARCH_VISIBLE_ONLY),
          &start_match, &end_match, NULL)) {
    gtk_text_buffer_get_end_iter(buffer_g, &end_match);
  }
  gtk_text_buffer_remove_tag_by_name(buffer_g, "yellow_bg", &text_offset_g,
                                     &end_match);

  int offset = gtk_text_iter_get_offset(&end_match);
  gtk_text_buffer_get_end_iter(buffer_g, &end);
  if (gtk_text_iter_get_offset(&end) > offset) {
    gtk_text_buffer_get_iter_at_offset(buffer_g, &text_offset_g, offset);
    if (not gtk_text_iter_forward_search(
            &text_offset_g, "\n",
            (GtkTextSearchFlags)(GTK_TEXT_SEARCH_TEXT_ONLY |
                                 GTK_TEXT_SEARCH_VISIBLE_ONLY),
            &start_match, &end_match, NULL)) {
      gtk_text_buffer_get_end_iter(buffer_g, &end_match);
      //      gtk_text_buffer_get_iter_at_offset(buffer_g, &text_offset_g,
      //      offset - 1);
      gchar *text =
          gtk_text_buffer_get_text(buffer_g, &text_offset_g, &end_match, FALSE);
      gtk_text_buffer_apply_tag_by_name(buffer_g, "yellow_bg", &text_offset_g,
                                        &end_match);
      if (isDebugging) {
        cerr << "speaking " << text << endl;
      }
      ekho_g->speak(text);
    } else {
      //      gtk_text_buffer_get_iter_at_offset(buffer_g, &text_offset_g,
      //      offset - 1);
      gchar *text = gtk_text_buffer_get_text(buffer_g, &text_offset_g,
                                             &start_match, FALSE);
      gtk_text_buffer_apply_tag_by_name(buffer_g, "yellow_bg", &text_offset_g,
                                        &start_match);
      if (isDebugging) {
        cerr << "speaking " << text << endl;
      }
      if (strcmp(text, "") != 0) {
        gtk_widget_queue_draw(GTK_WIDGET(window_g));  // ??? refresh screen
        ekho_g->speak(text, continue_speak, NULL);
      }
    }
  }
}

static void speak_with_highlight(GtkWidget *w, gpointer data) {
  if (is_pause_g) {
    resume(NULL, NULL);
  } else {
    GtkTextIter start, end, start_match, end_match;
    gchar *text;

    if (gtk_text_buffer_get_selection_bounds(buffer_g, &start, &end)) {
      /* Obtain iters for the start and end of points of the buffer */
      text = gtk_text_buffer_get_text(buffer_g, &start, &end, FALSE);
      ekho_g->speak(text);
    } else {
      gtk_text_buffer_get_start_iter(buffer_g, &start);
      gtk_text_buffer_get_start_iter(buffer_g, &text_offset_g);
      if (not gtk_text_iter_forward_search(
              &start, "\n", (GtkTextSearchFlags)(GTK_TEXT_SEARCH_TEXT_ONLY |
                                                 GTK_TEXT_SEARCH_VISIBLE_ONLY),
              &start_match, &end_match, NULL)) {
        gtk_text_buffer_get_end_iter(buffer_g, &end_match);
        text = gtk_text_buffer_get_text(buffer_g, &start, &end_match, FALSE);
        if (isDebugging) {
          cerr << "speaking " << text << endl;
        }
        gtk_text_buffer_apply_tag_by_name(buffer_g, "yellow_bg", &text_offset_g,
                                          &end_match);
        ekho_g->speak(text);
      } else {
        text = gtk_text_buffer_get_text(buffer_g, &start, &start_match, FALSE);
        if (isDebugging) {
          cerr << "speaking " << text << endl;
        }
        gtk_text_buffer_apply_tag_by_name(buffer_g, "yellow_bg", &text_offset_g,
                                          &start_match);
        ekho_g->speak(text, continue_speak, NULL);
      }
    }
  }
}

static void speak(GtkWidget *w, gpointer data) {
  if (is_pause_g) {
    resume(NULL, NULL);
  } else {
    GtkTextIter start;
    GtkTextIter end;

    gchar *text;

    /* Obtain iters for the start and end of points of the buffer */
    gtk_text_buffer_get_start_iter(buffer_g, &start);
    gtk_text_buffer_get_end_iter(buffer_g, &end);

    /* Get the entire buffer text. */
    text = gtk_text_buffer_get_text(buffer_g, &start, &end, FALSE);
    ekho_g->speak(text);
  }
}

static void my_pause(GtkWidget *w, gpointer data) {
  is_pause_g = true;
  ekho_g->pause();
}

static void stop(GtkWidget *w, gpointer data) {
  resume(NULL, NULL);
  ekho_g->stop();
}

/* tune sound */
static void set_tempo(GtkWidget *w, gpointer data) {
  GtkWidget *dialog = gtk_dialog_new_with_buttons(
      "Set Speed", GTK_WINDOW(window_g), GTK_DIALOG_MODAL, GTK_STOCK_CANCEL,
      GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
  GtkWidget *label =
      gtk_label_new("Please input speed delta in percent (-50 .. 100):");
  GtkWidget *entry = gtk_entry_new_with_max_length(3);
  int tempo_delta = ekho_g->getSpeed();
  char tempo_buf[4];
  gcvt(tempo_delta, 3, tempo_buf);
  gtk_entry_set_text((GtkEntry *)entry, tempo_buf);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), entry);
  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));
  if (result == GTK_RESPONSE_OK) {
    ekho_g->setSpeed(atoi(gtk_entry_get_text(GTK_ENTRY(entry))));
  }
  gtk_widget_destroy(dialog);
}

static void set_volume(GtkWidget *w, gpointer data) {
  GtkWidget *dialog = gtk_dialog_new_with_buttons(
      "Set Volume", GTK_WINDOW(window_g), GTK_DIALOG_MODAL, GTK_STOCK_CANCEL,
      GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
  GtkWidget *label = gtk_label_new("Please input volume delta (-100 .. 100):");
  GtkWidget *entry = gtk_entry_new_with_max_length(5);
  int volume_delta = ekho_g->getVolume();
  char volume_buf[6];
  gcvt(volume_delta, 4, volume_buf);
  gtk_entry_set_text((GtkEntry *)entry, volume_buf);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), entry);
  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));
  if (result == GTK_RESPONSE_OK) {
    ekho_g->setVolume(atoi(gtk_entry_get_text(GTK_ENTRY(entry))));
  }
  gtk_widget_destroy(dialog);
}

static void set_pitch(GtkWidget *w, gpointer data) {
  GtkWidget *dialog = gtk_dialog_new_with_buttons(
      "Set Pitch", GTK_WINDOW(window_g), GTK_DIALOG_MODAL, GTK_STOCK_CANCEL,
      GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
  GtkWidget *label = gtk_label_new("Please input pitch delta (-100 .. 100):");
  GtkWidget *entry = gtk_entry_new_with_max_length(5);
  int pitch_delta = ekho_g->getPitch();
  char pitch_buf[6];
  gcvt(pitch_delta, 4, pitch_buf);
  gtk_entry_set_text((GtkEntry *)entry, pitch_buf);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), entry);
  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));
  if (result == GTK_RESPONSE_OK) {
    ekho_g->setPitch(atoi(gtk_entry_get_text(GTK_ENTRY(entry))));
  }
  gtk_widget_destroy(dialog);
}

static void set_rate(GtkWidget *w, gpointer data) {
  GtkWidget *dialog = gtk_dialog_new_with_buttons(
      "Set Rate", GTK_WINDOW(window_g), GTK_DIALOG_MODAL, GTK_STOCK_CANCEL,
      GTK_RESPONSE_CANCEL, GTK_STOCK_OK, GTK_RESPONSE_OK, NULL);
  GtkWidget *label =
      gtk_label_new("Please input rate delta in percent (-50 .. 100):");
  GtkWidget *entry = gtk_entry_new_with_max_length(3);
  float rate_delta = ekho_g->getRate();
  char rate_buf[4];
  gcvt(rate_delta, 3, rate_buf);
  gtk_entry_set_text((GtkEntry *)entry, rate_buf);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), entry);
  gtk_widget_show_all(dialog);
  gint result = gtk_dialog_run(GTK_DIALOG(dialog));
  if (result == GTK_RESPONSE_OK) {
    ekho_g->setRate(atoi(gtk_entry_get_text(GTK_ENTRY(entry))));
  }
  gtk_widget_destroy(dialog);
}

static void about(GtkWidget *w, gpointer data) {
  gchar *authors[1] = {(gchar *)"Cameron Wong(黄冠能), hgneng at gmail.com"};
  gtk_show_about_dialog(
      GTK_WINDOW(window_g), "name", "Ekho(余音)", "title", "About Ekho",
      "authors", (GStrv)authors, "license",
      "Ekho is distributed under the Creative Commons GNU GPL.\n\nTo get "
      "Human-Readable description of this licese, please refer "
      "http://creativecommons.org/licenses/GPL/2.0/\n\nTo get Commons Deed "
      "Lawyer-Readable description of this licese, please refer "
      "http://www.gnu.org/licenses/gpl-2.0.html",
      "version", VERSION, "website", "http://www.eguidedog.net/ekho.php", NULL);
}

/* Returns a menubar widget made from the above menu */
static GtkWidget *get_menubar_menu(GtkWidget *window) {
  /* build file menu */
  GtkWidget *menu_item_wav = gtk_menu_item_new_with_mnemonic("Export to _WAV");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_wav), "activate",
                            GTK_SIGNAL_FUNC(export_wav), NULL);
  GtkWidget *menu_item_ogg = gtk_menu_item_new_with_mnemonic("Export to _OGG");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_ogg), "activate",
                            GTK_SIGNAL_FUNC(export_ogg), NULL);
#ifdef HAVE_MP3LAME
  GtkWidget *menu_item_mp3 = gtk_menu_item_new_with_mnemonic("Export to _MP3");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_mp3), "activate",
                            GTK_SIGNAL_FUNC(export_mp3), NULL);
#endif
  GtkWidget *menu_item_quit = gtk_menu_item_new_with_mnemonic("_Quit");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_quit), "activate",
                            GTK_SIGNAL_FUNC(on_window_destroy), NULL);

  GtkWidget *menu_file = gtk_menu_new();
  GtkWidget *menu_item_file = gtk_menu_item_new_with_mnemonic("_File");
  gtk_menu_item_set_submenu((GtkMenuItem *)menu_item_file, menu_file);
  gtk_menu_shell_append((GtkMenuShell *)menu_file, menu_item_wav);
  gtk_menu_shell_append((GtkMenuShell *)menu_file, menu_item_ogg);
#ifdef HAVE_MP3LAME
  gtk_menu_shell_append((GtkMenuShell *)menu_file, menu_item_mp3);
#endif
  gtk_menu_shell_append((GtkMenuShell *)menu_file, menu_item_quit);

  /* build edit menu */
  GtkWidget *menu_item_cut = gtk_menu_item_new_with_mnemonic("Cu_t");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_cut), "activate",
                            GTK_SIGNAL_FUNC(cut), NULL);
  GtkWidget *menu_item_copy = gtk_menu_item_new_with_mnemonic("_Copy");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_copy), "activate",
                            GTK_SIGNAL_FUNC(mycopy), NULL);
  GtkWidget *menu_item_paste = gtk_menu_item_new_with_mnemonic("_Paste");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_paste), "activate",
                            GTK_SIGNAL_FUNC(paste), NULL);
  GtkWidget *menu_item_select_all =
      gtk_menu_item_new_with_mnemonic("Sellect _All");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_select_all), "activate",
                            GTK_SIGNAL_FUNC(select_all), NULL);

  GtkWidget *menu_edit = gtk_menu_new();
  GtkWidget *menu_item_edit = gtk_menu_item_new_with_mnemonic("_Edit");
  gtk_menu_item_set_submenu((GtkMenuItem *)menu_item_edit, menu_edit);
  gtk_menu_shell_append((GtkMenuShell *)menu_edit, menu_item_cut);
  gtk_menu_shell_append((GtkMenuShell *)menu_edit, menu_item_copy);
  gtk_menu_shell_append((GtkMenuShell *)menu_edit, menu_item_paste);
  gtk_menu_shell_append((GtkMenuShell *)menu_edit, menu_item_select_all);

  /* build language menu */
  GtkWidget *menu_lang = gtk_menu_new();
  GtkWidget *menu_item_lang = gtk_menu_item_new_with_mnemonic("_Language");
  gtk_menu_item_set_submenu((GtkMenuItem *)menu_item_lang, menu_lang);

  GtkWidget *menu_item_sep = gtk_separator_menu_item_new();
  gtk_menu_shell_append((GtkMenuShell *)menu_lang, menu_item_sep);

  GtkWidget *menu_item_voice =
      gtk_menu_item_new_with_mnemonic("English (only)");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_voice), "activate",
                            GTK_SIGNAL_FUNC(set_voice), (gpointer) "English");
  gtk_menu_shell_append((GtkMenuShell *)menu_lang, menu_item_voice);

  DIR *dirp = opendir(ekho_g->mDict.mDataPath.c_str());
  if (!dirp) {
    fprintf(stderr, "Can't open directory %s\n",
            ekho_g->mDict.mDataPath.c_str());
    exit(1);
  }
  struct dirent *entry = NULL;
  char lang_name[255] = {0};
  while ((entry = readdir(dirp)) != NULL) {
    if (strcmp(entry->d_name, "jyutping") == 0 ||
        strstr(entry->d_name, "jyutping-") == entry->d_name) {
      lang_name[0] = 0;
      strcat(lang_name, entry->d_name);
      strcat(lang_name, " (Cantonese)");
      GtkWidget *menu_item_voice = gtk_menu_item_new_with_mnemonic(lang_name);
      /* BUG: free it in future!! */
      char *name = (char *)malloc(strlen(entry->d_name) + 1);
      strcpy(name, entry->d_name);
      gtk_signal_connect_object(GTK_OBJECT(menu_item_voice), "activate",
                                GTK_SIGNAL_FUNC(set_voice), name);
      gtk_menu_shell_append((GtkMenuShell *)menu_lang, menu_item_voice);
    } else if (strcmp(entry->d_name, "pinyin") == 0 ||
               strstr(entry->d_name, "pinyin-") == entry->d_name) {
      lang_name[0] = 0;
      strcat(lang_name, entry->d_name);
      strcat(lang_name, " (Mandarin)");
      /* BUG: free it in future!! */
      char *name = (char *)malloc(strlen(entry->d_name) + 1);
      strcpy(name, entry->d_name);
      GtkWidget *menu_item_voice = gtk_menu_item_new_with_mnemonic(lang_name);
      gtk_signal_connect_object(GTK_OBJECT(menu_item_voice), "activate",
                                GTK_SIGNAL_FUNC(set_voice), name);
      gtk_menu_shell_append((GtkMenuShell *)menu_lang, menu_item_voice);
    } else if (strcmp(entry->d_name, "hangul") == 0 ||
               strstr(entry->d_name, "hangul-") == entry->d_name) {
      lang_name[0] = 0;
      strcat(lang_name, entry->d_name);
      strcat(lang_name, " (Korean)");
      /* BUG: free it in future!! */
      char *name = (char *)malloc(strlen(entry->d_name) + 1);
      strcpy(name, entry->d_name);
      GtkWidget *menu_item_voice = gtk_menu_item_new_with_mnemonic(lang_name);
      gtk_signal_connect_object(GTK_OBJECT(menu_item_voice), "activate",
                                GTK_SIGNAL_FUNC(set_voice), name);
      gtk_menu_shell_append((GtkMenuShell *)menu_lang, menu_item_voice);
    }
  }
  closedir(dirp);

  /* build speech menu */
  GtkWidget *menu_item_speak = gtk_menu_item_new_with_mnemonic("_Speak");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_speak), "activate",
                            GTK_SIGNAL_FUNC(speak), NULL);
  GtkWidget *menu_item_pause = gtk_menu_item_new_with_mnemonic("_Pause");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_pause), "activate",
                            GTK_SIGNAL_FUNC(my_pause), NULL);
  GtkWidget *menu_item_stop = gtk_menu_item_new_with_mnemonic("S_top");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_paste), "activate",
                            GTK_SIGNAL_FUNC(stop), NULL);

  GtkWidget *menu_speech = gtk_menu_new();
  GtkWidget *menu_item_speech = gtk_menu_item_new_with_mnemonic("_Speech");
  gtk_menu_item_set_submenu((GtkMenuItem *)menu_item_speech, menu_speech);
  gtk_menu_shell_append((GtkMenuShell *)menu_speech, menu_item_speak);
  gtk_menu_shell_append((GtkMenuShell *)menu_speech, menu_item_pause);
  gtk_menu_shell_append((GtkMenuShell *)menu_speech, menu_item_stop);

  /* build option menu */
  GtkWidget *menu_item_speed = gtk_menu_item_new_with_mnemonic("_Speed");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_speed), "activate",
                            GTK_SIGNAL_FUNC(set_tempo), NULL);
  GtkWidget *menu_item_pitch = gtk_menu_item_new_with_mnemonic("_Pitch");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_pitch), "activate",
                            GTK_SIGNAL_FUNC(set_pitch), NULL);
  GtkWidget *menu_item_volume = gtk_menu_item_new_with_mnemonic("_Volume");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_volume), "activate",
                            GTK_SIGNAL_FUNC(set_volume), NULL);
  GtkWidget *menu_item_rate = gtk_menu_item_new_with_mnemonic("_Rate");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_rate), "activate",
                            GTK_SIGNAL_FUNC(set_rate), NULL);

  GtkWidget *menu_option = gtk_menu_new();
  GtkWidget *menu_item_option = gtk_menu_item_new_with_mnemonic("_Options");
  gtk_menu_item_set_submenu((GtkMenuItem *)menu_item_option, menu_option);
  gtk_menu_shell_append((GtkMenuShell *)menu_option, menu_item_speed);
  gtk_menu_shell_append((GtkMenuShell *)menu_option, menu_item_pitch);
  gtk_menu_shell_append((GtkMenuShell *)menu_option, menu_item_volume);
  gtk_menu_shell_append((GtkMenuShell *)menu_option, menu_item_rate);

  /* build help menu */
  GtkWidget *menu_item_about = gtk_menu_item_new_with_mnemonic("_About");
  gtk_signal_connect_object(GTK_OBJECT(menu_item_about), "activate",
                            GTK_SIGNAL_FUNC(about), NULL);

  GtkWidget *menu_help = gtk_menu_new();
  GtkWidget *menu_item_help = gtk_menu_item_new_with_mnemonic("_Help");
  gtk_menu_item_set_submenu((GtkMenuItem *)menu_item_help, menu_help);
  gtk_menu_shell_append((GtkMenuShell *)menu_help, menu_item_about);

  /* build menu bar */
  GtkWidget *menu_bar = gtk_menu_bar_new();
  gtk_menu_shell_append((GtkMenuShell *)menu_bar, menu_item_file);
  gtk_menu_shell_append((GtkMenuShell *)menu_bar, menu_item_edit);
  gtk_menu_shell_append((GtkMenuShell *)menu_bar, menu_item_lang);
  gtk_menu_shell_append((GtkMenuShell *)menu_bar, menu_item_speech);
#ifdef ENABLE_SOUNDTOUCH
  gtk_menu_shell_append((GtkMenuShell *)menu_bar, menu_item_option);
#endif
  gtk_menu_shell_append((GtkMenuShell *)menu_bar, menu_item_help);
  return menu_bar;
}

int xmain(int *argc, char ***argv) {
  GtkWidget *text_view;

  /* init ekho */
  ekho_g = new Ekho("English");

  gtk_init(argc, argv);

  /* set locale to zh_CN.UTF-8 */
  setlocale(LC_ALL, "zh_CN.UTF-8");

  /* Create a Window. */
  window_g = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  char title[20];
  strcpy(title, "Ekho(余音) ");
  strcat(title, VERSION);
  gtk_window_set_title(GTK_WINDOW(window_g), title);
  gtk_window_set_position(GTK_WINDOW(window_g), GTK_WIN_POS_CENTER);

  /* Set a decent default size for the window. */
  gtk_window_set_default_size(GTK_WINDOW(window_g), 400, 300);
  g_signal_connect(G_OBJECT(window_g), "destroy", G_CALLBACK(on_window_destroy),
                   NULL);

  GtkWidget *vbox = gtk_vbox_new(FALSE, 2);
  gtk_container_add(GTK_CONTAINER(window_g), vbox);

  /* Menu */
  clipboard_g = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  GtkWidget *menubar = get_menubar_menu(window_g);
  gtk_box_pack_start(GTK_BOX(vbox), menubar, FALSE, TRUE, 0);

  /* Popup Menu for editing */

  /* Create a scroll window */
  GtkWidget *pScrollWin = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(pScrollWin),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), pScrollWin, 1, 1, 0);

  /* Create a multiline text widget. */
  text_view = gtk_text_view_new();
  gtk_text_view_set_wrap_mode((GtkTextView *)text_view, GTK_WRAP_CHAR);
  gtk_container_add(GTK_CONTAINER(pScrollWin), text_view);

  /* Obtaining the buffer associated with the widget. */
  buffer_g = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text_view));

  gtk_text_buffer_create_tag(buffer_g, "yellow_bg", "background", "yellow",
                             NULL);

  /* Set the default buffer text. */
  //    gtk_text_buffer_set_text (buffer_g, "我挥一挥衣袖，不带走一片云彩", -1);

  /* Create a close button. */
  GtkWidget *button_speak = gtk_button_new_with_label("Speak");
  g_signal_connect(G_OBJECT(button_speak), "clicked", G_CALLBACK(speak),
                   buffer_g);
  GtkWidget *button_speak_with_highlight =
      gtk_button_new_with_label("Speak with hightlight");
  g_signal_connect(G_OBJECT(button_speak_with_highlight), "clicked",
                   G_CALLBACK(speak_with_highlight), buffer_g);
  GtkWidget *button_pause = gtk_button_new_with_label("Pause");
  g_signal_connect(G_OBJECT(button_pause), "clicked", G_CALLBACK(my_pause),
                   buffer_g);
  GtkWidget *button_stop = gtk_button_new_with_label("Stop");
  g_signal_connect(G_OBJECT(button_stop), "clicked", G_CALLBACK(stop),
                   buffer_g);
  GtkWidget *hbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_start(GTK_BOX(hbox), button_speak, 0, 0, 0);
  gtk_box_pack_start(GTK_BOX(hbox), button_speak_with_highlight, 0, 0, 0);
  gtk_box_pack_start(GTK_BOX(hbox), button_pause, 0, 0, 0);
  gtk_box_pack_start(GTK_BOX(hbox), button_stop, 0, 0, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hbox, 0, 0, 0);

  gtk_widget_show_all(window_g);

  gtk_main();

  return 0;
}
#else
int xmain(int *argc, char ***argv) {
  //fprintf(stderr, "GTK is not supported!\n");
  return 0;
}
#endif