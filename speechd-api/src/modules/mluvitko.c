/* Speechd module for mluvitko (czech software synthetizer)
 * CVS revision: $Id: mluvitko.c,v 1.1 2001-04-10 10:42:05 cerha Exp $
 * Author: Tomas Cerha <cerha@brailcom.cz> */

#define VERSION "0.0.1"

#include <stdio.h>
#include <glib.h>

#include "module.h"

gint       mluvitko_write      (const gchar *data, gint len);
gint       mluvitko_stop       (void);
gint       mluvitko_pause      (void);
gint       mluvitko_release    (void);

/* fill the module_info structure with pointers to this modules functions */
OutputModule modinfo = {
   "mluvitko",
   "Czech software synthesizer",
   NULL, /* filename */
   mluvitko_write,
   mluvitko_stop,
   mluvitko_pause,
   mluvitko_release
};

/* entry point of this module */
OutputModule *module_init(void) {
   printf("mluvitko: init_module()\n");

   /*modinfo.name = g_strdup("mluvitko"),
   modinfo.description = g_strdup_printf("Czech software synthesizer, version %s",VERSION);*/
   return &modinfo;
}


/* module operations */
gint mluvitko_write(const gchar *data, gint len) {
   int i;

   printf("mluvitko: write()\n");

   for (i=0; i<len; i++) {
      printf("%c ",data[i]);
   }
   printf("\n");

   return len;
}

gint mluvitko_stop(void) {
   printf("mluvitko: stop()\n");
   return 1;
}

gint mluvitko_pause(void) {
   printf("mluvitko: pause()\n");
   return 1;
}

gint mluvitko_release(void) {
   printf("mluvitko: release()\n");
   return 1;
}
