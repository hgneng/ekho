
#include "speechd.h"

#ifndef ALLOC_H
 #define ALLOC_H

/* Like malloc(), but execute FATAL() if it can't allocate
   the memmory. It never returns NULL. */
void* spd_malloc(size_t bytes);

/* Like realloc(), but execute FATAL() if it can't allocate
   the memmory. It never returns NULL. */
void* spd_realloc(void* ptr, size_t bytes);

/* Like free(), but don't try to free NULL data. After freeing
   the data, fill the pointer with NULL. */
void spd_free(void *data);

/* Like strdup(), but copies also the empty "NULL" strings. */
char* spd_strdup(char* string);

TSpeechDQueue* speechd_queue_alloc();					

/* Copy a message */
TSpeechDMessage* spd_message_copy(TSpeechDMessage *old);

/* Free a message */
void mem_free_message(TSpeechDMessage *msg);

/* Free a settings element */
void mem_free_fdset(TFDSetElement *set);


#endif
		
