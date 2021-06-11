LOCAL_PATH := $(call my-dir)

TOP=../../..

# Include the shared library
#include $(CLEAR_VARS)
#LOCAL_MODULE := ffts
#LOCAL_SRC_FILES :=  ../../../src/.libs/libffts.so
#include $(PREBUILT_SHARED_LIBRARY)

# Include the static library in shared lib
include $(CLEAR_VARS)
LOCAL_MODULE := ffts
LOCAL_SRC_FILES := $(TOP)/java/android/bin/lib/libffts.a
LOCAL_EXPORT_C_INCLUDES := $(TOP)/include
include $(PREBUILT_STATIC_LIBRARY)

include $(CLEAR_VARS)
LOCAL_MODULE := ffts_jni
LOCAL_CFLAGS := -I$(TOP)/include -I$(TOP)/java/jni -I$(TOP) -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast
LOCAL_SRC_FILES := $(TOP)/java/jni/ffts_jni.c
LOCAL_LDLIBS := -L$(SYSROOT)/usr/lib -llog 
LOCAL_STATIC_LIBRARIES := ffts

include $(BUILD_SHARED_LIBRARY)
