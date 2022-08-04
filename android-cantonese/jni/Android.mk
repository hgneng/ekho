LOCAL_PATH:= $(call my-dir)

###########################################################################
# Setup Flite related paths

# We require that FLITEDIR be defined
ifndef FLITEDIR
  $(error "FLITEDIR variable should be set to path where flite is compiled")
endif

FLITE_BUILD_SUBDIR:=$(TARGET_ARCH_ABI)

ifeq "$(TARGET_ARCH_ABI)" "armeabi-v7a"
  FLITE_BUILD_SUBDIR:="armeabiv7a"
endif

FLITE_LIB_DIR:= $(FLITEDIR)/build/$(FLITE_BUILD_SUBDIR)-android/lib
###########################################################################

include $(CLEAR_VARS)

# sndfile
SNDFILE_SRC_PATH := ../../../android_engine2/external/ekho/libsndfile/src

SNDFILE_SRC_FILES := \
  $(subst $(LOCAL_PATH)/$(SNDFILE_SRC_PATH),$(SNDFILE_SRC_PATH),$(wildcard $(LOCAL_PATH)/$(SNDFILE_SRC_PATH)/*.c*))

SNDFILE_SRC_FILES += \
  $(subst $(LOCAL_PATH)/$(SNDFILE_SRC_PATH)/GSM610,$(SNDFILE_SRC_PATH)/GSM610,$(wildcard $(LOCAL_PATH)/$(SNDFILE_SRC_PATH)/GSM610/*.c*))

SNDFILE_SRC_FILES += \
  $(subst $(LOCAL_PATH)/$(SNDFILE_SRC_PATH)/G72x,$(SNDFILE_SRC_PATH)/G72x,$(wildcard $(LOCAL_PATH)/$(SNDFILE_SRC_PATH)/G72x/*.c*))
  
LOCAL_SRC_FILES += $(SNDFILE_SRC_FILES)

LOCAL_C_INCLUDES += \
  $(LOCAL_PATH)/$(SNDFILE_SRC_PATH) \

LOCAL_MODULE    := libsndfile
include $(BUILD_STATIC_LIBRARY)

# Ekho
include $(CLEAR_VARS)

BLACKLIST_SRC_FILES := \
  %/ekho.cpp \
  %/test_ekho.cpp

EKHO_SRC_PATH := ../../../src

EKHO_SRC_FILES := \
  $(subst $(LOCAL_PATH)/$(EKHO_SRC_PATH),$(EKHO_SRC_PATH),$(wildcard $(LOCAL_PATH)/$(EKHO_SRC_PATH)/*.c*))

EKHO_SRC_FILES += ../../../sr-convert/dsp.cpp ../../../sonic/sonic.c
  
LOCAL_SRC_FILES += \
  $(filter-out $(BLACKLIST_SRC_FILES),$(EKHO_SRC_FILES))

LOCAL_CFLAGS := -DOUTPUT16BIT -DNO_SSE -O0 -DANDROID #-DDEBUG_ANDROID

# JNI

LOCAL_SRC_FILES += \
  $(subst $(LOCAL_PATH)/jni,jni,$(wildcard $(LOCAL_PATH)/jni/*.c*))

LOCAL_C_INCLUDES += \
  $(LOCAL_PATH)/include \
  $(LOCAL_PATH)/$(SNDFILE_SRC_PATH) \
  $(LOCAL_PATH)/$(EKHO_SRC_PATH) \
  $(LOCAL_PATH)/$(EKHO_SRC_PATH)/../utfcpp/source \
  $(LOCAL_PATH)/$(EKHO_SRC_PATH)/../sr-convert \
  $(LOCAL_PATH)/$(EKHO_SRC_PATH)/../sonic \
  $(FLITEDIR)/include

LOCAL_LDLIBS := -llog \
  $(FLITE_LIB_DIR)/libflite_cmulex.a \
  $(FLITE_LIB_DIR)/libflite_usenglish.a \
  $(FLITE_LIB_DIR)/libflite.a \
  
# Common

LOCAL_MODULE := libttsekho
LOCAL_MODULE_TAGS := optional
LOCAL_PRELINK_MODULE := false
LOCAL_STATIC_LIBRARIES := libsndfile

include $(BUILD_SHARED_LIBRARY)
