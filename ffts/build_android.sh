#!/bin/sh
# Compiles ffts for Android
# Make sure you have NDK_ROOT defined in .bashrc or .bash_profile
# Modify INSTALL_DIR to suit your situation

INSTALL_DIR="`pwd`/java/android/bin"

PLATFORM=android-8
TOOL="4.6"

case $(uname -s) in
  Darwin)
    CONFBUILD=i386-apple-darwin`uname -r`
    HOSTPLAT=darwin-x86
  ;;
  Linux)
    CONFBUILD=x86-unknown-linux
    HOSTPLAT=linux-`uname -m`
  ;;
  *) echo $0: Unknown platform; exit
esac

case arm in
  arm)
    TARGPLAT=arm-linux-androideabi
    ARCH=arm
    CONFTARG=arm-eabi
  ;;
  x86)
    TARGPLAT=x86
    ARCH=x86
    CONFTARG=x86
  ;;
  mips)
  ## probably wrong
    TARGPLAT=mipsel-linux-android
    ARCH=mips
    CONFTARG=mips
  ;;
  *) echo $0: Unknown target; exit
esac

: ${NDK_ROOT:?}

echo "Using: $NDK_ROOT/toolchains/${TARGPLAT}-${TOOL}/prebuilt/${HOSTPLAT}/bin"

export PATH="$NDK_ROOT/toolchains/${TARGPLAT}-${TOOL}/prebuilt/${HOSTPLAT}/bin/:$PATH"
export SYS_ROOT="$NDK_ROOT/platforms/${PLATFORM}/arch-${ARCH}/"
export CC="${TARGPLAT}-gcc --sysroot=$SYS_ROOT"
export LD="${TARGPLAT}-ld"
export AR="${TARGPLAT}-ar"
export RANLIB="${TARGPLAT}-ranlib"
export STRIP="${TARGPLAT}-strip"
export CFLAGS="-Os"

mkdir -p $INSTALL_DIR
./configure --enable-neon --build=${CONFBUILD} --host=${CONFTARG} --prefix=$INSTALL_DIR LIBS="-lc -lgcc"

make clean
make
make install

if [ -z "$ANDROID_HOME" ] ; then
    echo ""
    echo " No ANDROID_HOME defined"
    echo " Android JNI interfaces will not be built"
    echo
else
    echo
    echo "Using android_home ${ANDROID_HOME}"
    echo
    ( cd java/android ; ${ANDROID_HOME}/tools/android update lib-project -p . ) || exit 1
    ( cd java/android/jni ; ${NDK_ROOT}/ndk-build V=1 ) || exit 1
    ( cd java/android ; ant release ) || exit 1
    echo
    echo "Android library project location:"
    echo " `pwd`/java/android"
    echo
fi
exit 0
