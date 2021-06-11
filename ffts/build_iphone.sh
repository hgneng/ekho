#/bin/sh
# Compiles ffts for iOS 
# Modify INSTALL_DIR, SDKVER and DEVROOT to suit your situation

INSTALL_DIR="`pwd`/build"

export SDKVER="6.1"
export DEVROOT="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer"
export SDKROOT="$DEVROOT/SDKs/iPhoneOS$SDKVER.sdk"
export CFLAGS="-O3 -Wreturn-type -Wparentheses -Wswitch -Wno-unused-parameter -Wno-unused-variable -Wunused-value -Wno-shorten-64-to-32 -Wno-trigraphs -fpascal-strings -miphoneos-version-min=5.0 -mcpu=cortex-a9 -arch armv7 -mfpu=neon -pipe -isysroot $SDKROOT -isystem $SDKROOT/usr/include -isystem $DEVROOT/usr/include -mno-thumb -no-integrated-as"
export AR="$DEVROOT/usr/bin/ar"
export CC="clang"


mkdir -p $INSTALL_DIR
./configure --enable-neon --build=i386-apple-darwin`uname -r` --host=arm-eabi --prefix=$INSTALL_DIR

make clean
make 
make install

exit 0
