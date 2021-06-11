# FFTS -- The Fastest Fourier Transform in the South

[![Build Status](https://travis-ci.org/linkotec/ffts.svg?branch=master)](https://travis-ci.org/linkotec/ffts)

To build for Android, edit and run build_android.sh

To build for iOS, edit and run build_iphone.sh 

To build for Linux or OS X on x86, run 
  ./configure --enable-sse --enable-single --prefix=/usr/local
  make
  make install

Optionally build for Windows and Linux with CMake, run
  mkdir build
  cd build
  cmake ..
  
FFTS dynamically generates code at runtime. This can be disabled with 
--disable-dynamic-code

Note that 32 bit x86 dynamic machine code generation is not supported at the moment.

For JNI targets: --enable-jni will build the jni stuff automatically for
the host target, and --enable-shared must also be added manually for it to
work.

If you like FFTS, please show your support by sending a postcard to:

Anthony Blake<br>
Department of Computer Science<br>
The University of Waikato<br>
Private Bag 3105<br>
Hamilton 3240<br>
NEW ZEALAND
