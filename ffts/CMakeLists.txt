cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)

project(ffts C ASM)

# TODO: to support AutoConfigure building, this should came from "template" file
set(FFTS_MAJOR 0)
set(FFTS_MINOR 9)
set(FFTS_MICRO 0)

set(FFTS_VERSION "ffts-${FFTS_MAJOR}.${FFTS_MINOR}.${FFTS_MICRO}")

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# default build type is Debug which means no optimization
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release")
endif(NOT CMAKE_BUILD_TYPE)

# installation parameters
set(INCLUDE_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/include/ffts)
set(LIB_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/lib)

# common options
option(ENABLE_NEON
  "Enables the use of NEON instructions." OFF
)

option(ENABLE_VFP
  "Enables the use of VFP instructions." OFF
)

option(DISABLE_DYNAMIC_CODE
  "Disables the use of dynamic machine code generation." OFF
)

option(GENERATE_POSITION_INDEPENDENT_CODE
  "Generate position independent code" OFF
)

option(ENABLE_SHARED
  "Enable building a shared library." ON
)

option(ENABLE_STATIC
  "Enable building a static library." OFF
)

include(CheckCSourceCompiles)
include(CheckCSourceRuns)
include(CheckFunctionExists)
include(CheckIncludeFile)
include(CheckSymbolExists)

# Ensure defined when building FFTS (as opposed to using it from
# another project). Used to export functions from Windows DLL.
add_definitions(-DFFTS_BUILD)

# check existence of various headers
check_include_file(malloc.h    HAVE_MALLOC_H)
check_include_file(mm_malloc.h HAVE_MM_MALLOC_H)
check_include_file(stdint.h    HAVE_STDINT_H)
check_include_file(stdlib.h    HAVE_STDLIB_H)
check_include_file(string.h    HAVE_STRING_H)
check_include_file(sys/mman.h  HAVE_SYS_MMAN_H)
check_include_file(unistd.h    HAVE_UNISTD_H)

if(HAVE_MALLOC_H)
  add_definitions(-DHAVE_MALLOC_H)
endif(HAVE_MALLOC_H)

if(HAVE_MM_MALLOC_H)
  add_definitions(-DHAVE_MM_MALLOC_H)
endif(HAVE_MM_MALLOC_H)

if(HAVE_STDINT_H)
  add_definitions(-DHAVE_STDINT_H)
endif(HAVE_STDINT_H)

if(HAVE_STDLIB_H)
  add_definitions(-DHAVE_STDLIB_H)
endif(HAVE_STDLIB_H)

if(HAVE_STRING_H)
  add_definitions(-DHAVE_STRING_H)
endif(HAVE_STRING_H)

if(HAVE_SYS_MMAN_H)
  add_definitions(-DHAVE_SYS_MMAN_H)
endif(HAVE_SYS_MMAN_H)

if(HAVE_UNISTD_H)
  add_definitions(-DHAVE_UNISTD_H)
endif(HAVE_UNISTD_H)

# check existence of various declarations
check_symbol_exists(aligned_alloc  stdlib.h HAVE_DECL_ALIGNED_ALLOC)
check_symbol_exists(memalign       malloc.h HAVE_DECL_MEMALIGN)
check_symbol_exists(posix_memalign stdlib.h HAVE_DECL_POSIX_MEMALIGN)
check_symbol_exists(valloc         stdlib.h HAVE_DECL_VALLOC)
check_symbol_exists(_mm_malloc     malloc.h HAVE_DECL__MM_MALLOC)

if(HAVE_DECL_ALIGNED_ALLOC)
  add_definitions(-DHAVE_DECL_ALIGNED_ALLOC)
endif(HAVE_DECL_ALIGNED_ALLOC)

if(HAVE_DECL_MEMALIGN)
  add_definitions(-DHAVE_DECL_MEMALIGN)
endif(HAVE_DECL_MEMALIGN)

if(HAVE_DECL_POSIX_MEMALIGN)
  add_definitions(-DHAVE_DECL_POSIX_MEMALIGN)
endif(HAVE_DECL_POSIX_MEMALIGN)

if(HAVE_DECL_VALLOC)
  add_definitions(-DHAVE_DECL_VALLOC)
endif(HAVE_DECL_VALLOC)

if(HAVE_DECL__MM_MALLOC)
  add_definitions(-DHAVE_DECL__MM_MALLOC)
endif(HAVE_DECL__MM_MALLOC)

# check existence of various functions
check_function_exists(aligned_alloc  HAVE_ALIGNED_ALLOC)
check_function_exists(memalign       HAVE_MEMALIGN)
check_function_exists(posix_memalign HAVE_POSIX_MEMALIGN)
check_function_exists(valloc         HAVE_VALLOC)
check_function_exists(_mm_malloc     HAVE__MM_MALLOC)

if(HAVE_ALIGNED_ALLOC)
  add_definitions(-DHAVE_ALIGNED_ALLOC)
endif(HAVE_ALIGNED_ALLOC)

if(HAVE_MEMALIGN)
  add_definitions(-DHAVE_MEMALIGN)
endif(HAVE_MEMALIGN)

if(HAVE_POSIX_MEMALIGN)
  add_definitions(-DHAVE_POSIX_MEMALIGN)
endif(HAVE_POSIX_MEMALIGN)

if(HAVE_VALLOC)
  add_definitions(-DHAVE_VALLOC)
endif(HAVE_VALLOC)

if(HAVE__MM_MALLOC)
  add_definitions(-DHAVE__MM_MALLOC)
endif(HAVE__MM_MALLOC)

# backup flags
set(CMAKE_REQUIRED_FLAGS_SAVE ${CMAKE_REQUIRED_FLAGS})

# Determinate if we are cross-compiling
if(NOT CMAKE_CROSSCOMPILING)
  if(CMAKE_SYSTEM_PROCESSOR MATCHES "^arm")
    # Determinate ARM architecture

    # Try to execute quietly without messages
    set(CMAKE_REQUIRED_QUIET 1)

    # The test for ARM architecture
    set(TEST_SOURCE_CODE "int main() { return 0; }")

    # GCC documentation says "native" is only supported on Linux, but let's try
    set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -march=native")
    check_c_source_runs("${TEST_SOURCE_CODE}" GCC_MARCH_NATIVE_FLAG_SUPPORTED)

    if(NOT GCC_MARCH_NATIVE_FLAG_SUPPORTED)
      # Fallback trying generic ARMv7
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -march=armv7-a")
      check_c_source_runs("${TEST_SOURCE_CODE}" GCC_MARCH_ARMV7A_FLAG_SUPPORTED)

      if(NOT GCC_MARCH_ARMV7A_FLAG_SUPPORTED)
        # Fallback trying generic ARMv6
        set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -march=armv6")
        check_c_source_runs("${TEST_SOURCE_CODE}" GCC_MARCH_ARMV6_FLAG_SUPPORTED)

        if(NOT GCC_MARCH_ARMV6_FLAG_SUPPORTED)
          message(WARNING "FFTS failed to determinate ARM architecture")
          set(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS_SAVE})
        else()
          message("FFTS is build using 'march=armv6'")
          set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -march=armv6")
          set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=armv6")
        endif(NOT GCC_MARCH_ARMV6_FLAG_SUPPORTED)
      else()
        message("FFTS is build using 'march=armv7-a'")
        set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -march=armv7-a")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=armv7-a")
      endif(NOT GCC_MARCH_ARMV7A_FLAG_SUPPORTED)
    else()
       message("FFTS is build using 'march=native'")
       set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -march=native")
       set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=native")
    endif(NOT GCC_MARCH_NATIVE_FLAG_SUPPORTED)

    # Determinate what floating-point hardware (or hardware emulation) is available
    set(CMAKE_REQUIRED_FLAGS_SAVE ${CMAKE_REQUIRED_FLAGS})

    # The test for ARM NEON support
    set(TEST_SOURCE_CODE "
      #include <arm_neon.h>
      int main()
      {
       float32x4_t v;
       float zeros[4] = {0.0f, 0.0f, 0.0f, 0.0f};
       v = vld1q_f32(zeros);
       return 0;
      }"
    )

    # Test running with -mfpu=neon and -mfloat-abi=hard
    set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -mfpu=neon -mfloat-abi=hard")
    check_c_source_runs("${TEST_SOURCE_CODE}" NEON_HARDFP_SUPPORTED)

    if(NOT NEON_HARDFP_SUPPORTED)
      # Test running with -mfpu=neon and -mfloat-abi=softfp
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -mfpu=neon -mfloat-abi=softfp")
      check_c_source_runs("${TEST_SOURCE_CODE}" NEON_SOFTFP_SUPPORTED)
      
      if(NOT NEON_SOFTFP_SUPPORTED)
        if(ENABLE_NEON)
          message(FATAL_ERROR "FFTS cannot enable NEON on this platform")
        endif(ENABLE_NEON)
      else()
        message("FFTS is using 'neon' FPU and 'softfp' float ABI")
        set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -mfpu=neon -mfloat-abi=softfp")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mfpu=neon -mfloat-abi=softfp")
        set(ENABLE_NEON ON)
      endif(NOT NEON_SOFTFP_SUPPORTED)
    else()
      message("FFTS is using 'neon' FPU and 'hard' float ABI")
      set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -mfpu=neon -mfloat-abi=hard")
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mfpu=neon -mfloat-abi=hard")
      set(ENABLE_NEON ON)
    endif(NOT NEON_HARDFP_SUPPORTED)

    # Fallback using VFP if NEON is not supported
    if(NOT NEON_HARDFP_SUPPORTED AND NOT NEON_SOFTFP_SUPPORTED)
      # Test for ARM VFP support
      set(TEST_SOURCE_CODE "
        double sum(double a, double b)
        {
         return a + b;
        }
        int main()
        {
         double s1, s2, v1 = 1.0, v2 = 2.0, v3 = 1.0e-322;
         s1 = sum(v1, v2);
         s2 = sum(v3, v3);
         return 0;
        }"
      )

      # Test running with -mfpu=vfp
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -mfpu=vfp")
      check_c_source_runs("${TEST_SOURCE_CODE}" VFP_SUPPORTED)

      if(NOT VFP_SUPPORTED)
        # Fallback using emulation if VFP is not supported
        if(ENABLE_VFP)
          message(FATAL_ERROR "FFTS cannot enable VFP on this platform")
        endif(ENABLE_VFP)

        message(WARNING "FFTS is using 'soft' FPU")
      else()
        message("FFTS is using 'vfp' FPU")
        set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -mfpu=vfp")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mfpu=vfp")
        set(CMAKE_REQUIRED_FLAGS_SAVE ${CMAKE_REQUIRED_FLAGS})
        set(ENABLE_VFP ON)
      endif(NOT VFP_SUPPORTED)

      # Test running with -mfloat-abi=hard
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -mfloat-abi=hard")

      # Use the same test as before
      check_c_source_runs("${TEST_SOURCE_CODE}" HARDFP_SUPPORTED)

      if(NOT HARDFP_SUPPORTED)
        # Test running with -mfloat-abi=softfp
        set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -mfloat-abi=softfp")
        check_c_source_runs("${TEST_SOURCE_CODE}" SOFTFP_SUPPORTED)

        if(NOT SOFTFP_SUPPORTED)
          # Most likely development libraries are missing
          message(WARNING "FFTS is using 'soft' float ABI")
        else()
          message("FFTS is using 'softfp' float ABI")
          set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -mfloat-abi=softfp")
          set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mfloat-abi=softfp")
        endif(NOT SOFTFP_SUPPORTED)
      else()
        message("FFTS is using 'hard' float ABI")
        set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} -mfloat-abi=hard")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mfloat-abi=hard")
      endif(NOT HARDFP_SUPPORTED)
    endif(NOT NEON_HARDFP_SUPPORTED AND NOT NEON_SOFTFP_SUPPORTED)
  else()
    # enable SSE code generation
    if(CMAKE_COMPILER_IS_GNUCC)
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -msse")
    endif(CMAKE_COMPILER_IS_GNUCC)

    # check if the platform has support for SSE intrinsics
    check_include_file(xmmintrin.h HAVE_XMMINTRIN_H)
    if(HAVE_XMMINTRIN_H)
      add_definitions(-DHAVE_SSE)
      set(CMAKE_REQUIRED_FLAGS_SAVE ${CMAKE_REQUIRED_FLAGS})
    endif(HAVE_XMMINTRIN_H)

    # enable SSE2 code generation
    if(CMAKE_COMPILER_IS_GNUCC)
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -msse2")
    endif(CMAKE_COMPILER_IS_GNUCC)

    # check if the platform has support for SSE2 intrinsics
    check_include_file(emmintrin.h HAVE_EMMINTRIN_H)
    if(HAVE_EMMINTRIN_H)
      add_definitions(-DHAVE_SSE2)
      set(CMAKE_REQUIRED_FLAGS_SAVE ${CMAKE_REQUIRED_FLAGS})
    endif(HAVE_EMMINTRIN_H)

    # enable SSE3 code generation
    if(CMAKE_COMPILER_IS_GNUCC)
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS_SAVE} -msse3")
    endif(CMAKE_COMPILER_IS_GNUCC)

    # check if the platform has support for SSE3 intrinsics
    check_include_file(pmmintrin.h HAVE_PMMINTRIN_H)
    if(HAVE_PMMINTRIN_H)
      add_definitions(-DHAVE_PMMINTRIN_H)
      add_definitions(-DHAVE_SSE3)
      set(CMAKE_REQUIRED_FLAGS_SAVE ${CMAKE_REQUIRED_FLAGS})
    else()
      # check if the platform has specific intrinsics
      check_include_file(intrin.h HAVE_INTRIN_H)
      if(HAVE_INTRIN_H)
        add_definitions(-DHAVE_INTRIN_H)

        check_c_source_compiles("
          #include<intrin.h>
          int main(int argc, char** argv)
          {
           (void) argv;
           (void) argc;
           return _mm_movemask_ps(_mm_moveldup_ps(_mm_set_ss(1.0f)));
          }" HAVE__MM_MOVELDUP_PS
        )

        if(HAVE__MM_MOVELDUP_PS)
          # assume that we have all SSE3 intrinsics
          add_definitions(-DHAVE_SSE3)
        endif(HAVE__MM_MOVELDUP_PS)
      endif(HAVE_INTRIN_H)
    endif(HAVE_PMMINTRIN_H)
  endif(CMAKE_SYSTEM_PROCESSOR MATCHES "^arm")
else()
  # TODO: Add detections for compiler support and headers
endif(NOT CMAKE_CROSSCOMPILING)

# restore flags
set(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS_SAVE})

# compiler settings
if(MSVC)
  # enable all warnings but also disable some..
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W4 /wd4127")

  # mark debug versions
  set(CMAKE_DEBUG_POSTFIX "d")

  add_definitions(-D_USE_MATH_DEFINES)
elseif(CMAKE_COMPILER_IS_GNUCC)
  include(CheckCCompilerFlag)
  include(CheckLibraryExists)

  # enable all warnings
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")

  # check if we can control visibility of symbols
  check_c_compiler_flag(-fvisibility=hidden HAVE_GCC_VISIBILITY)
  if(HAVE_GCC_VISIBILITY)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fvisibility=hidden")
    add_definitions(-DHAVE_GCC_VISIBILITY)
  endif(HAVE_GCC_VISIBILITY)

  # some systems need libm for the math functions to work
  check_library_exists(m pow "" HAVE_LIBM)
  if(HAVE_LIBM)
    list(APPEND CMAKE_REQUIRED_LIBRARIES m)
    list(APPEND FFTS_EXTRA_LIBRARIES m)
  endif(HAVE_LIBM)

  if(HAVE_PMMINTRIN_H)
    add_definitions(-msse3)
  elseif(HAVE_EMMINTRIN_H)
    add_definitions(-msse2)
  elseif(HAVE_XMMINTRIN_H)
    add_definitions(-msse)
  endif(HAVE_PMMINTRIN_H)
endif(MSVC)

include_directories(include)
include_directories(src)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(FFTS_HEADERS
  include/ffts.h
)

set(FFTS_SOURCES
  src/ffts_attributes.h
  src/ffts.c
  src/ffts_chirp_z.c
  src/ffts_chirp_z.h
  src/ffts_internal.h
  src/ffts_nd.c
  src/ffts_nd.h
  src/ffts_real.h
  src/ffts_real.c
  src/ffts_real_nd.c
  src/ffts_real_nd.h
  src/ffts_transpose.c
  src/ffts_transpose.h
  src/ffts_trig.c
  src/ffts_trig.h
  src/ffts_static.c
  src/ffts_static.h
  src/macros.h
  src/patterns.h
  src/types.h
)

if(ENABLE_NEON)
  list(APPEND FFTS_SOURCES
    src/neon.s
  )

  if(DISABLE_DYNAMIC_CODE)
    list(APPEND FFTS_SOURCES
      src/neon_static.s
    )
  endif(DISABLE_DYNAMIC_CODE)

  add_definitions(-DHAVE_NEON)
elseif(ENABLE_VFP)
  if(NOT DISABLE_DYNAMIC_CODE)
    list(APPEND FFTS_SOURCES
      src/vfp.s
    )
  endif(NOT DISABLE_DYNAMIC_CODE)

  add_definitions(-DHAVE_VFP)
elseif(HAVE_XMMINTRIN_H)
  add_definitions(-DHAVE_SSE)

  list(APPEND FFTS_SOURCES
    src/macros-sse.h
  )

  if(NOT DISABLE_DYNAMIC_CODE)
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
      list(APPEND FFTS_SOURCES
        src/codegen_sse.h
      )
    else()
      message(WARNING "Dynamic code is only supported with x64, disabling dynamic code.")
      set(DISABLE_DYNAMIC_CODE ON)
    endif(CMAKE_SIZEOF_VOID_P EQUAL 8)
  endif(NOT DISABLE_DYNAMIC_CODE)
endif(ENABLE_NEON)

if(DISABLE_DYNAMIC_CODE)
  add_definitions(-DDYNAMIC_DISABLED)
else()
  list(APPEND FFTS_SOURCES
    src/codegen.c
    src/codegen.h
  )
endif(DISABLE_DYNAMIC_CODE)

if(GENERATE_POSITION_INDEPENDENT_CODE)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif(GENERATE_POSITION_INDEPENDENT_CODE)

if(ENABLE_SHARED)
  add_library(ffts_shared SHARED
    ${FFTS_HEADERS}
    ${FFTS_SOURCES}
  )

  # On unix-like platforms the library is called "libffts.so" and on Windows "ffts.dll"
  set_target_properties(ffts_shared PROPERTIES
    DEFINE_SYMBOL FFTS_SHARED
    OUTPUT_NAME ffts
    VERSION ${FFTS_MAJOR}.${FFTS_MINOR}.${FFTS_MICRO}
  )

  install( TARGETS ffts_shared DESTINATION ${LIB_INSTALL_DIR} )
endif(ENABLE_SHARED)

if(ENABLE_STATIC)
  add_library(ffts_static STATIC
    ${FFTS_HEADERS}
    ${FFTS_SOURCES}
  )

  if(UNIX)
    # On unix-like platforms the library is called "libffts.a"
    set_target_properties(ffts_static PROPERTIES OUTPUT_NAME ffts)
  endif(UNIX)

  install( TARGETS ffts_static DESTINATION ${LIB_INSTALL_DIR} )
endif(ENABLE_STATIC)

if(ENABLE_STATIC OR ENABLE_SHARED)
  add_executable(ffts_test
    tests/test.c
  )

  # link with static library by default
  if(ENABLE_STATIC)
    add_library(ffts ALIAS ffts_static)
  else()
    add_library(ffts ALIAS ffts_shared)
  endif(ENABLE_STATIC)

  target_link_libraries(ffts_test
    ffts
    ${FFTS_EXTRA_LIBRARIES}
  )
endif(ENABLE_STATIC OR ENABLE_SHARED)

# generate packageconfig file
if(UNIX)
  include(FindPkgConfig QUIET)
  if(PKG_CONFIG_FOUND)
      # convert lists of link libraries into -lstdc++ -lm etc..
      foreach(LIB ${CMAKE_CXX_IMPLICIT_LINK_LIBRARIES} ${PLATFORM_LIBS})
          set(PRIVATE_LIBS "${PRIVATE_LIBS} -l${LIB}")
      endforeach()
      # Produce a pkg-config file for linking against the shared lib
      configure_file("ffts.pc.cmake.in" "ffts.pc" @ONLY)
      install(FILES       "${CMAKE_CURRENT_BINARY_DIR}/ffts.pc"
              DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig")
  endif(PKG_CONFIG_FOUND)
endif(UNIX)

install( FILES
    ${FFTS_HEADERS}
  DESTINATION ${INCLUDE_INSTALL_DIR} )
