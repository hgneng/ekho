#######################################
# CMAKE libmusicxml2
#######################################
project(libmusicxml2)
cmake_minimum_required(VERSION 2.4)
if(CMAKE_CONFIGURATION_TYPES)
	set(CMAKE_CONFIGURATION_TYPES Debug Release)
endif()

#######################################
# versions management
set (VERSION 3.00)
set (STRVERS "v.3.00")
set (SSTRVERS "3.00")

if(UNIX)
	add_definitions(-Wall -DGCC)
endif(UNIX)

set (libtype SHARED)
if(APPLE)
	if ( IOS STREQUAL "yes" )
		message (STATUS "Generates project for iOS - Use -DIOS=no to change.")
		set (CMAKE_XCODE_EFFECTIVE_PLATFORMS "iPhoneOS")
		set (CMAKE_OSX_ARCHITECTURES "armv6 armv7")
		set (CMAKE_OSX_SYSROOT "iphoneos5.1")
		set (libtype STATIC)
	else ()
		set (CMAKE_OSX_ARCHITECTURES "x86_64 i386")
		if (${CMAKE_SYSTEM_VERSION} VERSION_LESS 9.0.0)
			set (CMAKE_OSX_SYSROOT /Developer/SDKs/MacOSX10.4u.sdk)
		elseif (${CMAKE_SYSTEM_VERSION} VERSION_LESS 10.8.0)
			set (CMAKE_OSX_SYSROOT /Developer/SDKs/MacOSX10.5.sdk)
		elseif (${XCODE_VERSION} VERSION_LESS 4.0.0)
			set (CMAKE_OSX_SYSROOT /Developer/SDKs/MacOSX10.6.sdk)
		else ()
			set (CMAKE_OSX_SYSROOT "macosx10.7")
		endif ()
		set (CMAKE_C++_FLAGS -mmacosx-version-min=10.4)
	endif()
endif(APPLE)

if(WIN32)
 add_definitions(-DWINVER=0x0400 -DWIN32)
 if(${CMAKE_GENERATOR} STREQUAL "Visual Studio 6")
  add_definitions(-DVC6)
 elseif(${CMAKE_GENERATOR} STREQUAL "Visual Studio 8 2005")
  add_definitions(-DVC2005)
 endif(${CMAKE_GENERATOR} STREQUAL "Visual Studio 6")
endif(WIN32)


#######################################
# set directories, src and headers.
set (LXML 		${CMAKE_CURRENT_SOURCE_DIR}/..)
set (LXMLSRC 	${LXML}/src)
set (LXMLSAMPLE ${LXML}/samples)
set (SRCFOLDERS  factory files interface elements guido lib operations parser visitors)

foreach(folder ${SRCFOLDERS})
	set(SRC ${SRC} "${LXMLSRC}/${folder}/*.cpp")			# add source files
endforeach(folder)
file (GLOB CORESRC RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${SRC})

foreach(folder ${SRCFOLDERS})
	set(HEADERS ${HEADERS} "${LXMLSRC}/${folder}/*.h")		# add header files
endforeach(folder)
file (GLOB COREH RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${HEADERS})

foreach(folder ${SRCFOLDERS})
	set(INCL ${INCL} "${LXMLSRC}/${folder}")				# add include folders
endforeach(folder)


#######################################
# set includes
include_directories( ${INCL})
set_source_files_properties (${COREH} PROPERTIES HEADER_FILE_ONLY TRUE)


#######################################
# set library target
if(APPLE)
	if (IOS STREQUAL "yes")
		set(target musicxml2)
	else()
		set(target libmusicxml2)
	endif()
elseif(MSVC)
	set(target libmusicxml2)
else()
	set(target musicxml2)
endif()

if(MSVC)
	enable_language(RC)
	set(LIBCONTENT ${CORESRC} ${COREH} ${LXML}/win32/libmusicxml/libmusicxml.rc)
else()
	set(LIBCONTENT ${CORESRC} ${COREH})
endif()

add_library(${target} ${libtype} ${LIBCONTENT})
set_target_properties (${target} PROPERTIES 
			VERSION ${VERSION}
			FRAMEWORK_VERSION ${VERSION}
			PUBLIC_HEADER "${COREH}"
			DEFINE_SYMBOL LIBMUSICXML_EXPORTS
			MACOSX_FRAMEWORK_SHORT_VERSION_STRING ${STRVERS}
			MACOSX_FRAMEWORK_BUNDLE_VERSION ${SSTRVERS}
)
if ( IOS STREQUAL "no" )
	set_target_properties (${target} PROPERTIES  FRAMEWORK TRUE )
endif()

#######################################
# set sample targets
set (SAMPLES RandomMusic xmlfactory partsummary xml2guido xmliter countnotes readunrolled xml2midi xmlread xmltranspose xmlclone)
set (TOOLS  xml2guido xmltranspose)

foreach(sample ${SAMPLES})
	add_executable( ${sample} ${LXMLSAMPLE}/${sample}.cpp )
	target_link_libraries( ${sample} ${target})
	add_dependencies(${sample} ${target})
endforeach(sample)


#######################################
# install setup
if(NOT APPLE)
install ( TARGETS ${target} ${TOOLS} 
	RUNTIME DESTINATION bin 
    	LIBRARY DESTINATION lib 
    	ARCHIVE DESTINATION lib 
    	FRAMEWORK DESTINATION "/Library/Frameworks" CONFIGURATIONS Release
    	PUBLIC_HEADER DESTINATION include/libmusicxml
)
endif()
