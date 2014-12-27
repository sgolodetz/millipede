# CMakeLists.txt for apps/m3c

############################
# Specify the project name #
############################

SET(targetname m3c)

##########################################
# Specify the project files by directory #
##########################################

##
SET(m3c_sources
main.cpp
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${m3c_sources}
)

#############################
# Specify the source groups #
#############################

##
SOURCE_GROUP(.cpp FILES ${m3c_sources})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseGDCM.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseITK.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseWxWidgets.cmake)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}
${millipede_SOURCE_DIR}/apps
)

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/apps/${targetname}/bin)
ADD_EXECUTABLE(${targetname} ${sources} ${headers})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES PREFIX "../")
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES IMPORT_PREFIX "../")

	# Make Visual Studio use the correct subsystem when linking, and make the program large address aware
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS /LARGEADDRESSAWARE")
ELSE(MSVC_IDE)
	# Disable the annoying deprecation warnings - they're obscuring any real issues
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES COMPILE_FLAGS "-Wno-deprecated-declarations")
ENDIF(MSVC_IDE)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} common)
INCLUDE(${millipede_SOURCE_DIR}/LinkBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/LinkITK.cmake)
INCLUDE(${millipede_SOURCE_DIR}/LinkWxWidgets.cmake)
