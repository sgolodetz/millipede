# CMakeLists.txt for tests/test-wxWidgets-2.8.10

############################
# Specify the project name #
############################

SET(targetname test-wxWidgets-2.8.10)

#############################
# Specify the project files #
#############################

SET(sources
MyApp.cpp
MyFrame.cpp
MyGLCanvas.cpp
)

SET(headers
MyApp.h
MyFrame.h
MyGLCanvas.h
)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(.cpp FILES ${sources})
SOURCE_GROUP(.h FILES ${headers})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseWxWidgets.cmake)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${millipede_SOURCE_DIR}/SetTestTarget.cmake)

IF(MSVC_IDE)
	# Make Visual Studio use the correct subsystem when linking
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
ENDIF(MSVC_IDE)

#################################
# Specify the libraries to link #
#################################

INCLUDE(${millipede_SOURCE_DIR}/LinkWxWidgets.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
