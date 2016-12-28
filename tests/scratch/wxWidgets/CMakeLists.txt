########################################
# CMakeLists.txt for scratch/wxWidgets #
########################################

############################
# Specify the target name #
############################

SET(targetname scratchtest_wxWidgets)

################################
# Specify the libraries to use #
################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseOpenGL.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseWxWidgets.cmake)

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

##########################################
# Specify additional include directories #
##########################################

# None needed

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetScratchTestTarget.cmake)

IF(MSVC_IDE)
	# Make Visual Studio use the correct subsystem when linking.
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
ENDIF(MSVC_IDE)

#################################
# Specify the libraries to link #
#################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkOpenGL.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkWxWidgets.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
