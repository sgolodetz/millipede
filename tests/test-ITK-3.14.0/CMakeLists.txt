# CMakeLists.txt for tests/test-ITK-3.14.0

############################
# Specify the project name #
############################

SET(targetname test-ITK-3.14.0)

#############################
# Specify the project files #
#############################

SET(sources main.cpp)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(.cpp FILES ${sources})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseITK.cmake)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${millipede_SOURCE_DIR}/SetTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

INCLUDE(${millipede_SOURCE_DIR}/LinkITK.cmake)

##########################
# Specify resource files #
##########################

SET(resources test.bmp)

#########################################
# Copy resource files to the build tree #
#########################################

FOREACH(resource ${resources})
	CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${resource} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../resources/${resource} COPYONLY)
ENDFOREACH(resource)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
INSTALL(FILES ${resources} DESTINATION bin/tests/${targetname}/resources)
