# CMakeLists.txt for tests/test-partitionforest

############################
# Specify the project name #
############################

SET(targetname test-partitionforest)

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

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR})

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${millipede_SOURCE_DIR}/SetTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} common)

##########################
# Specify resource files #
##########################

SET(resources build-graphs.bat)

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
