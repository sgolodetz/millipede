# CMakeLists.txt for tests/test-gdcm-1.2.5

############################
# Specify the project name #
############################

SET(targetname test-gdcm-1.2.5)

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

INCLUDE(${millipede_SOURCE_DIR}/UseGDCM.cmake)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${millipede_SOURCE_DIR}/SetTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

INCLUDE(${millipede_SOURCE_DIR}/LinkGDCM.cmake)

##########################
# Specify resource files #
##########################

SET(resources DICOMDIR)

#########################################
# Copy resource files to the build tree #
#########################################

FOREACH(resource ${resources})
	CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${resource} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../resources/${resource} COPYONLY)
ENDFOREACH(resource)

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/test-gdcm-1.2.5.sh ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-gdcm-1.2.5.sh COPYONLY)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
INSTALL(FILES ${resources} DESTINATION bin/tests/${targetname}/resources)
INSTALL(FILES test-gdcm-1.2.5.sh DESTINATION bin/tests/${targetname}/bin)
