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

#############
# Find GDCM #
#############

FIND_PACKAGE(GDCM REQUIRED)
IF(GDCM_FOUND)
	INCLUDE(${GDCM_USE_FILE})
ELSE(GDCM_FOUND)
	MESSAGE(FATAL_ERROR "GDCM not found. Please set GDCM_DIR.")
ENDIF(GDCM_FOUND)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
)

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/tests/${targetname}/bin)
ADD_EXECUTABLE(${targetname} ${sources})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(${targetname} PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(${targetname} PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)

###########################################
# Specify the necessary libraries to link #
###########################################

TARGET_LINK_LIBRARIES(${targetname} gdcm)

##########################
# Specify resource files #
##########################

SET(resources
DICOMDIR
)

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
