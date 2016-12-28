###################################
# CMakeLists.txt for scratch/gdcm #
###################################

###########################
# Specify the target name #
###########################

SET(targetname scratchtest_gdcm)

################################
# Specify the libraries to use #
################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseGDCM.cmake)

#############################
# Specify the project files #
#############################

SET(sources main.cpp)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(sources FILES ${sources})

##########################################
# Specify additional include directories #
##########################################

# None needed

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetScratchTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkGDCM.cmake)

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
