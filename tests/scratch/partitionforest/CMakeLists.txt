##############################################
# CMakeLists.txt for scratch/partitionforest #
##############################################

###########################
# Specify the target name #
###########################

SET(targetname scratchtest_partitionforest)

################################
# Specify the libraries to use #
################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseBoost.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseITK.cmake)

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

INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/modules/millipede/include)
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/modules/srgutil/include)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetScratchTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} millipede srgutil)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkBoost.cmake)

#########################################
# Copy resource files to the build tree #
#########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/CopyResources.cmake)

#############################
# Specify things to install #
#############################

#INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
#INSTALL(FILES ${resources} DESTINATION bin/tests/${targetname}/resources)
