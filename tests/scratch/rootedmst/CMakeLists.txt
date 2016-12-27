########################################
# CMakeLists.txt for scratch/rootedmst #
########################################

############################
# Specify the target name #
############################

SET(targetname scratchtest_rootedmst)

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/cmake/UseBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/cmake/UseITK.cmake)

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

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetScratchTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} millipede)
INCLUDE(${millipede_SOURCE_DIR}/cmake/LinkITK.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
