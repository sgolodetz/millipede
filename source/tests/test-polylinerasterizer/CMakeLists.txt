# CMakeLists.txt for tests/test-polylinerasterizer

############################
# Specify the project name #
############################

SET(targetname test-polylinerasterizer)

#############################
# Specify the project files #
#############################

SET(sources main.cpp)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(.cpp FILES ${sources})

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)

#####################################
# Specify additional compiler flags #
#####################################

INCLUDE(${millipede_SOURCE_DIR}/BoostTestCompilerFlags.cmake)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${millipede_SOURCE_DIR}/SetTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} common)

###############################
# Specify the post-build step #
###############################

INCLUDE(${millipede_SOURCE_DIR}/BoostTestPostBuild.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
