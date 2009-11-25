# CMakeLists.txt for tests/test-disjointsetforest

############################
# Specify the project name #
############################

SET(targetname test-disjointsetforest)

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

INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}
)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${millipede_SOURCE_DIR}/SetTestTarget.cmake)

###########################################
# Specify the necessary libraries to link #
###########################################

TARGET_LINK_LIBRARIES(${targetname} common)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
