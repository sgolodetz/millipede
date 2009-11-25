# CMakeLists.txt for tests/test-watershed

############################
# Specify the project name #
############################

SET(targetname test-watershed)

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
INCLUDE(${millipede_SOURCE_DIR}/LinkITK.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
