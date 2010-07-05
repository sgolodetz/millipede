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

INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)

#####################################
# Specify additional compiler flags #
#####################################

IF(MSVC_IDE)
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHa")
ENDIF(MSVC_IDE)

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

IF(MSVC_IDE)
	ADD_CUSTOM_COMMAND(TARGET ${targetname} COMMAND ${targetname} ARGS --result-code=no --report-level=no)
ENDIF(MSVC_IDE)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
