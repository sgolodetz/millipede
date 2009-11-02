# CMakeLists.txt for tests/test-boost_1_39_0

#############################
# Specify the project files #
#############################

SET(sources main.cpp)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(.cpp FILES ${sources})

##############
# Find Boost #
##############

SET(Boost_ADDITIONAL_VERSIONS "1.39" "1.39.0")
SET(BOOST_ROOT ${millipede_SOURCE_DIR}/../libraries/boost_1_39_0)
SET(Boost_USE_STATIC_LIBS ON)
FIND_PACKAGE(Boost 1.39.0 REQUIRED)
IF(Boost_FOUND)
	INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS})
ELSE(Boost_FOUND)
	MESSAGE(FATAL_ERROR "Boost not found. Please set the directories manually using the advanced view in CMake.")
ENDIF(Boost_FOUND)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
)

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/tests/test-boost_1_39_0/bin)
ADD_EXECUTABLE(test-boost_1_39_0 ${sources})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(test-boost_1_39_0 PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(test-boost_1_39_0 PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS test-boost_1_39_0 DESTINATION bin/tests/test-boost_1_39_0/bin)
