# CMakeLists.txt for tests/test-ITK-3.14.0

#############################
# Specify the project files #
#############################

SET(sources
main.cpp
)

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

############
# Find ITK #
############

FIND_PACKAGE(ITK)
IF(ITK_FOUND)
	INCLUDE(${ITK_USE_FILE})
ELSE(ITK_FOUND)
	MESSAGE(FATAL_ERROR "ITK not found. Please set ITK_DIR.")
ENDIF(ITK_FOUND)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
)

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/tests/test-ITK-3.14.0/bin)
ADD_EXECUTABLE(test-ITK-3.14.0 ${sources})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(test-ITK-3.14.0 PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(test-ITK-3.14.0 PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)

###########################################
# Specify the necessary libraries to link #
###########################################

TARGET_LINK_LIBRARIES(test-ITK-3.14.0 ITKAlgorithms ITKCommon ITKIO)

##########################
# Specify resource files #
##########################

SET(resources
test.bmp
)

#########################################
# Copy resource files to the build tree #
#########################################

FOREACH(resource ${resources})
	CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${resource} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../resources/${resource} COPYONLY)
ENDFOREACH(resource)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS test-ITK-3.14.0 DESTINATION bin/tests/test-ITK-3.14.0/bin)
INSTALL(FILES ${resources} DESTINATION bin/tests/test-ITK-3.14.0/resources)
