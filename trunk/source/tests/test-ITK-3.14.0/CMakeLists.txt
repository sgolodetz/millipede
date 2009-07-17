# CMakeLists.txt for tests/test-ITK-3.14.0

# Specify the project files
SET(sources
main.cpp
)

# Specify the source groups
SOURCE_GROUP(.cpp FILES ${sources})

# Specify the necessary paths
INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}/../libraries/boost_1_39_0
${millipede_BINARY_DIR}
)

# Find ITK
FIND_PACKAGE(ITK)
IF(ITK_FOUND)
	INCLUDE(${ITK_USE_FILE})
ELSE(ITK_FOUND)
	MESSAGE(FATAL_ERROR "ITK not found. Please set ITK_DIR.")
ENDIF(ITK_FOUND)

# Specify the target
ADD_EXECUTABLE(test-ITK-3.14.0 ${sources})

# Specify the necessary libraries to link
TARGET_LINK_LIBRARIES(test-ITK-3.14.0 ITKAlgorithms ITKCommon ITKIO)

# Specify things to install
INSTALL(TARGETS test-ITK-3.14.0 DESTINATION bin)
