# CMakeLists.txt for tests/test-boost_1_39_0

# Specify the project files
SET(sources
main.cpp
)

# Specify the source groups
SOURCE_GROUP(.cpp FILES ${sources})

# Specify the necessary paths
INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR}/../libraries/boost_1_39_0)

# Specify the target and where to put it
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/tests/test-boost_1_39_0/bin)
ADD_EXECUTABLE(test-boost_1_39_0 ${sources})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(test-boost_1_39_0 PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(test-boost_1_39_0 PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)

# Specify things to install
INSTALL(TARGETS test-boost_1_39_0 DESTINATION bin/tests/test-boost_1_39_0/bin)
