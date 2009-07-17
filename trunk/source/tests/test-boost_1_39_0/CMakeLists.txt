# CMakeLists.txt for tests/test-boost_1_39_0

# Specify the project files
SET(sources
main.cpp
)

# Specify the source groups
SOURCE_GROUP(.cpp FILES ${sources})

# Specify the necessary paths
INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR}/../libraries/boost_1_39_0)

# Specify the target
ADD_EXECUTABLE(test-boost_1_39_0 ${sources})
