#######################
# SetTestTarget.cmake #
#######################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/tests/${targetname}/bin)
ADD_EXECUTABLE(${targetname} ${sources} ${headers})
INCLUDE(${millipede_SOURCE_DIR}/VCTargetHack.cmake)
