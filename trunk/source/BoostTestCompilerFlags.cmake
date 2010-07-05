################################
# BoostTestCompilerFlags.cmake #
################################

IF(MSVC_IDE)
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHa")
ENDIF(MSVC_IDE)
