####################
# UseCppTest.cmake #
####################

IF(MSVC_IDE)
	SET(CPPTEST_INCLUDE_DIR ${millipede_SOURCE_DIR}/../libraries/cpptest-1.1.1/src CACHE PATH "The location of the CppTest include files")
	SET(CPPTEST_LIBRARY_DIR "${millipede_SOURCE_DIR}/../libraries/cpptest-1.1.1/win/VisualStudio.NET/cpptest" CACHE PATH "The location of the CppTest library")
ELSE(MSVC_IDE)
	SET(CPPTEST_INCLUDE_DIR ${millipede_SOURCE_DIR}/../libraries/cpptest-1.1.1/include CACHE PATH "The location of the CppTest include files")
	SET(CPPTEST_LIBRARY_DIR ${millipede_SOURCE_DIR}/../libraries/cpptest-1.1.1/lib CACHE PATH "The location of the CppTest library")
ENDIF(MSVC_IDE)

INCLUDE_DIRECTORIES(${CPPTEST_INCLUDE_DIR})
LINK_DIRECTORIES(${CPPTEST_LIBRARY_DIR})
