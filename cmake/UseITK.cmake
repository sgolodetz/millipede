################
# UseITK.cmake #
################

FIND_PACKAGE(ITK REQUIRED HINTS "${PROJECT_SOURCE_DIR}/libraries/ITK-4.10.1/build")

IF(ITK_FOUND)
	INCLUDE(${ITK_USE_FILE})
ELSE(ITK_FOUND)
	MESSAGE(FATAL_ERROR "ITK not found. Please set ITK_DIR.")
ENDIF(ITK_FOUND)
