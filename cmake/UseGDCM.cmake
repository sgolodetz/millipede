#################
# UseGDCM.cmake #
#################

FIND_PACKAGE(GDCM REQUIRED HINTS "${PROJECT_SOURCE_DIR}/libraries/gdcm-1.2.5/installed")

IF(GDCM_FOUND)
	INCLUDE(${GDCM_USE_FILE})
	INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/libraries/gdcm-1.2.5/installed/include)
ELSE(GDCM_FOUND)
	MESSAGE(FATAL_ERROR "GDCM not found. Please set GDCM_DIR.")
ENDIF(GDCM_FOUND)
