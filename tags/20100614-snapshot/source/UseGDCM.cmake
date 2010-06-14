#################
# UseGDCM.cmake #
#################

FIND_PACKAGE(GDCM REQUIRED)
IF(GDCM_FOUND)
	INCLUDE(${GDCM_USE_FILE})
ELSE(GDCM_FOUND)
	MESSAGE(FATAL_ERROR "GDCM not found. Please set GDCM_DIR.")
ENDIF(GDCM_FOUND)
