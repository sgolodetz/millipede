######################
# UseWxWidgets.cmake #
######################

SET(wxWidgets_DIR "${PROJECT_SOURCE_DIR}/libraries/wxWidgets-3.1.0" CACHE PATH "")
SET(wxWidgets_ROOT_DIR ${wxWidgets_DIR})
SET(WX_ROOT_DIR ${wxWidgets_DIR})

SET(wxWidgets_LIB_DIR "${PROJECT_SOURCE_DIR}/libraries/wxWidgets-3.1.0/lib/vc_lib" CACHE PATH "")
SET(WX_LIB_DIR ${wxWidgets_LIB_DIR})

FIND_PACKAGE(wxWidgets REQUIRED COMPONENTS adv gl html core base)

IF(wxWidgets_FOUND)
	INCLUDE(${wxWidgets_USE_FILE})
ELSE(wxWidgets_FOUND)
	MESSAGE(FATAL_ERROR "wxWidgets not found.")
ENDIF(wxWidgets_FOUND)
