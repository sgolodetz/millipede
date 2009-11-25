######################
# UseWxWidgets.cmake #
######################

FIND_PACKAGE(wxWidgets REQUIRED COMPONENTS gl core base)
IF(wxWidgets_FOUND)
	INCLUDE(${wxWidgets_USE_FILE})
ELSE(wxWidgets_FOUND)
	MESSAGE(FATAL_ERROR "wxWidgets not found.")
ENDIF(wxWidgets_FOUND)

FIND_PACKAGE(OpenGL)
