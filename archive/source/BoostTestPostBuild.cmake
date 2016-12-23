############################
# BoostTestPostBuild.cmake #
############################

IF(MSVC_IDE)
	ADD_CUSTOM_COMMAND(TARGET ${targetname} COMMAND ${targetname} ARGS --result-code=no --report-level=no)
ENDIF(MSVC_IDE)
