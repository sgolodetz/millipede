###################
# LinkBoost.cmake #
###################

TARGET_LINK_LIBRARIES(${targetname} ${Boost_LIBRARIES})

IF(NOT MSVC_IDE)
	TARGET_LINK_LIBRARIES(${targetname} pthread)
ENDIF(NOT MSVC_IDE)
