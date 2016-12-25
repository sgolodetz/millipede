##################
# LinkGDCM.cmake #
##################

TARGET_LINK_LIBRARIES(${targetname} ${GDCM_LIBRARY} ${GDCM_JPEG8_LIBRARY} ${GDCM_JPEG12_LIBRARY} ${GDCM_JPEG16_LIBRARY})

IF(MSVC_IDE)
  TARGET_LINK_LIBRARIES(${targetname} snmpapi)
ENDIF()
