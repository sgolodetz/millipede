# CMakeLists.txt for plugins

##########################################
# Specify the project files by directory #
##########################################

SET(millipede_SOURCE_DIR ../../../)
SET(millipede_BINARY_DIR ../../../../build)

##
SET(plugin_sources
PluginMultiFeatureIdentifier3D.cpp
)

SET(plugin_headers
PluginMultiFeatureIdentifier3D.h
PluginWrapper.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${plugin_sources}
)

SET(headers
${plugin_headers}
)

SET(templates
)

#############################
# Specify the source groups #
#############################

##
SOURCE_GROUP(adts\\.cpp FILES ${plugin_sources})
SOURCE_GROUP(adts\\.h FILES ${plugin_headers})


################################
# Specify the libraries to use #
################################

TARGET_LINK_LIBRARIES(${targetname} plugins)
INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseGDCM.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseITK.cmake)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR})

####################################
# Specify preprocessor definitions #
####################################

IF(MSVC_IDE)
	# Prevent the deprecation warnings that VC++ arbitrarily decides to produce for C Standard Library functions
	ADD_DEFINITIONS (/D _CRT_SECURE_NO_WARNINGS)
ENDIF(MSVC_IDE)

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/lib/plugins)
ADD_LIBRARY(plugins ${sources} ${headers} ${templates})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(plugins PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(plugins PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)
