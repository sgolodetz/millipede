# CMakeLists.txt for apps/mast

############################
# Specify the project name #
############################

SET(targetname mast)

##########################################
# Specify the project files by directory #
##########################################

##
SET(mast_sources
Application.cpp
)

SET(mast_headers
Application.h
)

##
SET(gui_base_sources
gui/base/Canvas.cpp
)

SET(gui_base_headers
gui/base/Canvas.h
)

##
SET(gui_dialogs_sources
gui/dialogs/VolumeChooserDialog.cpp
)

SET(gui_dialogs_headers
gui/dialogs/VolumeChooserDialog.h
)

##
SET(gui_windows_main_sources
gui/windows/main/MainWindow.cpp
)

SET(gui_windows_main_headers
gui/windows/main/MainWindow.h
)

##
SET(gui_windows_partition_sources
gui/windows/partition/BaseCanvas.cpp
gui/windows/partition/PartitionCanvas.cpp
gui/windows/partition/PartitionWindow.cpp
gui/windows/partition/PartitionWindow_GUI.cpp
gui/windows/partition/StratumCanvas.cpp
gui/windows/partition/ViewedVolume.cpp
)

SET(gui_windows_partition_headers
gui/windows/partition/BaseCanvas.h
gui/windows/partition/PartitionCanvas.h
gui/windows/partition/PartitionWindow.h
gui/windows/partition/StratumCanvas.h
gui/windows/partition/ViewedVolume.h
gui/windows/partition/ViewedVolumeListener.h
)

##
SET(util_sources
util/DialogUtil.cpp
util/StringConversion.cpp
)

SET(util_headers
util/DialogUtil.h
util/StringConversion.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${mast_sources}
${gui_base_sources}
${gui_dialogs_sources}
${gui_windows_main_sources}
${gui_windows_partition_sources}
${util_sources}
)

SET(headers
${mast_headers}
${gui_base_headers}
${gui_dialogs_headers}
${gui_windows_main_headers}
${gui_windows_partition_headers}
${util_headers}
)

SET(templates
)

#############################
# Specify the source groups #
#############################

##
SOURCE_GROUP(.cpp FILES ${mast_sources})
SOURCE_GROUP(.h FILES ${mast_headers})

##
SOURCE_GROUP(gui\\base\\.cpp FILES ${gui_base_sources})
SOURCE_GROUP(gui\\base\\.h FILES ${gui_base_headers})

##
SOURCE_GROUP(gui\\dialogs\\.cpp FILES ${gui_dialogs_sources})
SOURCE_GROUP(gui\\dialogs\\.h FILES ${gui_dialogs_headers})

##
SOURCE_GROUP(gui\\windows\\main\\.cpp FILES ${gui_windows_main_sources})
SOURCE_GROUP(gui\\windows\\main\\.h FILES ${gui_windows_main_headers})

##
SOURCE_GROUP(gui\\windows\\partition\\.cpp FILES ${gui_windows_partition_sources})
SOURCE_GROUP(gui\\windows\\partition\\.h FILES ${gui_windows_partition_headers})

##
SOURCE_GROUP(util\\.cpp FILES ${util_sources})
SOURCE_GROUP(util\\.h FILES ${util_headers})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseGDCM.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseITK.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseWxWidgets.cmake)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}
${millipede_SOURCE_DIR}/apps
)

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/apps/${targetname}/bin)
ADD_EXECUTABLE(${targetname} ${sources} ${headers})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES PREFIX "../")
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES IMPORT_PREFIX "../")

	# Make Visual Studio use the correct subsystem when linking
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
ENDIF(MSVC_IDE)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} common)
INCLUDE(${millipede_SOURCE_DIR}/LinkBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/LinkITK.cmake)
INCLUDE(${millipede_SOURCE_DIR}/LinkWxWidgets.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/apps/${targetname}/bin)
