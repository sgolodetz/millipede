# CMakeLists.txt for apps/mast

############################
# Specify the project name #
############################

SET(targetname mast)

##########################################
# Specify the project files by directory #
##########################################

##
SET(mast_sources Application.cpp)
SET(mast_headers Application.h)

##
SET(gui_dialogs_sources gui/dialogs/VolumeChooserDialog.cpp)
SET(gui_dialogs_headers gui/dialogs/VolumeChooserDialog.h)

##
SET(gui_windows_main_sources
gui/windows/main/MainWindow.cpp
gui/windows/main/MainWindow_Menus.cpp
)

SET(gui_windows_main_headers gui/windows/main/MainWindow.h)

##
SET(gui_windows_partition_sources gui/windows/partition/PartitionWindow.cpp)
SET(gui_windows_partition_headers gui/windows/partition/PartitionWindow.h)

##
SET(util_sources
util/IOUtil.cpp
util/StringConversion.cpp
)

SET(util_headers
util/IOUtil.h
util/StringConversion.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${mast_sources}
${gui_dialogs_sources}
${gui_windows_main_sources}
${gui_windows_partition_sources}
${util_sources}
)

SET(headers
${mast_headers}
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

##############
# Find Boost #
##############

SET(Boost_ADDITIONAL_VERSIONS "1.39" "1.39.0")
SET(BOOST_ROOT ${millipede_SOURCE_DIR}/../libraries/boost_1_39_0)
SET(Boost_USE_STATIC_LIBS ON)
FIND_PACKAGE(Boost 1.39.0 REQUIRED COMPONENTS thread)
IF(Boost_FOUND)
	INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS})
	LINK_DIRECTORIES(${Boost_LIBRARY_DIRS})
ELSE(Boost_FOUND)
	MESSAGE(FATAL_ERROR "Boost not found. Please set the directories manually using the advanced view in CMake.")
ENDIF(Boost_FOUND)

#############
# Find GDCM #
#############

FIND_PACKAGE(GDCM REQUIRED)
IF(GDCM_FOUND)
	INCLUDE(${GDCM_USE_FILE})
ELSE(GDCM_FOUND)
	MESSAGE(FATAL_ERROR "GDCM not found. Please set GDCM_DIR.")
ENDIF(GDCM_FOUND)

############
# Find ITK #
############

FIND_PACKAGE(ITK)
IF(ITK_FOUND)
	INCLUDE(${ITK_USE_FILE})
ELSE(ITK_FOUND)
	MESSAGE(FATAL_ERROR "ITK not found. Please set ITK_DIR.")
ENDIF(ITK_FOUND)

###############
# Find OpenGL #
###############

FIND_PACKAGE(OpenGL)

##################
# Find wxWidgets #
##################

FIND_PACKAGE(wxWidgets REQUIRED COMPONENTS gl core base)
IF(wxWidgets_FOUND)
	INCLUDE(${wxWidgets_USE_FILE})
ELSE(wxWidgets_FOUND)
	MESSAGE(FATAL_ERROR "wxWidgets not found.")
ENDIF(wxWidgets_FOUND)

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

###########################################
# Specify the necessary libraries to link #
###########################################

TARGET_LINK_LIBRARIES(${targetname} common ${Boost_LIBRARIES} gdcm ITKAlgorithms ITKCommon ITKIO ${OPENGL_LIBRARY} ${wxWidgets_LIBRARIES})

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/apps/${targetname}/bin)
