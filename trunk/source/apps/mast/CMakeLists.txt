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
SET(drawingtools_sources
drawingtools/BoxDrawingTool.cpp
drawingtools/LassoDrawingTool.cpp
drawingtools/LineBasedDrawingTool.cpp
drawingtools/LineLoopDrawingTool.cpp
)

SET(drawingtools_headers
drawingtools/BoxDrawingTool.h
drawingtools/DrawingTool.h
drawingtools/LassoDrawingTool.h
drawingtools/LineBasedDrawingTool.h
drawingtools/LineLoopDrawingTool.h
)

##
SET(gui_components_canvas_sources
gui/components/canvas/Canvas.cpp
)

SET(gui_components_canvas_headers
gui/components/canvas/Canvas.h
)

##
SET(gui_components_meshview_sources
gui/components/meshview/MeshCanvas.cpp
gui/components/meshview/MeshView.cpp
gui/components/meshview/SphereMeshCamera.cpp
)

SET(gui_components_meshview_headers
gui/components/meshview/MeshCamera.h
gui/components/meshview/MeshCanvas.h
gui/components/meshview/MeshView.h
gui/components/meshview/SphereMeshCamera.h
)

##
SET(gui_components_partitionview_sources
gui/components/partitionview/BaseCanvas.cpp
gui/components/partitionview/DICOMCanvas.cpp
gui/components/partitionview/PartitionCamera.cpp
gui/components/partitionview/PartitionCanvas.cpp
gui/components/partitionview/PartitionView.cpp
)

SET(gui_components_partitionview_headers
gui/components/partitionview/BaseCanvas.h
gui/components/partitionview/DICOMCanvas.h
gui/components/partitionview/NodeSplitManager.h
gui/components/partitionview/ParentSwitchManager.h
gui/components/partitionview/PartitionCamera.h
gui/components/partitionview/PartitionCanvas.h
gui/components/partitionview/PartitionView.h
)

##
SET(gui_components_selectionview_headers
gui/components/selectionview/SelectionView.h
)

##
SET(gui_dialogs_sources
gui/dialogs/DialogUtil.cpp
gui/dialogs/SegmentDICOMVolumeDialog.cpp
gui/dialogs/VisualizeIn3DDialog.cpp
gui/dialogs/VolumeChooserDialog.cpp
)

SET(gui_dialogs_headers
gui/dialogs/DialogUtil.h
gui/dialogs/FeatureVolumesDialog.h
gui/dialogs/ManageFeatureSelectionsDialog.h
gui/dialogs/SegmentDICOMVolumeDialog.h
gui/dialogs/SegmentVolumeDialog.h
gui/dialogs/ValidateFeatureSelectionDialog.h
gui/dialogs/VisualizeIn3DDialog.h
gui/dialogs/VolumeChooserDialog.h
)

##
SET(gui_overlays_sources
gui/overlays/IPFOverlayTools.cpp
gui/overlays/PartitionOverlay.cpp
gui/overlays/PartitionOverlayManager.cpp
)

SET(gui_overlays_headers
gui/overlays/HighlightNodesOverlay.h
gui/overlays/IPFOverlayTools.h
gui/overlays/IPFMultiFeatureSelectionOverlay.h
gui/overlays/IPFSelectionOverlay.h
gui/overlays/PartitionOverlay.h
gui/overlays/PartitionOverlayManager.h
)

##
SET(gui_windows_sources
gui/windows/MainWindow.cpp
gui/windows/SegmentationWindow.cpp
gui/windows/VisualizationWindow.cpp
)

SET(gui_windows_headers
gui/windows/MainWindow.h
gui/windows/SegmentationWindow.h
gui/windows/VisualizationWindow.h
)

##
SET(models_headers
models/PartitionModel.h
)

##
SET(util_sources
util/HelpController.cpp
util/StringConversion.cpp
)

SET(util_headers
util/HelpController.h
util/StringConversion.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${mast_sources}
${drawingtools_sources}
${gui_components_canvas_sources}
${gui_components_meshview_sources}
${gui_components_partitionview_sources}
${gui_dialogs_sources}
${gui_overlays_sources}
${gui_windows_sources}
${util_sources}
)

SET(headers
${mast_headers}
${drawingtools_headers}
${gui_components_canvas_headers}
${gui_components_meshview_headers}
${gui_components_partitionview_headers}
${gui_components_selectionview_headers}
${gui_dialogs_headers}
${gui_overlays_headers}
${gui_windows_headers}
${models_headers}
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
SOURCE_GROUP(drawingtools\\.cpp FILES ${drawingtools_sources})
SOURCE_GROUP(drawingtools\\.h FILES ${drawingtools_headers})

##
SOURCE_GROUP(gui\\components\\canvas\\.cpp FILES ${gui_components_canvas_sources})
SOURCE_GROUP(gui\\components\\canvas\\.h FILES ${gui_components_canvas_headers})

##
SOURCE_GROUP(gui\\components\\meshview\\.cpp FILES ${gui_components_meshview_sources})
SOURCE_GROUP(gui\\components\\meshview\\.h FILES ${gui_components_meshview_headers})

##
SOURCE_GROUP(gui\\components\\partitionview\\.cpp FILES ${gui_components_partitionview_sources})
SOURCE_GROUP(gui\\components\\partitionview\\.h FILES ${gui_components_partitionview_headers})

##
SOURCE_GROUP(gui\\components\\selectionview\\.h FILES ${gui_components_selectionview_headers})

##
SOURCE_GROUP(gui\\dialogs\\.cpp FILES ${gui_dialogs_sources})
SOURCE_GROUP(gui\\dialogs\\.h FILES ${gui_dialogs_headers})

##
SOURCE_GROUP(gui\\overlays\\.cpp FILES ${gui_overlays_sources})
SOURCE_GROUP(gui\\overlays\\.h FILES ${gui_overlays_headers})

##
SOURCE_GROUP(gui\\windows\\.cpp FILES ${gui_windows_sources})
SOURCE_GROUP(gui\\windows\\.h FILES ${gui_windows_headers})

##
SOURCE_GROUP(models\\.h FILES ${models_headers})

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
ELSE(MSVC_IDE)
	# Disable the annoying deprecation warnings - they're obscuring any real issues
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES COMPILE_FLAGS "-Wno-deprecated-declarations")
ENDIF(MSVC_IDE)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} common)
INCLUDE(${millipede_SOURCE_DIR}/LinkBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/LinkITK.cmake)
INCLUDE(${millipede_SOURCE_DIR}/LinkWxWidgets.cmake)

##########################
# Specify resource files #
##########################

SET(resources
phong.frag
phong.vert
testvolume1.vcf
testvolume2.vcf
)

#########################################
# Copy resource files to the build tree #
#########################################

FOREACH(resource ${resources})
	CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/${resource} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../resources/${resource} COPYONLY)
ENDFOREACH(resource)

#####################################
# Copy help files to the build tree #
#####################################

CONFIGURE_FILE(${millipede_SOURCE_DIR}/../help/millipede.chm ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../resources/millipede.chm COPYONLY)
CONFIGURE_FILE(${millipede_SOURCE_DIR}/../help/millipede.htb ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../resources/millipede.htb COPYONLY)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION bin/apps/${targetname}/bin)
INSTALL(FILES ${resources} DESTINATION bin/apps/${targetname}/resources)
INSTALL(FILES ${millipede_SOURCE_DIR}/../help/millipede.chm DESTINATION bin/apps/${targetname}/resources)
INSTALL(FILES ${millipede_SOURCE_DIR}/../help/millipede.htb DESTINATION bin/apps/${targetname}/resources)
