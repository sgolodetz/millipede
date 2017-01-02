########################################
# CMakeLists.txt for modules/millipede #
########################################

###########################
# Specify the target name #
###########################

SET(targetname millipede)

################################
# Specify the libraries to use #
################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseBoost.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseGDCM.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseITK.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseOpenMP.cmake)

#############################
# Specify the project files #
#############################

##
SET(adts_sources
src/adts/Edge.cpp
)

SET(adts_headers
include/millipede/adts/AdjacencyGraph.h
include/millipede/adts/DisjointSetForest.h
include/millipede/adts/Edge.h
include/millipede/adts/Map.h
include/millipede/adts/PriorityQueue.h
include/millipede/adts/RootedMST.h
include/millipede/adts/WeightedEdge.h
)

##
SET(commands_sources
src/commands/BasicCommandManager.cpp
src/commands/Command.cpp
src/commands/CommandSequenceGuard.cpp
src/commands/ICommandManager.cpp
src/commands/SequenceCommand.cpp
src/commands/UndoableCommandManager.cpp
)

SET(commands_headers
include/millipede/commands/BasicCommandManager.h
include/millipede/commands/Command.h
include/millipede/commands/CommandSequenceGuard.h
include/millipede/commands/ICommandManager.h
include/millipede/commands/ListenerAlertingCommandSequenceGuard.h
include/millipede/commands/SequenceCommand.h
include/millipede/commands/UndoableCommandManager.h
)

##
SET(dicom_directories_sources
src/dicom/directories/DICOMDirectory.cpp
src/dicom/directories/PatientRecord.cpp
src/dicom/directories/SeriesRecord.cpp
src/dicom/directories/StudyRecord.cpp
)

##
SET(dicom_directories_headers
include/millipede/dicom/directories/DICOMDirectory.h
include/millipede/dicom/directories/PatientRecord.h
include/millipede/dicom/directories/SeriesRecord.h
include/millipede/dicom/directories/StudyRecord.h
)

##
SET(dicom_util_sources
src/dicom/util/WindowSettings.cpp
)

SET(dicom_util_headers
include/millipede/dicom/util/WindowSettings.h
)

##
SET(dicom_volumes_sources
src/dicom/volumes/DICOMVolume.cpp
src/dicom/volumes/DICOMVolumeChoice.cpp
src/dicom/volumes/DICOMVolumeLoader.cpp
)

SET(dicom_volumes_headers
include/millipede/dicom/volumes/DICOMVolume.h
include/millipede/dicom/volumes/DICOMVolumeChoice.h
include/millipede/dicom/volumes/DICOMVolumeLoader.h
)

##
SET(exceptions_headers
include/millipede/exceptions/Exception.h
include/millipede/exceptions/FileNotFoundException.h
)

##
SET(featureid_sources
src/featureid/AortaIdentifier3D.cpp
src/featureid/FeatureIdentifier.cpp
src/featureid/KidneysIdentifier3D.cpp
src/featureid/LiverIdentifier3D.cpp
src/featureid/MultiFeatureIdentifier3D.cpp
src/featureid/RibsIdentifier3D.cpp
src/featureid/SpinalCordIdentifier3D.cpp
src/featureid/SpineIdentifier3D.cpp
src/featureid/SpleenIdentifier3D.cpp
)

SET(featureid_headers
include/millipede/featureid/AortaIdentifier3D.h
include/millipede/featureid/FeatureIdentifier.h
include/millipede/featureid/KidneysIdentifier3D.h
include/millipede/featureid/LiverIdentifier3D.h
include/millipede/featureid/MultiFeatureIdentifier3D.h
include/millipede/featureid/RibsIdentifier3D.h
include/millipede/featureid/SpinalCordIdentifier3D.h
include/millipede/featureid/SpineIdentifier3D.h
include/millipede/featureid/SpleenIdentifier3D.h
)

##
SET(graphics_sources
src/graphics/PolylineRasterizer.cpp
)

SET(graphics_headers
include/millipede/graphics/PolylineRasterizer.h
)

##
SET(io_files_sources
src/io/files/DataTableFile.cpp
src/io/files/DICOMDIRFile.cpp
src/io/files/VolumeChoiceFile.cpp
)

SET(io_files_headers
include/millipede/io/files/DataTableFile.h
include/millipede/io/files/DICOMDIRFile.h
include/millipede/io/files/VolumeChoiceFile.h
)

##
SET(io_sections_sources
src/io/sections/VolumeChoiceSection.cpp
)

SET(io_sections_headers
include/millipede/io/sections/VolumeChoiceSection.h
)

##
SET(io_util_sources
src/io/util/FieldIO.cpp
src/io/util/LineIO.cpp
src/io/util/OSSWrapper.cpp
)

SET(io_util_headers
include/millipede/io/util/FieldIO.h
include/millipede/io/util/LineIO.h
include/millipede/io/util/OSSWrapper.h
)

SET(io_util_templates
include/millipede/io/util/FieldIO.tpp
)

##
SET(jobs_sources
src/jobs/CompositeJob.cpp
src/jobs/Job.cpp
src/jobs/MainThreadJobQueue.cpp
src/jobs/SimpleJob.cpp
)

SET(jobs_headers
include/millipede/jobs/CompositeJob.h
include/millipede/jobs/DataHook.h
include/millipede/jobs/Job.h
include/millipede/jobs/MainThreadJobQueue.h
include/millipede/jobs/SimpleJob.h
)

##
SET(listeners_headers
include/millipede/listeners/CompositeListenerBase.h
)

##
SET(math_sources
src/math/MathConstants.cpp
src/math/Plane.cpp
)

SET(math_headers
include/millipede/math/MathConstants.h
include/millipede/math/NumericUtil.h
include/millipede/math/Plane.h
include/millipede/math/Vector2.h
include/millipede/math/Vector3.h
)

##
SET(ogl_c_sources
src/ogl/GLee.c
)

SET(ogl_headers
include/millipede/ogl/GLee.h
include/millipede/ogl/WrappedGL.h
)

##
SET(partitionforests_base_sources
src/partitionforests/base/PFNodeID.cpp
)

SET(partitionforests_base_headers
include/millipede/partitionforests/base/FeatureUtil.h
include/millipede/partitionforests/base/IForestLayer.h
include/millipede/partitionforests/base/PartitionForest.h
include/millipede/partitionforests/base/PartitionForestMFSManager.h
include/millipede/partitionforests/base/PartitionForestMultiFeatureSelection.h
include/millipede/partitionforests/base/PartitionForestSelection.h
include/millipede/partitionforests/base/PartitionForestTouchListener.h
include/millipede/partitionforests/base/PFNodeID.h
)

##
SET(partitionforests_graphviz_headers
include/millipede/partitionforests/graphviz/PartitionForestGraphvizOutputter.h
)

##
SET(partitionforests_images_sources
src/partitionforests/images/AbdominalFeature.cpp
src/partitionforests/images/DICOMImageLeafLayer.cpp
src/partitionforests/images/DICOMPixelProperties.cpp
src/partitionforests/images/DICOMRegionProperties.cpp
src/partitionforests/images/SimpleImageLeafLayer.cpp
src/partitionforests/images/SimplePixelProperties.cpp
src/partitionforests/images/SimpleRegionProperties.cpp
)

SET(partitionforests_images_headers
include/millipede/partitionforests/images/AbdominalFeature.h
include/millipede/partitionforests/images/DICOMImageBranchLayer.h
include/millipede/partitionforests/images/DICOMImageLeafLayer.h
include/millipede/partitionforests/images/DICOMPixelProperties.h
include/millipede/partitionforests/images/DICOMRegionProperties.h
include/millipede/partitionforests/images/ImageBranchLayer.h
include/millipede/partitionforests/images/ImageLeafLayer.h
include/millipede/partitionforests/images/LabelImageCreator.h
include/millipede/partitionforests/images/MosaicImageCreator.h
include/millipede/partitionforests/images/MosaicTextureSetUpdater.h
include/millipede/partitionforests/images/SimpleImageBranchLayer.h
include/millipede/partitionforests/images/SimpleImageLeafLayer.h
include/millipede/partitionforests/images/SimplePixelProperties.h
include/millipede/partitionforests/images/SimpleRegionProperties.h
include/millipede/partitionforests/images/VolumeIPF.h
include/millipede/partitionforests/images/VolumeIPFMultiFeatureSelection.h
include/millipede/partitionforests/images/VolumeIPFSelection.h
)

##
SET(segmentation_sources
src/segmentation/DICOMLowestLayersBuilder.cpp
src/segmentation/DICOMSegmentationOptions.cpp
src/segmentation/SubvolumeToVolumeIndexMapper.cpp
)

SET(segmentation_headers
include/millipede/segmentation/DICOMLowestLayersBuilder.h
include/millipede/segmentation/DICOMSegmentationOptions.h
include/millipede/segmentation/ForestBuildingWaterfallPassListener.h
include/millipede/segmentation/SubvolumeToVolumeIndexMapper.h
include/millipede/segmentation/VolumeIPFBuilder.h
)

##
SET(segmentation_waterfall_headers
include/millipede/segmentation/waterfall/DeepWaterfallPass.h
include/millipede/segmentation/waterfall/GolodetzWaterfallPass.h
include/millipede/segmentation/waterfall/MarcoteguiWaterfallPass.h
include/millipede/segmentation/waterfall/NichollsWaterfallPass.h
include/millipede/segmentation/waterfall/WaterfallPass.h
)

##
SET(segmentation_watershed_headers
include/millipede/segmentation/watershed/MeijsterRoerdinkWatershed.h
)

##
SET(shaders_sources
src/shaders/Shader.cpp
src/shaders/ShaderProgram.cpp
)

SET(shaders_headers
include/millipede/shaders/Shader.h
include/millipede/shaders/ShaderProgram.h
)

##
SET(slices_sources
src/slices/SliceLocation.cpp
)

SET(slices_headers
include/millipede/slices/SliceLocation.h
include/millipede/slices/SliceOrientation.h
include/millipede/slices/SliceTextureSet.h
include/millipede/slices/SliceTextureSetFiller.h
)

##
SET(textures_sources
src/textures/Greyscale8ImageTexture.cpp
src/textures/RGB24ImageTexture.cpp
src/textures/RGBA32ImageTexture.cpp
src/textures/Texture.cpp
src/textures/TextureFactory.cpp
)

SET(textures_headers
include/millipede/textures/Greyscale8ImageTexture.h
include/millipede/textures/ITKImageTexture.h
include/millipede/textures/RGB24ImageTexture.h
include/millipede/textures/RGBA32ImageTexture.h
include/millipede/textures/Texture.h
include/millipede/textures/TextureFactory.h
)

##
SET(util_sources
src/util/DataTable.cpp
src/util/GridUtil.cpp
src/util/ITKImageUtil.cpp
)

SET(util_headers
include/millipede/util/DataTable.h
include/millipede/util/EnumUtil.h
include/millipede/util/GridUtil.h
include/millipede/util/ITKImageUtil.h
include/millipede/util/NullType.h
)

##
SET(visualization_sources
src/visualization/CubeFace.cpp
src/visualization/CubeFaceDesignator.cpp
src/visualization/CubeTable.cpp
src/visualization/MeshRenderer.cpp
src/visualization/MeshRendererCreator.cpp
src/visualization/VisualizationOptions.cpp
)

SET(visualization_headers
include/millipede/visualization/CubeFace.h
include/millipede/visualization/CubeFaceDesignator.h
include/millipede/visualization/CubeFaceGenerator.h
include/millipede/visualization/CubeInternalGenerator.h
include/millipede/visualization/CubeTable.h
include/millipede/visualization/CubeTriangleGenerator.h
include/millipede/visualization/FanTriangulator.h
include/millipede/visualization/GlobalNodeTable.h
include/millipede/visualization/LaplacianSmoother.h
include/millipede/visualization/Mesh.h
include/millipede/visualization/MeshBuilder.h
include/millipede/visualization/MeshBuildingData.h
include/millipede/visualization/MeshDecimator.h
include/millipede/visualization/MeshNode.h
include/millipede/visualization/MeshNodeDecimator.h
include/millipede/visualization/MeshRenderer.h
include/millipede/visualization/MeshRendererCreator.h
include/millipede/visualization/MeshTransformer.h
include/millipede/visualization/MeshTriangle.h
include/millipede/visualization/MeshUtil.h
include/millipede/visualization/NodeLoop.h
include/millipede/visualization/SchroederTriangulator.h
include/millipede/visualization/SimpleMeshNodeDecimator.h
include/millipede/visualization/SourcedLabel.h
include/millipede/visualization/VisualizationOptions.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${adts_sources}
${commands_sources}
${dicom_directories_sources}
${dicom_util_sources}
${dicom_volumes_sources}
${featureid_sources}
${graphics_sources}
${io_files_sources}
${io_sections_sources}
${io_util_sources}
${jobs_sources}
${math_sources}
${ogl_c_sources}
${partitionforests_base_sources}
${partitionforests_images_sources}
${segmentation_sources}
${shaders_sources}
${slices_sources}
${textures_sources}
${util_sources}
${visualization_sources}
)

SET(headers
${adts_headers}
${commands_headers}
${dicom_directories_headers}
${dicom_util_headers}
${dicom_volumes_headers}
${exceptions_headers}
${featureid_headers}
${graphics_headers}
${io_files_headers}
${io_sections_headers}
${io_util_headers}
${jobs_headers}
${listeners_headers}
${math_headers}
${partitionforests_base_headers}
${partitionforests_graphviz_headers}
${partitionforests_images_headers}
${ogl_headers}
${segmentation_headers}
${segmentation_waterfall_headers}
${segmentation_watershed_headers}
${shaders_headers}
${slices_headers}
${textures_headers}
${util_headers}
${visualization_headers}
)

SET(templates
${io_util_templates}
)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(adts FILES ${adts_sources} ${adts_headers})
SOURCE_GROUP(commands FILES ${commands_sources} ${commands_headers})
SOURCE_GROUP(dicom\\directories FILES ${dicom_directories_sources} ${dicom_directories_headers})
SOURCE_GROUP(dicom\\util FILES ${dicom_util_sources} ${dicom_util_headers})
SOURCE_GROUP(dicom\\volumes FILES ${dicom_volumes_sources} ${dicom_volumes_headers})
SOURCE_GROUP(exceptions FILES ${exceptions_headers})
SOURCE_GROUP(featureid FILES ${featureid_sources} ${featureid_headers})
SOURCE_GROUP(graphics FILES ${graphics_sources} ${graphics_headers})
SOURCE_GROUP(io\\files FILES ${io_files_sources} ${io_files_headers})
SOURCE_GROUP(io\\sections FILES ${io_sections_sources} ${io_sections_headers})
SOURCE_GROUP(io\\util FILES ${io_util_sources} ${io_util_headers} ${io_util_templates})
SOURCE_GROUP(jobs FILES ${jobs_sources} ${jobs_headers})
SOURCE_GROUP(listeners FILES ${listeners_headers})
SOURCE_GROUP(math FILES ${math_sources} ${math_headers})
SOURCE_GROUP(ogl FILES ${ogl_c_sources} ${ogl_headers})
SOURCE_GROUP(partitionforests\\base FILES ${partitionforests_base_sources} ${partitionforests_base_headers})
SOURCE_GROUP(partitionforests\\graphviz FILES ${partitionforests_graphviz_headers})
SOURCE_GROUP(partitionforests\\images FILES ${partitionforests_images_sources} ${partitionforests_images_headers})
SOURCE_GROUP(segmentation FILES ${segmentation_sources} ${segmentation_headers})
SOURCE_GROUP(segmentation\\waterfall FILES ${segmentation_waterfall_headers})
SOURCE_GROUP(segmentation\\watershed FILES ${segmentation_watershed_headers})
SOURCE_GROUP(shaders FILES ${shaders_sources} ${shaders_headers})
SOURCE_GROUP(slices FILES ${slices_sources} ${slices_headers})
SOURCE_GROUP(textures FILES ${textures_sources} ${textures_headers})
SOURCE_GROUP(util FILES ${util_sources} ${util_headers})
SOURCE_GROUP(visualization FILES ${visualization_sources} ${visualization_headers})

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(include/millipede)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetLibTarget.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION lib)
