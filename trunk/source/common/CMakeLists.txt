# CMakeLists.txt for common

##########################################
# Specify the project files by directory #
##########################################

##
SET(adts_headers
adts/AdjacencyGraph.h
adts/DisjointSetForest.h
adts/Edge.h
adts/Map.h
adts/PriorityQueue.h
adts/RootedMST.h
adts/WeightedEdge.h
)

##
SET(commands_sources
commands/BasicCommandManager.cpp
commands/Command.cpp
commands/CommandSequenceGuard.cpp
commands/ICommandManager.cpp
commands/SequenceCommand.cpp
commands/UndoableCommandManager.cpp
)

SET(commands_headers
commands/BasicCommandManager.h
commands/Command.h
commands/CommandSequenceGuard.h
commands/ICommandManager.h
commands/ListenerAlertingCommandSequenceGuard.h
commands/SequenceCommand.h
commands/UndoableCommandManager.h
)

##
SET(dicom_directories_sources
dicom/directories/DICOMDirectory.cpp
dicom/directories/PatientRecord.cpp
dicom/directories/SeriesRecord.cpp
dicom/directories/StudyRecord.cpp
)

##
SET(dicom_directories_headers
dicom/directories/DICOMDirectory.h
dicom/directories/PatientRecord.h
dicom/directories/SeriesRecord.h
dicom/directories/StudyRecord.h
)

##
SET(dicom_util_sources
dicom/util/WindowSettings.cpp
)

SET(dicom_util_headers
dicom/util/WindowSettings.h
)

##
SET(dicom_volumes_sources
dicom/volumes/DICOMVolume.cpp
dicom/volumes/DICOMVolumeChoice.cpp
dicom/volumes/DICOMVolumeLoader.cpp
)

SET(dicom_volumes_headers
dicom/volumes/DICOMVolume.h
dicom/volumes/DICOMVolumeChoice.h
dicom/volumes/DICOMVolumeLoader.h
)

##
SET(exceptions_headers
exceptions/Exception.h
exceptions/FileNotFoundException.h
)

##
SET(io_files_sources
io/files/DICOMDIRFile.cpp
io/files/VolumeChoiceFile.cpp
)

SET(io_files_headers
io/files/DICOMDIRFile.h
io/files/VolumeChoiceFile.h
)

##
SET(io_sections_sources
io/sections/VolumeChoiceSection.cpp
)

SET(io_sections_headers
io/sections/VolumeChoiceSection.h
)

##
SET(io_util_sources
io/util/FieldIO.cpp
io/util/LineIO.cpp
io/util/OSSWrapper.cpp
)

SET(io_util_headers
io/util/FieldIO.h
io/util/LineIO.h
io/util/OSSWrapper.h
)

SET(io_util_templates
io/util/FieldIO.tpp
)

##
SET(jobs_sources
jobs/CompositeJob.cpp
jobs/Job.cpp
jobs/MainThreadJobQueue.cpp
jobs/SimpleJob.cpp
)

SET(jobs_headers
jobs/CompositeJob.h
jobs/DataHook.h
jobs/Job.h
jobs/MainThreadJobQueue.h
jobs/SimpleJob.h
)

##
SET(listeners_headers
listeners/CompositeListenerBase.h
)

##
SET(partitionforests_base_sources
partitionforests/base/PFNodeID.cpp
)

SET(partitionforests_base_headers
partitionforests/base/IForestLayer.h
partitionforests/base/PartitionForest.h
partitionforests/base/PartitionForestMultiFeatureSelection.h
partitionforests/base/PartitionForestSelection.h
partitionforests/base/PFNodeID.h
)

##
SET(partitionforests_images_sources
partitionforests/images/AbdominalFeature.cpp
partitionforests/images/DICOMImageLeafLayer.cpp
partitionforests/images/DICOMPixelProperties.cpp
partitionforests/images/DICOMRegionProperties.cpp
partitionforests/images/SimpleImageLeafLayer.cpp
partitionforests/images/SimplePixelProperties.cpp
partitionforests/images/SimpleRegionProperties.cpp
)

SET(partitionforests_images_headers
partitionforests/images/AbdominalFeature.h
partitionforests/images/DICOMImageBranchLayer.h
partitionforests/images/DICOMImageLeafLayer.h
partitionforests/images/DICOMPixelProperties.h
partitionforests/images/DICOMRegionProperties.h
partitionforests/images/ImageBranchLayer.h
partitionforests/images/ImageLeafLayer.h
partitionforests/images/MosaicImageCreator.h
partitionforests/images/SimpleImageBranchLayer.h
partitionforests/images/SimpleImageLeafLayer.h
partitionforests/images/SimplePixelProperties.h
partitionforests/images/SimpleRegionProperties.h
partitionforests/images/VolumeIPF.h
partitionforests/images/VolumeIPFMultiFeatureSelection.h
partitionforests/images/VolumeIPFSelection.h
)

##
SET(ogl_headers
ogl/WrappedGL.h
)

##
SET(segmentation_sources
segmentation/DICOMLowestLayersBuilder.cpp
segmentation/DICOMSegmentationOptions.cpp
segmentation/SubvolumeToVolumeIndexMapper.cpp
)

SET(segmentation_headers
segmentation/DICOMLowestLayersBuilder.h
segmentation/DICOMSegmentationOptions.h
segmentation/ForestBuildingWaterfallPassListener.h
segmentation/SubvolumeToVolumeIndexMapper.h
segmentation/VolumeIPFBuilder.h
)

##
SET(segmentation_waterfall_headers
segmentation/waterfall/NichollsWaterfallPass.h
segmentation/waterfall/WaterfallPass.h
)

##
SET(segmentation_watershed_headers
segmentation/watershed/MeijsterRoerdinkWatershed.h
)

##
SET(slices_sources
slices/SliceLocation.cpp
slices/SliceTextureSet.cpp
)

SET(slices_headers
slices/SliceLocation.h
slices/SliceOrientation.h
slices/SliceTextureSet.h
slices/SliceTextureSetFiller.h
)

##
SET(textures_sources
textures/Greyscale8ImageTexture.cpp
textures/RGB24ImageTexture.cpp
textures/RGBA32ImageTexture.cpp
textures/Texture.cpp
textures/TextureFactory.cpp
)

SET(textures_headers
textures/Greyscale8ImageTexture.h
textures/ITKImageTexture.h
textures/RGB24ImageTexture.h
textures/RGBA32ImageTexture.h
textures/Texture.h
textures/TextureFactory.h
)

##
SET(util_sources
util/GridUtil.cpp
util/ITKImageUtil.cpp
)

SET(util_headers
util/EnumUtil.h
util/GridUtil.h
util/ITKImageUtil.h
util/NullType.h
util/NumericUtil.h
)

##
SET(visualization_sources
visualization/CubeFace.cpp
visualization/CubeFaceDesignator.cpp
visualization/CubeFaceTable.cpp
)

SET(visualization_headers
visualization/CubeFace.h
visualization/CubeFaceDesignator.h
visualization/CubeFaceGenerator.h
visualization/CubeFaceTable.h
visualization/MeshBuilder.h
visualization/MeshBuildingData.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${commands_sources}
${dicom_directories_sources}
${dicom_util_sources}
${dicom_volumes_sources}
${io_files_sources}
${io_sections_sources}
${io_util_sources}
${jobs_sources}
${partitionforests_base_sources}
${partitionforests_images_sources}
${segmentation_sources}
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
${io_files_headers}
${io_sections_headers}
${io_util_headers}
${jobs_headers}
${listeners_headers}
${partitionforests_base_headers}
${partitionforests_images_headers}
${ogl_headers}
${segmentation_headers}
${segmentation_waterfall_headers}
${segmentation_watershed_headers}
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

##
SOURCE_GROUP(adts\\.h FILES ${adts_headers})

##
SOURCE_GROUP(commands\\.cpp FILES ${commands_sources})
SOURCE_GROUP(commands\\.h FILES ${commands_headers})

##
SOURCE_GROUP(dicom\\directories\\.cpp FILES ${dicom_directories_sources})
SOURCE_GROUP(dicom\\directories\\.h FILES ${dicom_directories_headers})

##
SOURCE_GROUP(dicom\\util\\.cpp FILES ${dicom_util_sources})
SOURCE_GROUP(dicom\\util\\.h FILES ${dicom_util_headers})

##
SOURCE_GROUP(dicom\\volumes\\.cpp FILES ${dicom_volumes_sources})
SOURCE_GROUP(dicom\\volumes\\.h FILES ${dicom_volumes_headers})

##
SOURCE_GROUP(exceptions\\.h FILES ${exceptions_headers})

##
SOURCE_GROUP(io\\files\\.cpp FILES ${io_files_sources})
SOURCE_GROUP(io\\files\\.h FILES ${io_files_headers})

##
SOURCE_GROUP(io\\sections\\.cpp FILES ${io_sections_sources})
SOURCE_GROUP(io\\sections\\.h FILES ${io_sections_headers})

##
SOURCE_GROUP(io\\util\\.cpp FILES ${io_util_sources})
SOURCE_GROUP(io\\util\\.h FILES ${io_util_headers})
SOURCE_GROUP(io\\util\\.tpp FILES ${io_util_templates})

##
SOURCE_GROUP(jobs\\.cpp FILES ${jobs_sources})
SOURCE_GROUP(jobs\\.h FILES ${jobs_headers})

##
SOURCE_GROUP(listeners\\.h FILES ${listeners_headers})

##
SOURCE_GROUP(partitionforests\\base\\.cpp FILES ${partitionforests_base_sources})
SOURCE_GROUP(partitionforests\\base\\.h FILES ${partitionforests_base_headers})

##
SOURCE_GROUP(partitionforests\\images\\.cpp FILES ${partitionforests_images_sources})
SOURCE_GROUP(partitionforests\\images\\.h FILES ${partitionforests_images_headers})

##
SOURCE_GROUP(ogl\\.h FILES ${ogl_headers})

##
SOURCE_GROUP(segmentation\\.cpp FILES ${segmentation_sources})
SOURCE_GROUP(segmentation\\.h FILES ${segmentation_headers})

##
SOURCE_GROUP(segmentation\\waterfall\\.h FILES ${segmentation_waterfall_headers})

##
SOURCE_GROUP(segmentation\\watershed\\.h FILES ${segmentation_watershed_headers})

##
SOURCE_GROUP(slices\\.cpp FILES ${slices_sources})
SOURCE_GROUP(slices\\.h FILES ${slices_headers})

##
SOURCE_GROUP(textures\\.cpp FILES ${textures_sources})
SOURCE_GROUP(textures\\.h FILES ${textures_headers})

##
SOURCE_GROUP(util\\.cpp FILES ${util_sources})
SOURCE_GROUP(util\\.h FILES ${util_headers})

##
SOURCE_GROUP(visualization\\.cpp FILES ${visualization_sources})
SOURCE_GROUP(visualization\\.h FILES ${visualization_headers})

################################
# Specify the libraries to use #
################################

INCLUDE(${millipede_SOURCE_DIR}/UseBoost.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseGDCM.cmake)
INCLUDE(${millipede_SOURCE_DIR}/UseITK.cmake)

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(${millipede_SOURCE_DIR})

##########################################
# Specify the target and where to put it #
##########################################

SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/lib/common)
ADD_LIBRARY(common ${sources} ${headers} ${templates})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(common PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(common PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)
