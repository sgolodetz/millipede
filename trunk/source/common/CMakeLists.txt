# CMakeLists.txt for common

##########################################
# Specify the project files by directory #
##########################################

##
SET(adts_headers
adts/DisjointSetForest.h
adts/Map.h
)

SET(adts_templates adts/DisjointSetForest.tpp)

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
SET(dicom_util_sources dicom/util/WindowSettings.cpp)
SET(dicom_util_headers dicom/util/WindowSettings.h)

##
SET(dicom_volumes_sources
dicom/volumes/Volume.cpp
dicom/volumes/VolumeTextureSet.cpp
)

SET(dicom_volumes_headers
dicom/volumes/Volume.h
dicom/volumes/VolumeTextureSet.h
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
io/files/VolumeLoader.cpp
)

SET(io_files_headers
io/files/DICOMDIRFile.h
io/files/VolumeChoiceFile.h
io/files/VolumeLoader.h
)

##
SET(io_sections_sources io/sections/VolumeChoiceSection.cpp)
SET(io_sections_headers io/sections/VolumeChoiceSection.h)

##
SET(io_util_sources
io/util/FieldIO.cpp
io/util/LineIO.cpp
io/util/OSSWrapper.cpp
io/util/VolumeChoice.cpp
)

SET(io_util_headers
io/util/FieldIO.h
io/util/LineIO.h
io/util/OSSWrapper.h
io/util/VolumeChoice.h
)

SET(io_util_templates io/util/FieldIO.tpp)

##
SET(ipfs_construction_sources
ipfs/construction/Waterfall.cpp
ipfs/construction/WaterfallEdge.cpp
)

SET(ipfs_construction_headers
ipfs/construction/Waterfall.h
ipfs/construction/WaterfallEdge.h
ipfs/construction/Watershed.h
)

SET(ipfs_construction_templates ipfs/construction/Watershed.tpp)

##
SET(ogl_headers
ogl/WrappedGL.h
)

##
SET(textures_sources
textures/Greyscale8ImageTexture.cpp
textures/Texture.cpp
textures/TextureFactory.cpp
)

SET(textures_headers
textures/Greyscale8ImageTexture.h
textures/Texture.h
textures/TextureFactory.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${dicom_directories_sources}
${dicom_util_sources}
${dicom_volumes_sources}
${io_files_sources}
${io_sections_sources}
${io_util_sources}
${ipfs_construction_sources}
${textures_sources}
)

SET(headers
${adts_headers}
${dicom_directories_headers}
${dicom_util_headers}
${dicom_volumes_headers}
${exceptions_headers}
${io_files_headers}
${io_sections_headers}
${io_util_headers}
${ipfs_construction_headers}
${ogl_headers}
${textures_headers}
)

SET(templates
${adts_templates}
${io_util_templates}
${ipfs_construction_templates}
)

#############################
# Specify the source groups #
#############################

##
SOURCE_GROUP(adts\\.h FILES ${adts_headers})
SOURCE_GROUP(adts\\.tpp FILES ${adts_templates})

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
SOURCE_GROUP(ipfs\\construction\\.cpp FILES ${ipfs_construction_sources})
SOURCE_GROUP(ipfs\\construction\\.h FILES ${ipfs_construction_headers})
SOURCE_GROUP(ipfs\\construction\\.tpp FILES ${ipfs_construction_templates})

##
SOURCE_GROUP(ogl\\.h FILES ${ogl_headers})

##
SOURCE_GROUP(textures\\.cpp FILES ${textures_sources})
SOURCE_GROUP(textures\\.h FILES ${textures_headers})

##############
# Find Boost #
##############

SET(Boost_ADDITIONAL_VERSIONS "1.39" "1.39.0")
SET(BOOST_ROOT ${millipede_SOURCE_DIR}/../libraries/boost_1_39_0)
SET(Boost_USE_STATIC_LIBS ON)
FIND_PACKAGE(Boost 1.39.0 REQUIRED COMPONENTS thread)
IF(Boost_FOUND)
	INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS})
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

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}
)

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
