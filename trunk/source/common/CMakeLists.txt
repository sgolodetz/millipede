# CMakeLists.txt for common

##########################################
# Specify the project files by directory #
##########################################

##
SET(adts_headers adts/DisjointSetForest.h)
SET(adts_templates adts/DisjointSetForest.tpp)

##
SET(dicom_util_sources dicom/util/WindowSettings.cpp)
SET(dicom_util_headers dicom/util/WindowSettings.h)

##
SET(exceptions_headers exceptions/Exception.h)

##
SET(io_files_sources io/files/VolumeChoiceFile.cpp)
SET(io_files_headers io/files/VolumeChoiceFile.h)

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

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${dicom_util_sources}
${io_files_sources}
${io_sections_sources}
${io_util_sources}
${ipfs_construction_sources}
)

SET(headers
${adts_headers}
${dicom_util_headers}
${exceptions_headers}
${io_files_headers}
${io_sections_headers}
${io_util_headers}
${ipfs_construction_headers}
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
SOURCE_GROUP(dicom\\util\\.cpp FILES ${dicom_util_sources})
SOURCE_GROUP(dicom\\util\\.h FILES ${dicom_util_headers})

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

###############################
# Specify the necessary paths #
###############################

INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}
${millipede_SOURCE_DIR}/../libraries/boost_1_39_0
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
