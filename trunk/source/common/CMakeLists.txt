# CMakeLists.txt for common

# Specify the project files
SET(io_util_sources io/util/OSSWrapper.cpp)
SET(io_util_headers io/util/OSSWrapper.h)

SET(ipfs_construction_sources
ipfs/construction/Waterfall.cpp
ipfs/construction/WaterfallEdge.cpp
)

SET(ipfs_construction_headers
ipfs/construction/Waterfall.h
ipfs/construction/WaterfallEdge.h
)

SET(ipfs_construction_templates)

SET(sources
${io_util_sources}
${ipfs_construction_sources}
)

SET(headers
${io_util_headers}
${ipfs_construction_headers}
)

SET(templates
${ipfs_construction_templates}
)

# Specify the source groups
SOURCE_GROUP(io\\util\\.cpp FILES ${io_util_sources})
SOURCE_GROUP(io\\util\\.h FILES ${io_util_headers})
SOURCE_GROUP(ipfs\\construction\\.cpp FILES ${ipfs_construction_sources})
SOURCE_GROUP(ipfs\\construction\\.h FILES ${ipfs_construction_headers})
SOURCE_GROUP(ipfs\\construction\\.tpp FILES ${ipfs_construction_templates})

# Specify the necessary paths
INCLUDE_DIRECTORIES(
${millipede_SOURCE_DIR}/../libraries/boost_1_39_0
)

# Specify the target and where to put it
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/lib/common)
ADD_LIBRARY(common ${sources} ${headers} ${templates})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
    SET_TARGET_PROPERTIES(common PROPERTIES PREFIX "../")
    SET_TARGET_PROPERTIES(common PROPERTIES IMPORT_PREFIX "../")
ENDIF(MSVC_IDE)
