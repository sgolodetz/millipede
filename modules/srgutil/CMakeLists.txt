######################################
# CMakeLists.txt for modules/srgutil #
######################################

###########################
# Specify the target name #
###########################

SET(targetname srgutil)

################################
# Specify the libraries to use #
################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseBoost.cmake)

#############################
# Specify the project files #
#############################

##
SET(filesystem_sources
src/filesystem/PathFinder.cpp
)

SET(filesystem_headers
include/srgutil/filesystem/PathFinder.h
)

#################################################################
# Collect the project files into sources, headers and templates #
#################################################################

SET(sources
${filesystem_sources}
)

SET(headers
${filesystem_headers}
)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(filesystem FILES ${filesystem_sources} ${filesystem_headers})

##########################################
# Specify additional include directories #
##########################################

INCLUDE_DIRECTORIES(include/srgutil)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetLibTarget.cmake)

#############################
# Specify things to install #
#############################

INSTALL(TARGETS ${targetname} DESTINATION lib)
