#####################################
# CMakeLists.txt for unit/millipede #
#####################################

###############################
# Specify the test suite name #
###############################

SET(suitename millipede)

##########################
# Specify the test names #
##########################

SET(testnames
DisjointSetForest
)

FOREACH(testname ${testnames})

SET(targetname "unittest_${suitename}_${testname}")

################################
# Specify the libraries to use #
################################

#INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseArrayFire.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseBoost.cmake)
#INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseCUDA.cmake)
#INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseInfiniTAM.cmake)
#INCLUDE(${PROJECT_SOURCE_DIR}/cmake/UseOpenMP.cmake)

#############################
# Specify the project files #
#############################

SET(sources
test_${testname}.cpp
)

#############################
# Specify the source groups #
#############################

SOURCE_GROUP(sources FILES ${sources})

##########################################
# Specify additional include directories #
##########################################

INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/modules/millipede/include)

##########################################
# Specify the target and where to put it #
##########################################

INCLUDE(${PROJECT_SOURCE_DIR}/cmake/SetUnitTestTarget.cmake)

#################################
# Specify the libraries to link #
#################################

TARGET_LINK_LIBRARIES(${targetname} millipede)

#INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkArrayFire.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkBoost.cmake)
#INCLUDE(${PROJECT_SOURCE_DIR}/cmake/LinkInfiniTAM.cmake)

ENDFOREACH()
