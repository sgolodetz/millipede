# CMakeLists.txt for tests/test-wxWidgets-2.8.10

# Specify the project name
SET(targetname test-wxWidgets-2.8.10)

# Specify the project files
SET(sources
MyApp.cpp
MyFrame.cpp
MyGLCanvas.cpp
)

SET(headers
MyApp.h
MyFrame.h
MyGLCanvas.h
)

# Specify the source groups
SOURCE_GROUP(.cpp FILES ${sources})
SOURCE_GROUP(.h FILES ${headers})

# Find OpenGL
FIND_PACKAGE(OpenGL)

# Find wxWidgets
FIND_PACKAGE(wxWidgets REQUIRED COMPONENTS gl core base)
IF(wxWidgets_FOUND)
	INCLUDE(${wxWidgets_USE_FILE})
ELSE(wxWidgets_FOUND)
	MESSAGE(FATAL_ERROR "wxWidgets not found.")
ENDIF(wxWidgets_FOUND)

# Specify the necessary paths
INCLUDE_DIRECTORIES(
)

# Specify the target and where to put it
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${millipede_BINARY_DIR}/bin/tests/${targetname}/bin)
ADD_EXECUTABLE(${targetname} ${sources} ${headers})
IF(MSVC_IDE)
	# A hack to get around the "Debug" and "Release" directories Visual Studio tries to add
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES PREFIX "../")
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES IMPORT_PREFIX "../")

	# Make Visual Studio use the correct subsystem when linking
	SET_TARGET_PROPERTIES(${targetname} PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")
ENDIF(MSVC_IDE)

# Specify the necessary libraries to link
TARGET_LINK_LIBRARIES(${targetname} ${wxWidgets_LIBRARIES} ${OPENGL_LIBRARY})

# Specify things to install
INSTALL(TARGETS ${targetname} DESTINATION bin/tests/${targetname}/bin)
