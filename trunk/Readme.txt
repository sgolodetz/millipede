millipede Readme
Last Updated by Stuart Golodetz on 13/06/10
-------------------------------------------

BUILDING MILLIPEDE

These instructions assume you have checked out the trunk to a directory which I will call 'millipede' (if you use another directory name, substitute appropriately), using a command like:

svn co <repository root>/trunk millipede

***

Step 1: Build the libraries.

millipede has dependencies on a number of external libraries, which must themselves be built before trying to build millipede. The setup files for the relevant versions of these libraries can be found under 'millipede/libraries/setup', and there are detailed instructions for how to build each library in a file called 'Instructions.txt' in each library's subdirectory.

The instructions explain how to build a library X so that it ends up in 'millipede/libraries/X'. More sophisticated users may of course want to build it elsewhere. This is generally possible, but it is unfortunately difficult to provide general instructions that will work in all cases. If you want to do something special that is not covered in the instructions, it is recommended that you contact me directly.

There is currently (as of the date shown at the top of the document) no interdependency between the different libraries used, so they can be built in any order.

Step 2: Build millipede itself.

The actual millipede project is built using CMake.

On Windows:

a) Use the CMake GUI. Set the source directory to millipede/source, and the build directory to a sibling of source called 'build' (this doesn't need to exist yet). The install directory (i.e. CMAKE_INSTALL_PREFIX) should be set to a sibling of source and build called 'install'. Then just keep changing the options and configuring until you can generate a Visual Studio solution. In particular, you may or may not want to build the tests, and you'll need to set the location of the wxWidgets configuration executable, which is at

millipede/libraries/wxWidgets-2.8.10/build28d/wx-config

assuming you've built wxWidgets using the instructions given.

b) The created solution (millipede.sln) will be in the build directory. Open it, and build the ALL_BUILD project.

To run any applications or test programs, just run them from the Visual Studio IDE. For projects which rely on external files in a particular location, it is necessary to set each project's working directory (for All Configurations) to "$(TargetDir)".

On Linux:

a) Make a subdirectory of millipede called build (this will be a sibling of the source directory) and change to it.
b) Run "ccmake ../source" and set the options as you would for Windows above.
c) Run "make".

To run any applications or test programs, you may want to run "make install", after which the relevant executables can be found at

millipede/install/bin/apps/{appname}/bin

and

millipede/install/bin/tests/{testname}/bin

respectively.