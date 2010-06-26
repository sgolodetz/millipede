millipede Readme
Last Updated by Stuart Golodetz on 26/06/10
-------------------------------------------

##################
BUILDING MILLIPEDE
##################

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

a) Use the CMake GUI. Set the source directory to millipede/source, and the build directory to a sibling of source called 'build' (this doesn't need to exist yet). The install directory (i.e. CMAKE_INSTALL_PREFIX) should be set to a sibling of source and build called 'install'. Then just keep changing the options and configuring until you can generate a Visual Studio solution. In particular, you may or may not want to build the tests, and you'll need to set the location of the wxWidgets configuration executable, which for the release version is at

millipede/libraries/wxWidgets-2.8.10/build28/wx-config

assuming you've built wxWidgets using the instructions given.

b) The created solution (millipede.sln) will be in the build directory. Open it, set the solution configuration to Release, and build the ALL_BUILD project.

To run any applications or test programs, just run them from the Visual Studio IDE. For projects which rely on external files in a particular location, it is necessary to set each project's working directory (for All Configurations) to "$(TargetDir)".

c) If you want a separate debug build (very useful, if you want to avoid a full rebuild every time you switch solution configurations), repeat (a) using 'build-debug' and 'install-debug' directories, setting the relevant paths to point to the debug builds of the various libraries. For example, the ITK build path would be set to millipede/libraries/ITK-3.14.0/build-debug in this instance. Then open millipede.sln in the build-debug directory, check that its solution configuration is set to Debug, and build the ALL_BUILD project as in (b).

On Linux:

a) Make a subdirectory of millipede called build (this will be a sibling of the source directory) and change to it.
b) Run "ccmake ../source" and set the options as you would for Windows above.
c) Run "make".
d) If you want a separate debug build (highly recommended), repeat (a)-(c) using 'build-debug' and 'install-debug' directories, setting CMAKE_BUILD_TYPE to Debug when asked.

To run any applications or test programs, you may want to run "make install" in the build directory, after which the relevant executables can be found at

millipede/install/bin/apps/{appname}/bin

and

millipede/install/bin/tests/{testname}/bin

respectively. For the debug version (i.e. after running "make install" in the build-debug directory), the built programs will be at the corresponding locations in the install-debug directory tree.

##############
USING BRANCHES
##############

It is desirable to keep the trunk of millipede in a stable state. For that reason, any major changes should be made in branches at any time when there is more than one person working on the project simultaneously. Branches are direct copies of the trunk and are located in:

<repository root>/branches

To make a new branch, you run (substituting in the correct repository root and desired branch name):

svn copy <repository root>/trunk <repository root>/branches/<branch name> -m "<An informative log message>"

To follow the project's convention, branches should use the naming scheme 'yyyymmdd-<description>' (without the quotes), e.g. 20100626-snapshot.

#################
BUILDING BRANCHES
#################

The simplest way to build a branch is just to check it out using e.g.:

svn co <repository root>/branches/<branch name> mpbranch

and build it as above. However, it is worth noting that the third-party libraries on each branch tend to be the same as those on the trunk (library changes are exceptionally rare), so it would be a huge waste of time (and disk space) to build them separately for each separate branch. A better way (if you were previously working on the trunk) is to check out the branch's source into a subdirectory of the existing trunk setup, e.g.

svn co <repository root>/branches/<branch name>/source millipede/source-<branchname>

You can then set up parallel build directories for the branch (named e.g. build-<branchname> and build-<branchname>-debug), pointing them to the trunk's libraries when asked. This allows you to work on multiple branches at the same time, without building the libraries multiple times. Of course, you want to be ***VERY VERY*** careful to check in your code from the source-<branchname> directory and ***NOT*** the top-level millipede directory when working on a branch. If you're worried about messing this up, rename the millipede/.svn directory to e.g. millipede/svn while working on the branch - the top-level millipede directory will then not count as a working copy until you rename it back again.

If you're starting from scratch, rather than from an existing working copy of the trunk, the simplest procedure is probably:

1) Create the top-level millipede directory and change to it.
2) Check out the libraries from the trunk using:

svn co svn://edison.comlab.ox.ac.uk/res07/stug/millipede/trunk/libraries libraries

3) Build them as described elsewhere.
4) Check out the branch's source directory as described above.
5) Build the branch as described elsewhere.
