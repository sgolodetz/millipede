#! /bin/bash -e

# Check that valid parameters have been specified.
if [ $# -ne 2 ] || ([ "$1" != "11" ] && [ "$1" != "12" ] && [ "$1" != "15" ]) || ([ "$2" != "Debug" ] && [ "$2" != "Release" ])
then
  echo "Usage: build-win.sh {11|12|15} {Debug|Release}"
  exit
fi

# Check that msbuild is on the system path.
./require-msbuild.sh

# Build the third-party libraries.
cd libraries
./build-boost_1_58_0-win.sh "$1"
./build-gdcm-1.2.5-win.sh "$1"
./build-ITK-4.10.1-win.sh "$1"
#./build-wxWidgets-3.1.0-win.sh
cd ..

# Build millipede itself.
echo "[millipede] Building millipede"

if [ ! -d build ]
then
  mkdir build
  cd build

  # Note: We need to configure twice to handle conditional building.
  echo "[millipede] ...Configuring using CMake..."
  CMAKE_GENERATOR=`../determine-cmakegenerator.sh $1`
  VS_TOOLSET_STRING=`../determine-vstoolsetstring.sh $1`
  cmake -G "$CMAKE_GENERATOR" $VS_TOOLSET_STRING ..
  cmake ..

  cd ..
fi

cd build

echo "[millipede] ...Running build..."
cmd //c "msbuild /p:Configuration=$2 millipede.sln"

echo "[millipede] ...Installing..."
cmd //c "msbuild /p:Configuration=$2 INSTALL.vcxproj"

echo "[millipede] ...Finished building millipede."
