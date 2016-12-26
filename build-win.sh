#! /bin/bash -e

# Check that valid parameters have been specified.
if [ $# -ne 2 ] || ([ "$1" != "11" ] && [ "$1" != "12" ]) || ([ "$2" != "Debug" ] && [ "$2" != "Release" ])
then
  echo "Usage: build-win.sh {11|12} {Debug|Release}"
  exit
fi

# Check that msbuild is on the system path.
./require-msbuild.sh

cd libraries
./build-boost_1_56_0-win.sh "msvc-$1.0"
./build-gdcm-1.2.5-win.sh "Visual Studio $1 Win64"
./build-ITK-4.10.1-win.sh "Visual Studio $1 Win64"
./build-wxWidgets-3.1.0-win.sh
cd ..

echo "[millipede] Building millipede"

if [ ! -d build ]
then
  mkdir build
  cd build

  # Note: We need to configure twice to handle conditional building.
  echo "[millipede] ...Configuring using CMake..."
  cmake -G "Visual Studio $1 Win64" ..
  cmake ..

  cd ..
fi

cd build

echo "[millipede] ...Running build..."
cmd //c "msbuild /p:Configuration=$2 millipede.sln"

echo "[millipede] ...Installing..."
cmd //c "msbuild /p:Configuration=$2 INSTALL.vcxproj"

echo "[millipede] ...Finished building millipede."
