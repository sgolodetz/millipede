#! /bin/bash -e

# Check that msbuild is on the system path.
../require-msbuild.sh

LOG=../../build-ITK-4.10.1.log

# Check that valid parameters have been specified.
SCRIPT_NAME=`basename "$0"`

if [ $# -ne 1 ] || ([ "$1" != "11" ] && [ "$1" != "12" ] && [ "$1" != "14" ] && [ "$1" != "15" ])
then
  echo "Usage: $SCRIPT_NAME {11|12|14|15}"
  exit 1
fi

# Determine the CMake generator and Visual Studio toolset to use.
CMAKE_GENERATOR=`../determine-cmakegenerator.sh $1`
VS_TOOLSET_STRING=`../determine-vstoolsetstring.sh $1`

# Build ITK.
echo "[millipede] Building ITK 4.10.1 for $CMAKE_GENERATOR"

if [ -d ITK-4.10.1 ]
then
  echo "[millipede] ...Skipping archive extraction (already extracted)"
else
  echo "[millipede] ...Extracting archive..."
  /bin/rm -fR tmp
  mkdir tmp
  cd tmp
  tar xzf ../setup/ITK-4.10.1/InsightToolkit-4.10.1.tar.gz
  cd ..
  mv tmp/InsightToolkit-4.10.1 ITK-4.10.1
  rmdir tmp
fi

cd ITK-4.10.1

if [ -d build ]
then
  echo "[millipede] ...Skipping build (already built)"
else
  mkdir build
  cd build

  echo "[millipede] ...Configuring using CMake..."
  cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=../install -DBUILD_DOXYGEN=OFF -DBUILD_EXAMPLES=OFF -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DITK_USE_KWSTYLE=OFF -G "$CMAKE_GENERATOR" $VS_TOOLSET_STRING .. > $LOG 2>&1

  echo "[millipede] ...Running Debug build..."
  cmd //c "msbuild /p:Configuration=Debug ITK.sln >> $LOG 2>&1"

  echo "[millipede] ...Running Release build..."
  cmd //c "msbuild /p:Configuration=Release ITK.sln >> $LOG 2>&1"

  cd ..
fi

echo "[millipede] ...Finished building ITK 4.10.1."
