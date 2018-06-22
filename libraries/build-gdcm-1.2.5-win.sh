#! /bin/bash -e

# Check that msbuild is on the system path.
../require-msbuild.sh

LOG=../../build-gdcm-1.2.5.log

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

# Build GDCM.
echo "[millipede] Building GDCM 1.2.5 for $CMAKE_GENERATOR"

if [ -d gdcm-1.2.5 ]
then
  echo "[millipede] ...Skipping archive extraction (already extracted)"
else
  echo "[millipede] ...Extracting archive..."
  /bin/rm -fR tmp
  mkdir tmp
  cd tmp
  tar xzf ../setup/gdcm-1.2.5/gdcm-1.2.5.tar.gz
  cd ..
  mv tmp/gdcm-1.2.5 .
  rmdir tmp
fi

cd gdcm-1.2.5

if [ ! -f HACKED ]
then
  echo "[millipede] ...Applying hacks..."
  perl -ibak -pe 's/\*fp == NULL/!fp/g' src/gdcmFile.cxx
  (find . -exec grep -l 'namespace gdcm' {} \;) 2>&1 | grep -v 'directory' | while read f; do perl -ibak -pe 's/namespace gdcm/namespace gdcm1/g' "$f"; done
  (find . -exec grep -l 'gdcm::' {} \;) 2>&1 | grep -v 'directory' | while read f; do perl -ibak -pe 's/gdcm::/gdcm1::/g' "$f"; done

  touch HACKED
else
  echo "[millipede] ...Skipping hacks (already applied)"
fi

if [ -d build ]
then
  echo "[millipede] ...Skipping build (already built)"
else
  mkdir build
  cd build

  echo "[millipede] ...Configuring for Debug configuration using CMake..."
  cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=../install-debug -DBUILD_TESTING=OFF -DGDCM_BUILD_EXAMPLES=OFF -DGDCM_BUILD_SHARED_LIBS=OFF -G "$CMAKE_GENERATOR" $VS_TOOLSET_STRING .. > $LOG 2>&1

  echo "[millipede] ...Building Debug configuration..."
  cmd //c "msbuild /p:Configuration=Debug GDCM.sln >> $LOG 2>&1"

  echo "[millipede] ...Installing Debug configuration..."
  cmd //c "msbuild /p:Configuration=Debug INSTALL.vcxproj >> $LOG 2>&1"

  echo "[millipede] ...Configuring for Release configuration using CMake..."
  cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=../install-release -DBUILD_TESTING=OFF -DGDCM_BUILD_EXAMPLES=OFF -DGDCM_BUILD_SHARED_LIBS=OFF -G "$CMAKE_GENERATOR" $VS_TOOLSET_STRING .. >> $LOG 2>&1

  echo "[millipede] ...Building Release configuration..."
  cmd //c "msbuild /p:Configuration=Release GDCM.sln >> $LOG 2>&1"

  echo "[millipede] ...Installing Release configuration..."
  cmd //c "msbuild /p:Configuration=Release INSTALL.vcxproj >> $LOG 2>&1"

  cd ..
fi

echo "[millipede] ...Finished building GDCM 1.2.5."
