#! /bin/bash -e

# Check that msbuild is on the system path.
../require-msbuild.sh

LOG=../../build-gdcm-1.2.5.log

# Check that valid parameters have been specified.
if [ $# -ne 1 ] || ([ "$1" != "Visual Studio 11 Win64" ] && [ "$1" != "Visual Studio 12 Win64" ])
then
  echo "Usage: build-gdcm-1.2.5-win.sh {Visual Studio 11 Win64|Visual Studio 12 Win64}"
  exit 1
fi

# Build GDCM.
echo "[millipede] Building GDCM 1.2.5 for $1"

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

  echo "[millipede] ...Configuring using CMake..."
  cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=../install -DBUILD_TESTING=OFF -DGDCM_BUILD_EXAMPLES=OFF -DGDCM_BUILD_SHARED_LIBS=OFF -G "$1" .. > $LOG 2>&1

  #echo "[millipede] ...Running Debug build..."
  #cmd //c "msbuild /p:Configuration=Debug ITK.sln >> $LOG 2>&1"

  echo "[millipede] ...Running Release build..."
  #cmd //c "msbuild /p:Configuration=Release GDCM.sln >> $LOG 2>&1"
  cmd //c "msbuild /p:Configuration=Release GDCM.sln"

  cd ..
fi

echo "[millipede] ...Finished building GDCM 1.2.5."
