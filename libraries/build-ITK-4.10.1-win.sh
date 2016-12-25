#! /bin/bash -e

# Check that msbuild is on the system path.
../require-msbuild.sh

LOG=../../build-ITK-4.10.1.log

# Check that valid parameters have been specified.
if [ $# -ne 1 ] || ([ "$1" != "Visual Studio 11 Win64" ] && [ "$1" != "Visual Studio 12 Win64" ])
then
  echo "Usage: build-ITK-4.10.1-win.sh {Visual Studio 11 Win64|Visual Studio 12 Win64}"
  exit 1
fi

# Build ITK.
echo "[millipede] Building ITK 4.10.1 for $1"

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
  cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=../install -DBUILD_DOXYGEN=OFF -DBUILD_EXAMPLES=OFF -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DITK_USE_KWSTYLE=OFF -G "$1" .. > $LOG 2>&1

  echo "[millipede] ...Running Debug build..."
  cmd //c "msbuild /p:Configuration=Debug ITK.sln >> $LOG 2>&1"

  echo "[millipede] ...Running Release build..."
  cmd //c "msbuild /p:Configuration=Release ITK.sln >> $LOG 2>&1"

  cd ..
fi

echo "[millipede] ...Finished building ITK 4.10.1."
