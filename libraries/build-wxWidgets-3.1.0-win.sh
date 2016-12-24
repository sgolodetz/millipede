#! /bin/bash -e

# Check that nmake is on the system path.
#../require-nmake.sh

LOG=../../build-wxWidgets-3.1.0.log

# Check that valid parameters have been specified.
if [ $# -ne 1 ] || ([ "$1" != "Visual Studio 11 Win64" ] && [ "$1" != "Visual Studio 12 Win64" ])
then
  echo "Usage: build-wxWidgets-3.1.0-win.sh {Visual Studio 11 Win64|Visual Studio 12 Win64}"
  exit 1
fi

# Build wxWidgets.
echo "[millipede] Building wxWidgets 3.1.0 for $1"

if [ -d wxWidgets-3.1.0 ]
then
  echo "[millipede] ...Skipping archive extraction (already extracted)"
else
  echo "[millipede] ...Extracting archive..."
  /bin/rm -fR tmp
  mkdir tmp
  cd tmp
  unzip ../setup/wxWidgets-3.1.0/wxWidgets-3.1.0.zip > /dev/null 2>&1
  cd ..
  mv tmp/wxWidgets-3.1.0 .
  rmdir tmp
fi

exit
cd wxWidgets-3.1.0

#if [ -d build ]
#then
#  echo "[millipede] ...Skipping build (already built)"
#else
#  mkdir build
#  cd build

#  echo "[millipede] ...Configuring using CMake..."
#  cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=../install -DBUILD_DOXYGEN=OFF -DBUILD_EXAMPLES=OFF -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DITK_USE_KWSTYLE=OFF -G "$1" .. > $LOG 2>&1

  #echo "[millipede] ...Running Debug build..."
  #cmd //c "msbuild /p:Configuration=Debug ITK.sln >> $LOG 2>&1"

#  echo "[millipede] ...Running Release build..."
#  cmd //c "msbuild /p:Configuration=Release ITK.sln >> $LOG 2>&1"

#  cd ..
#fi

echo "[millipede] ...Finished building wxWidgets-3.1.0."
