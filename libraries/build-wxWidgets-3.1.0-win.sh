#! /bin/bash -e

# Check that nmake is on the system path.
#../require-nmake.sh

LOG=../../../build-wxWidgets-3.1.0.log

# Check that valid parameters have been specified.
SCRIPT_NAME=`basename "$0"`

if [ $# -ne 1 ] || ([ "$1" != "11" ] && [ "$1" != "12" ] && [ "$1" != "14" ] && [ "$1" != "15" ])
then
  echo "Usage: $SCRIPT_NAME {11|12|14|15}"
  exit 1
fi

# Build wxWidgets.
CMAKE_GENERATOR=`../determine-cmakegenerator.sh $1`
echo "[millipede] Building wxWidgets 3.1.0 for $CMAKE_GENERATOR"

if [ -d wxWidgets-3.1.0 ]
then
  echo "[millipede] ...Skipping archive extraction (already extracted)"
else
  echo "[millipede] ...Extracting archive..."
  /bin/rm -fR tmp
  mkdir -p tmp/wxWidgets-3.1.0
  cd tmp/wxWidgets-3.1.0
  unzip ../../setup/wxWidgets-3.1.0/wxWidgets-3.1.0.zip > /dev/null 2>&1
  cd ../..
  mv tmp/wxWidgets-3.1.0 .
  rmdir tmp
fi

cd wxWidgets-3.1.0

echo "[millipede] ...Applying hacks..."
perl -ibak -pe 's/WXWIN_COMPATIBILITY_2_8 0/WXWIN_COMPATIBILITY_2_8 1/g' include/wx/msw/setup.h
perl -ibak -pe 's/wxUSE_UNICODE 1/wxUSE_UNICODE 0/g' include/wx/msw/setup.h

cd build/msw

if [ "$1" == "15" ]
then
  if [ -d /c/Program\ Files\ \(x86\)/Microsoft\ Visual\ Studio\ 14.0/VC ]
  then
    VSCMDPROMPT='"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64'
  else
    echo "Error: Cannot find Visual Studio 2015 build tools (we use these for our VS2017 build)"
    exit 1
  fi
else
  VSCMDPROMPT='"C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64'
fi

echo "[millipede] ...Running Debug build..."
echo "$VSCMDPROMPT && nmake -f makefile.vc SHARED=0 BUILD=debug MONOLITHIC=0 USE_OPENGL=1" > temp.bat
cmd //c "temp.bat > $LOG 2>&1"

echo "[millipede] ...Running Release build..."
echo "$VSCMDPROMPT && nmake -f makefile.vc SHARED=0 BUILD=release MONOLITHIC=0 USE_OPENGL=1" > temp.bat
cmd //c "temp.bat >> $LOG 2>&1"

echo "[millipede] ...Finished building wxWidgets-3.1.0."
