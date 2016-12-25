#! /bin/bash -e

# Check that nmake is on the system path.
#../require-nmake.sh

LOG=../../../build-wxWidgets-3.1.0.log

# Check that no parameters have been specified.
if [ $# -ne 0 ]
then
  echo "Usage: build-wxWidgets-3.1.0-win.sh"
  exit 1
fi

# Build wxWidgets.
echo "[millipede] Building wxWidgets 3.1.0"

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

echo "[millipede] ...Running Debug build..."
echo '"C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64 && nmake -f makefile.vc SHARED=0 BUILD=debug MONOLITHIC=0 USE_OPENGL=1' > temp.bat
cmd //c "temp.bat > $LOG 2>&1"

echo "[millipede] ...Running Release build..."
echo '"C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64 && nmake -f makefile.vc SHARED=0 BUILD=release MONOLITHIC=0 USE_OPENGL=1' > temp.bat
cmd //c "temp.bat >> $LOG 2>&1"

echo "[millipede] ...Finished building wxWidgets-3.1.0."
