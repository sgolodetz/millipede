#! /bin/bash -e

PLATFORM=`../detect-platform.sh`

TOOLSET=gcc
if [ $PLATFORM == "mac" ]
then
  TOOLSET=darwin
  OSXVERSION=`../detect-osxversion.sh`
fi

# Build Boost 1.58.0.
LOG=../build-boost_1_58_0.log

echo "[millipede] Building Boost 1.58.0"

if [ -d boost_1_58_0 ]
then
  echo "[millipede] ...Skipping build (already built)"
  exit
fi

if [ -d boost-setup ]
then
  echo "[millipede] ...Skipping archive extraction (already extracted)"
else
  mkdir -p setup/boost_1_58_0

  if [ ! -f setup/boost_1_58_0/boost_1_58_0.tar.gz ]
  then
    echo "[millipede] ...Downloading archive..."
    curl -sL http://sourceforge.net/projects/boost/files/boost/1.58.0/boost_1_58_0.tar.gz > setup/boost_1_58_0/boost_1_58_0.tar.gz
  fi

  echo "[millipede] ...Extracting archive..."
  /bin/rm -fR tmp
  mkdir tmp
  cd tmp
  tar xzf ../setup/boost_1_58_0/boost_1_58_0.tar.gz
  cd ..
  mv tmp/boost_1_58_0 boost-setup
  rmdir tmp
fi

cd boost-setup

if [ -e b2 ]
then
  echo "[millipede] ...Skipping bootstrapping (b2 already exists)"
else
  echo "[millipede] ...Bootstrapping..."
  ./bootstrap.sh > $LOG
fi

echo "[millipede] ...Running build..."
if [ $PLATFORM == "mac" ]
then
  if [ "$OSXVERSION" -ge 13 ]
  then
    STDLIBFLAGS='cxxflags="-stdlib=libstdc++" linkflags="-stdlib=libstdc++"'
  else
    STDLIBFLAGS=''
  fi
else
  STDLIBFLAGS='cxxflags="-std=c++11"'
fi

./b2 -j2 --libdir=../boost_1_58_0/lib --includedir=../boost_1_58_0/include --abbreviate-paths --with-chrono --with-date_time --with-filesystem --with-program_options --with-regex --with-serialization --with-test --with-thread --with-timer --build-type=complete --layout=tagged toolset=$TOOLSET architecture=x86 address-model=64 $STDLIBFLAGS install >> $LOG

echo "[millipede] ...Fixing headers..."
perl -i -pe 's/SPT<void>/SPT<const void>/g' ../boost_1_58_0/include/boost/serialization/shared_ptr_helper.hpp

echo "[millipede] ...Finished building Boost 1.58.0."
