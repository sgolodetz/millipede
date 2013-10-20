#! /bin/bash -e

cd libraries
./build-boost_1_39_0-mac.sh
./build-gdcm-1.2.5-mac.sh
./build-ITK-3.14.0-mac.sh
./build-wxWidgets-2.8.10-mac.sh
cd ..

echo "[millipede] Building millipede"

if [ ! -d build ]
then
	GDCM_DIR=`pwd`/libraries/gdcm-1.2.5/build
	ITK_DIR=`pwd`/libraries/ITK-3.14.0/build
	WXWIDGETS_DIR=`pwd`/libraries/wxWidgets-2.8.10/build28
	WXWIDGETS_CONFIG=$WXWIDGETS_DIR/wx-config
	WXWIDGETS_WXRC=$WXWIDGETS_DIR/wxrc

	INSTALL_DIR=`pwd`/install

	mkdir build
	cd build

	echo "[millipede] ...Configuring using CMake..."
	cmake -DCMAKE_BUILD_TYPE=Debug -DGDCM_DIR=$GDCM_DIR -DITK_DIR=$ITK_DIR -DwxWidgets_CONFIG_EXECUTABLE=$WXWIDGETS_CONFIG -DwxWidgets_wxrc_EXECUTABLE=$WXWIDGETS_WXRC -DCMAKE_OSX_ARCHITECTURES=i386 -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR ../source
	cd ..
fi

cd build

echo "[millipede] ...Running build..."
make -j2

echo "[millipede] ...Installing..."
make install

echo "[millipede] ...Finished building millipede."
