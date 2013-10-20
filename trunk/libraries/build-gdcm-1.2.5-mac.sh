#! /bin/bash -e

LOG=../../build-gdcm-1.2.5.log

echo "[millipede] Building GDCM 1.2.5"

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
	mkdir gdcm-1.2.5
	mv tmp/gdcm-1.2.5 gdcm-1.2.5/source
	rmdir tmp
fi

cd gdcm-1.2.5

if [ -d build ]
then
	echo "[millipede] ...Skipping build (already built)"
else
	INSTALL_DIR=`pwd`/install

	mkdir build
	cd build

	echo "[millipede] ...Configuring using CMake..."
	cmake -DBUILD_TESTING=OFF -DGDCM_BUILD_EXAMPLES=OFF -DGDCM_BUILD_SHARED_LIBS=ON -DCMAKE_OSX_ARCHITECTURES=i386 -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR ../source > $LOG

	echo "[millipede] ...Running build..."
	make -j2 >> $LOG
fi

echo "[millipede] ...Finished building GDCM 1.2.5."
