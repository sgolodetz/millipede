#! /bin/bash -e

LOG=../../build-ITK-3.14.0.log

echo "[millipede] Building ITK 3.14.0"

if [ -d ITK-3.14.0 ]
then
	echo "[millipede] ...Skipping archive extraction (already extracted)"
else
	echo "[millipede] ...Extracting archive..."
	/bin/rm -fR tmp
	mkdir tmp
	cd tmp
	tar xzf ../setup/ITK-3.14.0/InsightToolkit-3.14.0.tar.gz
	cd ..
	mkdir ITK-3.14.0
	mv tmp/InsightToolkit-3.14.0 ITK-3.14.0/source
	rmdir tmp
fi

cd ITK-3.14.0

if [ -d build ]
then
	echo "[millipede] ...Skipping build (already built)"
else
	INSTALL_DIR=`pwd`/install

	mkdir build
	cd build

	echo "[millipede] ...Configuring using CMake..."
	cmake -Wno-dev -DBUILD_DOXYGEN=OFF -DBUILD_EXAMPLES=OFF -DBUILD_SHARED_LIBS=OFF -DBUILD_TESTING=OFF -DITK_USE_KWSTYLE=OFF -DCMAKE_OSX_ARCHITECTURES=i386 -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR ../source > $LOG 2>&1

	echo "[millipede] ...Running build..."
	make -j2 >> $LOG 2>&1
fi

echo "[millipede] ...Finished building ITK 3.14.0."
