#! /bin/bash -e

LOG=../build-boost_1_39_0.log

echo "[millipede] Building Boost 1.39.0"

if [ -d boost_1_39_0 ]
then
	echo "[millipede] ...Skipping build (already built)"
	exit
fi

if [ -d boost-setup ]
then
	echo "[millipede] ...Skipping archive extraction (already extracted)"
else
	echo "[millipede] ...Extracting archive..."
	/bin/rm -fR tmp
	mkdir tmp
	cd tmp
	tar xzf ../setup/boost_1_39_0/boost_1_39_0.tar.gz
	cd ..
	mv tmp/boost_1_39_0 boost-setup
	rmdir tmp
fi

cd boost-setup

if [ -e bjam ]
then
	echo "[millipede] ...Skipping bootstrapping (bjam already exists)"
else
	echo "[millipede] ...Bootstrapping..."
	./bootstrap.sh > $LOG
fi

echo "[millipede] ...Running build..."
./bjam -j2 --libdir=../boost_1_39_0/lib --includedir=../boost_1_39_0/include --abbreviate-paths --with-date_time --with-filesystem --with-thread --build-type=complete toolset=darwin architecture=x86 install >> $LOG

echo "[millipede] ...Finished building Boost 1.39.0."
