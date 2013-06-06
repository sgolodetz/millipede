#! /bin/bash -e

# Note: This has been tested on Snow Leopard, but probably doesn't yet work on other versions of Mac OS X.

LOG=../../build-wxWidgets-2.8.10.log

echo "[millipede] Building wxWidgets 2.8.10"

if [ -d wxWidgets-2.8.10 ]
then
	echo "[millipede] ...Skipping archive extraction (already extracted)"
else
	echo "[millipede] ...Extracting archive..."
	/bin/rm -fR tmp
	mkdir tmp
	cd tmp
	tar xzf ../setup/wxWidgets-2.8.10/wxWidgets-2.8.10.tar.gz
	cd ..
	mv tmp/wxWidgets-2.8.10 .
	rmdir tmp
fi

cd wxWidgets-2.8.10

if [ -d build28 ]
then
	echo "[millipede] ...Skipping build (already built)"
else
	mkdir build28
	cd build28

	echo "[millipede] ...Configuring..."
	arch_flags="-arch i386"
	../configure CFLAGS="$arch_flags" CXXFLAGS="$arch_flags" CPPFLAGS="$arch_flags" LDFLAGS="$arch_flags" OBJCFLAGS="$arch_flags" OBJCXXFLAGS="$arch_flags" --disable-shared --with-opengl > $LOG 2>&1

	echo "[millipede] ...Running build..."
	make -j2 >> $LOG 2>&1
fi

echo "[millipede] ...Finished building wxWidgets 2.8.10."
