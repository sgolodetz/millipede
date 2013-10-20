#! /bin/bash -e

/bin/rm -fR build
/bin/rm -fR install

cd libraries
/bin/rm -fR boost_1_39_0
/bin/rm -fR boost-setup
/bin/rm -fR gdcm-1.2.5
/bin/rm -fR ITK-3.14.0
/bin/rm -fR wxWidgets-2.8.10
/bin/rm -fR *.log
