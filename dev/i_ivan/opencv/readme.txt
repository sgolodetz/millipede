Building opencv on Linux:
- follow the opencv install guide: http://opencv.willowgarage.com/wiki/InstallGuide_Linux

Marking the positive images:
- download the improved version of ObjectMarker from http://www.cse.unsw.edu.au/~gherman/ObjectMarker.zip
- to build just run make in $OBJECTMARKER_DIR(note: you might have to change the install and library directories in the Makefile to point to your opencv build)
- add the opencv libraries to LD_LIBRARY_PATH(also mentioned in the opencv install guide)
- copy the positive images to $OBJECTMARKER_DIR/Release/rawdata
- run the $OBJECTMARKER_DIR/Release/ObjectMarker program
- this should produce an annotation file under $OBJECTMARKER_DIR

Creating the sample set:
- create a directory for your training images, with a subdirectory for the positive images, and one for the negatives
- copy the positive images marked with ObjectMarker into $DATA/$POSITIVES/rawdata
- remove the header from the annotation file created by objectMarker and copy it into $DATA/$POSITIVES
- run the following command to normalize all images to the same dimensions and package them into a vec-file
	$OPENCV_INSTALL_DIN/bin/opencv_createsamples -info $DATA/$POSITIVES/annotation.txt -vec $DATA/positives.vec -num $NUM_POS_IMAGES -w $IMAGE_WIDTH_IN_PIXELS -h $IMAGE_HEIGHT_IN_PIXELS

Training:
- before training, create an annotation file in the negatives directory listing the file names for all the negative images
- run the following command:
	$OPENCV_INSTALL_DIN/bin/opencv_haartraining -data $DATA/cascade -vec $DATA/positives.vec -bg $DATA/$NEGATIVES/annotation.txt -npos $NUM_POS_IMAGES -nneg $NUM_NEGATIVE_IMAGES -mode ALL -w $IMAGE_WIDTH_IN_PIXELS -h $IMAGE_HEIGHT_IN_PIXELS

Testing:
- create testing subdirectories under $DATA/$NEGATIVES and $DATA/$POSITIVES
- add the positive and negative test images and annotation files in these directories
- run the following command:
	$OPENCV_INSTALL_DIN/bin/opencv_performance data $DATA/cascade -info $DATA/$POSITIVES/testing/annotation.txt -w $IMAGE_WIDTH_IN_PIXELS -h $IMAGE_HEIGHT_IN_PIXELS

More information about object recognition using opencv:
	http://lab.cntl.kyutech.ac.jp/~kobalab/nishida/opencv/OpenCV_ObjectDetection_HowTo.pdf
	http://note.sonots.com/SciSoftware/haartraining.html
	




