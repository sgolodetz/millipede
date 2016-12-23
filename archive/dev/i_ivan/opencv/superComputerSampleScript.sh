#!/bin/bash
#PBS -V
#PBS -l select=1
#PBS -l walltime=60:00:00
#PBS -N kidneys

cd $PBS_O_WORKDIR

opencv-createsamples -info kidneys/positives/annotation.txt -vec kidneys/data/positives.vec -num 245 -w 20 -h 20 &> kidneys/kidneys.log

opencv-haartraining -data kidneys/data/cascade -vec kidneys/data/positives.vec -bg kidneys/negatives/train.txt -npos 245 -nneg 1000 -nstages 30 -w 20 -h 20 -nonsym -mode ALL &> kidneys/kidneys.log

#The walltime parameter is set to 60 hours. This is how long the program will keep running. You can change this if you want.

