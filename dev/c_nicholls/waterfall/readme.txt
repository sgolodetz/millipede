ghc -XFlexibleInstances -XFlexibleContexts -O2 --make run.hs
# make sure there is an `output' directory locally
./run test200x200.pgm 
./run test400.pgm 
./run test512.pgm 
xv output/output*


# run with increased heap space
./run test512.pgm +RTS -H1G

# run with both heap and stack increased
./run test512.pgm +RTS -H1G -K100M


# profiling options
# rely on the presence of ~/.ghc/x86_64-linux-6.8.3/package.conf 
# which takes packages from /ecslab ...

ghc -O2 -prof -auto-all -caf-all -fforce-recomp --make run

# or

ghc -O2 -prof -auto-all --make run

# then generate run.prof (ASCII) and run.hp with

./run test400.pgm +RTS  -p
./run test400.pgm +RTS  -p -hc
./run test400.pgm +RTS -sstderr
./run test400.pgm +RTS -sstderr -B

# some info is in run.prof and some can be plotted with

hp2ps -c  run && evince run.ps


# to compile and use the GUI slide bar interface
# (relies on having 6 images in the output directory)

ghc --make gui
./gui

