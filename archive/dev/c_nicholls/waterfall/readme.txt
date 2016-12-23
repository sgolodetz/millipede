
# compile the code
ghc -O2 --make run.hs


# make sure there is an `output' directory locally
# (There is one in the repo)

# For small images, it is probably best to simply run the program with the
# desired input:
./run images/test200.pgm

#For larger images however, stack space will need to be increased and
# it is a good idea to also provide more heap space.
# With ghc version 6.10.3
# I have found the following is about optimal:

./run +RTS -H300M -A100m -K30m  -sstderr -RTS images/test512.pgm

# To profile the program:
# profiling options
# rely on the presence of ~/.ghc/x86_64-linux-6.8.3/package.conf
# which takes packages from /ecslab ...

ghc -O2 -prof -auto-all -caf-all -fforce-recomp --make run
# or
ghc -O2 -prof -auto-all --make run

# then generate run.prof (ASCII) and run.hp with

./run images/test400.pgm +RTS  -p
./run images/test400.pgm +RTS  -p -hc
./run images/test400.pgm +RTS -sstderr
./run images/test400.pgm +RTS -sstderr -B

# some info is in run.prof and some can be plotted with

hp2ps -c  run && evince run.ps

# more info on profiling at
# http://www.haskell.org/ghc/docs/latest/html/users_guide/prof-heap.html#retainer-prof

# to compile and use the GUI slide bar interface
# (relies on having 6 images in the output directory, rather brittle)
# requires gtk2hs package.

ghc --make gui
./gui


