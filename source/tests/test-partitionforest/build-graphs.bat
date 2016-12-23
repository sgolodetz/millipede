@echo off

REM ##################
REM Output hierarchies
REM ##################

FOR %%f IN (*-hierarchy.gv) DO (
dot -Tpng:gdiplus %%f -o %%~nf.png
)

REM #######################
REM Output adjacency graphs
REM #######################

FOR %%f IN (*-graph*.gv) DO (
neato -Tpng:gdiplus %%f -o %%~nf.png
)