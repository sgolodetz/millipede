@echo off

REM #########################################################
REM Compare the waterfall partition outputs using ImageMagick
REM #########################################################

FOR %%f IN (*partition*.png) DO (
	convert %%f -define png:color-type=2 %%f
)

FOR %%f IN (*partition*G.png) DO (
	FOR /F "delims=-. tokens=1,2,3" %%a IN ('echo %%f') DO (
		echo %%a %%b %%c
		compare %%a-%%b-%%c-G.png %%a-%%b-%%c-M.png %%a-diff-%%c-GM.png
		compare %%a-%%b-%%c-G.png %%a-%%b-%%c-NC.png %%a-diff-%%c-GNC.png
		compare %%a-%%b-%%c-G.png %%a-%%b-%%c-NT.png %%a-diff-%%c-GNT.png
		compare %%a-%%b-%%c-M.png %%a-%%b-%%c-NC.png %%a-diff-%%c-MNC.png
		compare %%a-%%b-%%c-M.png %%a-%%b-%%c-NT.png %%a-diff-%%c-MNT.png
		compare %%a-%%b-%%c-NC.png %%a-%%b-%%c-NT.png %%a-diff-%%c-NCNT.png
	)
)
