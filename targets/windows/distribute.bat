
python setup.py py2exe

set PYTHON=c:\python24\lib\site-packages
set OGLSrc=%PYTHON%\OpenGL
set NumarraySrc=%PYTHON%\numarray
set DEST=.\dist
set OGLDest=%DEST%\OpenGL

mkdir %DEST%\OpenGL
mkdir %DEST%\OpenGL\GL
mkdir %DEST%\OpenGL\GLU
rem mkdir %DEST%\OpenGL\Tk
mkdir %DEST%\OpenGL\WGL
copy %OGLSrc%\*.* %OGLDest%
xcopy /E %OGLSrc%\GL %OGLDest%\GL
xcopy /E %OGLSrc%\GLU %OGLDest%\GLU
rem xcopy /E %OGLSrc%\Tk %OGLDest%\Tk
xcopy /E %OGLSrc%\WGL %OGLDest%\WGL

mkdir %DEST%\numarray
xcopy /E %NumarraySrc% %Dest%\numarray

xcopy candoConfig.gml %DEST%
xcopy c:\windows\system\glut32.dll %DEST%
xcopy c:\windows\system32\msvcp71.dll %DEST%
xcopy c:\windows\system32\msvcr71.dll %DEST%

mkdir %DEST%\Examples
xcopy /E ..\..\..\examples %DEST%\Examples

xcopy ..\searcher\searcher.exe %DEST%



rem
rem Start Inno setup
rem

rem "c:\Program files\Inno Setup 4\iscc.exe" /cc cando.iss
"c:\Program files\Inno Setup 4\iscc.exe" cando.iss
