@Echo off
Rem 
Rem Install.bat
Rem 
Rem Installs wxWidgets and wxHaskell
Rem 
Rem 2015-10-03
Rem 

Set WXWIN=%CD%\wxWidgets
Set WXCFG=gcc_dll\mswu
Set PATH=%CD%;%PATH%

cabal unpack wxdirect
If ERRORLEVEL 1 (
    Echo Could not install wxHaskell
    Pause
    Exit /b 1
  )
For /d %%a In (wxdirect-*) Do (
    cabal install .\%%a
    Copy /y %%a\dist\build\wxdirect\wxdirect.exe .
    Rd /s/q %%a
  )

cabal unpack wxc
If ERRORLEVEL 1 (
    Echo Could not install wxHaskell
    Pause
    Exit /b 2
  )
For /d %%a In (wxc-*) Do (
    cabal install .\%%a
    Copy /y %%a\dist\build\wxc.dll DLLs
    Rd /s/q %%a
  )

cabal install wx
If ERRORLEVEL 1 (
    Echo Could not install wxHaskell
    Pause
    Exit /b 3
  ) Else (
    Echo Ready; now add
    Echo   %CD%\DLLs
    Echo and
    Echo   %WXWIN%\lib\gcc_dll
    Echo to your search path
  )

Pause
