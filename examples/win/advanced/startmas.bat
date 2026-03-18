@echo off
setlocal enabledelayedexpansion
cls
title "MAS"


:: Configuration
set main_home=..\..\..
set dali_home=..\..\..\src
set conf_dir=conf

:: Search for spwin.exe
for %%I in (spwin.exe) do set prolog=%%~$PATH:I
if "%prolog%"=="" (
	for /d %%D in ("%PROGRAMFILES%\SICStus*") do (
		if exist "%%D\bin\spwin.exe" set "prolog=%%D\bin\spwin.exe"
	)
)
if "%prolog%"=="" (
	for /d %%D in ("%PROGRAMFILES(x86)%\SICStus*") do (
		if exist "%%D\bin\spwin.exe" set "prolog=%%D\bin\spwin.exe"
	)
)

if "%prolog%"=="" (
	echo ERROR: spwin.exe not found.
	exit /b 1
)
echo [INFO] spwin.exe found at: %prolog%



:: Cleanup
echo [INFO] Closing previous processes and cleaning folders...
taskkill /F /IM spwin.exe /T >nul 2>&1
taskkill /F /IM sprt.exe /T >nul 2>&1
taskkill /F /IM sicstus.exe /T >nul 2>&1
for /R work %%F in (*.*) do (if not "%%~nxF"==".gitkeep" del /q "%%F" >nul 2>&1)
for /R build %%F in (*.*) do (if not "%%~nxF"==".gitkeep" del /q "%%F" >nul 2>&1)
for /R conf\mas %%F in (*.*) do (if not "%%~nxF"==".gitkeep" del /q "%%F" >nul 2>&1)
del /q server.txt >nul 2>&1
if not exist build mkdir build
if not exist work\log mkdir work\log
if not exist conf\mas mkdir conf\mas

:: Building agents
echo [INFO] Building agents...
for %%I in (mas\instances\*.txt) do (
	for /f "usebackq delims=" %%T in (%%I) do set type=%%T
	copy /y mas\types\!type!.txt build\%%~nxI >nul
)
copy build\*.txt work\ >nul 2>nul

:: Starting LINDA server
echo [INFO] Starting LINDA server...
start "MAS - Server" "%prolog%" --noinfo -l "%dali_home%\active_server_wi.pl" --goal "go(3010,'server.txt')."

:: Waiting for server
echo [INFO] Waiting for server.txt...
set server_ready=0
for /L %%i in (1,1,20) do (
    if exist "server.txt" (
        set server_ready=1
        goto :server_ok
    )
    timeout /t 1 /nobreak >nul
)
:server_ok

:: Starting user
echo [INFO] Starting user...
start "MAS - User" "%prolog%" --noinfo -l "%dali_home%\active_user_wi.pl" --goal "utente."
timeout /t 2 /nobreak >nul

:: Starting agents
echo [INFO] Starting agents...
set idx=1
for %%G in (build\*.txt) do (
	set "agent=%%~nG"
	call conf\makeconf.bat !agent! "%%G"
    start "MAS - Agent !idx!" "%prolog%" --noinfo -l "%dali_home%\active_dali_wi.pl" --goal "start0('conf/mas/%%~nxG')."
    set /a idx+=1
	timeout /t 2 /nobreak >nul
)



echo [INFO] MAS started. Press any key to shut down.
pause >nul

:: Final cleanup
taskkill /F /IM spwin.exe /T >nul 2>&1
taskkill /F /IM sprt.exe /T >nul 2>&1
taskkill /F /IM sicstus.exe /T >nul 2>&1
exit /b 0
