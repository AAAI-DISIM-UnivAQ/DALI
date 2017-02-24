@echo off
cls
title "MAS"
set sicstus_home=C:\Program Files (x86)\SICStus Prolog 4.2.3\bin
::set main_home=%~dp0\..
set main_home=..\..
set dali_home=..\..\src
set conf_dir=conf
set prolog=%sicstus_home%\spwin.exe
set WAIT=ping -n 4 127.0.0.1
set instances_home=mas\instances
set types_home=mas\types
set build_home=build

del /q work\*.pl
del /q /s build\*
del /q /s conf\mas\*

::QUI POSSO AGGIUNGERE PARAMETRI PER GLI AGENTI


FOR /F "tokens=*" %%G IN ('dir /b %instances_home%\*.txt') DO (

	::for /f "tokens=2" %%a in (%instances_home%\%%G) do echo %%a > %build_home%\%%G	
	for /f "skip=1 tokens=1,2 delims=#" %%a in (%instances_home%\%%G) do (
		echo %%a.>>%build_home%\%%G
	
	) 
	for /f "delims= " %%a in (%instances_home%\%%G) do type %types_home%\%%a.txt >> %build_home%\%%G
	
)

copy %build_home%\*.txt work

start /B "" "%prolog%" -l "%dali_home%\active_server_wi.pl" --goal go(3010,'%daliH%/server.txt').
echo Server ready. Starting the MAS....
%WAIT% >nul 

start /B "" "%prolog%" -l "%dali_home%\active_user_wi.pl" --goal utente.
echo Launching agents instances...
%WAIT% >nul 

FOR /F "tokens=*" %%G IN ('dir /b %build_home%\*.txt') DO (
	@echo agente: %%~nG
	call conf/makeconf %%~nG %%G
	call conf/startagent %%G "%prolog%" "%dali_home%"
	%WAIT% >nul 
)

echo MAS started.