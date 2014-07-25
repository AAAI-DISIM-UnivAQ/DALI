@echo off
cls
title "DALI MAS - Windows Starter script"
set sicstus_home=C:\Program Files (x86)\SICStus Prolog 4.2.3\bin
set main_home=..
set dali_home=..\src
set conf_dir=conf
set prolog=%sicstus_home%\spwin.exe
set WAIT=ping -n 4 127.0.0.1

del /q work\*.pl
copy mas\*.txt work

set daliH=%dali_home:\=/%

start /B "" "%prolog%" -l "%dali_home%\active_server_wi.pl" --goal go(3010,'%daliH%/server.txt').
echo DALI server running at port 3010. Press RETURN to start the MAS.
pause

start /B "" "%prolog%" -l "%dali_home%\active_user_wi.pl" --goal utente.
%WAIT% >nul 
FOR /F "tokens=*" %%G IN ('dir /b mas\*.txt') DO (
@echo agente: %%~nG
call conf/makeconf %%~nG %%G
call conf/startagent %%G "%prolog%" "%dali_home%"
%WAIT% >nul 
)

