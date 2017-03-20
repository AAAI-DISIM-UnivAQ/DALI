#exec 1>/dev/null # @echo off
clear # cls
#title "MAS"
sicstus_home=/usr/local/sicstus4.2.3
main_home=../..
dali_home=../../src
conf_dir=conf
prolog="$sicstus_home/bin/sicstus"
WAIT="ping -c 4 127.0.0.1" 
build_home=build

rm -f work/*.pl 
cp mas/*.txt work

xterm -hold -e "$prolog -l $dali_home/active_server_wi.pl --goal \"go(3010,'server.txt').\"" & #start /B "" "%prolog%" -l "%dali_home%/active_server_wi.pl" --goal go(3010,'%daliH%/server.txt').
echo Server ready. Starting the MAS....
$WAIT > /dev/null # %WAIT% >nul

xterm -hold -e "$prolog -l $dali_home/active_user_wi.pl --goal utente." & # start /B "" "%prolog%" -l "%dali_home%/active_user_wi.pl" --goal utente.
echo Launching agents instances...
$WAIT > /dev/null # %WAIT% > nul

# Launch agents
for agent_filename in mas/*.txt
do
	agent_base="${agent_filename##*/}"
    echo "Agente: $agent_base"
    xterm -e "./conf/makeconf.sh $agent_base $dali_home" &
    xterm -hold -e "./conf/startagent.sh $agent_base $prolog $dali_home" &
    $WAIT > /dev/null # %WAIT% >nul
done

echo MAS started.
echo Press a key to shutdown the MAS
read -p "$*"
echo Halting the MAS...
killall sicstus
killall xterm
