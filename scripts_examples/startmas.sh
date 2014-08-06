#exec 1>/dev/null # @echo off
clear # cls
#title "MAS"
sicstus_home=/usr/local/sicstus4.2.3
main_home=..
dali_home=../src
conf_dir=conf
prolog="$sicstus_home/bin/sicstus"
WAIT="ping -c 4 127.0.0.1" 
instances_home=mas/instances
types_home=mas/types
build_home=build

rm -rf tmp/*
rm -rf build/*
rm -f work/*.txt # remove everything if you want to clear agent history
rm -rf conf/mas/*

# Build agents by creating a file with the instance name containing the type content for each instance.
for instance_filename in $instances_home/*
do	
	type=$(<$instance_filename) # agent type name is the content of the instance file
	type_filename="$types_home/$type"
	instance_base="${instance_filename##*/}" # e.g. 'mas/instances/agent1.txt' -> 'agent1.txt'
	cat $type_filename >> "$build_home/$instance_base"
done

cp $build_home/*.txt work

xterm -hold -e "$prolog -l $dali_home/active_server_wi.pl --goal \"go(3010,'server.txt').\"" & #start /B "" "%prolog%" -l "%dali_home%/active_server_wi.pl" --goal go(3010,'%daliH%/server.txt').
echo Server attivato. Attivo il MAS....
$WAIT > /dev/null # %WAIT% >nul

xterm -hold -e "$prolog -l $dali_home/active_user_wi.pl --goal utente." & # start /B "" "%prolog%" -l "%dali_home%/active_user_wi.pl" --goal utente.
echo Server DALI attivato. Attivo gli agenti...
$WAIT > /dev/null # %WAIT% > nul

# Avvia gli agenti
for agent_filename in $build_home/*
do
	agent_base="${agent_filename##*/}"
    echo "Agente: $agent_base"
    xterm -e "./conf/makeconf.sh $agent_base $dali_home" &
    xterm -hold -e "./conf/startagent.sh $agent_base $prolog $dali_home" &
    $WAIT > /dev/null # %WAIT% >nul
done

echo Operazione completata.
echo Premere un tasto per terminare il MAS
read -p "$*"
echo Chiudo il MAS...
killall sicstus
killall xterm
