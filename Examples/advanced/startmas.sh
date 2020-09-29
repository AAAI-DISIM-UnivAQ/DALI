#!/bin/bash
#exec 1>/dev/null # @echo off
clear # cls
#title "MAS"
SICSTUS_HOME=/usr/local/sicstus4.6.0
MAIN_HOME=../..
DALI_HOME=../../src
CONF_DIR=conf
PROLOG="$SICSTUS_HOME/bin/sicstus"
WAIT="ping -c 4 127.0.0.1"
INSTANCES_HOME=mas/instances
TYPES_HOME=mas/types
BUILD_HOME=build
XTERM=xterm

rm -rf tmp/*
rm -rf build/*
rm -f work/* # remove everything if you want to clear agent history
rm -rf conf/mas/*

# Build agents by creating a file with the instance name containing the type content for each instance.
for instance_filename in $INSTANCES_HOME/*.txt
do
	type=$(<$instance_filename) # agent type name is the content of the instance file
	type_filename="$TYPES_HOME/$type.txt"
	instance_base="${instance_filename##*/}" # e.g. 'mas/instances/agent1.txt' -> 'agent1.txt'
	echo $type_filename
	cat $type_filename >> "$BUILD_HOME/$instance_base"
done

cp $BUILD_HOME/*.txt work

$XTERM -hold -e "$PROLOG -l $DALI_HOME/active_server_wi.pl --goal \"go(3010,'server.txt').\"" & #start /B "" "%PROLOG%" -l "%DALI_HOME%/active_server_wi.pl" --goal go(3010,'%daliH%/server.txt').
echo Server ready. Starting the MAS....
$WAIT > /dev/null # %WAIT% >nul

$XTERM -hold -e "$PROLOG -l $DALI_HOME/active_user_wi.pl --goal utente." & # start /B "" "%PROLOG%" -l "%DALI_HOME%/active_user_wi.pl" --goal utente.
echo Launching agents instances...
$WAIT > /dev/null # %WAIT% > nul

# Launch agents
for agent_filename in $BUILD_HOME/*
do
	agent_base="${agent_filename##*/}"
    echo "Agente: $agent_base"
    $XTERM -e "./conf/makeconf.sh $agent_base $DALI_HOME" &
    $XTERM -T "$agent_base" -hold -e "./conf/startagent.sh $agent_base $PROLOG $DALI_HOME" &
    sleep 2s
    $WAIT > /dev/null # %WAIT% >nul
done

echo MAS started.
echo Press a key to shutdown the MAS
read -p "$*"
echo Halting the MAS...
killall sicstus
killall xterm
