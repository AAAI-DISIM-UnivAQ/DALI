#!/bin/bash

# Enable debugging with `set -x` and disable with `set +x`
set -x  # Start debugging

#exec 1>/dev/null # @echo off
clear # cls

# Test if tmux is installed
if command -v tmux &> /dev/null
then
    echo "tmux is installed."
    tmux -V  # Display tmux version
else
    echo "TMUX is a requirement in Unix-like OS to run DALI"
    echo "tmux is not installed."
    echo "check installation instructions at https://github.com/tmux/tmux/wiki/Installing"
    exit -1
fi

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

if [[ -x "$PROLOG" ]]; then
  printf "SICStus Prolog found at %s\n" "$PROLOG"
else
  printf "Error: SICStus Prolog not found at %s or is not executable.\n" "$PROLOG" >&2
  exit -1
fi

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

tmux split-window -h "$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal \"go(3010,'server.txt').\""echo Server ready. Starting the MAS....
$WAIT > /dev/null # %WAIT% >nul

tmux split-window -h "$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal utente."
echo Launching agents instances...
$WAIT > /dev/null # %WAIT% > nul

# Launch agents
for agent_filename in $BUILD_HOME/*
do
	agent_base="${agent_filename##*/}"
    echo "Agente: $agent_base"
    tmux split-window -v "./conf/makeconf.sh $agent_base $DALI_HOME" &
    tmux split-window -v -t "$agent_base" "./conf/startagent.sh $agent_base $PROLOG $DALI_HOME" &
    sleep 2s
    $WAIT > /dev/null # %WAIT% >nul
done

echo MAS started.
echo Press a key to shutdown the MAS
read -p "$*"
echo Halting the MAS...
killall sicstus
killall tmux
