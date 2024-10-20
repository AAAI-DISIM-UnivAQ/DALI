#!/bin/bash
# Log file location
LOGFILE="logfile.log"
# Redirect both stdout and stderr to the log file
exec > "$LOGFILE" 2>&1
agent_name="${1%.[^.]*}" # e.g. 'agent1.txt' -> 'agent1'
echo "agent('work/$agent_name','$agent_name','no',italian,['conf/communication'],['$2/communication_fipa','$2/learning','$2/planasp'],'no','$2/onto/dali_onto.txt',[])." > "conf/mas/$1"
