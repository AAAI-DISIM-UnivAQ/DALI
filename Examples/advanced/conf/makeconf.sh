#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORK_DIR="$( cd "$SCRIPT_DIR/../work" && pwd )"
DALI_DIR="$( cd "$2" && pwd )"
agent_name="${1%.[^.]*}" # e.g. 'agent1.txt' -> 'agent1'
echo "agent('$WORK_DIR/$agent_name','$agent_name','no',italian,['$SCRIPT_DIR/communication'],['$DALI_DIR/communication_fipa','$DALI_DIR/learning','$DALI_DIR/planasp'],'no','$DALI_DIR/onto/dali_onto.txt',[])." > "$SCRIPT_DIR/mas/$1"
