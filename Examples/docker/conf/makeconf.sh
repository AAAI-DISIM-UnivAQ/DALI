#!/bin/bash
# makeconf.sh — generates the agent configuration file for Docker
#
# Usage: makeconf.sh <agent_filename.txt> <dali_home>

agent_name="${1%.[^.]*}"
dali_home="${2:-/dali/src}"

mkdir -p conf/mas

echo "agent('work/$agent_name',$agent_name,'no',italian,['conf/communication'],['$dali_home/communication_fipa','$dali_home/learning','$dali_home/planasp'],'no','$dali_home/onto/dali_onto.txt',[])." \
    > "conf/mas/$1"
