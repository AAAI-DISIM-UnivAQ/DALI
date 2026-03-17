#!/bin/bash
agent_name="${1%.[^.]*}" # e.g. 'agent1.txt' -> 'agent1'
echo "agent('work/$1',$1,'no',italian,['conf/communication'],['../../../src/communication_fipa','../../../src/learning','../../../src/planasp'],'no','../../../src/onto/dali_onto.txt',[])." > "conf/mas/$1"
