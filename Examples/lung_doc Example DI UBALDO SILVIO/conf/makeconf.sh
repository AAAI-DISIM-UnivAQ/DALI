agent_name="${1%.[^.]*}" # e.g. 'agent1.txt' -> 'agent1'
if [ -f "conf\communication_$agent_name.con" ];
then
   ss=0
else
   cp ../../src/communication_default/communication.con conf/communication_$agent_name.con
fi
echo "agent('work/$agent_name','$agent_name','no',italian,['conf/communication_$agent_name.con'],['$2/communication_fipa','$2/learning','$2/planasp'],'no','$2/onto/dali_onto.txt',[])." > "conf/mas/$1"
