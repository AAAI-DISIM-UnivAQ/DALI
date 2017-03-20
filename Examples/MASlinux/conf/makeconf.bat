if not exist conf\communication_%1.con (
	copy ..\..\src\communication_default\communication.con conf\communication_%1.con	
)
echo agent('work/%1',%1,'no',italian,['conf/communication_%1'],['../../src/communication_fipa','../../src/learning','../../src/planasp'],'no','../../src/onto/dali_onto.txt',[]). > conf/%2 
