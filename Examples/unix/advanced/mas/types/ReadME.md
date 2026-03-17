In this folder all the agent types shall 
exist as DALI sources. From these files all the agent 
instances will be forked from the same type, 
sharing the same DALI code.

If you add this fact at the first line of agent:

    t60.

indicate a default time windows of 60 seconds in which 
two subsequent external events have to be considered simultaneous.

If this line is missing, every agent instance will show a warning message.
