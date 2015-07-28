DALI Examples
=============


The examples are divided into two subfolders:
* [_basic_](basic) : aimed at basic Windows-based setup, no agent types, every agent living in a separated sicstus window.
* [_advanced_](advanced) : more complex, aimed at Unix-like based environment, with agent type, instances, each agent living in a separated xterm console
* [_more_](more) : MAS examples derived from our students projects

## Notes

### Fast restart of the MAS
With Linux if you need to start the MAS during debug many times, you can avoid the server error, with the following command

    sudo sysctl net.ipv4.tcp_tw_recycle=1
    
### Line command editor with memory
With Linux, Mac OS X when you start the MAS from the shell, the user agent may not have memory of the commands already sent. You can install rlwrap:

    sudo apt-get install rlwrapp

and modify the [startmas.sh](advanced/startmas.sh) launch script at line 45:

    xterm -hold -e "rlwrapp ./conf/startagent.sh $agent_base $prolog $dali_home" &
