# DALI Examples

The examples are divided into two subfolders:

* [_basic_](basic) : aimed at basic Windows-based setup, no agent types, every agent living in a separated sicstus window.
* [_advanced_](advanced) : more complex, aimed at Unix-like based environment, like GNU/Linux or macOS,
with agent type, instances, each agent living in a separated terminal console
* [_more_](more) : MAS examples derived from our students projects

## Linux Issues

Put attention to the TCP errors. The new startup script shoudl take care of this.

Otherwise:

### Fast restart of the MAS

With Linux if you need to start the MAS during debug many times, you can avoid the server error, with the following command

    sudo sysctl -w net.ipv4.tcp_tw_reuse=1

(Note: this does not work if you have kernel version >= 4.12. See [this](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=4396e46187ca5070219b81773c4e65088dac50cc) for more information.)

### Line command editor with memory

With Linux, Mac OS X when you start the MAS from the shell, the user agent may not have memory of the commands already sent. You can install rlwrap:

    sudo apt-get install rlwrap

and modify the [startmas.sh](advanced/startmas.sh) launch script to add the ```rlwrap``` command before launching the ```$cmd``` command.

### Sicstus 32 bit install

If you want to use the 32bit version of the sictsus prolog within an ubuntu derived linux box, use these commands:

    sudo dpkg --add-architecture i386
    sudo apt-get install gcc-multilib
    sudo apt-get install libc6:i386 libncurses5:i386 libstdc++6:i386
    
## OS X Issues

### Fast restart of the MAS in macOS

Within macOS, if you need to start the MAS during debug many times, you can avoid the server TCP error, with the following command

    sudo sysctl net.inet.tcp.msl=1

but the latest startup script should take care of this automatically.
