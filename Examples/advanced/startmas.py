#!/usr/bin/env python3
import os
import subprocess
import shutil
import glob
import time

# Enable debugging
# import pdb; pdb.set_trace()

# Clear the terminal
os.system("clear")

# Save the current directory to a variable
current_dir = os.getcwd()

# Test if tmux is installed
if not shutil.which("tmux"):
    print("TMUX is a requirement in Unix-like OS to run DALI")
    print("tmux is not installed.")
    print("Check installation instructions at https://github.com/tmux/tmux/wiki/Installing")
    exit(1)

# Create or attach to the tmux session
# session= "tmux new-session -d -s DALI_session top"

# Define paths and variables
SICSTUS_HOME = "/usr/local/sicstus4.6.0"

DALI_HOME = "../../"
CORE_DIR = os.path.join(DALI_HOME, "src", "core")
COMMUNICATION_DIR = os.path.join(DALI_HOME, "src", "communication")
EVENT_DIR = os.path.join(DALI_HOME, "src", "event")
UTILS_DIR = os.path.join(DALI_HOME, "src", "utils")

CONF_DIR = "conf"
PROLOG = os.path.join(SICSTUS_HOME, "bin", "sicstus")
WAIT = "ping -c 3 127.0.0.1"
INSTANCES_HOME = os.path.join("mas", "instances")
TYPES_HOME = os.path.join("mas", "types")
BUILD_HOME = "build"

# Check if SICStus Prolog exists and is executable
if not os.path.exists(PROLOG) or not os.access(PROLOG, os.X_OK):
    print("SICStus Prolog not found or not executable.")
    exit(1)

# Build agents based on instances
for instance_filename in glob.glob(f"{INSTANCES_HOME}/*.txt"):
    ag_name = instance_filename.split(os.path.sep)[-1]
    ag_type = open(instance_filename).read().strip()  # Agent type name is the content of the instance file
    type_filename = os.path.join(TYPES_HOME, ag_type + ".txt")
    print(f"Instance: {instance_filename} of type: {type_filename}")
    instance_base = os.path.splitext(instance_filename)[-2]  # Extract instance base name
    with open(type_filename, "r") as f:
        instance = os.path.join(BUILD_HOME, ag_name.split(os.path.sep)[0])
        print(type_filename, instance)
        with open(instance, "w") as f2:
            f2.write(f.read())

print(os.listdir(BUILD_HOME))
os.system(f"cp {BUILD_HOME}/*.txt work")

# Start the LINDA server in a new console
srvcmd = f'{PROLOG} --noinfo -l {COMMUNICATION_DIR}/communication_server.pl --goal "initialize_server(3010) ,server_loop."'
print(f"server: {srvcmd}")
subprocess.Popen(srvcmd, shell=True)

print("Server ready. Starting the MAS...")
time.sleep(3)
exit(0)

print("Launching agents instances...")
# Launch agents in horizontal splits, one after the other
for agent_filename in os.listdir(BUILD_HOME):
    agent_base = os.path.splitext(agent_filename)[0]
    print(f"Agent: {agent_base}")
    # Create the agent configuration
    os.system(f"{current_dir}/conf/makeconf.sh {agent_base} {DALI_HOME}")
    # Start the agent in the new pane
    subprocess.Popen(f"{current_dir}/conf/startagent.sh {agent_base} {PROLOG} {CORE_DIR}/core_interpreter.pl --goal 'initialize_agent(\"{agent_base}\"),reasoning_cycle.'", shell=True)
    subprocess.run(WAIT, shell=True, stdout=subprocess.DEVNULL)  # Wait a bit before launching the next agent

# Start user agent in another vertical split
subprocess.Popen(f"{PROLOG} --noinfo -l {COMMUNICATION_DIR}/communication_client.pl --goal 'initialize_client(\"localhost\", 3010),client_loop.'", shell=True)

print("MAS started.")

# Attach to the session so you can see everything
subprocess.run("tmux attach -t DALI_session", shell=True)

print("Press Enter to shutdown the MAS")
input()

# Clean up processes
subprocess.run("killall sicstus", shell=True)