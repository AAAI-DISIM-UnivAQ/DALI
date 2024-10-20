#!/bin/bash
# Log file location
LOGFILE="logfile.log"
# Redirect both stdout and stderr to the log file
exec > "$LOGFILE" 2>&1
eval "$2 --noinfo -l $3/active_dali_wi.pl --goal \"start0('conf/mas/$1').\""
