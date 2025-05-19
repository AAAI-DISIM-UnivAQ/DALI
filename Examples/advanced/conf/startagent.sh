#!/bin/bash
# set -x
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
eval "$2 --noinfo -l $3/active_dali_wi.pl --goal \"start0('$SCRIPT_DIR/mas/$1').\""
