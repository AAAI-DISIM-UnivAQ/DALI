#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
eval "$2 --noinfo -l $3/src/active_dali_wi.pl --goal \"start0('$SCRIPT_DIR/mas/$1').\""
