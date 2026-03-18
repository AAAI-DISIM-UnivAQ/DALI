#!/bin/bash
# startagent.sh
agent_base="$1"
prolog="$2"
dali_home="$3"

exec "$prolog" --noinfo -l "$dali_home/active_dali_wi.pl" \
    --goal "start0('conf/mas/$agent_base')."
