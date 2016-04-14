#!/bin/bash

#  ./sayfortune.sh [topic [rate [pitch]]
#    e.g.:  ./sayfortune.sh
#           ./sayfortune.sh literature 0 -20

printf "!SET SELF PRIORITY TEXT\n!SET SELF RATE $2\n!SET SELF PITCH $3\n!SPEAK \n `fortune $1` \n.\n S3" > fortune-speechd.tmp
./run_test fortune-speechd.tmp
rm fortune-speechd.tmp
