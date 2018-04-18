#!/usr/bin/env bash

for i in 1 10 50 100 250 500 1000 5000 10000; do
    ../airmcts.native -scenario ../data/scen4.json -horizon 20 -nsteps 20 -timeperstep 1 -expvexp $i
done
