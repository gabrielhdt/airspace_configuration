#!/bin/bash

MCTS_CMD="airmcts"

# Time per step test
lowerbound=0.0000001
upperbound=0.01
nval=200
step=$( echo "($upperbound - $lowerbound) / $nval" | bc -l )
echo "step of $step"
rm tps.out
for i in $(seq 1 $nval) ; do
	printf "$i/$nval\r"
	val=$( echo "$lowerbound + $i * $step" | bc -l )
	out="$($MCTS_CMD -nsteps 5 -timeperstep $val -scenario $1)"
	echo "$val $out" >> tps.out
done;
gnuplot -p -e 'plot "tps.out"'
