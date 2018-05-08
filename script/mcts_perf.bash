#!/bin/bash

NAME=$0
OPTIONS=hu:l:n:o:s:d:r:
LONGOPTIONS=help,scenario:,depth:,upper:,lower:,nval:,out:,runs:
USAGE="Usage: $0 -s <scenario> -d <depth>
Evaluates performance of the airmcts. Outputs a file containing several
columns, each column contains a given number of measures.
Param:
\t-l|--lower\t<float>\tlower bound of time per step
\t-u|--upper\t<float>\tupper bound of time per step
\t-r|--runs\t<int>\tnumber of runs
\t-n|--nval\t<int>\tnumber of measures per run
\t-o|--out\t<filepath>\toutput file"

SCENARIO=""
DETPH=10
OUTFILE='tps.data'
runs=1
lowerbound=0.0001
upperbound=0.1
nval=10

MCTS_CMD="airmcts"
ASTAR_CMD="astair"
TMPD=$(mktemp -d)

temp=$(getopt -o $OPTIONS --long $LONGOPTIONS -n $NAME -- "$@")

if [[ $? -ne 0 ]]; then
    echo "Terminating..." >&2
    exit 1
fi

eval set -- "$temp"
unset temp
while true; do
    case "$1" in
    '-s' | '--scenario')
        SCENARIO=$2
        shift 2
        continue
        ;;
    '-d' | '--depth')
        DEPTH=$2
        shift 2
        continue
        ;;
    '-o' | '--out')
        OUTFILE=$2
        shift 2
        continue
        ;;
    '-u' | '--upper')
        upperbound=$2
        shift 2
        continue
        ;;
    '-l' | '--lower')
        lowerbound=$2
        shift 2
        continue
        ;;
    '-n' | '--nval')
        nval=$2
        shift 2
        continue
        ;;
    '-r' | '--runs')
        runs=$2
        shift 2
        continue
        ;;
    '--')
        shift
        break
        ;;
    '-h' | '--help')
        echo -e "$USAGE"
        exit 1
        ;;
    *)
        echo -e "Wrong args"
        echo -e "$USAGE"
        exit 1
        ;;
    esac
done

if [[ -f $OUTFILE ]]; then
    rm -i $OUTFILE
fi

# Time per step test
step=$(echo "($upperbound - $lowerbound) / $nval" | bc -l)
echo "step of $step"
for j in $(seq 0 $runs); do
    suboutfile="$TMPD/$OUTFILE.$j"
    touch "$suboutfile"
    for i in $(seq 0 $nval); do
        printf "$j/$runs-$i/$nval\r"
        val=$(echo "$lowerbound + $i * $step" | bc -l)
        #echo "$MCTS_CMD -nsteps $DEPTH -horizon $DEPTH -timeperstep $val -scenario $SCENARIO"
        cost=$($MCTS_CMD -nsteps $DEPTH -horizon $DETPH -timeperstep $val -scenario $SCENARIO)
        #echo "$cost"
        echo "$val $cost" >>"$suboutfile"
    done
done

# keep only 2nd column
for f in $TMPD/$OUTFILE.*; do
    cut -d ' ' -f 2 $f >$f.c
done
# backup 1st column
cut -d ' ' -f 1 $TMPD/$OUTFILE.0 >$TMPD/abs.data
# Backup parameters
echo "# scen=$SCENARIO depth=$DEPTH runs=$runs nval=$nval" >>$OUTFILE
echo "# lowerbound=$lowerbound upperbound=$upperbound" >>$OUTFILE

# Astar computation
astarcost=$($ASTAR_CMD -horizon $DEPTH -scenario $SCENARIO)
echo "# $astarcost" >>"$OUTFILE"
# agglomerate everything
paste $TMPD/abs.data $TMPD/$OUTFILE.*.c >>"$OUTFILE"
rm -r $TMPD
#gnuplot -p -e "plot \"$OUTFILE\" with lines"
