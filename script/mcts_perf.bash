#!/bin/bash

NAME=$0
OPTIONS=hu:l:n:o:s:d:
LONGOPTIONS=help,scenario:,depth:,upper:,lower:,nval:,out:
USAGE="Usage: $0 -s <scenario> -d <depth>
Param:
\t-l|--lower\t<float>\tlower bound of time per step
\t-u|--upper\t<float>\tupper bound of time per step
\t-n|--nval\t<int>\tnumber of measures
\t-o|--out\t<filepath>\toutput file"

SCENARIO=""
DETPH=10
OUTFILE='tps.data'
lowerbound=0.0001
upperbound=0.1
nval=10

MCTS_CMD="airmcts"

temp=$(getopt -o $OPTIONS --long $LONGOPTIONS -n $NAME -- "$@")

if [[ $? -ne 0 ]]; then
    echo "Terminating..." >&2
    exit 1
fi

eval set -- "$temp"
unset temp
while true; do
    case "$1" in
        '-s'|'--scenario')
            SCENARIO=$2
            shift 2
            continue
            ;;
        '-d'|'--depth')
            DEPTH=$2
            shift 2
            continue
            ;;
        '-o'|'--out')
            OUTFILE=$2
            shift 2
            continue
            ;;
        '-u'|'--upper')
            upperbound=$2
            shift 2
            continue
            ;;
        '-l'|'--lower')
            lowerbound=$2
            shift 2
            continue
            ;;
        '-n'|'--nval')
            nval=$2
            shift 2
            continue
            ;;
        '--')
            shift
            break
            ;;
        '-h'|'--help')
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

if [[ -f $OUTFILE ]] ; then
    rm -i $OUTFILE
fi

# Time per step test
step=$( echo "($upperbound - $lowerbound) / $nval" | bc -l )
echo "step of $step"
for i in $(seq 0 $nval) ; do
    printf "$i/$nval\r"
    val=$( echo "$lowerbound + $i * $step" | bc -l )
    #echo "$MCTS_CMD -nsteps $DEPTH -horizon $DEPTH -timeperstep $val -scenario $SCENARIO"
    cost=$( $MCTS_CMD -nsteps $DEPTH -horizon $DETPH -timeperstep $val -scenario $SCENARIO )
    #echo "$cost"
    echo "$val $cost" >> $OUTFILE
done;
gnuplot -p -e "plot \"$OUTFILE\" with lines"
