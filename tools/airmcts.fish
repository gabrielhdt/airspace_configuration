#!/usr/bin/fish

complete -c airmcts -o scenario --description \
"Path to scenario file"
complete -c airmcts -o maxtime --description \
"Amount of time during which the program will run"
complete -c airmcts -o expvexp --description \
"Tradeoff between exploration and exploitation"
complete -c airmcts -o alpha --description \
"Controls influence of overload when choosing a partition"
complete -c airmcts -o beta --description \
"Controls influence of normal load"
complete -c airmcts -o gamma --description \
"Controls influence of underload"
complete -c airmcts -o lambda --description \
"Controls influence of amount of open positions"
complete -c airmcts -o theta --description \
"Sets cost associated to variations of partitions"
complete -c airmcts -o verbose --description "Verbose mode"
