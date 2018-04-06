#!/bin/bash
_comp_airmcts ()
{
	local cur prev options
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD - 1]}"

	options="-scenario -verbose -alpha -beta -gamma -delta -theta -timeperstep \
		-nsteps -horizon"
	case "$prev" in
		"-scenario")
			COMPREPLY=( $(compgen -f -- ${cur}) )
			return 0
			;;
		*)
			COMPREPLY=( $(compgen -W "${options}" -- ${cur}) )
			return 0
			;;
	esac
}
complete -o bashdefault -o filenames -F _comp_airmcts airmcts.native
complete -o bashdefault -o filenames -F _comp_airmcts astair.native
complete -o bashdefault -o filenames -F _comp_airmcts dijkstra.native
complete -o bashdefault -o filenames -F _comp_airmcts airmcts.byte
complete -o bashdefault -o filenames -F _comp_airmcts astair.byte
complete -o bashdefault -o filenames -F _comp_airmcts dijkstra.byte
