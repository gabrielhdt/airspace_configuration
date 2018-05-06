# Dynamic airspace configuration using Monte Carlo Tree Search

## Requirements
### Ocaml
* Yojson
* Str

### scripts
* bc

## Binaries
Typing `make` generates several binaries, which are
* airmcts: the core of this project, the monte carlo tree search algorithm
  applied to dynamic airspace configuration
* astair: an A star algorithm coded during ocaml lessons at enac applied to
  our problem
* astar: another A star implementation, much slower
* dijkstra: the dijkstra algorithm applied to our problem

Each binary takes at least as argument
* the scenario file with `-scenario <scenario file>`, which is a json file
  containing traffic data for each elementary module of a sector,
* the horizon, i.e. how many timesteps in the scenarion file to consider with
  `-horizon <positive int>`, beware of not putting a horizon larger than the
  number of timesteps in the scenario file (else it will fail with `List.nth`)
