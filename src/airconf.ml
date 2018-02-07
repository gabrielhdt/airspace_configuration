type t = {
  timestep : int ; (* Used to determine whether the node is terminal *)
}

let dummy = { timestep = 0 }

let confcost t = 0.

let produce t = []

let terminal t = true
