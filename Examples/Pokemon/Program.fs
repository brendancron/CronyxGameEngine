open Pokemon

open Types
open Mons
open Moves

let squirtle: Species = {
    name = "squirtle"
    hp = 44
    atk = 50
    def = 65
    types = [ Water ]
}

let charmander: Species = {
    name = "charmander"
    hp = 44
    atk = 50
    def = 65
    types = [ Fire ]
}

let bubbles = init squirtle
let scorch = init charmander

let s', b' = flamethrower { user = scorch; target = bubbles }

printfn "%A" bubbles 
printfn "%A" scorch

printfn "%A" b' 
printfn "%A" s' 
