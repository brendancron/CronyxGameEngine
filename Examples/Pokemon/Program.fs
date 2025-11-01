open Pokemon

open Types
open Mons
open Moves
open States

let squirtle: Species = {
    name = "squirtle"
    hp = 100
    atk = 100
    def = 100
    types = [ Water ]
}

let charmander: Species = {
    name = "charmander"
    hp = 100
    atk = 100
    def = 100
    types = [ Fire ]
}

let applyMoveAction moveAction userId targetId state =
    match Map.tryFind userId state, Map.tryFind targetId state with
    | Some user, Some target -> 
        let u', t' = moveAction user target
        state
        |> Map.add userId u'
        |> Map.add targetId t'
    | _ -> state

let state : State = 
    Map.ofList [
        ("bubbles", init squirtle)
        ("scorch", init charmander)
    ]

let state' = applyMoveAction flamethrower "scorch" "bubbles" state

printfn "%A" state
printfn "%A" state'
