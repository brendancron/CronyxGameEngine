namespace Pokemon

open Pokemon.Types

(*
    Contains data about the species itself, not about individuals
*)

type Species = {
    name: string
    hp: int
    atk: int
    def: int
    types: Type list
}
