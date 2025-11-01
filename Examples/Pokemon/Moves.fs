namespace Pokemon

open Mons
open Types

module Moves =

    let atkModifier (user: Mon) (target: Mon) : float =
        ((float)user.species.atk) / ((float)target.species.def)

    let damage_move (move_type: Type) (power: float) (user: Mon) (target: Mon) =
        let diff: float = atkModifier user target
        let effectiveness: float = multi_effectiveness move_type target.species.types
        let finalPower = power * diff * effectiveness
        user, { target with currentHP = target.currentHP - ((int)finalPower) }
    
    let flamethrower = damage_move Fire 30.0
