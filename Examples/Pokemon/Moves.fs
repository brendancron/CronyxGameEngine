namespace Pokemon

open Mons
open Types

module Moves =

    type SelfTarget = { user : Mon }
    type OtherTarget = { user : Mon ; target : Mon }

    type MoveTarget = 
    | Self of SelfTarget
    | Other of OtherTarget 

    let atkModifier (otherTarget: OtherTarget) : float =
        ((float)otherTarget.user.species.atk) / ((float)otherTarget.target.species.def)

    let damage_move (move_type: Type) (power: float) (targetOption: OtherTarget) =
        let diff: float = atkModifier targetOption
        let effectiveness: float = multi_effectiveness move_type targetOption.target.species.types
        let finalPower = power * diff * effectiveness
        targetOption.user, { targetOption.target with currentHP = targetOption.target.currentHP - ((int)finalPower) }
    
    let flamethrower = damage_move Fire 90.0
