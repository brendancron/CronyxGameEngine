namespace Pokemon

module Types =
    
    type Type = 
        | Fire
        | Water
        | Earth

    let effectiveness useType againstType =
        match useType, againstType with
        | Fire, Earth -> 2.0
        | Fire, Water -> 0.5
        | Fire, Fire -> 1.0
        | Water, Fire -> 2.0
        | Water, Earth -> 0.5
        | Water, Water -> 1.0
        | Earth, Water -> 2.0
        | Earth, Fire -> 0.5
        | Earth, Earth -> 1.0

    let multi_effectiveness (useType: Type) (againstTypes: Type list) =
        List.fold (fun acc elem -> acc * effectiveness useType elem) 1.0 againstTypes