namespace Pokemon

module Mons =
   
    type Mon = { 
        species: Species
        maxHP: int
        currentHP: int
    }

    let init (species:Species) =
        {
            species = species
            maxHP = species.hp
            currentHP = species.hp
        }
