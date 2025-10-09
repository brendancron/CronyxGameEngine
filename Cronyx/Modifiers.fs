namespace Cronyx

module Modifiers = 

    type Modifier<'s, 'a> =
        | Static of ('a -> 'a)
        | Dynamic of ('s -> 'a -> 'a)

    module Modifier =
        let modify state value =
            function
            | Static f -> f value
            | Dynamic f -> f state value

        let ofStatic f = Static f
        let ofDynamic f = Dynamic f