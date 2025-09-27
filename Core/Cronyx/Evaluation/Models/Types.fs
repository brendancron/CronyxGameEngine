namespace Cronyx.Evaluation.Models

module Types =

    type PrimitiveType =
        | Bool
        | Int
        | String
        | Float

    type TypeFunction =
        | Arrow of MonoType * MonoType
        | List of MonoType
        | PrimitiveType of PrimitiveType

    and MonoType = 
        | TypeVar of string
        | TypeFunctionApplication of TypeFunction

    type PolyType = 
        | Mono of MonoType
        | ForAll of string * PolyType

    type Context = Map<string, PolyType>