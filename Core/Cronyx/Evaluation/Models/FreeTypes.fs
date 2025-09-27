namespace Cronyx.Evaluation.Models

module FreeTypes =

    open Types

    let rec freeTypeVarsMono (mt: MonoType) : Set<string> =
        match mt with
        | TypeVar v -> Set.singleton v
        | TypeFunctionApplication tf ->
            match tf with
            | Arrow (arg, ret) -> Set.union (freeTypeVarsMono arg) (freeTypeVarsMono ret)
            | List elem -> freeTypeVarsMono elem
            | PrimitiveType _ -> Set.empty

    let rec freeTypeVarsPoly (pt: PolyType) : Set<string> =
        match pt with
        | Mono mt -> freeTypeVarsMono mt
        | ForAll (var, inner) -> Set.remove var (freeTypeVarsPoly inner)

    let freeTypeVarsContext (ctx: Context) : Set<string> =
        ctx
        |> Map.toSeq
        |> Seq.map (fun (_, pt) -> freeTypeVarsPoly pt)
        |> Set.unionMany
