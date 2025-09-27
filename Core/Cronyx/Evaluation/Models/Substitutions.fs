namespace Cronyx.Evaluation.Models

open Cronyx.Evaluation.Models.Types
open Cronyx.Evaluation.Models.FreeTypes

module Substitutions =

    /// Substitution: maps type variables to monotypes
    type Substitution = Map<string, MonoType>

    // ---- MonoType substitution ----
    let rec applySubstitutionToMonoType (substitution: Substitution) (mt: MonoType) : MonoType =
        match mt with
        | TypeVar v ->
            match Map.tryFind v substitution with
            | Some replacement -> replacement
            | None -> TypeVar v

        | TypeFunctionApplication tfApp ->
            let rec applyToTypeFunction tf =
                match tf with
                | Arrow (arg, ret) ->
                    let newArg = applySubstitutionToMonoType substitution arg
                    let newRet = applySubstitutionToMonoType substitution ret
                    Arrow (newArg, newRet)

                | List elemType ->
                    let newElem = applySubstitutionToMonoType substitution elemType
                    List newElem

                | PrimitiveType p ->
                    PrimitiveType p  // primitives are leaves, no recursion
            TypeFunctionApplication (applyToTypeFunction tfApp)

    // ---- PolyType substitution ----
    and applySubstitutionToPolyType (substitution: Substitution) (pt: PolyType) : PolyType =
        match pt with
        | Mono mt ->
            let newMt = applySubstitutionToMonoType substitution mt
            Mono newMt
        | ForAll (var, innerPt) ->
            // Remove shadowed variable from substitution so it won’t be replaced
            let newSub = Map.remove var substitution
            let newInner = applySubstitutionToPolyType newSub innerPt
            ForAll (var, newInner)

    // ---- Context substitution ----
    let applySubstitutionToContext (substitution: Substitution) (ctx: Context) : Context =
        ctx
        |> Map.map (fun _ polyType -> applySubstitutionToPolyType substitution polyType)

    let combine (s1: Substitution) (s2: Substitution) : Substitution =
        // Apply s1 to all types in s2, then merge
        let s2Applied =
            s2
            |> Map.map (fun _ mt -> applySubstitutionToMonoType s1 mt)
        Map.fold (fun acc k v -> Map.add k v acc) s1 s2Applied

    let generalize (ctx: Context) (mt: MonoType) : PolyType =
        let ctxFvs = freeTypeVarsContext ctx
        let mtFvs  = freeTypeVarsMono mt
        let generalized = Set.difference mtFvs ctxFvs
        // set first, then initial state
        Set.foldBack (fun v acc -> ForAll (v, acc)) generalized (Mono mt)

    let rec occurs (v: string) (mt: MonoType) : bool =
        match mt with
        | TypeVar v' -> v = v'
        | TypeFunctionApplication tf ->
            match tf with
            | Arrow (arg, ret) -> occurs v arg || occurs v ret
            | List elem -> occurs v elem
            | PrimitiveType _ -> false

    exception UnificationError of string

    let rec unify (t1: MonoType) (t2: MonoType) : Substitution =
        match t1, t2 with
        // identical vars → empty substitution
        | TypeVar v1, TypeVar v2 when v1 = v2 -> Map.empty

        // variable vs type
        | TypeVar v, t
        | t, TypeVar v ->
            if occurs v t then
                raise (UnificationError (sprintf "Occurs check failed: %s in %A" v t))
            else
                Map.ofList [(v, t)]

        // Arrow vs Arrow
        | TypeFunctionApplication (Arrow (arg1, ret1)),
            TypeFunctionApplication (Arrow (arg2, ret2)) ->
            let s1 = unify arg1 arg2
            let ret1' = applySubstitutionToMonoType s1 ret1
            let ret2' = applySubstitutionToMonoType s1 ret2
            let s2 = unify ret1' ret2'
            combine s2 s1

        // List vs List
        | TypeFunctionApplication (List elem1),
            TypeFunctionApplication (List elem2) ->
            unify elem1 elem2

        // Primitive vs Primitive
        | TypeFunctionApplication (PrimitiveType p1),
            TypeFunctionApplication (PrimitiveType p2) when p1 = p2 ->
            Map.empty

        // mismatch
        | _ ->
            raise (UnificationError (sprintf "Cannot unify %A with %A" t1 t2))

    type State<'s,'a> = State of ('s -> 'a * 's)

    module State =
        let run (State f) s = f s
        let returnM x = State (fun s -> (x, s))
        let bind (State m) f =
            State (fun s ->
                let (a, s') = m s
                let (State m') = f a
                m' s')

        type StateBuilder() =
            member _.Return(x) = returnM x
            member _.Bind(m, f) = bind m f
            member _.ReturnFrom(m) = m

        let state = StateBuilder()

        let eval (State f) s =
            let (a, _) = f s
            a

        let exec (State f) s =
            let (_, s') = f s
            s'

        let freshTypeVar : State<int, MonoType> =
            State (fun counter ->
                let name = sprintf "t%d" counter
                (TypeVar name, counter + 1))

        let rec instantiate (pt: PolyType) : State<int, MonoType> =
            state {
                match pt with
                | Mono mt -> return mt
                | ForAll (var, innerPt) ->
                    // get a fresh type var
                    let! freshVar = freshTypeVar
                    let subst: Substitution = Map.ofList [(var, freshVar)]
                    let replacedPt = applySubstitutionToPolyType subst innerPt
                    return! instantiate replacedPt
            }