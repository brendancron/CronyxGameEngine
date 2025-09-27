namespace Cronyx.Evaluation

open System
open System.Reflection

module Resolver =

let reflectiveResolver<'s,'e,'ev>
    (call : EffectCall<'s,'e,'ev>)
    (env  : Env<'s,'ev>)
    : 'e =

    // Step 1. Evaluate argument expressions
    let argValues =
        call.Args
        |> List.map (fun arg -> (arg :?> IExpr<obj,'s,'e,'ev>).Eval env)

    // Step 2. Find a type whose name matches the call.Name
    // Assume effects live in the same assembly as 'e
    let effectType =
        typeof<'e>.Assembly.GetTypes()
        |> Array.tryFind (fun t -> t.Name = call.Name)

    match effectType with
    | None ->
        failwithf "Unknown effect: %s" call.Name
    | Some t ->
        // Step 3. Find constructor by argument count
        let ctors = t.GetConstructors()
        let ctor =
            ctors
            |> Array.tryFind (fun c ->
                let parms = c.GetParameters()
                parms.Length = argValues.Length &&
                Array.forall2 (fun (p:ParameterInfo) (arg:obj) ->
                    p.ParameterType.IsAssignableFrom(arg.GetType())
                ) parms (argValues |> List.toArray)
            )
        match ctor with
        | None ->
            failwithf "No matching constructor for %s(%A)" call.Name argValues
        | Some c ->
            // Step 4. Invoke constructor
            c.Invoke(argValues |> List.toArray) :?> 'e
