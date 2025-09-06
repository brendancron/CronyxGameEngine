namespace Cronyx.Core

module Effects = 

    let applyModifiers
        (modifiers : IEffectModifier<'eff> list)
        (effect : 'eff)
        (provenance : Set<string> list)
        : 'eff * Set<string> list =
        modifiers
        |> List.fold (fun (eff, prov) (m: IEffectModifier<'eff>) ->
            if Env.containsProvenance m.Id prov then
                eff, prov
            else
                let eff' = m.Modify eff
                eff', Env.addProvenanceHelper m.Id prov
        ) (effect, provenance)


    let applyTriggers
        (triggers: IEventTrigger<'eff,'event,'state> list)
        (events: 'event list)
        : (IStmt<'eff,'event,'state> * string) list =

        events
        |> List.collect (fun ev ->
            triggers
            |> List.choose (fun (t: IEventTrigger<'eff,'event,'state>) ->
                match t.OnEvent ev with
                | Some stmt -> Some (stmt, t.Id)
                | None      -> None))