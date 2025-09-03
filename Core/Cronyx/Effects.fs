namespace Cronyx

open Cronyx.Core

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
        (triggers: IEventTrigger<'eff,'event> list)
        (events: 'event list)
        : (IStmt<'eff,'event,'state> * string) list =

        events
        |> List.collect (fun ev ->
            triggers
            |> List.choose (fun (t: IEventTrigger<'eff,'event>) ->
                match t.OnEvent ev with
                | Some stmt -> Some (stmt, t.Id)
                | None      -> None))

(*

    let private fold_modifiers modifiers tagged = 
        let finalEff, spawnedRev =
            modifiers
            |> List.fold (fun (eff, accRev) (m: IEffectModifier<'eff>) ->
                if tagged.Provenance.Contains m.Id then eff, accRev
                else
                    let eff', spawned = m.Modify eff
                    let spawnedTagged =
                        spawned |> List.map (fun e ->
                            { Effect = e; Provenance = tagged.Provenance.Add m.Id })
                    eff', List.fold (fun acc x -> x :: acc) accRev spawnedTagged
            ) (tagged.Effect, [])
        { Effect = finalEff; Provenance = tagged.Provenance }, List.rev spawnedRev

    let private trigger_spawns (triggers: IEventTrigger<'eff,'event> list) (origin: TaggedEffect<'eff>) (events:'event list) =
        events
        |> List.collect (fun event ->
            triggers
            |> List.collect (fun t ->
                if origin.Provenance.Contains t.Id then []
                else
                    t.OnEvent event
                    |> List.map (fun e ->
                        { Effect = e; Provenance = origin.Provenance.Add t.Id })))

    type private StepResult<'state, 'eff, 'event> = 
        | InvalidStep
        | ValidStep of state:'state * events:'event list * modifierSpawns:TaggedEffect<'eff> list * triggerSpawns:TaggedEffect<'eff> list

    type EffectResult<'state, 'event> = 
        | InvalidChain
        | ValidChain of state:'state * events:'event list

    let private eval_one<'eff, 'event, 'state when 'state :> IGameState<'eff, 'event>>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event list)
        (state: 'state)
        (tagged: TaggedEffect<'eff>)
        : StepResult<'state, 'eff, 'event> = 
        // Step 1: Validate the effect
        if not (validate_effect tagged.Effect state) then InvalidStep
        else 
            let modifiers = state.Modifiers
            let triggers = state.Triggers
            // Step 2: Apply Modifiers to the effect
            let modifiedEffect, modifierSpawns = fold_modifiers modifiers tagged
            // Step 3: Actually apply the effect
            let state', events = apply_effect modifiedEffect.Effect state
            let triggerSpawns = trigger_spawns triggers tagged events
            ValidStep(state', events, modifierSpawns, triggerSpawns)

    type private ExecutionItem<'eff,'event,'state when 'state :> IGameState<'eff,'event>> =
        | EffectItem of TaggedEffect<'eff>
        | StatementItem of IStmt<'eff,'event,'state>

    let eval_hybrid_execution<'eff, 'event, 'state when 'state :> IGameState<'eff, 'event>>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event list)
        (initialState : 'state)
        (initialEffect : 'eff)
        : EffectResult<'state, 'event> =
    
        let rec loop (env: Env<'eff,'event,'state>) (queue: ExecutionItem<'eff,'event,'state> list) =
            match queue with
            | [] -> ValidChain(env.GameState, env.Trace)
    
            | EffectItem(taggedEffect) :: rest ->
                match eval_one validate_effect apply_effect env.GameState taggedEffect with
                | InvalidStep -> loop env rest
                | ValidStep (state', events, modifierSpawns, triggerSpawns) ->
                    // Collect triggered statements (filter out None values)
                    let triggeredStatements = 
                        events
                        |> List.collect (fun event ->
                            state'.Triggers
                            |> List.choose (fun trigger -> trigger.OnEvent event))
                        |> List.map StatementItem
            
                    let effectItems = (triggerSpawns @ modifierSpawns) |> List.map EffectItem
                    let newQueue = triggeredStatements @ effectItems @ rest
                    let newEnv = { env with GameState = state'; Trace = env.Trace @ events }
                    loop newEnv newQueue
            
            | StatementItem(stmt) :: rest ->
                let newEnv = stmt.Exec env
                loop newEnv rest
    
        let initialQueue = [EffectItem({ Effect = initialEffect; Provenance = Set.empty })]
        let initialEnv = { Scopes = [Map.empty]; GameState = initialState; Trace = [] }
        loop initialEnv initialQueue

    let private eval_all_bfs<'eff, 'event, 'state when 'state :> IGameState<'eff, 'event>>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event list)
        (initialState : 'state)
        (initialEffect : 'eff)
        : EffectResult<'state, 'event> =
        
        let rec loop (state: 'state) (front:TaggedEffect<'eff> list) (back:TaggedEffect<'eff> list) (accEventRev: 'event list) =
            match front with
            | effect::front' ->
                match eval_one validate_effect apply_effect state effect with
                | InvalidStep ->
                    loop state front' back accEventRev
                | ValidStep (state', events, modifierSpawns, triggerSpawns) ->
                    // enqueue spawns at the end
                    let front'' = triggerSpawns @ front'
                    let back' = List.foldBack (fun x acc -> x::acc) modifierSpawns back
                    loop state' front'' back' (events @ accEventRev)
            | [] ->
                match back with
                | [] -> ValidChain(state, List.rev accEventRev)
                | backLst -> loop state backLst [] accEventRev

        let initial = [{ Effect = initialEffect; Provenance = Set.empty }]
        loop initialState initial [] []

    let eval_effect<'eff, 'event, 'state when 'state :> IGameState<'eff, 'event>>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event list)
        (initialState : 'state)
        (effect : 'eff)
        : EffectResult<'state, 'event> =
        eval_all_bfs validate_effect apply_effect initialState effect
*)