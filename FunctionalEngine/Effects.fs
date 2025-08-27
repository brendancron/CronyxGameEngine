namespace Pokemon

module Effects = 
    type State = int

    type IEffectModifier<'eff> =
        abstract member Id : string
        abstract member Modify: 'eff -> 'eff * 'eff list

    type IEventTrigger<'eff,'event> =
        abstract member Id : string
        abstract member OnEvent : 'event -> 'eff list

    type TaggedEffect<'eff> = {
        Effect : 'eff
        Provenance : Set<string> }

    let fold_modifiers modifiers tagged = 
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

    let trigger_spawns (triggers: IEventTrigger<'eff,'event> list) (origin: TaggedEffect<'eff>) (event:'event) =
        triggers
        |> List.collect (fun t ->
            if origin.Provenance.Contains t.Id then []
            else
                t.OnEvent event
                |> List.map (fun e ->
                    { Effect = e; Provenance = origin.Provenance.Add t.Id }))

    type StepResult<'state, 'eff, 'event> = 
        | InvalidStep
        | ValidStep of state:'state * event:'event * modifierSpawns:TaggedEffect<'eff> list * triggerSpawns:TaggedEffect<'eff> list

    type EffectResult<'state, 'event> = 
        | InvalidChain
        | ValidChain of state:'state * event:'event list

    let private eval_one<'state, 'eff, 'event>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event)
        (modifiers: IEffectModifier<'eff> list)
        (triggers: IEventTrigger<'eff,'event> list)
        (state: 'state)
        (tagged: TaggedEffect<'eff>)
        : StepResult<'state, 'eff, 'event> = 
        // Step 1: Validate the effect
        if not (validate_effect tagged.Effect state) then InvalidStep
        else 
            // Step 2: Apply Modifiers to the effect
            let modifiedEffect, modifierSpawns = fold_modifiers modifiers tagged
            // Step 3: Actually apply the effect
            let state', event = apply_effect modifiedEffect.Effect state
            let triggerSpawns = trigger_spawns triggers tagged event
            ValidStep(state', event, modifierSpawns, triggerSpawns)

    let eval_all_bfs<'eff, 'state, 'event>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event)
        (modifiers: IEffectModifier<'eff> list)
        (triggers: IEventTrigger<'eff,'event> list)
        (initialState : 'state)
        (initialEffect : 'eff)
        : EffectResult<'state, 'event> =
        
        let rec loop (state: 'state) (front:TaggedEffect<'eff> list) (back:TaggedEffect<'eff> list) (accEventRev: 'event list) =
            match front with
            | effect::front' ->
                match eval_one validate_effect apply_effect modifiers triggers state effect with
                | InvalidStep ->
                    loop state front' back accEventRev
                | ValidStep (state', event, modifierSpawns, triggerSpawns) ->
                    // enqueue spawns at the end
                    let front'' = triggerSpawns @ front'
                    let back' = List.foldBack (fun x acc -> x::acc) modifierSpawns back
                    loop state' front'' back' (event::accEventRev)
            | [] ->
                match back with
                | [] -> ValidChain(state, List.rev accEventRev)
                | backLst -> loop state backLst [] accEventRev

        let initial = [{ Effect = initialEffect; Provenance = Set.empty }]
        loop initialState initial [] []

    let eval_effect<'eff, 'state, 'event>
        (validate_effect: 'eff -> 'state -> bool)
        (apply_effect: 'eff -> 'state -> 'state * 'event)
        (modifiers: IEffectModifier<'eff> list)
        (triggers: IEventTrigger<'eff,'event> list)
        (initialState : 'state)
        (effect : 'eff)
        : EffectResult<'state, 'event> =
        eval_all_bfs validate_effect apply_effect modifiers triggers initialState effect