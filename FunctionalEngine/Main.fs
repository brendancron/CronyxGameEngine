open Pokemon.Effects

type PlayerId = string
type State    = Map<PlayerId,int>

type Effect =
  | Damage of player: PlayerId * amount: int
  | Heal   of player: PlayerId * amount: int

type Event = 
  | DamageResolved of player: PlayerId * amount: int
  | HealResolved of player: PlayerId * amount: int

let validate_effect (effect: Effect) (state: State) : bool =
  let playerExists pid = Map.containsKey pid state
  match effect with
  | Damage (pid, amt)
  | Heal   (pid, amt) ->
      playerExists pid && amt >= 0

let apply_effect (effect: Effect) (state: State) : State * Event =
  match effect with
  | Damage (pid, amt) ->
      // safe because we assume validate_effect was true
      let cur = state[pid]
      state.Add(pid, cur - amt), DamageResolved(pid, amt)
  | Heal (pid, amt) ->
      let cur = state[pid]
      state.Add(pid, cur + amt), HealResolved(pid, amt)

// Provide a utility method to reduce verbosity
let curry_eval = eval_effect validate_effect apply_effect

// MONAD
let bind (res: EffectResult<'state,'event>)
         (next: 'state -> EffectResult<'state,'event>)
         : EffectResult<'state,'event> =
    match res with
    | InvalidChain -> InvalidChain
    | ValidChain (s, evs) ->
        match next s with
        | InvalidChain -> InvalidChain
        | ValidChain (s2, evs2) -> ValidChain (s2, evs @ evs2)

let runChain modifiers triggers state effects =
    effects
    |> List.fold (fun acc eff ->
        bind acc (fun s -> curry_eval modifiers triggers s eff)
    ) (ValidChain (state, []))

// Modifier Definitions

type HealingScale(id: string, target: string, percent: float) =
    interface IEffectModifier<Effect> with
        member _.Id = id
        member _.Modify(effect: Effect) =
            match effect with
            | Heal (player, amt) when player = target -> Heal (player, int(float(amt) * percent)), []
            | other    -> other, []

type GlobalHealingScale(id: string, percent: float) =
    interface IEffectModifier<Effect> with
        member _.Id = id
        member _.Modify(effect: Effect) =
            match effect with
            | Heal (player, amt) -> Heal (player, int(float(amt) * percent)), []
            | other    -> other, []

type SiphonHeal(id: string, target: string, source: string, percent: float) =
    interface IEffectModifier<Effect> with
        member _.Id = id
        member _.Modify(effect: Effect) =
            match effect with
            | Heal (player, amt) when player = target -> Heal (target, int(float(amt) * (1.0 - percent))), [Heal (source, int(float(amt) * percent))]
            | other    -> other, []

type ReflectDamage(id: string, target: string, source: string, percent: float) =
    interface IEventTrigger<Effect, Event> with
        member _.Id = id
        member _.OnEvent event = 
            match event with
            | DamageResolved (pid, amt) when pid = source -> [Damage(target, int(float(amt) * percent))]
            | _ -> []

// Modifier Construction

let siphonHeal = SiphonHeal("siphon", "Alice", "Bob", 0.25)
let reflectDamage = ReflectDamage("reflect", "Alice", "Bob", 0.5)
let reflectDamage2 = ReflectDamage("reflect2", "Bob", "Alice", 0.5)

// State Definition

let state : State =
    [ "Alice", 100
      "Bob"  , 100 ]
    |> Map.ofList

// Effect Construction

let effect1 = Heal("Alice", 30)
let effect2 = Damage("Bob", 50)

match runChain [siphonHeal] [reflectDamage; reflectDamage2] state [effect1; effect2] with
| InvalidChain -> printfn "INVALID"
| ValidChain (s, evs) ->
    printfn "Final: %A" s
    printfn "Events: %A" evs