namespace Cronyx

open Cronyx.Effects

type Env<'eff,'event,'state when 'state :> IGameState<'eff,'event>> = {
    Scopes    : Map<string,obj> list
    GameState : 'state
    Trace     : 'event list
}

module Env =

    /// Create an empty lexical environment with an initial game state
    let empty (gameState : 'state) : Env<'eff,'event,'state> =
        { Scopes = [ Map.empty ]
          GameState = gameState
          Trace = [] }

    let push (env: Env<'eff,'event,'state>) =
        { env with Scopes = Map.empty :: env.Scopes }

    let pop (env: Env<'eff,'event,'state>) =
        match env.Scopes with
        | []        -> failwith "No scope to pop"
        | _ :: rest -> { env with Scopes = rest }

    let define name value (env: Env<'eff,'event,'state>) =
        match env.Scopes with
        | [] -> failwith "No scope to define in"
        | scope :: rest ->
            { env with Scopes = (scope.Add(name, value)) :: rest }

    let rec assign name value (env: Env<'eff,'event,'state>) =
        match env.Scopes with
        | [] -> failwithf "Undefined variable '%s'" name
        | scope :: rest when scope.ContainsKey name ->
            { env with Scopes = (scope.Add(name, value)) :: rest }
        | scope :: rest ->
            let updated = assign name value { env with Scopes = rest }
            { env with Scopes = scope :: updated.Scopes }

    let rec get name (env: Env<'eff,'event,'state>) =
        match env.Scopes with
        | [] -> failwithf "Undefined variable '%s'" name
        | scope :: rest ->
            match scope.TryFind name with
            | Some v -> v
            | None   -> get name { env with Scopes = rest }