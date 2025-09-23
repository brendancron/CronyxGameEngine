namespace Cronyx.Evaluation

module Environment =
    
    (* 
        Environment Definition

        Scopes - Allows for lexical scoping throughout interpretation

        GameState - Holds the current gamestate

        Trace - Provides a list of all events that occurred during exectuion, useful for animation/logging

        Provenance - Allows for a specialized trace of effect modification to prevent infinite loops
    *)

    type Env<'state, 'event> = {
        Scopes: Map<string, obj> list
        GameState: 'state
        Trace: 'event list
        Provenance: Set<string>
    }

    module Env =
        let intial (initialState : 'state) : Env<'state, 'event> =
            { Scopes = [ Map.empty ]
              GameState = initialState
              Trace = []
              Provenance = Set.empty }

        let push (env: Env<'state, 'event>) =
            { env with Scopes = Map.empty :: env.Scopes }

        let pop (env: Env<'state, 'event>) =
            match env.Scopes with
            | []        -> failwith "No scope to pop"
            | _ :: rest -> { env with Scopes = rest }

        let define name value (env: Env<'state, 'event>) =
            match env.Scopes with
            | [] -> failwith "No scope to define in"
            | scope :: rest ->
                { env with Scopes = (scope.Add(name, value)) :: rest }

        let rec assign name value (env: Env<'state, 'event>) =
            match env.Scopes with
            | [] -> failwithf "Undefined variable '%s'" name
            | scope :: rest when scope.ContainsKey name ->
                { env with Scopes = (scope.Add(name, value)) :: rest }
            | scope :: rest ->
                let updated = assign name value { env with Scopes = rest }
                { env with Scopes = scope :: updated.Scopes }

        let rec get name (env: Env<'state, 'event>) =
            match env.Scopes with
            | [] -> failwithf "Undefined variable '%s'" name
            | scope :: rest ->
                match scope.TryFind name with
                | Some v -> v
                | None   -> get name { env with Scopes = rest }