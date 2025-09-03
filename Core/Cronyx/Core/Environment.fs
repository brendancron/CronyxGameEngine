namespace Cronyx.Core

module Env =

    /// Create an empty lexical environment with an initial game state
    let empty (gameState : 'state) : Env<'eff,'event,'state> =
        { Scopes = [ Map.empty ]
          GameState = gameState
          Trace = []
          Provenance = [ Set.empty ] }

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

    let pushProvenance (env: Env<'eff,'event,'state>) =
        { env with Provenance = Set.empty :: env.Provenance }

    let popProvenance (env: Env<'eff,'event,'state>) =
        match env.Provenance with
        | [] -> failwith "No provenance to pop"
        | _ :: rest -> { env with Provenance = rest }

    let addProvenanceHelper id (provenance: Set<string> list) =
        match provenance with
        | [] -> failwith "No provenance scope"
        | prov :: rest ->
            (prov.Add id) :: rest

    let addProvenance id (env: Env<'eff,'event,'state>) =
        { env with Provenance = (addProvenanceHelper id env.Provenance) }

    let containsProvenance id (provenance: Set<string> list) =
        provenance |> List.exists (fun scope -> scope.Contains id)