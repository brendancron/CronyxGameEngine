namespace Cronyx

type Env =
    { Scopes : Map<string,obj> list }

module Env = 

    let empty = { Scopes = [ Map.empty ] }

    let push env =
        { Scopes = Map.empty :: env.Scopes }

    let pop env =
        match env.Scopes with
        | [] -> failwith "No scope to pop"
        | _::rest -> { Scopes = rest }

    let define name value env =
        match env.Scopes with
        | [] -> failwith "No scope to define in"
        | scope::rest ->
            { Scopes = (scope.Add(name, value)) :: rest }

    let rec assign name value env =
        match env.Scopes with
        | [] -> failwithf "Undefined variable '%s'" name
        | scope::rest when scope.ContainsKey name ->
            { Scopes = (scope.Add(name, value)) :: rest }
        | scope::rest ->
            let updated = assign name value { Scopes = rest }
            { Scopes = scope :: updated.Scopes }

    let rec get name env =
        match env.Scopes with
        | [] -> failwithf "Undefined variable '%s'" name
        | scope::rest ->
            match scope.TryFind name with
            | Some v -> v
            | None -> get name { Scopes = rest }