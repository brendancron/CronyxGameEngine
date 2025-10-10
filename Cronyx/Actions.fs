namespace Cronyx

module Actions =

    open Modifiers

    let traverse
        (traversal_strategy: 'a list -> 'a list -> 'a list)
        (expand: 's -> 'a -> 's * 'a list * 'e list)
        (initialNode: 'a)
        (initialState: 's)
        : 's * 'e list =

        let rec loop state queue accEvents =
            match queue with
            | [] -> (state, List.rev accEvents)
            | node::rest ->
                let newState, children, events = expand state node
                let newQueue = traversal_strategy rest children
                loop newState newQueue (Util.revAppend events accEvents)

        loop initialState [initialNode] []

    // default node traversal strategies

    let bfs rest children = rest @ children
    let dfs rest children = children @ rest

    let private invoke
        (state: 's)
        (action: 'a)
        (modify: 's -> 'a -> 'a)
        (invokeImpl: 'a -> 's -> 's * 'e list)
        (process_events: 'e list -> 'a list)
        (traversal_strategy: 'a list -> 'a list -> 'a list) 
        : 's * 'e list =

        let expand state action =
            let modifiedAction = modify state action
            let newState, events = invokeImpl modifiedAction state
            let newActions = process_events events
            (newState, newActions, events)
        
        traverse traversal_strategy expand action state

    [<AbstractClass>]
    type ActionBase<'state, 'event>() =
        abstract member InvokeImpl : 'state -> 'state * 'event list
        member this.Invoke(
            initialState: 'state,
            modifier: Modifier<'state, ActionBase<'state, 'event>>,
            process_events : 'event list -> ActionBase<'state,'event> list,
            traversal_strategy) =
            
            invoke
                initialState
                this
                (fun s a -> modifier |> Modifier.modify s a)
                (fun a s -> this.InvokeImpl s)
                process_events
                traversal_strategy