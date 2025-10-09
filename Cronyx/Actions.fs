namespace Cronyx

module Actions =

    [<AbstractClass>]
    type ActionBase<'state, 'event>() =
        abstract member InvokeImpl : 'state -> 'state * 'event list
        member this.Invoke(
            state: 'state,
            modify: ActionBase<'state, 'event> -> ActionBase<'state, 'event>,
            dispatch: 'event list -> unit) =
            let modified = modify this
            let newState, events = modified.InvokeImpl state
            dispatch events
            (newState, events)