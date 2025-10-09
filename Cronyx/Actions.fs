namespace Cronyx

module Actions =

    open Modifiers

    [<AbstractClass>]
    type ActionBase<'state, 'event>() =
        abstract member InvokeImpl : 'state -> 'state * 'event list
        member this.Invoke(
            state: 'state,
            modifier: Modifier<'state, ActionBase<'state, 'event>>,
            dispatch: 'event list -> unit) =
            let modifiedAction = modifier |> Modifier.modify state this
            let newState, events = modifiedAction.InvokeImpl state
            dispatch events
            (newState, events)