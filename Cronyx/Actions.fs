namespace Cronyx

module Actions =

    [<AbstractClass>]
    type ActionBase<'state, 'event>() =
        abstract member InvokeImpl : 'state -> 'state * 'event list
        member this.Invoke(state: 'state, modify: ActionBase<'state, 'event> -> ActionBase<'state, 'event>) =
            let modified = modify this
            modified.InvokeImpl state