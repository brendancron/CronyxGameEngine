namespace Cronyx

module Events =
    
    type IEventObserver<'Event> =
        abstract member OnEvent : 'Event -> unit

    type IAnyObserver =
        abstract member TryHandle : obj -> bool

    type Observer<'E>(impl: IEventObserver<'E>) =
        interface IAnyObserver with
            member _.TryHandle(evtObj: obj, state: State) =
                match evtObj with
                | :? 'E as e -> impl.OnEvent e state; true
                | _ -> false