namespace Cronyx

module Events =
    
    type IEventObserver<'Event> =
        abstract member OnEvent : 'Event -> unit

    type IAnyObserver =
        abstract member TryHandle : obj -> bool

    type Observer<'E>(impl: IEventObserver<'E>) =
        interface IAnyObserver with
            member _.TryHandle(evtObj: obj) =
                match evtObj with
                | :? 'E as e -> impl.OnEvent e; true
                | _ -> false

    let dispatch (observers: IAnyObserver list) (events: obj list) =
        for evt in events do
            for obs in observers do
                obs.TryHandle(evt) |> ignore