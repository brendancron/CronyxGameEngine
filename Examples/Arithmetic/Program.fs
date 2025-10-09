open Cronyx.Actions
open Cronyx.Events

type AddEvent = { Amount : int }
type SubEvent = { Amount : int }

type Event =
    | Add of AddEvent
    | Sub of SubEvent

let unwrap ev =
    match ev with
    | Add e -> box e
    | Sub e -> box e

type AddAction(amount: int) =
    inherit ActionBase<int, Event>()
    member _.Amount = amount
    override this.InvokeImpl(state) =
        let newState = state + this.Amount
        let events = [ Add { Amount = this.Amount } ]
        (newState, events)


type SubAction(amount: int) =
    inherit ActionBase<int, Event>()
    member _.Amount = amount
    override this.InvokeImpl(state) =
        let newState = state - this.Amount
        let events = [ Sub { Amount = this.Amount } ]
        (newState, events)

let invertAndDouble (action: ActionBase<int, Event>) : ActionBase<int, Event> =
    match action with
    | :? SubAction as sub ->
        let subAmount = sub.Amount
        AddAction(subAmount * 2)
    | _ ->
        action

type AddObserver() =
    interface IEventObserver<AddEvent> with
        member _.OnEvent e = printfn "Add %d" e.Amount

type SubObserver() =
    interface IEventObserver<SubEvent> with
        member _.OnEvent e = printfn "Sub %d" e.Amount

let observers : IAnyObserver list =
    [ Observer(AddObserver())
      Observer(SubObserver()) ]

let add5 = AddAction(5)
let sub3 = SubAction(3)

let dispatchEvent (events: Event list) =
    dispatch observers (List.map unwrap events)

let (s1, e1) = add5.Invoke(0, invertAndDouble, dispatchEvent)
let (s2, e2) = sub3.Invoke(s1, invertAndDouble, dispatchEvent)

printfn "Final state: %d" s2
printfn "Events: %A" (e1 @ e2)