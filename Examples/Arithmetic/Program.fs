open Cronyx.Actions

type AddEvent = { Amount : int }
type SubEvent = { Amount : int }

type Event =
    | Add of AddEvent
    | Sub of SubEvent

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

let add5 = AddAction(5)
let sub3 = SubAction(3)

let (s1, e1) = add5.Invoke(0, invertAndDouble)
let (s2, e2) = sub3.Invoke(s1, invertAndDouble)

printfn "Final state: %d" s2
printfn "Events: %A" (e1 @ e2)