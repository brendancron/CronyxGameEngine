namespace Cronyx

module Traversal = 
    
    // Abstracted Traversal method with generation-based tree expansion
    let traverse
        (traversal_strategy: 'a list -> 'a list -> 'a list)
        (expand: 's -> 'a -> 's * 'a list)
        (initialNode: 'a)
        (initialState: 's)
        : 's =

        let rec loop state queue =
            match queue with
            | [] -> state
            | node::rest ->
                let newState, children = expand state node
                let newQueue = traversal_strategy rest children
                loop newState newQueue

        loop initialState [initialNode]

    // default node traversal strategies

    let bfs rest children = rest @ children
    let dfs rest children = children @ rest