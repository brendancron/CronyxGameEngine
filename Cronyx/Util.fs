namespace Cronyx

module Util =

    let revAppend xs ys =
        let rec loop acc = function
            | [] -> acc
            | h::t -> loop (h::acc) t
        loop ys xs