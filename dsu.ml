let create n = Array.init n (fun i -> i)

let rec find s a =
    let parent = s.(a) in
    if parent = a then
        a
    else
        find s parent

(* returns if it connects two different components *)
let add s a b =
    let i = find s a in
    let j = find s b in
    s.(i) <- j;
    if i != j then
        true
    else
        false
