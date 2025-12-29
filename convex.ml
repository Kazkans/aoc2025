type orient =
    | ClockWise
    | CounterClockWise
    | Colinear

let leftmost points =
    let rec max (cur, p) = function
        | (x, _) as t :: tl ->
            if x < cur then
                max (x, t) tl
            else
                max (cur, p) tl
        | [] -> p in
    max (max_int, (0,0)) points

let left (p_x, p_y) (q_x, q_y) (r_x, r_y) =
    let v = (q_y - p_y) * (r_x - q_x) -
            (q_x - p_x) * (r_y - q_y) in
    if v < 0 then
        true
    else
        false

let point_orient (a_x, a_y) (b_x, b_y) (p_x, p_y) =
    let v = (b_x - a_x) * (p_y - a_y) -
            (b_y - a_y) * (p_x - a_x) in
    if v < 0 then
        ClockWise
    else if v > 0 then
        CounterClockWise
    else
        Colinear

let convex points =
    let start = leftmost points in
    let rec loop acc =
        let p = List.hd acc in
        let k = List.fold_left (fun endpoint cur ->
            if endpoint = p || (left p cur endpoint) then
                cur
            else
                endpoint) p points in
        if k == start then
            acc
        else
            loop (k :: acc) in
    loop [start]

let inside a b c x o =
    let correct_orient i j = (point_orient i j x) = o || (point_orient i j x) = Colinear in
    (correct_orient a b) &&
    (correct_orient b c)


let orientation points =
    let p' = points @ [List.hd points] in
    let rec shoelace acc = function
        | (a_x, a_y) :: (b_x, b_y) :: tl ->
            let acc = acc + a_x * b_y - b_x * a_y in
            shoelace acc ((b_x, b_y) :: tl)
        | [_] -> acc
        | [] -> acc in
    if shoelace 0 p' > 0 then
        CounterClockWise (* ccw *)
    else
        ClockWise (* cw *)
