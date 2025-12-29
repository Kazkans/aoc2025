let explode s = s |> String.to_seq |> List.of_seq
let of_digit c = (int_of_char c) - (int_of_char '0')
let last l = List.nth l ((List.length l) - 1)

type blade =
    | Range of int * int
    | Col of int

type edge_type =
    | Horizontal of int * int * int
    | Vertical of int * int * int

let calculate_area ((a_x, a_y), (b_x, b_y)) =
    (abs (a_x - b_x) + 1) * (abs (a_y - b_y) + 1)

let compare_rect a b = Int.compare (calculate_area b) (calculate_area a)

let compare_col a b = match (a, b) with
    | (Col a, Col b) -> Int.compare a b
    | (Range (a, b), Col c) -> Int.compare a c
    | (Col a, Range (b, c)) -> Int.compare a b
    | (Range (a, b), Range (c, d)) -> Int.compare a c

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let parse lines =
    List.map (fun line ->
        let [a;b] = line |> String.split_on_char ',' |> List.map int_of_string in
        (a, b)) lines

let largest points =
    let max = ref 0 in
    List.iter (fun (a_x, a_y) ->
        List.iter (fun (b_x, b_y) ->
            let v = (abs (a_x - b_x) + 1) * (abs (a_y - b_y) + 1) in
            if v > !max then
                max := v
            else
                ()) points) points;
    !max


let solve1 () =
    read_lines "anc9.txt" |> parse
    |> Convex.convex |> largest

let group cols start_x end_x = 
    let rec aux prev cols = 
        if prev > end_x then [] else
        match cols with
        | Range (a, b) :: tl ->
            if a <= prev+1 then
                aux b tl
            else
                (prev+1) :: (aux b tl)
        | Col a :: tl ->
            if a <= prev+1 then
                aux a tl
            else
                (prev+1) :: (aux a tl)
        | [] -> [prev+1] in
    aux (start_x-1) cols

let inside range = 
    List.length range mod 2 = 1

let continous groups cols =
    let rec cols_from cols i =
        match cols with
        | Col a :: tl when a > i -> cols
        | _ :: tl -> cols_from tl i
        | [] -> [] in
    let rec aux groups cols = 
        match groups with
        | [] -> inside cols
        | i :: tl ->
            let cols' = cols_from cols i in
            if inside cols' then
                aux tl cols'
            else
                false in
    aux groups cols
;;

let min_range = function
    | Range (a, b) ->
        if a > b then Range (b, a)
        else Range (a, b)
    | Col x -> Col x

let type_of_edge (ax, ay) (bx, by) =
    if ax = bx then
        if ay > by then
            Vertical (by, ay, ax)
        else
            Vertical (ay, by, ax)
    else if ay=by then
        if ax > bx then
            Horizontal (bx, ax, ay)
        else
            Horizontal (ax, bx, ay)
    else
        failwith "Error: Non grid line"
            

let collisions_from start_x end_x y points =
    let collide v1 v2 =
        (match type_of_edge v1 v2 with
        | Vertical (y1, y2, x) ->
            if start_x < x && (not ((y1 > y) = (y2>y))) then
                [Col x]
            else
                []
        | Horizontal (x1, x2, yh) ->
            if yh = y then
                [Range (x1, x2)]
            else
                []) in
    let rec aux = function
        | v1 :: v2 :: tl -> (collide v1 v2) @ (aux (v2 :: tl))
        | _ -> []
    in
    let cols = aux points |> List.sort compare_col in
    let groups = group cols start_x end_x in
    let cols = List.filter (function | Range _ -> false | Col _ -> true) cols in
    (groups, cols)

let collisions_fromy start_y end_y x points =
    let collide v1 v2 =
        (match type_of_edge v1 v2 with
        | Vertical (y1, y2, xw) ->
            if xw = x then
                [Range (y1, y2)]
            else
                []
        | Horizontal (x1, x2, y) ->
            if start_y < y && (not ((x1 > x) = (x2>x))) then
                [Col y]
            else
                []) in
    let rec aux = function
        | v1 :: v2 :: tl -> (collide v1 v2) @ (aux (v2 :: tl))
        | _ -> []
    in
    let cols = aux points |> List.sort compare_col in
    let groups = group cols start_y end_y in
    let cols = List.filter (function | Range _ -> false | Col _ -> true) cols in
    (groups, cols)


let rectangle (x1, y1) (x2, y2) points =
    let start_x = min x1 x2 in
    let start_y = min y1 y2 in
    let end_x = max x1 x2 in
    let end_y = max y1 y2 in
    (* we assume that rectangle borders are continous *)
    let (groups1, cols1) = collisions_from start_x end_x start_y points in
    let (groups2, cols2) = collisions_from start_x end_x end_y points in
    let (groups3, cols3) = collisions_fromy start_y end_y start_x points in
    let (groups4, cols4) = collisions_fromy start_y end_y end_x points in
    continous groups1 cols1 && continous groups2 cols2 &&
    continous groups4 cols4 && continous groups3 cols3

let largest3 points =
    let rects = ref [] in
    List.iter (fun (a_x, a_y) ->
        List.iter (fun (b_x, b_y) ->
            if a_x > b_x then 
                ()
            else
                rects := ((a_x, a_y), (b_x, b_y)) :: !rects
        ) points) points;
    !rects |> List.sort compare_rect |> List.find (fun (a, b) ->
        rectangle a b points) |> calculate_area

let solve2 () =
    let points = read_lines "anc9.txt" |> parse in
    let points = points @ [List.hd points] in
    points |> largest3
