let explode s = s |> String.to_seq |> List.of_seq
let of_digit c = (int_of_char c) - (int_of_char '0')

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let print_list lst =
    Printf.printf "[";
    List.iter (Printf.printf "%d,") lst;
    Printf.printf "]\n"

let fold_lefti f acc lst =
  let _, acc =
    List.fold_left
      (fun (i, acc) x -> (i + 1, f i acc x))
      (0, acc)
      lst
  in
  acc

let rec take n l = 
    match n with
    | 0 -> []
    | _ ->
        (match l with
        | hd :: tl -> hd :: (take (n-1) tl))

(* multiple neighbors in our list dont matter *)
let add_edge i j g =
    (g.(i) <- j :: g.(i);
    g.(j) <- i :: g.(j));
    g

let sq a = a *. a
    
let euclid (x, y, z) (a, b, c) =
    (sq (x -. a)) +. (sq (y -. b)) +. (sq (z -. c))

let closest p points =
    fold_lefti (fun i (acc, j) t ->
        if p != t && (euclid p t) < acc then
            (euclid p t), i
        else
            acc, j) (max_float, -1) points

let all_distances points j p =
    List.mapi (fun i t -> 
        if p != t then
            Some ((euclid p t), (if i>j then (i,j) else (j,i)))
        else
            None) points |> List.filter_map (fun i -> i)

let visited = Array.make 1000 false

let count = ref 0
let rec dfs g i =
    let visited_ = visited.(i) in
    visited.(i) <- true;
    if visited_ = false then (
        incr count;
        List.iter (dfs g) g.(i))
    else
        ()

let count_components g =
    let range = Seq.init (Array.length g) (fun i -> i) in
    Seq.fold_left (fun acc i ->
        if visited.(i) = false then (
            count := 0;
            dfs g i;
            Printf.printf "Count: %d\n" !count;
            !count :: acc)
        else
            acc) [] range

let create_graph points = 
    let empty_graph = Array.make (List.length points) [] in
    let a = List.mapi (all_distances points) points |> List.concat in
    List.sort_uniq
        (fun (f1, _) (f2, _) -> Float.compare f1 f2)
        a |> take 1000 |> List.fold_left (fun g (_, (i, j)) -> add_edge i j g) empty_graph

let add_till_one n edges =
    let dsu = Dsu.create n in
    let rec aux acc = function
        | (_, (i, j)) :: tail ->
            let acc' = acc - (if Dsu.add dsu i j then 1 else 0) in
            if acc' = 1 then
                (i, j)
            else
                aux acc' tail
        | [] -> failwith "did not connect into one component"
    in aux n edges

let create_graph2 points = 
    let a = List.mapi (all_distances points) points |> List.concat in
    List.sort_uniq
        (fun (f1, _) (f2, _) -> Float.compare f1 f2)
        a 

let parse lst =
    List.map (fun line ->
        line |> String.split_on_char ',' |> List.map (fun d -> Printf.printf "=%s\n" d;d)
        |> List.map float_of_string |> fun [x;y;z] -> (x,y,z)) lst

let run2 () =
    let points = read_lines "anc8_test.txt" |> parse in
    let (a, b) = points |> create_graph2 |> add_till_one (List.length points) in
    let (x1, _, _) = List.nth points a in
    let (x2, _, _) = List.nth points b in
    x1*.x2

