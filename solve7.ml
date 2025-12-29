let explode s = s |> String.to_seq |> List.of_seq
let of_digit c = (int_of_char c) - (int_of_char '0')

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let print_list lst =
    Printf.printf "[";
    List.iter (Printf.printf "%d,") lst;
    Printf.printf "]\n"

module Map = Stdlib.Map.Make(Int)

let flat_pair_lists l =
    let rec aux last = function
        | [a; b] :: tl when a = last -> b :: (aux b tl)
        | [a; b] :: tl -> a :: b :: (aux b tl)
        | [a] :: tl when a = last -> (aux a tl)
        | [a] :: tl -> a :: (aux a tl)
        | _ -> []
    in aux (-1) l

let add k v m =
    Map.update k (function
        | Some x -> Some (x+v)
        | None -> Some v) m

let split beams splitters =
    let count = ref 0 in
    let beams = List.map (fun b -> if List.mem b splitters then
            (count := !count + 1;
            [b-1; b+1])
        else
            [b]) beams |> flat_pair_lists in
    (beams, !count)

let split2 beams splitters =
    let m = Map.fold (fun k v m ->
        if List.mem k splitters then        
            m |> add (k-1) v |> add (k+1) v
        else
            m |> add k v
    ) beams (Map.empty) in
    Map.iter (fun k v -> Printf.printf "%d:%d\n" k v) m;
    print_list splitters;
    Printf.printf "======\n";
    m

let get_all_occ s =
    let rec aux i =
        match String.index_from_opt s i '^' with
        | Some j -> j :: (aux (j+1))
        | None -> [] in
    aux 0

let run () =
    let lines = read_lines "anc7.txt" in
    let first = List.hd lines in
    let tail = List.tl lines in
    let s = String.index first 'S' in
    Printf.printf "F:%s\n" (List.hd tail);
    let rows = List.map get_all_occ tail in
    let count = ref 0 in
    Printf.printf "S:%d\n" s;
    print_list (List.hd rows);
    let _ = List.fold_left (fun beams row ->
        let (beams', c) = split beams row in
        count := !count + c;
        beams') [s] rows in
    !count

let run2 () =
    let lines = read_lines "anc7.txt" in
    let first = List.hd lines in
    let tail = List.tl lines in
    let s = String.index first 'S' in
    let rows = List.map get_all_occ tail in
    let final = List.fold_left split2 (Map.singleton s 1) rows in
    Map.fold (fun _ v acc -> acc + v) final 0
