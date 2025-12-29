 module IntPairs =
   struct
     type t = int * int
     let compare (x0,y0) (x1,y1) =
       match Stdlib.compare x0 x1 with
           0 -> Stdlib.compare y0 y1
         | c -> c
   end

 module PairsMap = Map.Make(IntPairs)

let explode s = s |> String.to_seq |> List.of_seq

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let count_neighbours b (i, j) =
    let ns = PairsMap.(
        [find_opt (i+1, j)  b;
        find_opt (i+1, j+1) b;
        find_opt (i,   j+1) b;
        find_opt (i-1, j+1) b;
        find_opt (i-1, j)   b;
        find_opt (i-1, j-1) b;
        find_opt (i,   j-1) b;
        find_opt (i+1, j-1) b]) in
    List.filter_map (fun x -> x) ns |> List.length


let create_map lines =
    let b = ref PairsMap.empty in
    let add i j = 
        b := PairsMap.add (i,j) () !b in
    List.iteri (fun i line ->
        (List.iteri (fun j c -> match c with
            | '@' -> add i j
            | _ -> ()) (explode line))) lines;
    !b

let solve1 b =
    b |> PairsMap.filter (fun k _-> (count_neighbours b k) < 4) |> PairsMap.cardinal

let solve2 b =
    let rec iter b =
        let b' = PairsMap.filter (fun k _-> (count_neighbours b k) >= 4) b in
        if PairsMap.((cardinal b') = (cardinal b)) then b
        else iter b' in
    let start = PairsMap.cardinal b in
    let last = b |> iter  |> (PairsMap.cardinal) in
    Printf.printf "%d - %d\n" start last;
    start-last

let main () =
    read_lines "anc4.txt" |> create_map |> solve2
