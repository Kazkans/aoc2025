let explode s = s |> String.to_seq |> List.of_seq

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let in_range x (a, b) = a <= x && x <= b 

let any_range ranges x =
    List.exists (in_range x) ranges

let solve1 ids ranges =
    List.filter (any_range ranges) ids |>
    List.length

let find_a ranges a =
    let rec aux hd rest = match rest with
    | [] -> a, hd, []
    | (x, y) :: tl ->
        if a <= x then
           a, hd, rest
        else if x <= a && a <= y then
           x, hd, rest 
        else
            aux (hd @ [(x, y)]) tl
    in
    aux [] ranges

let find_b ranges b =
    let rec aux rest = match rest with
    | [] -> b, []
    | (x, y) :: tl ->
        if b < x then
           b, rest
        else if x <= b && b <= y then
           y, tl
        else
           aux tl
    in
    aux ranges

let sum_range ranges range =
    let (a, b) = range in
    let start_r, init, rest = find_a ranges a in
    let end_r, tl = find_b rest b in
    init @ [(start_r, end_r)] @ tl
    
let sum_ranges ranges =
    List.fold_left sum_range [] ranges

let solve2 ranges = 
    sum_ranges ranges |>
    List.fold_left (fun acc (a, b) -> acc + (b - a + 1)) 0

let rec parse_ranges lines acc = match lines with
    | "" :: tl -> acc, tl
    | line :: tl -> 
        let [aS; bS] = String.split_on_char '-' line in
        let a, b = int_of_string aS, int_of_string bS in
        parse_ranges tl ((a,b) :: acc)
    | [] -> failwith "unreachable"

let rec parse_ids lines acc = match lines with
    | line :: tl -> 
        let id = int_of_string line in
        parse_ids tl (id :: acc)
    | [] -> acc

let parse lines = 
    let ranges, tl = parse_ranges lines [] in
    let ids = parse_ids tl [] in
    (ids, ranges)

let main () =
    let (ids, ranges) = read_lines "anc5.txt" |> parse in
    solve2 ranges
