let explode s = s |> String.to_seq |> List.of_seq
let of_digit c = (int_of_char c) - (int_of_char '0')

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let execute ops = 
    List.map (fun (args, f) -> f args) ops

let split_at_end lst =
    let r_l = List.rev lst in
    let last = List.hd r_l in
    let rest = List.tl r_l |> List.rev in
    (rest, last)

let parse_op = function
    | "+" -> (List.fold_left (+) 0)
    | "*" -> (List.fold_left ( * ) 1)

let print_list lst =
    Printf.printf "[";
    List.iter (Printf.printf "%c,") lst;
    Printf.printf "]\n"

let fromdig ds =
    let actual_ds = List.filter (fun s -> not (s = ' ')) ds |> List.map of_digit in
    List.fold_left (fun acc a -> acc*10 + a) 0 actual_ds

let parse_one lst i =
    let len = String.length (List.hd lst) in
    let rec aux acc i =
        if i >= len then acc, i else
        let ds = List.map (fun s -> String.get s i) lst in
        if List.for_all ((=) ' ') ds then
            acc, i+1
        else aux ((fromdig ds) :: acc) (i+1) in
    aux [] i

let recusive lst =
    let len = String.length (List.hd lst) in
    let rec aux i =
        if i >= len then [] else
        let (nums, j) = parse_one lst i in
        nums :: (aux j) in
    aux 0

let snif s =
    s

let rec parse_nums rows ops =
    match (rows, ops) with
    | (_, _ :: _) ->
        let args = List.map (fun s -> s |> List.hd |> snif |> int_of_string) rows in
        let op = List.hd ops |> parse_op in
        (args, op) :: (parse_nums (List.map List.tl rows) (List.tl ops))
    | _ -> [] 

let main2 () =
    let input = read_lines "anc6.txt" in
    let (init, last) = split_at_end input in
    let last' = last |> String.split_on_char ' ' |> List.filter (fun p -> not (p = "")) in
    let nums = recusive init in
    let ops = List.map parse_op last' in
    List.combine nums ops |> execute |> List.fold_left (+) 0

let main1 () =
    let input = read_lines "anc6.txt"
    |> List.map (fun s -> s |> String.split_on_char ' ' |> List.filter (fun p -> not (p = ""))) in
    let (init, last) = split_at_end input in
    parse_nums init last |> execute |> List.fold_left (+) 0
