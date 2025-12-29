let explode s = s |> String.to_seq |> List.of_seq
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let parse_light lights =
    String.sub lights 1 ((String.length lights) - 2)
    |> explode 
    |> List.map (function
        | '#' -> true
        | '.' -> false
        | _ -> failwith "malformed input" )

let parse_button button =
    String.sub button 1 ((String.length button) - 2)
    |> String.split_on_char ','
    |> List.map int_of_string

let parse_button2 n button =
    let s = Array.make n 0 in
    String.sub button 1 ((String.length button) - 2)
    |> String.split_on_char ','
    |> List.map int_of_string
    |> List.iter (fun i -> s.(i) <- 1);
    s


let remove_last l = List.rev l |> List.tl |> List.rev
let last l = List.nth l ((List.length l) - 1)

let rangei a =
    List.init a (fun i -> i+1)

let range a =
    List.init a (fun i -> i)

let range2 a =
    List.init a (fun i -> [i])

let rec choose k n = 
    match k with
    | 1 -> range2 n
    | _ ->
        let lst = choose (k-1) n in
        List.concat_map (fun (a :: tl) ->
            List.map (fun i -> i :: a :: tl) (range a)) lst


let correct l =
    let correct = ref false in
    Array.iter (fun i -> correct := i || !correct) l;
    not !correct

exception Result of int

let check used buttons lights =
    let lights = Array.of_list lights in
    List.iter (fun i ->
        let button = buttons.(i) in
        List.iter (fun j -> lights.(j) <- not (lights.(j))) button) used;
    if correct lights then
        raise (Result (List.length used))
    else
        ()


let bruteforce lights buttons =
    let n = List.length buttons in
    let buttons = Array.of_list buttons in
    try 
        List.iter (fun i -> 
            let opts = choose (i+1) n in
            List.iter (fun u -> check u buttons lights) opts
            ) (range n); -1 with
    | Result i -> i

let parse_joltage joltage =
    let s = String.sub joltage 1 ((String.length joltage) - 2) in
    "[" ^ s ^ "]"

let print_buttons buttons =
    "[" ^
    (String.concat ";" (List.map (fun b ->
        let b = Array.to_list b in
        (String.concat " " (List.map string_of_int b))) buttons)) ^
    "]"

let run line =
    let parts = String.split_on_char ' ' line in
    let lights = List.hd parts |> parse_light in
    let buttons = remove_last (List.tl parts) |> List.map parse_button in
    bruteforce lights buttons

let run2 line =
    let parts = String.split_on_char ' ' line in
    let n = List.hd parts |> parse_light |> List.length in
    let joltage = last parts |> parse_joltage in
    let buttons = remove_last (List.tl parts) |> List.map (parse_button2 n) in
    Printf.printf "solve(%s, %s)\n" (print_buttons buttons) joltage


let solve () =
    let lines = read_lines "acc10.txt" in
    let nums = List.map run lines in
    List.fold_left (+) 0 nums

let solve2 () =
    let lines = read_lines "acc10.txt" in
    List.iter run2 lines
