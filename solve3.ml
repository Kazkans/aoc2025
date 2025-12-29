let of_digit c = (int_of_char c) - (int_of_char '0')
let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let rec maximum_aux lst (m, ms) j limit = 
    if j > limit then
        (m, ms) else
    match lst with
    | [] -> (m, ms)
    | x :: xs ->
        if x > m then
            maximum_aux xs (x, xs) (j+1) limit
        else
            maximum_aux xs (m, ms) (j+1) limit

let f lst limit = maximum_aux lst (min_int, []) 0 limit

let rec loop lst i acc = match i with
    | 0 -> acc
    | _ ->
        let available = (List.length lst) - i in
        let (m, tl) = f lst available in
        loop tl (i-1) (acc*10 + m)

let start lst = loop lst 12 0

let maximum lst = maximum_aux lst (min_int, []) 0 3

let main () =
    read_lines "anc3.txt" 
    |> List.map (fun s -> s |> String.to_seq |> List.of_seq |> (List.map of_digit)
        |> start)
    |> List.fold_left (+) 0 
