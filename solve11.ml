let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let not_preamble l = String.length l > 4

let count_hash row =
    explode row |> List.filter ((=) '#') |> List.length

let parse_input line =
    let trim s = String.sub s 0 ((String.length s) - 1) in
    let parts = String.split_on_char ' ' line in
    let coords = List.hd parts |> trim |> String.split_on_char 'x' |> List.map int_of_string in
    let presents = List.tl parts |> List.map int_of_string in
    (coords, presents)

let parse_rects lines =
    let rec aux = function
    | l :: lst when not_preamble l -> l :: lst
    | _ :: rst -> aux rst in
    aux lines

let run () =
    let lines = read_lines "anc12.txt" in
    let rest = parse_rects lines in
    let inputs = rest |> List.map (fun l -> parse_input l) in
    let correct = List.map (fun ([x;y], inp) ->
        let sum = List.fold_right (+) inp 0 in
        Printf.printf "%d %d %d\n" x y sum;
        if (x/3) * (y/3) >= sum then
            true
        else
            false) inputs in
    List.filter (fun i -> i) correct |> List.length
