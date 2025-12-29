let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let generate_graph pairs =
    let num = List.length pairs + 1 in
    let g = Array.make num [] in
    let cur = ref 0 in
    let ass = Hashtbl.create 0 in
    let assign c = (match Hashtbl.find_opt ass c with
        | Some i -> i
        | None -> 
            let i = !cur in
            incr cur;
            Hashtbl.add ass c i;
            i) in
    List.iter (fun (from, tos) ->
        let ids_tos = List.map assign tos in
        let id_from = assign from in
        g.(id_from) <- ids_tos @ g.(id_from)) pairs;
    (g, ass) (*Hashtbl.find ass "you", Hashtbl.find ass "out", Hashtbl.find ass "fft", Hashtbl.find ass "dac") *)

let num_of_paths s t g = 
    let len = Array.length g in
    let visited = Array.make len false in
    let count = Array.make len 0 in
    count.(t) <- 1;
    let rec dfs n = 
        visited.(n) <- true;
        if n = t then () else
        let paths = List.fold_right (fun m acc ->
            if not visited.(m) then
                dfs m
            else ();
            acc + count.(m)) g.(n) 0 in
        count.(n) <- paths in
    dfs s;
    count.(s)

let parse line =
    let parts = line |> String.split_on_char ' ' in
    let from = List.hd parts |> (fun s -> String.sub s 0 3) in
    (from, List.tl parts)

let run1 () =
    let lines = read_lines "anc11.txt" in
    let pairs = List.map parse lines in
    let g, ass = generate_graph pairs in
    let s, t = (Hashtbl.find ass "you", Hashtbl.find ass "out") in
    let a = num_of_paths s t g in
    a

let run2 () =
    let lines = read_lines "anc11.txt" in
    let pairs = List.map parse lines in
    let g, ass = generate_graph pairs in
    let s, t = (Hashtbl.find ass "svr", Hashtbl.find ass "out") in
    let fft, dac = (Hashtbl.find ass "fft", Hashtbl.find ass "dac") in
    let a = num_of_paths s t g in
    let b = num_of_paths s fft g in
    let c = num_of_paths fft dac g in
    let d = num_of_paths dac t g in
    (a,b*c*d)

