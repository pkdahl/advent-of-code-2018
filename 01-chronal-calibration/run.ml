open Containers

let fn = "input.txt"

let changes =
  let parse str =
    let n = Int.of_string str in
    match n with
    | Some n -> `Left n
    | None -> `Right str
  in IO.with_in fn IO.read_lines_l |> List.partition_map parse

let final_frequency =
    List.fold_left (+) 0

let first_repeated_frequency changes =
  let rec aux frequencies nums =
    match nums with
    | [] -> aux frequencies changes
    | n::ns -> let cur = List.hd frequencies + n in
               if List.memq cur frequencies then cur else aux (cur::frequencies) ns
  in aux [0] changes

let () =
  let (left, right) = changes in
  assert (List.is_empty right);
  final_frequency left |> Int.to_string |> (^) "Final frequency is " |> print_endline;
  first_repeated_frequency left |> Int.to_string |> (^) "First repeated frequency is " |> print_endline
