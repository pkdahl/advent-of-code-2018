open Base
open Stdio

let max_size_3 grid =
  let rec aux cells best =
    match cells with
    | [] -> best
    | cell :: rest ->
       let (x, y) = cell in
       let power = Grid.cell_square_power grid cell 3 in
       let (b_power, _ ,_) = best in
       let best' = if power > b_power then (power, x, y) else best in
       aux rest best'
  in
  let cells = Grid.cell_coords grid in
  let init = (Grid.cell_square_power grid (1, 1) 3, 1, 1) in
  let best = aux cells init in
  let to_string (_, x, y) = Int.to_string x ^ "," ^ Int.to_string y in
  best |> to_string

let best_any_size grid =
  let max = Grid.max_square_power grid in
  let to_string (_, x, y, s) =
    Int.to_string x ^ "," ^ Int.to_string y ^ "," ^ Int.to_string s
  in
  max |> to_string

let gsn = 5034
let gsize = 300

let () =
  let g = Grid.create gsn gsize in
  max_size_3 g |> print_endline;
  best_any_size g |> print_endline
