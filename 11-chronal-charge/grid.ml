open Base

type t = { sn : int; size : int }

let create sn size =
  { sn = sn; size = size }

let cell_coords t =
  let r = List.range 1 (t.size + 1) in
  List.cartesian_product r r

(* Auxiliary function *)
let hundreds_digit n =
  let open Option.Monad_infix in
  let c = Int.to_string n
          |> String.to_list
          |> List.rev
          |> Fn.flip List.drop 2
          |> List.hd
          >>= Char.get_digit
  in
  match c with
  | Some c -> c
  | None -> 0

let cell_power t (x, y) =
  let rack_id = x + 10 in
  rack_id * y
  |> (+) t.sn
  |> ( * ) rack_id
  |> hundreds_digit
  |> Fn.flip (-) 5

let cell_square_power t (x, y) size =
  let rec aux size =
    match size with
    | 1 -> cell_power t (x, y)
    | _ ->
       let (x', y') = x + size - 1, y + size - 1 in
       let col = List.map (List.range y y') ~f:(fun y -> (x', y)) in
       let row = List.map (List.range x x') ~f:(fun x -> (x, y')) in
       let cells = [(x', y')] @ col @ row in
       let p = List.fold cells ~f:(fun acc c -> acc + cell_power t c) ~init:0 in
       p + aux (size - 1)
  in
  aux size

let cell_max_square_power t (x, y) =
  let larger = if x > y then x else y in

  let rec aux sizes prev best =
    match sizes with
    | [] -> best
    | size :: rest ->
       let (x', y') = x + size - 1, y + size - 1 in
       let col = List.map (List.range y y') ~f:(fun y -> (x', y)) in
       let row = List.map (List.range x x') ~f:(fun x -> (x, y')) in
       let cells = [(x', y')] @ col @ row in
       let power =
         prev + List.fold cells
                  ~f:(fun acc c -> acc + cell_power t c)
                  ~init:0
       in
       let (b_power, _) = best in
       let best' = if power > b_power then (power, size) else best in
       aux rest power best'
  in

  let sizes = List.range 2 (t.size - larger + 2) in
  let init = cell_square_power t (x, y) 1 in
  aux sizes init (init, 1)

let max_square_power t =

  let rec aux cells best =
    match cells with
    | [] -> best
    | cell :: rest ->
       let (x, y) = cell in
       let (b_power, _, _, _) = best in
       let (power, size) = cell_max_square_power t (x, y) in
       let best' = if power > b_power then (power, x, y, size) else best in
       aux rest best'
  in

  let init = (cell_power t (1, 1), 1, 1, 1) in
  let cells = cell_coords t in
  aux cells init
