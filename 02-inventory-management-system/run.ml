open Containers

let input = "input.txt"

let ids = IO.with_in input IO.read_lines_l

let has_count count str =
  let letters = String.to_list str in
  let uniques = List.uniq ~eq:Char.equal letters in
  let pred char = List.count (Char.equal char) letters = count in
  let rec aux uniques =
    match uniques with
    | [] -> false
    | c::cs -> if pred c then true else aux cs
  in aux uniques

let has_two = has_count 2

let has_three = has_count 3

let twothrees str =
  let two = if has_two str then 1 else 0 in
  let three = if has_three str then 1 else 0 in
  (two, three)

let checksum ids =
  let add_twothrees (x2, x3) (y2, y3) = (x2+y2, x3+y3) in
  let (twos, threes) = List.map twothrees ids |> List.fold_left add_twothrees (0, 0) in
  twos * threes

let good_id left right =
  let rec aux left right diff =
    match left, right, diff with
    | [], [], _ -> true
    | _, [], _ -> false
    | [], _, _ -> false
    | x::xs, y::ys, true -> if Char.equal x y then aux xs ys true else false
    | x::xs, y::ys, false -> if Char.equal x y then aux xs ys false else aux xs ys true
  in aux (String.to_list left) (String.to_list right) false

let common_letters left right =
  let rec aux left right result =
    match left, right with
    | [], _ -> List.rev result |> String.of_list
    | _, [] -> List.rev result |> String.of_list
    | x::xs, y::ys -> if Char.equal x y then aux xs ys (x::result) else aux xs ys result
  in aux (String.to_list left) (String.to_list right) []

let good_ids ids =
  let rec aux ids result =
    match ids, result with
    | [], _ -> None
    | _, Some pair -> Some pair
    | x::ys, _ -> let res_ = List.filter (fun y -> good_id x y) ys in
                  if List.is_empty res_ then aux ys None else Some (x, List.hd res_)
  in aux ids None

let good_id_to_string id =
  match id with
  | None -> "No good id found"
  | Some (left, right) -> common_letters left right

let () =
  checksum ids |> Int.to_string |> print_endline;
  good_ids ids |> good_id_to_string |> print_endline
