open Base
open Stdio

module Point = struct
  type t = int * int

  let create x y =
    (x, y)

  let of_string s =
    let l = String.split s ~on:','
            |> List.map ~f:String.strip
            |> List.map ~f:Int.of_string
    in
    match l with
    | x :: y :: _ -> create x y
    | _ -> failwith "Invalid input"

  let to_string t =
    let (x, y) = t in
    "(" ^ Int.to_string x ^ "," ^ Int.to_string y ^ ")"

  let mdist (xa, ya) (xb, yb) =
    abs (xa - xb) + abs (ya - yb)

  let is_same (xa, ya) (xb, yb) =
    Int.compare xa xb = 0 && Int.compare ya  yb = 0
end

module Cell = struct
  type t = int * int * Point.t option

  let create x y points =
    let find_closest (x, y) ps =
      let combine (d, plist) p =
        let d' = Point.mdist (x, y) p in
        let cmp = Int.compare d' d in
        match cmp < 0, cmp = 0 with
        | true, _ -> (d', [p])
        | _, true -> (d, p :: plist)
        | _, _    -> (d, plist)
      in
      let hd =  Option.value_exn (List.hd ps) in
      let tl = List.drop ps 1 in
      let init = (Point.mdist (x, y) hd, [hd]) in
      let (_, plist) = List.fold tl ~f:combine ~init:init in
      match plist with
      | [p] -> Some p
      | _ -> None
    in
    let closest = find_closest (x, y) points in
    (x, y, closest)

end

module Grid = struct
  type t = int * int * Point.t list * Cell.t list

  let create points =
    let combine (xmax, ymax) (x, y) =
      let xmax' = if Int.compare x xmax > 0 then x else xmax in
      let ymax' = if Int.compare y ymax > 0 then y else ymax in
      (xmax', ymax')
    in
    let (xdim, ydim) = List.fold points ~f:combine ~init:(0, 0) in
    let cells =
      let xr = List.range 0 (xdim + 1) in
      let yr = List.range 0 (ydim + 1) in
      let coords = List.cartesian_product xr yr in
      List.map coords ~f:(fun (x, y) -> Cell.create x y points)
    in
    (xdim, ydim, points, cells)

  let is_on_edge t (x, y) =
    let (xdim, ydim, _, _) = t in
    Int.compare x 0 = 0
    || Int.compare y 0 = 0
    || Int.compare x xdim = 0
    || Int.compare y ydim = 0

  let largest_area t =
    let (_, _, points, cells) = t in
    let combine p acc c =
      let (_, _, cp) = c in
      match cp with
      | None -> acc
      | Some p' -> if Point.is_same p p' then acc + 1 else acc
    in
    let area p =
      List.fold cells ~f:(combine p) ~init:0
    in
    let has_inf_area p =
      List.exists cells
        ~f:(fun (x, y, cp) ->
          match cp with
          | None -> false
          | Some cp -> if Point.is_same p cp && is_on_edge t (x, y) then true else false)
    in
    List.filter points ~f:(fun p -> has_inf_area p |> not)
    |> List.map ~f:area
    |> List.fold
         ~f:(fun acc x ->
           if Int.compare x acc > 0 then x
           else acc)
         ~init:0

  let safe_area_size t sdist =
    let (_, _, points, cells) = t in
    List.map cells ~f:(fun (xc, yc, _) ->
        let tdist = List.fold points
                      ~f:(fun acc p ->  acc + (Point.mdist (xc, yc) p))
                      ~init:0
        in
        Int.compare tdist sdist < 0 |> Bool.to_int)
    |> List.fold ~f:(+) ~init:0
end

let simple () =
  [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)]

let points () =
  In_channel.read_lines "input.txt"
  |> List.map ~f:Point.of_string

let () =
  let g = points () |> Grid.create in
  (* Part 1 *)
  Grid.largest_area g
  |> Int.to_string
  |> print_endline;
  (* Part 2 *)
  Grid.safe_area_size g 10000
  |> Int.to_string
  |> print_endline
