open Base
open Stdio

module Mer = struct

  (* encode capital letters with polarity true *)
  type t = { kind : char; polarity : bool }

  let of_char char =
    { kind = Char.lowercase char;
      polarity = Char.is_uppercase char }

  let to_char t =
    if t.polarity then Char.uppercase t.kind else t.kind

  let compare t other_t =
    let cmp = Char.compare t.kind other_t.kind in
    if cmp <> 0 then cmp else Bool.compare t.polarity other_t.polarity

  let is_kind t char =
    Char.compare t.kind (Char.lowercase char) = 0

  let react t other_t =
    let cmp = Char.compare t.kind other_t.kind in
    if cmp <> 0 then false else Bool.compare t.polarity other_t.polarity <> 0
end

module Polymer = struct

  type t = Mer.t list

  let of_string str =
    String.to_list str
    |> List.filter ~f:(Char.is_alpha)
    |> List.map ~f:Mer.of_char

  let to_string t =
    List.map t ~f:Mer.to_char |> String.of_char_list

  let length =
    List.length

  let react t =
    let rec loop l r =
      match l, r with
      | [], h :: t -> loop [h] t
      | lh :: lt, rh :: rt -> if Mer.react lh rh then loop lt rt else loop (rh :: l) rt
      | _, [] -> List.rev l
    in
    loop [] t

  let shortest t =
    let rec aux t kinds best =
      match kinds with
      | [] -> best
      | k :: rest ->
         let polymer = List.filter t ~f:(fun m -> Mer.is_kind m k |> not) in
         let len = react polymer |> length in
         let best' = if len < best then len else best in
         aux t rest best'
    in
    let kinds = String.to_list "abcdefghijklmnopqrstuvwxyz" in
    let init = react t |> length in
    aux t kinds init
end

let () =
  let polymer =
    In_channel.read_all "input.txt" |> Polymer.of_string
  in
  let length_after_reaction =
    polymer |> Polymer.react |> Polymer.length |> Int.to_string
  in
  let shortest_remove_one_mer =
    polymer |> Polymer.shortest |> Int.to_string
  in
  print_endline length_after_reaction;
  print_endline shortest_remove_one_mer
