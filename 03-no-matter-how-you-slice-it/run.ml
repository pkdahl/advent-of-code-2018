open Base
open Stdio

module Coord = struct
  module T = struct
    type t = int * int

    let sexp_of_t (x, y) : Sexp.t = List [ Atom (Int.to_string x); Atom (Int.to_string y) ]

    let compare (x, y) (x', y') =
      let cmp_x = Int.compare x x' in
      if cmp_x <> 0 then cmp_x
      else Int.compare y y'

  end
  include T
  include Comparator.Make(T)

  let create c = c

  let equal (x, y) (x', y') = Int.equal x x' && Int.equal y y'
end

module Claim = struct
  type t = int * int * int * int * int

  let create (id, x, y, w, h) = (id, x, y, w, h)

  let id (id, _, _, _, _) = id

  let coordinates (_, x, y, w, h) =
    let xs = List.range x (x + w) in
    let ys = List.range y (y + h) in
    List.cartesian_product xs ys
end

module Sheet = struct
  type t = (Coord.t, int list, Coord.comparator_witness) Map.t
  (* type t = Coor.t * int list list *)

  let empty = Map.empty (module Coord)

  let add t claim =
     let coords = Claim.coordinates claim in
     let rec loop coords result =
       match coords with
       | [] -> result
       | coord :: rest -> let result' = Map.add_multi result ~key:coord ~data:(Claim.id claim) in
                          loop rest result'
     in
     loop coords t

  let add_multi t claims =
    let rec loop claims result =
      match claims with
      | [] -> result
      | claim :: rest -> let result' = add result claim in
                         loop rest result'
    in
    loop claims t

  let areas_multiple_claims t =
    let pred claims = List.length claims > 1 in
    Map.filter t ~f:pred

  let areas_exclusive_claims t =
    let pred claims = List.length claims = 1 in
    Map.filter t ~f:pred

  let claim_ids t =
    let alist = Map.to_alist t in
    let id_sets = List.map alist ~f:(fun (_,l) -> Set.of_list (module Int) l) in
    Set.union_list (module Int) id_sets
end

let parse lexbuf = Parser.claims Lexer.read lexbuf

let process_input fname =
  In_channel.with_file fname ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.lex_curr_p <-
        {lexbuf.lex_curr_p with pos_fname = fname};
        parse lexbuf)


let () =
  let claims = process_input "input.txt"
               |> List.map ~f:Claim.create
  in
  let sheet = Sheet.add_multi Sheet.empty claims in
  let mult = Sheet.areas_multiple_claims sheet in
  let excl = Sheet.areas_exclusive_claims sheet in
  let non_overlapping = Set.diff (Sheet.claim_ids excl) (Sheet.claim_ids mult)
                        |> Set.to_list
  in
  let n_mult = mult |> Map.length |> Int.to_string in
  let excl_id = assert (List.length non_overlapping = 1);
                List.hd_exn non_overlapping |> Int.to_string
  in
  n_mult ^ " square inches of fabric have multiple claims." |> print_endline;
  "Claim with id " ^ excl_id ^ " is the only claim who does not overlap "
  ^ "with any other claim."
  |> print_endline
