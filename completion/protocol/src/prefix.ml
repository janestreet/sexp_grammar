module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type t =
      | Hole
      | In_list of Sexp.V1.t list * t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 74309a8fba149c43b1e1c0a1310b5723 |}]
    ;;

    include Comparator.V1.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
      end)
  end

  module Model = V1
end

open! Core
open! Import

include (
  Stable.Model :
    Comparator.S
    with type t = Stable.Model.t
    with type comparator_witness = Stable.Model.comparator_witness)

type t = Stable.Model.t =
  | Hole
  | In_list of Sexp.t list * t
[@@deriving compare, sexp_of]

let of_sexp_prefix (_, in_sexp) =
  let rec of_in_sexp : Parsexp_prefix.Sexp_prefix.in_sexp -> _ = function
    | Hole atom_prefix -> Hole, atom_prefix
    | In_list (sexps, in_sexp) ->
      let t, atom_prefix = of_in_sexp in_sexp in
      In_list (sexps, t), atom_prefix
  in
  of_in_sexp in_sexp
;;

let of_substring s ~pos ~len =
  Parsexp_prefix.Sexp_prefix.of_substring s ~pos ~len |> Option.map ~f:of_sexp_prefix
;;

module Unstable = Stable.Model
