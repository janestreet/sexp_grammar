module Stable = struct
  open! Core.Core_stable
  module Candidate = Candidate.Stable

  module V1 = struct
    type t =
      { candidates : Candidate.V1.t list
      ; exhaustive : bool
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ba195173b7eda196351d95eb623ed48d |}]
    ;;

    include Comparator.V1.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
      end)
  end

  module V2 = struct
    type t =
      { candidates : Candidate.V2.t list
      ; exhaustive : bool
      }
    [@@deriving bin_io, compare, sexp, stable_record ~version:V1.t ~modify:[ candidates ]]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 83cb90e359af6944f4c3f5d635ac303d |}]
    ;;

    include Comparator.V1.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
      end)

    let to_prev v2 = to_V1_t v2 ~modify_candidates:(Base.List.map ~f:Candidate.V2.to_prev)

    let from_prev v1 =
      of_V1_t v1 ~modify_candidates:(Base.List.map ~f:Candidate.V2.from_prev)
    ;;
  end

  module Model = V2
end

open! Core
open! Import

include (
  Stable.Model :
    Comparator.S
    with type t = Stable.Model.t
    with type comparator_witness = Stable.Model.comparator_witness)

type t = Stable.Model.t =
  { candidates : Candidate.t list
  ; exhaustive : bool
  }
[@@deriving compare, fields ~getters, sexp_of]

module Unstable = Stable.Model
