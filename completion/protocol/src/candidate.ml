module Stable = struct
  open! Core.Core_stable

  module Case_sensitivity = struct
    module V1 = struct
      type t = Sexp_grammar.case_sensitivity =
        | Case_insensitive
        | Case_sensitive
        | Case_sensitive_except_first_character
      [@@deriving bin_io, compare, sexp]
    end

    module Model = V1
  end

  module Atom_to_add = struct
    module V1 = struct
      type t =
        { ignore_capitalization : bool
        ; signified : string
        }
      [@@deriving bin_io, compare, sexp]

      include Comparator.V1.Make (struct
          type nonrec t = t [@@deriving compare, sexp_of]
        end)
    end

    module V2 = struct
      type t =
        { atom_signified : string
        ; case_sensitivity : Case_sensitivity.V1.t
        ; documentation : string list
        }
      [@@deriving
        bin_io
        , compare
        , sexp
        , stable_record
            ~version:V1.t
            ~add:[ ignore_capitalization; signified ]
            ~remove:[ atom_signified; case_sensitivity; documentation ]]

      include Comparator.V1.Make (struct
          type nonrec t = t [@@deriving compare, sexp_of]
        end)

      let to_prev ({ atom_signified; case_sensitivity; _ } as v2) =
        to_V1_t
          v2
          ~ignore_capitalization:
            (match case_sensitivity with
             | Case_sensitive -> false
             | Case_sensitive_except_first_character -> true
             | Case_insensitive ->
               Base.Error.raise_s
                 [%sexp "Cannot convert fully case-insensitive grammar to older version."])
          ~signified:atom_signified
      ;;

      let from_prev ({ ignore_capitalization; signified } as v1 : V1.t) =
        of_V1_t
          v1
          ~atom_signified:signified
          ~case_sensitivity:
            (match ignore_capitalization with
             | true -> Case_sensitive_except_first_character
             | false -> Case_sensitive)
          ~documentation:[]
      ;;
    end

    module Model = V2
  end

  module V1 = struct
    type t =
      | Add_atom of Atom_to_add.V1.t
      | Enter_list
      | Enter_list_and_add_atom of Atom_to_add.V1.t
    [@@deriving bin_io, compare, sexp, stable_variant]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a6c0d6e720ab153a1148854a6a8372e9 |}]
    ;;

    include Comparator.V1.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
      end)
  end

  module V2 = struct
    type t =
      | Add_atom of Atom_to_add.V2.t
      | Enter_list
      | Enter_list_and_add_atom of Atom_to_add.V2.t
    [@@deriving
      bin_io
      , compare
      , sexp
      , stable_variant ~version:V1.t ~modify:[ Add_atom; Enter_list_and_add_atom ]]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 61cd329fbe790fa35afb07dbc60eb43d |}]
    ;;

    include Comparator.V1.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
      end)

    let to_prev v2 =
      to_V1_t
        v2
        ~modify_Add_atom:(fun x -> Add_atom (Atom_to_add.V2.to_prev x))
        ~modify_Enter_list_and_add_atom:(fun x ->
          Enter_list_and_add_atom (Atom_to_add.V2.to_prev x))
    ;;

    let from_prev v1 =
      of_V1_t
        v1
        ~modify_Add_atom:(fun x -> Add_atom (Atom_to_add.V2.from_prev x))
        ~modify_Enter_list_and_add_atom:(fun x ->
          Enter_list_and_add_atom (Atom_to_add.V2.from_prev x))
    ;;
  end

  module Model = V2
end

open! Core
open! Import

module Case_sensitivity = struct
  type t = Stable.Case_sensitivity.Model.t =
    | Case_insensitive
    | Case_sensitive
    | Case_sensitive_except_first_character
  [@@deriving compare, enumerate, sexp_of]
end

module Atom_to_add = struct
  type t = Stable.Atom_to_add.Model.t =
    { atom_signified : string
    ; case_sensitivity : Case_sensitivity.t
    ; documentation : string list
    }
  [@@deriving compare, fields ~getters, sexp_of]

  include (
    Stable.Atom_to_add.Model :
      Comparator.S
      with type t := t
      with type comparator_witness = Stable.Atom_to_add.Model.comparator_witness)

  let matches_atom_prefix { atom_signified; case_sensitivity; _ } = function
    | None -> true
    | Some atom_prefix ->
      let prefix =
        match Parsexp_prefix.Atom_prefix.get_signified atom_prefix with
        | Complete { prefix } | Incomplete { prefix_of_prefix = prefix } -> prefix
      in
      let normalize =
        match case_sensitivity with
        | Case_sensitive_except_first_character -> String.uncapitalize
        | Case_sensitive -> Fn.id
        | Case_insensitive -> String.uppercase
      in
      String.is_prefix (normalize atom_signified) ~prefix:(normalize prefix)
  ;;

  let signifier { atom_signified; case_sensitivity = _; _ } =
    Sexp.to_string [%sexp (atom_signified : string)]
  ;;
end

include Stable.Model
module Unstable = Stable.Model

let insert_to_left = function
  | Add_atom atom_to_add -> Atom_to_add.signifier atom_to_add
  | Enter_list -> "("
  | Enter_list_and_add_atom atom_to_add -> "(" ^ Atom_to_add.signifier atom_to_add ^ " "
;;

let insert_to_right = function
  | Add_atom _ -> ""
  | Enter_list | Enter_list_and_add_atom _ -> ")"
;;

let matches_atom_prefix t atom_prefix_option =
  match t with
  | Enter_list | Enter_list_and_add_atom _ -> Option.is_none atom_prefix_option
  | Add_atom atom_to_add -> Atom_to_add.matches_atom_prefix atom_to_add atom_prefix_option
;;

let documentation = function
  | Enter_list -> []
  | Enter_list_and_add_atom atom_to_add | Add_atom atom_to_add ->
    Atom_to_add.documentation atom_to_add
;;
