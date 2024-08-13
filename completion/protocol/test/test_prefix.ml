open! Core
open! Import

module _ : module type of Prefix = struct
  (* Tested through [of_substring] *)
  let of_sexp_prefix = Prefix.of_sexp_prefix
  let of_substring = Prefix.of_substring

  let%expect_test "[of_sexp_prefix]" =
    let test s =
      for i = 0 to String.length s do
        print_endline "";
        print_endline [%string {|%{String.subo s ~len:i}│|}];
        match of_substring s ~pos:0 ~len:i with
        | None -> print_s [%message "No prefix here."]
        | Some (t, atom_prefix) ->
          print_s [%sexp (t : Prefix.t)];
          print_s [%message (atom_prefix : Parsexp_prefix.Atom_prefix.t option)]
      done
    in
    test {|(aa (b) "c c")|};
    [%expect
      {|
      │
      Hole
      (atom_prefix ())

      (│
      (In_list () Hole)
      (atom_prefix ())

      (a│
      (In_list () Hole)
      (atom_prefix
       (((signified (Complete (prefix a))) (signifier_begin_offset 1)
         (signifier_end_offset 2))))

      (aa│
      (In_list () Hole)
      (atom_prefix
       (((signified (Complete (prefix aa))) (signifier_begin_offset 1)
         (signifier_end_offset 3))))

      (aa │
      (In_list (aa) Hole)
      (atom_prefix ())

      (aa (│
      (In_list (aa) (In_list () Hole))
      (atom_prefix ())

      (aa (b│
      (In_list (aa) (In_list () Hole))
      (atom_prefix
       (((signified (Complete (prefix b))) (signifier_begin_offset 5)
         (signifier_end_offset 6))))

      (aa (b)│
      (In_list (aa (b)) Hole)
      (atom_prefix ())

      (aa (b) │
      (In_list (aa (b)) Hole)
      (atom_prefix ())

      (aa (b) "│
      (In_list (aa (b)) Hole)
      (atom_prefix
       (((signified (Complete (prefix ""))) (signifier_begin_offset 8)
         (signifier_end_offset 9))))

      (aa (b) "c│
      (In_list (aa (b)) Hole)
      (atom_prefix
       (((signified (Complete (prefix c))) (signifier_begin_offset 8)
         (signifier_end_offset 10))))

      (aa (b) "c │
      (In_list (aa (b)) Hole)
      (atom_prefix
       (((signified (Complete (prefix "c "))) (signifier_begin_offset 8)
         (signifier_end_offset 11))))

      (aa (b) "c c│
      (In_list (aa (b)) Hole)
      (atom_prefix
       (((signified (Complete (prefix "c c"))) (signifier_begin_offset 8)
         (signifier_end_offset 12))))

      (aa (b) "c c"│
      (In_list (aa (b) "c c") Hole)
      (atom_prefix ())

      (aa (b) "c c")│
      Hole
      (atom_prefix ())
      |}]
  ;;

  include (
    Prefix :
    sig
      type t = Prefix.t =
        | Hole
        | In_list of Sexp.t list * t
      [@@deriving compare, sexp_of]

      include
        Comparator.S
        with type t := t
        with type comparator_witness = Prefix.comparator_witness

      module Stable = Prefix.Stable
      module Unstable = Prefix.Unstable
    end)
end
