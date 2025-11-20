open! Core
open Sexplib0.Sexp_grammar
module Generator = Quickcheck.Generator
open Generator.Let_syntax

module Gen_recursives = struct
  type t =
    | Frequently
    | Occasionally
    | Never
  [@@deriving sexp_of]
end

type defn_params =
  { defn_name : string
  ; tyvar_names : string list
  }
[@@deriving sexp_of]

(* In addition to generating inhabitable sexps, we need to make sure we don't generate
   sexps with uninhabitable branches, e.g.:

   [{| type t = | Ok of int | Bad of Nothing.t |}]

   To do this, we never generate `Union []`, and pass around a [Context.t], via which we
   can prevent generating cycles of [Recursive]s which never bottom out.
*)

module Permitted_recursives = struct
  type t =
    | Not_in_tycon
    | These of String.Set.t
    | All
end

module List_grammar_kind = struct
  type t =
    | Args (* e.g. the arguments to a variant or record field *)
    | Grammar (* e.g. list, record, tuple *)
end

module Context = struct
  type t =
    { defns : defn_params list
    ; tyvar_names : string list
    ; permitted_recursives : Permitted_recursives.t
    ; gen_recursives : Gen_recursives.t
    }
end

let gen_small_string =
  let%bind small_int = Int.gen_incl 3 8 in
  String.gen_with_length small_int Char.gen_alphanum
;;

let gen_list_unique gen_len gen ~compare =
  let%bind len = gen_len in
  Generator.list_with_length len gen
  |> Generator.filter ~f:(Fn.non (List.contains_dup ~compare))
;;

let gen_weighted_of_list alist =
  List.Assoc.map alist ~f:return |> Generator.weighted_union
;;

let gen_with_tag gen =
  let%bind key = gen_small_string in
  let%bind value = Sexp.quickcheck_generator in
  let%map grammar = gen in
  { key; value; grammar }
;;

let gen_with_tag_list gen =
  Generator.weighted_recursive_union
    [ ( 0.8
      , let%map x = gen in
        No_tag x )
    ]
    ~f:(fun self_tag_list ->
      [ ( 0.2
        , let%map x = gen_with_tag self_tag_list in
          Tag x )
      ])
;;

let gen_with_unique_names ~how_many ~f =
  let%bind names =
    gen_list_unique how_many gen_small_string ~compare:[%compare: string]
  in
  let%bind sizes =
    Base_quickcheck.Generator.sizes
      ~min_length:(List.length names)
      ~max_length:(List.length names)
      ()
  in
  List.zip_exn names sizes
  |> List.mapi ~f:(fun i (name, size) -> Generator.with_size ~size (f i ~name))
  |> Generator.all
;;

let leaf_grammars = [ Any ""; Bool; Char; Integer; Float; String ]

let rec gen_grammar ~(context : Context.t) =
  let%bind size = Generator.size in
  let nonrec_gens = Generator.of_list leaf_grammars in
  let rec_gens =
    let gen_grammar ~context =
      Generator.with_size (gen_grammar ~context) ~size:(size - 1)
    in
    let tyvar =
      match context.tyvar_names with
      | [] -> None
      | names ->
        Some
          (let%map name = Generator.of_list names in
           Tyvar name)
    in
    let recursive =
      let defns_to_choose_from =
        match context.permitted_recursives with
        | Not_in_tycon -> []
        | All -> context.defns
        | These safe_defns ->
          List.filter context.defns ~f:(fun { defn_name; _ } ->
            Set.mem safe_defns defn_name)
      in
      match defns_to_choose_from with
      | [] -> None
      | defns ->
        Some
          (let%bind chosen_defn = Generator.of_list defns in
           let%map gs =
             Generator.list_with_length
               (List.length chosen_defn.tyvar_names)
               (gen_grammar ~context)
           in
           Recursive (chosen_defn.defn_name, gs))
    in
    let tycon = gen_tycon context in
    let list =
      let%map lg = gen_list_grammar ~kind:List_grammar_kind.Grammar ~context in
      List lg
    in
    let variant =
      let%map v = gen_variant context in
      Variant v
    in
    let union = gen_union context in
    let option =
      (* If a recursive cycle goes through an [Option], it's always possible to bottom out
         via [None]. *)
      let%map g = gen_grammar ~context:{ context with permitted_recursives = All } in
      Option g
    in
    let tagged =
      let%bind key = String.gen_nonempty in
      let%bind value = Sexp.quickcheck_generator in
      let%map grammar = gen_grammar ~context in
      Tagged { key; value; grammar }
    in
    let lazy_ =
      let%map g = gen_grammar ~context in
      Lazy (Portable_lazy.from_val g)
    in
    Generator.weighted_union
      (List.concat
         [ Option.value_map tyvar ~default:[] ~f:(fun g -> [ 1., g ])
         ; Option.value_map recursive ~default:[] ~f:(fun g ->
             [ ( (match context.gen_recursives with
                  | Frequently -> 1.
                  | Occasionally -> 0.2
                  | Never -> 0.)
               , g )
             ])
         ; [ 0.5, tycon
           ; 3., option
           ; 2., list
           ; 2., variant
           ; 1., union
           ; 0.5, tagged
           ; 0.5, lazy_
           ]
         ])
  in
  if size <= 0
  then nonrec_gens
  else Generator.weighted_union [ 0.2, nonrec_gens; 0.8, rec_gens ]

and gen_list_grammar ~kind ~context =
  let%bind size = Generator.size in
  let gen_list_grammar ~context ~kind =
    Generator.with_size (gen_list_grammar ~context ~kind) ~size:(size - 1)
  in
  if size <= 0
  then return Empty
  else (
    let gen_grammar ~context =
      Generator.with_size (gen_grammar ~context) ~size:(size - 1)
    in
    let empty = return Empty in
    let cons =
      let%bind g = gen_grammar ~context in
      let%map lg = gen_list_grammar ~context ~kind in
      Cons (g, lg)
    in
    let many =
      let%map g =
        (* If a recursive cycle goes through a [List(Many)], it's always possible to
           bottom out by providing no list elements. *)
        gen_grammar ~context:{ context with permitted_recursives = All }
      in
      Many g
    in
    let record =
      let%map r = gen_record context in
      Fields r
    in
    Generator.weighted_union
      (match kind with
       | Grammar -> [ 1., empty; 1., cons; 1., many; 1., record ]
       | Args ->
         (* The [list_grammar]s that are arguments to variants or record fields are almost
            always nested [Cons]s terminated by an [Empty], so primarily generate those. *)
         [ 0.19, empty; 0.8, cons; 0.005, many; 0.005, record ]))

and gen_record context =
  let%bind allow_extra_fields = Generator.bool in
  let%map fields =
    gen_with_unique_names ~how_many:(Int.gen_incl 1 10) ~f:(fun _i ~name ->
      let gen_field =
        let%bind required = Generator.bool in
        let%map args = gen_list_grammar ~kind:Args ~context in
        { name; required; args }
      in
      gen_with_tag_list gen_field)
  in
  { allow_extra_fields; fields }

and gen_case_sensitivity =
  gen_weighted_of_list
    [ 0.1, Case_insensitive
    ; 0.1, Case_sensitive
    ; 0.8, Case_sensitive_except_first_character
    ]

and gen_union context =
  let%bind num_branches = gen_weighted_of_list [ 0.1, 1; 2., 2; 2., 3; 1., 4; 0.5, 5 ] in
  let%map gs = Generator.list_with_length num_branches (gen_grammar ~context) in
  Union gs

and gen_variant context =
  let%bind case_sensitivity = gen_case_sensitivity in
  let%bind clauses_unpermuted =
    gen_with_unique_names ~how_many:(Int.gen_incl 1 5) ~f:(fun i ~name ->
      let gen_clause =
        let%map clause_kind =
          Generator.union
            [ return Atom_clause
            ; (let context =
                 match i with
                 | 0 -> context
                 (* If a recursive cycle goes through a [Variant], and one of the branches
                    is guarunteed to lead to a path that bottoms out, we can bottom out
                    through that branch. We always generate such a branch ([i = 0]), and
                    then shuffle the order of branches. *)
                 | _ -> { context with permitted_recursives = All }
               in
               let%map args = gen_list_grammar ~kind:Args ~context in
               List_clause { args })
            ]
        in
        { name; clause_kind }
      in
      gen_with_tag_list gen_clause)
  in
  let%map clauses = List.gen_permutations clauses_unpermuted in
  { case_sensitivity; clauses }

and gen_tycon context =
  let%bind defns_params =
    gen_list_unique
      ~compare:(Comparable.lift [%compare: string] ~f:(fun param -> param.defn_name))
      (match context.gen_recursives with
       (* If recursives aren't generated, or are rarely generated, some tycon branches
          will be unusable. We still want to generate them though, because we want to
          ensure those grammars are properly handled. *)
       | Never | Occasionally ->
         gen_weighted_of_list [ 0.5, 1; 0.3, 2; 0.2, 3; 0.1, 4; 0.05, 5 ]
       | Frequently -> Int.gen_incl 1 5)
      (let%bind tyvar_names =
         gen_list_unique
           (Int.gen_incl 1 3)
           (let%bind small_int = Int.gen_incl 1 3 in
            String.gen_with_length small_int Char.gen_alphanum)
           ~compare:[%compare: string]
       in
       let%map defn_name = gen_small_string in
       { defn_name; tyvar_names })
  in
  let%bind defns_unpermuted =
    List.folding_map
      defns_params
      ~init:String.Set.empty
      ~f:(fun prev_names { defn_name; tyvar_names } ->
        let generator =
          let%map grammar =
            gen_grammar
              ~context:
                { defns = defns_params
                ; tyvar_names
                ; permitted_recursives = These prev_names
                ; gen_recursives = context.gen_recursives
                }
          in
          { tycon = defn_name; tyvars = tyvar_names; grammar }
        in
        let new_names = Set.add prev_names defn_name in
        new_names, generator)
    |> Generator.all
  in
  let%bind defns = List.gen_permutations defns_unpermuted in
  let%bind chosen_defn = Generator.of_list defns_params in
  let%map tyvar_grammars =
    Generator.list_with_length
      (List.length chosen_defn.tyvar_names)
      (gen_grammar ~context)
  in
  Tycon (chosen_defn.defn_name, tyvar_grammars, defns)
;;

let quickcheck_generator ~gen_recursives =
  gen_grammar
    ~context:
      { tyvar_names = []
      ; defns = []
      ; permitted_recursives = Not_in_tycon
      ; gen_recursives
      }
;;

(* A description of not generating recursive cycles that don't bottom out:

   When we start generating a grammar, we may not generate any [Recursive]s. This makes
   sense, because [Recursive]s only make sense in the context of a [Tycon].

   Whenever we pass through a [Tycon], we generate [defns] sequentially, and then shuffle
   them. In the first [defn], we are not allowed to generate any [Recursive]s. In the
   second, we can only generate [Recursive]s to the first defn. And so on.

   There's an exception to this: whenever we pass through a [List(Many x)] or [Option x],
   we are allowed to generate any [Recursive name]s inside of [x], even with [name] pointing
   towards the current [defn] or to a [defn] that has not yet been generated.
   The same applies on all branches of a [Variant] except for one. This is because lists,
   options, and variants provide the ability to bottom out via [[]], [None], and the
   "safe" arm of a [Variant].

   ---

   It turns out _detecting_ the [Recursive] cycles is considerably harder than not generating
   them. One heuristic is "if we arrive twice at a [Recursive] with the same name, enclosing
   tycon, and args". But this fails for [Recursive("branch", [Option (Tycon "arg"), ...])],
   because even though this is a cycle, the args won't be the same. It's difficult to "reduce"
   the args to something simpler, because it's possible that we might need to go through that
   recursive more than once to determine inhabitability, e.g.:

   {|
    Tycon ("t", [ Integer; Union []; Union []; Union [] ], [
      { tycon = "t"
      ; tyvars = ["a"; "b"; "c"; "d"]
      ; grammar = Union [ Tyvar "d"; Recursive ("t", [ Tyvar "d"; Tyvar "a"; Tyvar "b"; Tyvar "c" ]) ] })
   |}
*)
