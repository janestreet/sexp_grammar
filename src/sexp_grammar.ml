[@@@warning "-30"] (* allow duplicate field names *)

open! Base
include Sexp_grammar_intf

open struct
  module Sexp_grammar = Sexplib0.Sexp_grammar
end

module Case_sensitivity = struct
  type t = Sexp_grammar.case_sensitivity =
    | Case_insensitive
    | Case_sensitive
    | Case_sensitive_except_first_character
  [@@deriving sexp_of]

  module String_capitalized = struct
    type t = string [@@deriving sexp_of]

    let compare = Comparable.lift String.compare ~f:String.capitalize

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  let to_string_comparator t : (module Comparator.S with type t = string) =
    match t with
    | Case_insensitive -> (module String.Caseless)
    | Case_sensitive -> (module String)
    | Case_sensitive_except_first_character -> (module String_capitalized)
  ;;
end

module Field = struct
  include Field

  let map t ~f =
    match t with
    | Optional x -> Optional (f x)
    | Required x -> Required (f x)
  ;;
end

type grammar = Sexp_grammar.grammar =
  | Any of string
  | Bool
  | Char
  | Integer
  | Float
  | String
  | Option of grammar
  | List of list_grammar
  | Variant of variant
  | Union of grammar list
  | Tagged of grammar with_tag
  | Tyvar of string
  | Tycon of string * grammar list
  | Recursive of grammar * defn list
  | Lazy of grammar Lazy.t

and list_grammar = Sexp_grammar.list_grammar =
  | Empty
  | Cons of grammar * list_grammar
  | Many of grammar
  | Fields of record

and record = Sexp_grammar.record =
  { allow_extra_fields : bool
  ; fields : field with_tag_list list
  }

and field = Sexp_grammar.field =
  { name : string
  ; required : bool
  ; args : list_grammar
  }

and case_sensitivity = Sexp_grammar.case_sensitivity =
  | Case_insensitive
  | Case_sensitive
  | Case_sensitive_except_first_character

and variant = Sexp_grammar.variant =
  { case_sensitivity : case_sensitivity
  ; clauses : clause with_tag_list list
  }

and clause = Sexp_grammar.clause =
  { name : string
  ; clause_kind : clause_kind
  }

and clause_kind = Sexp_grammar.clause_kind =
  | Atom_clause
  | List_clause of { args : list_grammar }

and 'a with_tag = 'a Sexp_grammar.with_tag =
  { key : string
  ; value : Sexp.t
  ; grammar : 'a
  }

and 'a with_tag_list = 'a Sexp_grammar.with_tag_list =
  | Tag of 'a with_tag_list with_tag
  | No_tag of 'a

and defn = Sexp_grammar.defn =
  { tycon : string
  ; tyvars : string list
  ; grammar : grammar
  }
[@@deriving sexp_of]

type 'a t = 'a Sexp_grammar.t = { untyped : grammar } [@@unboxed] [@@deriving sexp_of]

let coerce = Sexp_grammar.coerce

(* Ties the knot for recursive grammars. *)
module Tie_the_knot (Callbacks : Callbacks_for_fold_recursive) : sig
  (* Stage 1: nonrecursive fold callbacks to explicitly handle recursive grammars. *)
  include Callbacks_for_fold_nonrecursive

  (* Stage 2: convert the result back to the expected result type. *)
  val finish_grammar_exn : t -> Callbacks.t
  val finish_list_grammar_exn : list_t -> Callbacks.list_t
end = struct
  (* Builds up a recursive computation as a staged function taking tycon and tyvar
     environments as input.

     The notion behind the staging is that the various constructors ([any], [list], etc.)
     build up staged functions. Then [finish_exn] unstages the result and runs it with
     initial environments.

     Staged functions should only be applied inside [finish_exn], or inside other staged
     functions. *)

  type tyvar_env = Callbacks.t Map.M(String).t

  type tycon_env =
    (string list * (tyvar_env:tyvar_env -> Callbacks.t) Staged.t) Map.M(String).t

  type 'a with_envs = (tycon_env:tycon_env -> tyvar_env:tyvar_env -> 'a) Staged.t
  type t = Callbacks.t with_envs
  type list_t = Callbacks.list_t with_envs

  let return x = Staged.stage (fun ~tycon_env:_ ~tyvar_env:_ -> x)

  let map t ~f =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Staged.unstage t ~tycon_env ~tyvar_env |> f)
  ;;

  let any string = return (Callbacks.any string)
  let bool = return Callbacks.bool
  let char = return Callbacks.char
  let integer = return Callbacks.integer
  let float = return Callbacks.float
  let string = return Callbacks.string
  let option t = map t ~f:Callbacks.option
  let list t = map t ~f:Callbacks.list
  let many t = map t ~f:Callbacks.many
  let empty = return Callbacks.empty
  let tag t key value = map t ~f:(fun t -> Callbacks.tag t key value)

  let cons head tail =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Callbacks.cons
        (Staged.unstage head ~tycon_env ~tyvar_env)
        (Staged.unstage tail ~tycon_env ~tyvar_env))
  ;;

  let record fields ~allow_extra_fields =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Callbacks.record
        (List.Assoc.map fields ~f:(fun (field, tags) ->
           Field.map field ~f:(fun t -> Staged.unstage t ~tycon_env ~tyvar_env), tags))
        ~allow_extra_fields)
  ;;

  let variant clauses ~case_sensitivity =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Callbacks.variant
        (List.Assoc.map clauses ~f:(fun (option, tags) ->
           ( Option.map option ~f:(fun t -> Staged.unstage t ~tycon_env ~tyvar_env)
           , tags )))
        ~case_sensitivity)
  ;;

  let union list =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Callbacks.union
        (List.map list ~f:(fun t -> Staged.unstage t ~tycon_env ~tyvar_env)))
  ;;

  let tyvar tyvar_name =
    Staged.stage (fun ~tycon_env:_ ~tyvar_env ->
      match Map.find tyvar_env tyvar_name with
      | None -> raise_s [%message "unbound type variable in grammar" ~tyvar_name]
      | Some x -> x)
  ;;

  let tycon tycon_name ~params =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      let params =
        List.map params ~f:(fun t -> Staged.unstage t ~tycon_env ~tyvar_env)
      in
      match Map.find tycon_env tycon_name with
      | None -> raise_s [%message "unbound type constructor in grammar" ~tycon_name]
      | Some (tyvar_names, make_t) ->
        (match List.zip tyvar_names params with
         | Unequal_lengths ->
           raise_s [%message "type constructor arity mismatch in grammar" ~tycon_name]
         | Ok tyvar_alist ->
           (match Map.of_alist (module String) tyvar_alist with
            | `Duplicate_key tyvar_name ->
              raise_s [%message "duplicate type constructor name in grammar" ~tyvar_name]
            | `Ok new_tyvar_env -> Staged.unstage make_t ~tyvar_env:new_tyvar_env)))
  ;;

  let recursive t ~defns =
    (* We define our new environments lazily. We only force them in staged functions. *)
    let rec lazy_tycon_env =
      lazy
        (match Map.of_alist (module String) defns with
         | `Duplicate_key tycon_name ->
           raise_s [%message "duplicate type constructor name in grammar" ~tycon_name]
         | `Ok defns ->
           Map.map defns ~f:(fun (tyvar_names, defn) ->
             let make_t =
               Staged.stage (fun ~tyvar_env ->
                 let tycon_env = Lazy.force lazy_tycon_env in
                 (* This [of_lazy_recursive] allows the recursion to stop unrolling at
                    some point. *)
                 Callbacks.of_lazy_recursive
                   (lazy (Staged.unstage defn ~tycon_env ~tyvar_env)))
             in
             tyvar_names, make_t))
    in
    let lazy_tyvar_env = lazy (Map.empty (module String)) in
    Staged.stage (fun ~tycon_env:_ ~tyvar_env:_ ->
      let tycon_env = Lazy.force lazy_tycon_env in
      let tyvar_env = Lazy.force lazy_tyvar_env in
      Staged.unstage t ~tycon_env ~tyvar_env)
  ;;

  let lazy_ lazy_t =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Callbacks.lazy_ (lazy (Staged.unstage (Lazy.force lazy_t) ~tycon_env ~tyvar_env)))
  ;;

  let finish_exn t =
    Staged.unstage
      t
      ~tycon_env:(Map.empty (module String))
      ~tyvar_env:(Map.empty (module String))
  ;;

  let finish_grammar_exn = finish_exn
  let finish_list_grammar_exn = finish_exn
end

module Fold_nonrecursive (Callbacks : Callbacks_for_fold_nonrecursive) :
  Fold with type t := Callbacks.t and type list_t := Callbacks.list_t = struct
  let rec of_grammar = function
    | Any name -> Callbacks.any name
    | Bool -> Callbacks.bool
    | Char -> Callbacks.char
    | Integer -> Callbacks.integer
    | Float -> Callbacks.float
    | String -> Callbacks.string
    | Option grammar -> Callbacks.option (of_grammar grammar)
    | List list_grammar -> Callbacks.list (of_list_grammar list_grammar)
    | Variant { case_sensitivity; clauses } ->
      List.map clauses ~f:of_clause_with_tag_list |> Callbacks.variant ~case_sensitivity
    | Union grammars -> Callbacks.union (List.map ~f:of_grammar grammars)
    | Tyvar tyvar_name -> Callbacks.tyvar tyvar_name
    | Tycon (tycon_name, params) ->
      Callbacks.tycon tycon_name ~params:(List.map ~f:of_grammar params)
    | Recursive (grammar, defns) ->
      let defns =
        List.map defns ~f:(fun { tycon; tyvars; grammar } ->
          tycon, (tyvars, of_grammar grammar))
      in
      Callbacks.recursive (of_grammar grammar) ~defns
    | Lazy lazy_grammar -> Callbacks.lazy_ (Lazy.map ~f:of_grammar lazy_grammar)
    | Tagged { key; value; grammar } -> Callbacks.tag (of_grammar grammar) key value

  and of_clause_with_tag_list = function
    | No_tag { name; clause_kind } ->
      (match clause_kind with
       | Atom_clause -> name, (None, [])
       | List_clause { args } -> name, (Some (of_list_grammar args), []))
    | Tag { key; value; grammar } ->
      let name, (grammar, tags) = of_clause_with_tag_list grammar in
      name, (grammar, (key, value) :: tags)

  and of_field_with_tag_list = function
    | No_tag { name; required; args } ->
      let args = of_list_grammar args in
      name, ((if required then Field.Required args else Field.Optional args), [])
    | Tag { key; value; grammar } ->
      let name, (grammar, tags) = of_field_with_tag_list grammar in
      name, (grammar, (key, value) :: tags)

  and of_list_grammar = function
    | Empty -> Callbacks.empty
    | Cons (grammar, list_grammar) ->
      Callbacks.cons (of_grammar grammar) (of_list_grammar list_grammar)
    | Many grammar -> Callbacks.many (of_grammar grammar)
    | Fields { fields; allow_extra_fields } ->
      let fields = List.map fields ~f:of_field_with_tag_list in
      Callbacks.record fields ~allow_extra_fields
  ;;

  let of_typed_grammar t = of_grammar t.untyped
end

module Fold_recursive (Callbacks : Callbacks_for_fold_recursive) :
  Fold_partial with type t := Callbacks.t and type list_t := Callbacks.list_t = struct
  module Step2 = Tie_the_knot (Callbacks)
  module Step1 = Fold_nonrecursive (Step2)

  let of_grammar_exn grammar = grammar |> Step1.of_grammar |> Step2.finish_grammar_exn

  let of_list_grammar_exn list_grammar =
    list_grammar |> Step1.of_list_grammar |> Step2.finish_list_grammar_exn
  ;;

  let of_typed_grammar_exn t = of_grammar_exn t.untyped
end

module Copy_callbacks = struct
  type t = grammar
  type list_t = list_grammar

  let any name = Any name
  let bool = Bool
  let char = Char
  let integer = Integer
  let float = Float
  let string = String
  let option grammar = Option grammar
  let union grammars = Union grammars
  let list list_grammar = List list_grammar
  let empty = Empty
  let cons grammar list_grammar = Cons (grammar, list_grammar)
  let many grammar = Many grammar
  let tag grammar key value = Tagged { key; value; grammar }

  let fold_tags untagged ~tags =
    List.fold_right tags ~init:(No_tag untagged) ~f:(fun (key, value) grammar ->
      Tag { key; value; grammar })
  ;;

  let record fields ~allow_extra_fields =
    let fields =
      List.map fields ~f:(fun (name, (field, tags)) ->
        let field =
          match (field : _ Field.t) with
          | Required args -> { name; args; required = true }
          | Optional args -> { name; args; required = false }
        in
        fold_tags field ~tags)
    in
    Fields { allow_extra_fields; fields }
  ;;

  let variant clauses ~case_sensitivity =
    let clauses =
      List.map clauses ~f:(fun (name, (maybe_args, tags)) ->
        let clause =
          match maybe_args with
          | None -> { name; clause_kind = Atom_clause }
          | Some args -> { name; clause_kind = List_clause { args } }
        in
        fold_tags clause ~tags)
    in
    Variant { case_sensitivity; clauses }
  ;;

  let tyvar name = Tyvar name
  let tycon name ~params = Tycon (name, params)

  let recursive grammar ~defns =
    let defns =
      List.map defns ~f:(fun (tycon, (tyvars, grammar)) -> { tycon; tyvars; grammar })
    in
    Recursive (grammar, defns)
  ;;
end

module Recursive_copy_callbacks = struct
  include Copy_callbacks

  let lazy_ lazy_t = Lazy lazy_t
  let of_lazy_recursive lazy_t = Lazy lazy_t
end

module Unroll_recursion = Fold_recursive (Recursive_copy_callbacks)

module Eager_copy_callbacks = struct
  include Copy_callbacks

  let lazy_ = Lazy.force
end

module Eager_copy = Fold_nonrecursive (Eager_copy_callbacks)

(* Leave [Lazy] constructors out of sexp. *)
let sexp_of_t _ t = sexp_of_grammar (Eager_copy.of_grammar t.untyped)

let first_tag_value tags name of_sexp =
  match List.Assoc.find tags name ~equal:String.equal with
  | None -> None
  | Some value -> Some (Or_error.try_with (fun () -> of_sexp value))
;;

let completion_suggested = "completion-suggested"
