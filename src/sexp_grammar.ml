[@@@warning "-30"] (* allow duplicate field names *)

open! Core
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

    let compare a b = Comparable.lift String.compare ~f:String.capitalize a b

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
  | Tycon of string * grammar list * defn list
  | Recursive of string * grammar list
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
[@@deriving bin_io, compare, equal, hash, sexp]

type 'a t = 'a Sexp_grammar.t = { untyped : grammar }
[@@unboxed] [@@deriving bin_io, compare, equal, hash, sexp]

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
           Option.map option ~f:(fun t -> Staged.unstage t ~tycon_env ~tyvar_env), tags))
        ~case_sensitivity)
  ;;

  let union list =
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Callbacks.union (List.map list ~f:(fun t -> Staged.unstage t ~tycon_env ~tyvar_env)))
  ;;

  let tyvar tyvar_name =
    Staged.stage (fun ~tycon_env:_ ~tyvar_env ->
      match Map.find tyvar_env tyvar_name with
      | None -> raise_s [%message "unbound type variable in grammar" ~tyvar_name]
      | Some x -> x)
  ;;

  let tycon_generic tycon_name ~params =
    Staged.stage (fun ~tycon_env_for_name ~tycon_env_for_params ~tyvar_env ->
      let params =
        List.map params ~f:(fun t ->
          Staged.unstage t ~tycon_env:tycon_env_for_params ~tyvar_env)
      in
      match Map.find tycon_env_for_name tycon_name with
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

  let recursive tycon_name ~params =
    let generic = tycon_generic tycon_name ~params in
    Staged.stage (fun ~tycon_env ~tyvar_env ->
      Staged.unstage
        generic
        ~tycon_env_for_name:tycon_env
        ~tycon_env_for_params:tycon_env
        ~tyvar_env)
  ;;

  let tycon tycon_name ~params ~defns =
    let generic = tycon_generic tycon_name ~params in
    (* We define our new environments lazily. We only force them in staged functions. *)
    let rec lazy_tycon_env_for_name =
      lazy
        (match Map.of_alist (module String) defns with
         | `Duplicate_key tycon_name ->
           raise_s [%message "duplicate type constructor name in grammar" ~tycon_name]
         | `Ok defns ->
           Map.map defns ~f:(fun (tyvar_names, defn) ->
             let make_t =
               Staged.stage (fun ~tyvar_env ->
                 let tycon_env = Lazy.force lazy_tycon_env_for_name in
                 (* This [of_lazy_recursive] allows the recursion to stop unrolling at
                    some point. *)
                 Callbacks.of_lazy_recursive
                   (lazy (Staged.unstage defn ~tycon_env ~tyvar_env)))
             in
             tyvar_names, make_t))
    in
    Staged.stage (fun ~tycon_env:tycon_env_for_params ~tyvar_env ->
      let tycon_env_for_name = Lazy.force lazy_tycon_env_for_name in
      Staged.unstage generic ~tycon_env_for_name ~tycon_env_for_params ~tyvar_env)
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
    | Tycon (tycon, params, defns) ->
      let params = List.map ~f:of_grammar params in
      let defns =
        List.map defns ~f:(fun { tycon; tyvars; grammar } ->
          tycon, (tyvars, of_grammar grammar))
      in
      Callbacks.tycon tycon ~params ~defns
    | Recursive (tycon_name, params) ->
      Callbacks.recursive tycon_name ~params:(List.map ~f:of_grammar params)
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
  let recursive name ~params = Recursive (name, params)

  let tycon tycon_name ~params ~defns =
    let defns =
      List.map defns ~f:(fun (tycon, (tyvars, grammar)) -> { tycon; tyvars; grammar })
    in
    Tycon (tycon_name, params, defns)
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

let completion_suggested = Sexp_grammar.completion_suggested

let rec map_tag_list tag_list ~f =
  match tag_list with
  | No_tag grammar -> No_tag (f grammar)
  | Tag { key; value; grammar } -> Tag { key; value; grammar = map_tag_list grammar ~f }
;;

let rec without_tag_list tag_list =
  match tag_list with
  | No_tag grammar -> grammar
  | Tag { key = _; value = _; grammar } -> without_tag_list grammar
;;

let subst_tycon_body ~name ~params ~defns ~tag_prefix =
  let defn =
    match List.find defns ~f:(fun { tycon; _ } -> String.equal tycon name) with
    | Some defn -> defn
    | None ->
      raise_s
        [%message
          "could not find sexp grammar definition" (name : string) (defns : defn list)]
  in
  let tyvar_env =
    match List.zip defn.tyvars params with
    | Ok alist -> String.Map.of_alist_exn alist
    | Unequal_lengths ->
      raise_s
        [%message
          "wrong number of type variable parameters"
            (name : string)
            (defn : defn)
            (params : grammar list)]
  in
  let on_tyvar name =
    match Map.find tyvar_env name with
    | Some grammar -> grammar
    | None ->
      raise_s
        [%message
          "could not find type parameter"
            (name : string)
            (tyvar_env : grammar String.Map.t)]
  in
  let tag grammar ~suffix ~name =
    match tag_prefix with
    | None -> grammar
    | Some prefix ->
      let key = String.concat ~sep:"." [ prefix; suffix ] in
      Tagged { key; value = Atom name; grammar }
  in
  let rec on_grammar grammar =
    match grammar with
    | Any _ | Bool | Char | Integer | Float | String -> grammar
    | Option grammar -> Option (on_grammar grammar)
    | List list_grammar -> List (on_list_grammar list_grammar)
    | Variant { case_sensitivity; clauses } ->
      Variant
        { case_sensitivity; clauses = List.map clauses ~f:(map_tag_list ~f:on_clause) }
    | Union grammars -> Union (List.map grammars ~f:on_grammar)
    | Tagged { key; value; grammar } ->
      Tagged { key; value; grammar = on_grammar grammar }
    | Tyvar tyvar_name -> tag (on_tyvar tyvar_name) ~suffix:"tyvar" ~name:tyvar_name
    | Recursive (tycon_name, params) ->
      let grammar = Tycon (tycon_name, List.map params ~f:on_grammar, defns) in
      tag grammar ~suffix:"tycon" ~name:tycon_name
    | Lazy lazy_grammar -> Lazy (Lazy.map lazy_grammar ~f:on_grammar)
    | Tycon (name, params, defns) -> Tycon (name, List.map params ~f:on_grammar, defns)
  and on_list_grammar list_grammar =
    match list_grammar with
    | Empty -> Empty
    | Cons (first, rest) -> Cons (on_grammar first, on_list_grammar rest)
    | Many grammar -> Many (on_grammar grammar)
    | Fields { allow_extra_fields; fields } ->
      Fields
        { allow_extra_fields; fields = List.map fields ~f:(map_tag_list ~f:on_field) }
  and on_clause { name; clause_kind } = { name; clause_kind = on_clause_kind clause_kind }
  and on_clause_kind = function
    | Atom_clause -> Atom_clause
    | List_clause { args } -> List_clause { args = on_list_grammar args }
  and on_field { name; required; args } =
    { name; required; args = on_list_grammar args }
  in
  on_grammar defn.grammar
;;

let unroll_tycon_untyped ?tag_prefix grammar =
  let rec loop grammar =
    match grammar with
    | Any _
    | Bool
    | Char
    | Integer
    | Float
    | String
    | Option _
    | List _
    | Variant _
    | Union _
    | Tagged _
    | Tyvar _
    | Recursive _
    | Lazy _ -> grammar
    | Tycon (name, params, defns) ->
      let result = subst_tycon_body ~name ~params ~defns ~tag_prefix |> loop in
      (fun x -> x) (fun x -> x) result
  in
  loop grammar [@nontail]
;;

let unroll_tycon ?tag_prefix { untyped } =
  { untyped = unroll_tycon_untyped ?tag_prefix untyped }
;;

module Validation = struct
  open Or_error.Let_syntax

  module type Of_sexpable = sig
    type t [@@deriving of_sexp]
  end

  let of_type name (module M : Of_sexpable) =
    Staged.stage (fun sexp ->
      match M.t_of_sexp sexp with
      | (_ : M.t) -> Ok ()
      | exception reason ->
        let s = Printf.sprintf "invalid %s" name in
        Or_error.error_s [%message s (reason : exn)])
  ;;

  let require_list_with_leading_atom name sexp =
    match (sexp : Sexp.t) with
    | List (Atom field_name :: sexps) -> Ok (field_name, sexps)
    | Atom _ | List [] | List (List _ :: _) ->
      let s = Printf.sprintf "invalid %s; expected a list with a leading atom" name in
      Or_error.error_s [%message s (sexp : Sexp.t)]
  ;;

  module Z = struct
    type t = Zarith.Z.t

    (* Equivalent to [Bigint.t_of_sexp], without depending on all of [Core]. *)
    include Sexpable.Of_stringable (Zarith.Z)
  end

  module Seen_or_unseen = struct
    type 'a t =
      | Unseen of 'a
      | Seen
  end

  (* Thunked in order to allow fresh memory for each grammar being validated. *)
  module Handlers () : sig
    val on_grammar : grammar -> (Sexp.t -> unit Or_error.t) Staged.t
    val on_list_grammar : list_grammar -> (Sexp.t list -> unit Or_error.t) Staged.t
  end = struct
    (* Memoize the result of validating tycons. Avoids exponential explosion in
       pathological cases. *)
    module Memo_key = struct
      type t =
        { tycon_name : string
        ; tycon_args : grammar list
        ; tycon_defns : defn list
        ; sexp : Sexp.t
        }
      [@@deriving compare, hash, sexp_of]
    end

    let memo_table = Hashtbl.create (module Memo_key)

    let rec on_grammar grammar =
      match grammar with
      | Any _ -> Staged.stage (fun (_ : Sexp.t) -> Ok ())
      | Bool -> of_type "bool" (module Bool)
      | Char -> of_type "char" (module Char)
      | Integer -> of_type "integer" (module Z)
      | Float -> of_type "float" (module Float)
      | String -> of_type "string" (module String)
      | Tagged { key = _; value = _; grammar } -> on_grammar grammar
      | Option grammar ->
        let f = on_grammar grammar in
        let read_old_option_format = !Sexplib0.Sexp_conv.read_old_option_format in
        Staged.stage (fun sexp ->
          match (sexp : Sexp.t) with
          | Atom ("none" | "None") -> Ok ()
          | List [ Atom ("some" | "Some"); value_sexp ] -> Staged.unstage f value_sexp
          | List [] when read_old_option_format -> Ok ()
          | List [ value_sexp ] when read_old_option_format -> Staged.unstage f value_sexp
          | _ -> Or_error.error_s [%message "expected an option" (sexp : Sexp.t)])
      | Union grammars ->
        let fs = List.map grammars ~f:on_grammar in
        Staged.stage (fun sexp ->
          match Or_error.find_ok (List.map fs ~f:(fun f -> Staged.unstage f sexp)) with
          | Ok _ as ok -> ok
          | Error error ->
            let s = "expected union of several grammars, but none were satisfied." in
            Or_error.error_s [%message s ~_:(error : Error.t)])
      | Lazy grammar ->
        let lazy_f = Lazy.map grammar ~f:on_grammar in
        Staged.stage (fun sexp -> Staged.unstage (Lazy.force lazy_f) sexp)
      | List list_grammar ->
        let list_t = on_list_grammar list_grammar in
        Staged.stage (fun sexp ->
          match (sexp : Sexp.t) with
          | List sexps -> Staged.unstage list_t sexps
          | Atom _ -> Or_error.error_s [%message "expected a list" (sexp : Sexp.t)])
      | Variant { case_sensitivity; clauses } ->
        let clauses = List.map clauses ~f:without_tag_list in
        let clauses =
          List.map clauses ~f:(fun { name; clause_kind } ->
            let maybe_args =
              match clause_kind with
              | Atom_clause -> None
              | List_clause { args } -> Some (on_list_grammar args)
            in
            name, maybe_args)
        in
        let (module Name) = Case_sensitivity.to_string_comparator case_sensitivity in
        let clauses =
          match Map.of_alist (module Name) clauses with
          | `Ok clauses -> clauses
          | `Duplicate_key clause_name ->
            raise_s [%message "duplicate clause name in grammar" ~clause_name]
        in
        Staged.stage (fun sexp ->
          let%bind clause_name, maybe_sexps =
            match (sexp : Sexp.t) with
            | Atom name -> Ok (name, None)
            | List (Atom name :: args) -> Ok (name, Some args)
            | List [] | List (List _ :: _) ->
              Or_error.error_s
                [%message
                  "invalid variant; expected an atom or a list with a leading atom"
                    (sexp : Sexp.t)]
          in
          match Map.find clauses clause_name with
          | None ->
            Or_error.error_s
              [%message
                "invalid variant; unrecognized name"
                  (clause_name : string)
                  (case_sensitivity : Case_sensitivity.t)
                  ~recognized:(Map.keys clauses : string list)]
          | Some maybe_t_list ->
            (match maybe_sexps, maybe_t_list with
             | None, None -> Ok ()
             | None, Some _ ->
               let s =
                 Printf.sprintf
                   "invalid %S variant; expected a list but got an atom"
                   clause_name
               in
               Or_error.error_s [%message s (sexp : Sexp.t)]
             | Some _, None ->
               let s =
                 Printf.sprintf
                   "invalid %S variant; expected an atom but got a list"
                   clause_name
               in
               Or_error.error_s [%message s (sexp : Sexp.t)]
             | Some sexps, Some t_list -> Staged.unstage t_list sexps))
      | Tycon (tycon_name, tycon_args, tycon_defns) ->
        let lazy_t = lazy (on_grammar (unroll_tycon_untyped grammar)) in
        Staged.stage (fun sexp ->
          let (key : Memo_key.t) = { tycon_name; tycon_args; tycon_defns; sexp } in
          match Hashtbl.find memo_table key with
          | Some result -> result
          | None ->
            let result = Staged.unstage (Lazy.force lazy_t) sexp in
            Hashtbl.set memo_table ~key ~data:result;
            result)
      | Tyvar _ | Recursive _ ->
        raise_s
          [%message
            "unexpected [Tyvar] or [Recursive] should have been unrolled"
              ~grammar:(grammar : grammar)]

    and on_list_grammar list_grammar =
      match list_grammar with
      | Empty ->
        Staged.stage (fun remaining_sexps ->
          if List.is_empty remaining_sexps
          then Ok ()
          else
            Or_error.error_s
              [%message "too many sexps in list" (remaining_sexps : Sexp.t list)])
      | Cons (grammar, list_grammar) ->
        let t = on_grammar grammar in
        let list_t = on_list_grammar list_grammar in
        Staged.stage (fun sexps ->
          match sexps with
          | [] -> Or_error.error_s [%message "too few sexps in list"]
          | head :: tail ->
            (* Short-circuit on failure on head to avoid potential combinatorial explosion *)
            let%bind () = Staged.unstage t head in
            let%bind () = Staged.unstage list_t tail in
            Ok ())
      | Many grammar ->
        let t = on_grammar grammar in
        Staged.stage (fun sexps ->
          (* Short-circuit on failure to avoid potential combinatorial explosion *)
          match
            List.find_map sexps ~f:(fun sexp -> Result.error (Staged.unstage t sexp))
          with
          | None -> Ok ()
          | Some error ->
            let s = "An item in list did not satisfy grammar." in
            Or_error.error_s [%message s ~_:(error : Error.t)])
      | Fields { allow_extra_fields; fields } -> on_fields ~fields ~allow_extra_fields

    and on_field ~fields ~allow_extra_fields =
      let stop_error sexp = Continue_or_stop.Stop (Or_error.error_s sexp) in
      Staged.stage (fun sexp : (_, _) Continue_or_stop.t ->
        (match require_list_with_leading_atom "record field" sexp with
         | Error _ as stop -> Stop stop
         | Ok (field_name, sexps) ->
           (match (Map.find fields field_name : _ Seen_or_unseen.t option) with
            | None ->
              if allow_extra_fields
              then Continue fields
              else
                stop_error
                  [%message
                    "unrecognized record field"
                      (field_name : string)
                      ~recognized:(Map.keys fields : string list)
                      (sexp : Sexp.t)]
            | Some Seen ->
              stop_error [%message "duplicate record field" (field_name : string)]
            | Some (Unseen (Required list_grammar | Optional list_grammar : _ Field.t)) ->
              let t_list = on_list_grammar list_grammar in
              (match Staged.unstage t_list sexps with
               | Ok () -> Continue (Map.set fields ~key:field_name ~data:Seen)
               | Error _ as reject -> Stop reject))))

    and on_fields ~fields ~allow_extra_fields =
      let fields = List.map fields ~f:without_tag_list in
      let fields =
        List.map fields ~f:(fun { name; required; args } ->
          name, if required then Field.Required args else Optional args)
      in
      let fields =
        match
          fields
          |> List.Assoc.map ~f:(fun field -> Seen_or_unseen.Unseen field)
          |> Map.of_alist (module String)
        with
        | `Ok fields -> fields
        | `Duplicate_key field_name ->
          raise_s [%message "duplicate field name in grammar" ~field_name]
      in
      Staged.stage (fun sexps ->
        List.fold_until
          sexps
          ~init:fields
          ~f:(fun fields sexp ->
            Staged.unstage (on_field ~fields ~allow_extra_fields) sexp)
          ~finish:(fun fields ->
            Or_error.find_map_ok (Map.to_alist fields) ~f:(fun (field_name, status) ->
              match status with
              | Seen | Unseen (Optional _) -> Ok ()
              | Unseen (Required _) ->
                Or_error.error_s [%message "missing record field" (field_name : string)])))
    ;;
  end
end

let validate_sexp_untyped grammar =
  let module Handlers = Validation.Handlers () in
  Handlers.on_grammar grammar
;;

let validate_sexp_list list_grammar =
  let module Handlers = Validation.Handlers () in
  Handlers.on_list_grammar list_grammar
;;

let validate_sexp { untyped } = validate_sexp_untyped untyped
