open! Core

let doc_comment_tag = Sexplib.Sexp_grammar.doc_comment_tag

module Type = struct
  type _ t =
    | Record : Sexp_grammar.field t
    | Variant : Sexp_grammar.clause t

  let get_name : type a. a t -> a -> string =
    fun t item ->
    match t with
    | Record -> item.name
    | Variant -> item.name
  ;;

  type packed = P : _ t -> packed [@@unboxed]

  let [%compare: packed] = Poly.compare
end

module Item = struct
  (** A item is a single variant constructor or record field. *)

  type t =
    { name : string
    ; doc_comment : string option
    }
  [@@deriving compare]

  let is_undocumented t = Option.is_none t.doc_comment
end

module Group = struct
  (** A group is a variant or record type. *)

  type t =
    { type_ : Type.packed
    ; items : Item.t list
    ; number_undocumented : int
    }
  [@@deriving compare]

  let create type_ items =
    let items =
      List.map items ~f:(fun (item, tags) : Item.t ->
        let name = Type.get_name type_ item in
        let doc_comment =
          List.Assoc.find tags doc_comment_tag ~equal:[%equal: string]
          |> Option.map ~f:(fun (tag : Sexp.t) ->
            match tag with
            | Atom doc_comment -> doc_comment
            | List _ ->
              raise_s
                [%message "Value not atomic" ~key:doc_comment_tag ~value:(tag : Sexp.t)])
        in
        { name; doc_comment })
    in
    let number_undocumented = List.count items ~f:Item.is_undocumented in
    { type_ = P type_; items; number_undocumented }
  ;;

  let is_incompletely_documented t = t.number_undocumented > 0
end

module Path = struct
  type t = string list

  let compare =
    (* prefer shorter for legibility, then lexicographic for stability *)
    Comparable.lift [%compare: int * string list] ~f:(fun t -> List.length t, t)
  ;;

  let create = Reversed_list.rev
end

type t = Group.t * Path.t [@@deriving compare]

let create type_ items path = Group.create type_ items, Path.create path

let of_sexp_grammar ({ untyped = grammar } : _ Sexp_grammar.t) =
  let rec of_grammar path (grammar : Sexp_grammar.grammar) : t list =
    match grammar with
    | Any _ | Bool | Char | Integer | Float | String -> []
    | Option grammar -> of_grammar path grammar
    | List list_grammar -> of_list_grammar path list_grammar
    | Variant { case_sensitivity = _; clauses } ->
      let clauses = List.map clauses ~f:Sexp_grammar.extract_tag_list in
      let t = create Variant clauses path in
      let ts =
        List.concat_map clauses ~f:(fun ({ name; clause_kind }, _) ->
          match clause_kind with
          | Atom_clause -> []
          | List_clause { args } -> of_list_grammar (name :: path) args)
      in
      t :: ts
    | Union grammars -> List.concat_map grammars ~f:(of_grammar path)
    | Tagged { key = _; value = _; grammar } ->
      (* [Tagged] nodes that are attached to [Variant]s and [Fields] are already handled
         in their respective branches. *)
      of_grammar path grammar
    | Tyvar _ -> []
    | Tycon (_name, params, defns) ->
      List.concat_map params ~f:(of_grammar path)
      @ List.concat_map defns ~f:(fun { tycon = _; tyvars = _; grammar } ->
        of_grammar path grammar)
    | Recursive (_name, params) -> List.concat_map params ~f:(of_grammar path)
    | Lazy lazy_ -> of_grammar path (Portable_lazy.force lazy_)
  and of_list_grammar path : Sexp_grammar.list_grammar -> t list = function
    | Empty -> []
    | Cons (grammar, list_grammar) ->
      of_grammar path grammar @ of_list_grammar path list_grammar
    | Many grammar -> of_grammar path grammar
    | Fields { allow_extra_fields = _; fields } ->
      let fields = List.map fields ~f:Sexp_grammar.extract_tag_list in
      let t = create Record fields path in
      let ts =
        List.concat_map fields ~f:(fun ({ name; required = _; args }, _) ->
          of_list_grammar (name :: path) args)
      in
      t :: ts
  in
  of_grammar [] grammar
  |> List.Assoc.sort_and_group ~compare:Group.compare
  |> List.Assoc.map ~f:(fun paths ->
    List.min_elt paths ~compare:Path.compare |> Option.value_exn)
;;

let to_string_hum =
  let make_table name_and_docs =
    let uuid = "c7fa3db9-45f9-459f-94e0-2de8846528e5" (* randomly chosen by emacs *) in
    Ascii_table_kernel.to_string_noattr
      Ascii_table_kernel.[ Column.create "" fst; Column.create uuid snd ]
      name_and_docs
      ~bars:`Ascii
      ~display:Ascii_table_kernel.Display.line
      ~limit_width_to:85
    |> String.split_lines
    |> List.filter ~f:(fun line ->
      (* for compactness, we omit the purely box-drawing lines and the table header. *)
      (not (String.is_prefix line ~prefix:"|-"))
      && not (String.is_substring line ~substring:uuid))
  in
  let make_header number_undocumented type_ =
    let items =
      match (type_ : Type.packed) with
      | P Record -> "this record's fields"
      | P Variant -> "this variant's constructors"
    in
    {%string|%{number_undocumented#Int} of %{items} are undocumented:|}
  in
  let make_footer path =
    {%string|Here's one path by which this type is included in the sexp grammar:|}
    ::
    (match path with
     | [] -> [ "(toplevel)" ]
     | _ :: _ -> List.map path ~f:(sprintf "- %s"))
  in
  fun ({ number_undocumented; type_; items } : Group.t) ~path ~debug ->
    let header = make_header number_undocumented type_ in
    let rows =
      match debug with
      | false ->
        List.filter_map items ~f:(fun { name; doc_comment } ->
          match doc_comment with
          | Some _ -> None
          | None -> Some {%string|- %{name}|})
      | true ->
        make_table
          (List.map items ~f:(fun { name; doc_comment } ->
             name, String.strip (Option.value doc_comment ~default:"")))
    in
    let footer = make_footer path in
    String.concat_lines ((header :: rows) @ footer)
;;

let print_undocumented ?(debug = false) sexp_grammar =
  of_sexp_grammar sexp_grammar
  |> List.filter ~f:(fun (group, _path) -> Group.is_incompletely_documented group)
  (* Sort by path lexicographically for stability across documentation changes. *)
  |> List.sort ~compare:(Comparable.lift [%compare: string list] ~f:snd)
  |> List.iter ~f:(fun (t, path) -> print_endline (to_string_hum t ~path ~debug))
;;
