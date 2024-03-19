open! Core
open! Expect_test_helpers_base

let () = sexp_style := Sexp_style.simple_pretty

module Unrolled = struct
  type t =
    | Unchanged
    | Changed of Sexp_grammar.grammar
end

let unroll original : Unrolled.t =
  let unrolled = Sexp_grammar.unroll_tycon_untyped original in
  match Sexp_grammar.equal_grammar original unrolled with
  | true -> Unchanged
  | false -> Changed unrolled
;;

let require_does_unroll original =
  match unroll original with
  | Unchanged ->
    print_cr
      [%here]
      [%message "unrolling did not change grammar" (original : Sexp_grammar.grammar)]
  | Changed unrolled ->
    print_s
      [%message "" (original : Sexp_grammar.grammar) (unrolled : Sexp_grammar.grammar)]
;;

let require_does_not_unroll original =
  match unroll original with
  | Unchanged -> print_s [%sexp (original : Sexp_grammar.grammar)]
  | Changed unrolled ->
    print_cr
      [%here]
      [%message
        "unrolling changed grammar"
          (original : Sexp_grammar.grammar)
          (unrolled : Sexp_grammar.grammar)]
;;

let%expect_test "non-tycon definitions" =
  require_does_not_unroll Integer;
  [%expect {| Integer |}];
  require_does_not_unroll
    (Variant
       { case_sensitivity = Case_insensitive
       ; clauses = [ No_tag { name = "A"; clause_kind = Atom_clause } ]
       });
  [%expect
    {|
    (Variant
     ((case_sensitivity Case_insensitive)
      (clauses ((No_tag ((name A) (clause_kind Atom_clause)))))))
    |}]
;;

let id grammar =
  Sexp_grammar.Tycon
    ("id", [ grammar ], [ { tycon = "id"; tyvars = [ "a" ]; grammar = Tyvar "a" } ])
;;

let%expect_test "nonrecursive tycon" =
  require_does_unroll (id String);
  [%expect
    {|
    ((original
      (Tycon id (String) (((tycon id) (tyvars (a)) (grammar (Tyvar a))))))
     (unrolled String))
    |}]
;;

let sexp grammar =
  Sexp_grammar.Tycon
    ( "sexp"
    , [ grammar ]
    , [ { tycon = "sexp"
        ; tyvars = [ "atom" ]
        ; grammar = Union [ List (Many (Recursive ("sexp", [ grammar ]))); Tyvar "atom" ]
        }
      ] )
;;

let%expect_test "recursive tycon" =
  require_does_unroll (sexp String);
  [%expect
    {|
    ((original
      (Tycon
       sexp
       (String)
       ((
        (tycon sexp)
        (tyvars (atom))
        (grammar (Union ((List (Many (Recursive sexp (String)))) (Tyvar atom))))))))
     (unrolled
      (Union
       ((List
         (Many
          (Tycon
           sexp
           (String)
           ((
            (tycon sexp)
            (tyvars (atom))
            (grammar
             (Union ((List (Many (Recursive sexp (String)))) (Tyvar atom)))))))))
        String))))
    |}]
;;

let%expect_test "tycon is not topmost node" =
  require_does_not_unroll (Option (sexp String));
  [%expect
    {|
    (Option
     (Tycon
      sexp
      (String)
      ((
       (tycon sexp)
       (tyvars (atom))
       (grammar (Union ((List (Many (Recursive sexp (String)))) (Tyvar atom))))))))
    |}]
;;

let%expect_test "argument of tycons are not unrolled" =
  require_does_unroll (sexp (id String));
  [%expect
    {|
    ((original
      (Tycon
       sexp
       ((Tycon id (String) (((tycon id) (tyvars (a)) (grammar (Tyvar a))))))
       ((
        (tycon sexp)
        (tyvars (atom))
        (grammar
         (Union
          ((List
            (Many
             (Recursive
              sexp
              ((Tycon id (String) (((tycon id) (tyvars (a)) (grammar (Tyvar a)))))))))
           (Tyvar atom))))))))
     (unrolled
      (Union
       ((List
         (Many
          (Tycon
           sexp
           ((Tycon id (String) (((tycon id) (tyvars (a)) (grammar (Tyvar a))))))
           ((
            (tycon sexp)
            (tyvars (atom))
            (grammar
             (Union
              ((List
                (Many
                 (Recursive
                  sexp
                  ((
                   Tycon
                   id
                   (String)
                   (((tycon id) (tyvars (a)) (grammar (Tyvar a)))))))))
               (Tyvar atom)))))))))
        (Tycon id (String) (((tycon id) (tyvars (a)) (grammar (Tyvar a)))))))))
    |}]
;;

let alist grammar =
  Sexp_grammar.Tycon
    ( "alist"
    , [ grammar ]
    , [ { tycon = "pair"
        ; tyvars = [ "fst"; "snd" ]
        ; grammar = List (Cons (Tyvar "fst", Cons (Tyvar "snd", Empty)))
        }
      ; { tycon = "list"; tyvars = [ "elt" ]; grammar = List (Many (Tyvar "elt")) }
      ; { tycon = "alist"
        ; tyvars = [ "data" ]
        ; grammar = Recursive ("list", [ Recursive ("pair", [ String; grammar ]) ])
        }
      ] )
;;

let%expect_test "only directly nested tycons are unrolled" =
  require_does_unroll (alist Integer);
  [%expect
    {|
    ((original
      (Tycon
       alist
       (Integer)
       (((tycon pair)
         (tyvars (fst snd))
         (grammar (List (Cons (Tyvar fst) (Cons (Tyvar snd) Empty)))))
        ((tycon list) (tyvars (elt)) (grammar (List (Many (Tyvar elt)))))
        ((tycon alist)
         (tyvars (data))
         (grammar (Recursive list ((Recursive pair (String Integer)))))))))
     (unrolled
      (List
       (Many
        (Tycon
         pair
         (String Integer)
         (((tycon pair)
           (tyvars (fst snd))
           (grammar (List (Cons (Tyvar fst) (Cons (Tyvar snd) Empty)))))
          ((tycon list) (tyvars (elt)) (grammar (List (Many (Tyvar elt)))))
          ((tycon alist)
           (tyvars (data))
           (grammar (Recursive list ((Recursive pair (String Integer))))))))))))
    |}]
;;

let tree grammar =
  Sexp_grammar.Tycon
    ( "tree"
    , [ grammar ]
    , [ { tycon = "tree"
        ; tyvars = [ "key" ]
        ; grammar =
            List
              (Cons
                 ( Recursive ("tree", [ Tyvar "key" ])
                 , Cons (Tyvar "key", Cons (Recursive ("tree", [ Tyvar "key" ]), Empty))
                 ))
        }
      ] )
;;

let%expect_test "tycon substituted in multiple places" =
  require_does_unroll (tree String);
  [%expect
    {|
    ((original
      (Tycon
       tree
       (String)
       ((
        (tycon tree)
        (tyvars (key))
        (grammar
         (List
          (Cons
           (Recursive tree ((Tyvar key)))
           (Cons (Tyvar key) (Cons (Recursive tree ((Tyvar key))) Empty)))))))))
     (unrolled
      (List
       (Cons
        (Tycon
         tree
         (String)
         ((
          (tycon tree)
          (tyvars (key))
          (grammar
           (List
            (Cons
             (Recursive tree ((Tyvar key)))
             (Cons (Tyvar key) (Cons (Recursive tree ((Tyvar key))) Empty))))))))
        (Cons
         String
         (Cons
          (Tycon
           tree
           (String)
           ((
            (tycon tree)
            (tyvars (key))
            (grammar
             (List
              (Cons
               (Recursive tree ((Tyvar key)))
               (Cons (Tyvar key) (Cons (Recursive tree ((Tyvar key))) Empty))))))))
          Empty))))))
    |}]
;;
