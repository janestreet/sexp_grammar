open! Base
include Expect_test_helpers_base
include Expect_test_patdiff
include Sexp_grammar_completion_test_helpers
module Prefix = Sexp_grammar_completion_protocol.Prefix

let ok_exn = Or_error.ok_exn
