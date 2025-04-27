(** [Complete] requests a completion.

    The client and server must agree out-of-band about the grammar to be used.

    The division of labor between client and server is as follows:

    1. The client determines the current sexp and current atom. It sends the server a
       prefix of the current sexp, excluding the current atom.
    2. The server returns all candidates consistent with the current sexp.
    3. The client additionally filters for consistency with the current atom.

    Steps 1 and 3 happen on the client so we can use editor-specific APIs to improve
    latency and user experience. E.g., the client may know that only the atom prefix has
    been edited, and avoid an unnecessary call to the server. Or the client can display
    and incrementally refine the candidates using an editor-specific fuzzy matching API.

    See {!Candidate.matches_atom_prefix} for a default implementation of step 3.

    Step 2 happens on the server so that the editor need not link an up-to-date grammar.
    The server may also know more about the semantics of the sexp being edited, and use
    that to implement better completion than would be possible using the grammar alone.

    [1] Preceding complete sexps are ignored because the sexp grammar completion model
    does not take advantage of that information. *)

open! Core
open! Async_rpc_kernel
open! Import

type query = Prefix.t [@@deriving compare, sexp]
type response = Candidates.t [@@deriving compare, sexp_of]

include
  Async_rpc_kernel.Versioned_rpc.Both_convert.Plain.S
  with type callee_query := query
  with type callee_response := response
  with type caller_query := query
  with type caller_response := response
