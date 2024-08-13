module Stable = struct
  module Candidates = Candidates.Stable
  module Prefix = Prefix.Stable

  module Query = struct
    module V1 = Prefix.V1
    module Model = V1
  end

  module Response = struct
    module V1 = Candidates.V1
    module V2 = Candidates.V2
    module Model = V2
  end
end

open! Core
open! Async_rpc_kernel
open! Import

type query = Stable.Query.Model.t [@@deriving compare, sexp]
type response = Stable.Response.Model.t [@@deriving compare, sexp_of]

include Versioned_rpc.Both_convert.Plain.Make (struct
    let name = "sexp-grammar-complete"

    module Callee = struct
      type nonrec query = query
      type nonrec response = response
    end

    module Caller = Callee
  end)

include Register (struct
    let version = 1

    type query = Stable.Query.V1.t [@@deriving bin_io]

    let caller_model_of_response = Stable.Response.V2.from_prev
    let response_of_callee_model = Stable.Response.V2.to_prev

    type response = Stable.Response.V1.t [@@deriving bin_io]

    let callee_model_of_query q = q
    let query_of_caller_model q = q
  end)

include Register (struct
    let version = 2

    type query = Stable.Query.V1.t [@@deriving bin_io]

    let callee_model_of_query q = q
    let query_of_caller_model q = q

    type response = Stable.Response.V2.t [@@deriving bin_io]

    let caller_model_of_response r = r
    let response_of_callee_model r = r
  end)
