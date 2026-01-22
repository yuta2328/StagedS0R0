open Alcotest
open Common.All
open StagedS0R0
open Infer

module TestType = struct
  type t = (InferResult.t, InferError.t) result

  let equal a b = a = b

  let pp fmt = function
    | Ok infer_result -> Format.fprintf fmt "Ok %a" InferResult.pp infer_result
    | Error err -> Format.fprintf fmt "Error %s" (InferError.show err)
end

