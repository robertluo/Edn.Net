module Tests

open Expecto
open Robertluo.Edn
open FParsec

[<Tests>]
let tests =
  testList "samples" [
    testCase "parse overall" <| fun _ ->
      match Parse "[3.0 :foo/bar]" with
      | Success (v, _, _) -> Expect.equal v (EVector [
                                              (EFloat 3.0);
                                              (EKeyword {ns = Some "foo"; symbol = "bar"})])
                                          "should equal"
      | _ -> failwith "unable to match"
  ]