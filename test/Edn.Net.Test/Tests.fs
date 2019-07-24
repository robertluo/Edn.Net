module Tests

open Expecto
open Robertluo.Edn
open FParsec

[<Tests>]
let tests =
  testList "Edn Parsing Test" [
    testCase "when input is a valid edn string" <| fun _ ->
      let matrix = dict [ ("-322.5", EFloat -322.5) 
                          ("2554.0", EFloat 2554.0)
                          ("false", EBool false)
                          ("true", EBool true)
                          ("nil", ENull)
                          ("\"foo\"", EString "foo")
                          (":foo/bar", EKeyword {ns = Some("foo"); symbol = "bar"})
                          (":foo", EKeyword {ns = None; symbol = "foo"})
                          ("#{25.0, :foo}", ESet (Set.ofList [EFloat 25.0; EKeyword {ns = None; symbol = "foo"}]))]
      for KeyValue (input , expected) in matrix do
        match Parse input with
        | Success(actual, _, _) -> Expect.equal actual expected "should parse"
        | _ -> failtest "not able to parse"

    testCase "can skip comment" <| fun _ ->
      let input = """
                  [true, ;ok
                   nil]
                   """
      let expected = EVector [EBool true; ENull]
      match Parse input with
      | Success (actual, _, _) -> Expect.equal actual expected "as white space"
      | Failure (errorMsg, _, _) -> failtest errorMsg

    testCase "can parse a nested map and other" <| fun _ ->
      let input = """
                  {:foo/bar [35.1, false,]
                   :a nil}
                  """
      let expected = EMap (Map.ofList [(EKeyword {ns = Some "foo"; symbol = "bar"},
                                        EVector [EFloat 35.1; EBool false])
                                       (EKeyword {ns = None; symbol = "a"},
                                        ENull)])
      match Parse input with
      | Success (actual, _, _) -> Expect.equal actual expected "without problem"
      | Failure (errorMsg, _, _) -> failtest errorMsg
]