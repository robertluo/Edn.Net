module Tests

open Expecto
open Robertluo
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
                          (":foo/bar", EKeyword {Ns = Some("foo"); Name = "bar"})
                          (":foo", EKeyword {Ns = None; Name = "foo"})
                          ("#{25.0, :foo1}", ESet (Set.ofList [EFloat 25.0; EKeyword {Ns = None; Name = "foo1"}]))]
      for KeyValue (input , expected) in matrix do
        match Edn.Parse input with
        | Success(actual, _, _) -> Expect.equal actual expected "should parse"
        | _ -> failtest "not able to parse"

    testCase "can skip comment" <| fun _ ->
      let input = """
                  [true, ;ok
                   nil]
                   """
      let expected = EVector [EBool true; ENull]
      match Edn.Parse input with
      | Success (actual, _, _) -> Expect.equal actual expected "as white space"
      | Failure (errorMsg, _, _) -> failtest errorMsg

    testCase "can parse a nested map and other" <| fun _ ->
      let input = """
                  {:foo/bar [35.1, false,]
                   :a nil}
                  """
      let expected = EMap (Map.ofList [(EKeyword {Ns = Some "foo"; Name = "bar"},
                                        EVector [EFloat 35.1; EBool false])
                                       (EKeyword {Ns = None; Name = "a"},
                                        ENull)])
      match Edn.Parse input with
      | Success (actual, _, _) -> Expect.equal actual expected "without problem"
      | Failure (errorMsg, _, _) -> failtest errorMsg

]

[<Tests>]
let test2 = testList "ToString" [
  testCase "keyword" <| fun _ ->
    let input = (EKeyword {Ns = None; Name = "foo"}).ToString()
    Expect.equal input ":foo" "should like :foo"
    let input = (EKeyword {Ns = Some "foo"; Name = "bar"}).ToString()
    Expect.equal input ":foo/bar" "should contains ns"

  testCase "complex" <| fun _ ->
    let input =  EMap (Map.ofList [(EKeyword {Ns = Some "foo"; Name = "bar"},
                                    EVector [EFloat 35.1; EBool false])
                                   (EKeyword {Ns = None; Name = "a"},
                                    ENull)])
    let s = input.ToString()
    Expect.equal s "{:a nil, :foo/bar [35.1, false]}" ""                                     
]
