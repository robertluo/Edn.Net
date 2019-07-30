module Tests

open Expecto
open Robertluo
open FParsec
open System

let testParse input expected comment =
    match Edn.Parse input with
    | Success(actual,_,_) -> Expect.equal actual expected comment
    | Failure(msg,_,_) -> failtest msg

[<Tests>]
let tests =
    testList "Edn Parsing Test"
        [ testCase "When parse integer" <| fun _ ->
              let inputs =
                  dict [ ("+3575", EInteger 3575L)
                         ("-857", EInteger -857L)
                         ("-0", EInteger 0L)]
              for KeyValue(input, expected) in inputs do
                  testParse input expected "should parse"

          testCase "when input is a valid edn string" <| fun _ ->
              let matrix =
                  dict [ ("-322.5", EFloat -322.5)
                         ("2554.0", EFloat 2554.0)
                         ("false", EBool false)
                         ("true", EBool true)
                         ("nil", ENull)
                         ("\"foo\"", EString "foo")
                         (":foo/bar",
                          EKeyword { Ns = Some("foo")
                                     Name = "bar" })
                         (":foo",
                          EKeyword { Ns = None
                                     Name = "foo" })
                         ("#{25.0, :foo1}",
                          ESet(Set.ofList [ EFloat 25.0
                                            EKeyword { Ns = None
                                                       Name = "foo1" } ])) ]
              for KeyValue(input, expected) in matrix do
                  testParse input expected "should parse"

          testCase "can skip comment" <| fun _ ->
              let input = """
                  [true, ;ok
                   nil]
                   """
              let expected =
                  EVector [ EBool true
                            ENull ]
              testParse input expected "should ok"

          testCase "can parse a nested map and other" <| fun _ ->
              let input = """
                  {:foo/bar [35.1, false,]
                   :a nil}
                  """
              let expected =
                  EMap(Map.ofList [ (EKeyword { Ns = Some "foo"
                                                Name = "bar" },
                                     EVector [ EFloat 35.1
                                               EBool false ])
                                    (EKeyword { Ns = None
                                                Name = "a" }, ENull) ])
              testParse input expected "should match"

          testCase "clojure 1.9 map key compaction" <| fun _ ->
              let input = "#:foo{:id true, :bar/baz nil}"
              let expected = EMap (Map.ofList [ (EKeyword {Ns = Some "foo"; Name = "id"}, EBool true)
                                                (EKeyword {Ns = Some "bar"; Name = "baz"}, ENull)])
              testParse input expected "should append ns to empty ns keys"

          testCase "tagged uuid" <| fun _ ->
              let input = "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
              let expected = EUuid (Guid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
              testParse input expected "should run" 

          testCase "tagged instant" <| fun _ ->
              let input = "#inst \"1985-04-12T23:20:50.52Z\""
              match Edn.Parse input with
              | Success(EInstant v, _, _) -> Expect.equal v.Year 1985 "should readable"
              | v -> failtestf "not understandable: %A" v
        ]
            
[<Tests>]
let test2 =
    testList "ToString"
        [ testCase "keyword" <| fun _ ->
              let input =
                  (EKeyword { Ns = None
                              Name = "foo" })
                      .ToString()
              Expect.equal input ":foo" "should like :foo"
              let input =
                  (EKeyword { Ns = Some "foo"
                              Name = "bar" })
                      .ToString()
              Expect.equal input ":foo/bar" "should contains ns"
          testCase "complex" <| fun _ ->
              let input =
                  EMap(Map.ofList [ (EKeyword { Ns = Some "foo"
                                                Name = "bar" },
                                     EVector [ EFloat 35.1
                                               EBool false ])
                                    (EKeyword { Ns = None
                                                Name = "a" }, ENull) ])

              let s = input.ToString()
              Expect.equal s "{:a nil, :foo/bar [35.1, false]}" "" ]
