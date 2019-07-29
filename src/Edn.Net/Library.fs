namespace Robertluo

type Keyword =
    { Ns : string option
      Name : string }

type Edn =
    | EString of string
    | EFloat of float
    | ENull
    | EBool of bool
    | EKeyword of Keyword
    | EVector of Edn list
    | ESet of Set<Edn>
    | EMap of Map<Edn, Edn>
    override this.ToString() =
        match this with
        | ENull -> "nil"
        | EBool v ->
            if v then "true"
            else "false"
        | EKeyword { Ns = ns; Name = name } ->
            match ns with
            | Some n -> sprintf ":%s/%s" n name
            | None -> sprintf ":%s" name
        | EVector v ->
            v
            |> List.map (fun i -> i.ToString())
            |> String.concat ", "
            |> sprintf "[%s]"
        | ESet v ->
            Set.toList v
            |> List.map (fun i -> i.ToString())
            |> String.concat ", "
            |> sprintf "#{%s}"
        | EMap m ->
            Map.toList m
            |> List.map (fun (k, v) -> k.ToString() + " " + v.ToString())
            |> String.concat ", "
            |> sprintf "{%s}"
        | EString s -> "\"" + s + "\""
        | EFloat f -> f.ToString()

[<RequireQualifiedAccessAttribute>]
module Edn =
    open FParsec

    let test p str =
        match run p str with
        | Success(result, _, _) -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Error: %A" errorMsg

    /// null parser
    let enull = stringReturn "nil" ENull

    /// bool parser
    let ebool =
        (stringReturn "true" (EBool true))
        <|> (stringReturn "false" (EBool false))

    let efloat = pfloat |>> EFloat
    // -------------- string ------------------------
    let str s = pstring s

    let stringLiteral : Parser<string, unit> =
        let escape =
            anyOf "\"\\/bfnrt" |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c

        let escapedCharSnippet = str "\\" >>. escape
        let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')
        between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)

    let estring = stringLiteral |>> EString
    let comment = str ";" >>. skipRestOfLine true
    //--------------- keyword -----------------
    let name =
        many (letter <|> anyOf "-_*?!$%&=><0123456789")
        |>> (List.toArray >> (System.String))

    let ekeyword =
        ((str ":") >>. name .>> optional (str "/")) .>>. name |>> (fun p ->
        match p with
        | (sym, "") ->
            EKeyword { Ns = None
                       Name = sym }
        | (ns, sym) ->
            EKeyword { Ns = Some ns
                       Name = sym })

    // forward declare
    let evalue, evalueRef = createParserForwardedToRef<Edn, unit>()

    let ws =
        skipSepBy (many (anyOf " ,\t\n\r")) comment

    let listBetween sopen sclose pElement f =
        between (str sopen) (str sclose) (ws >>. many (pElement .>> ws) |>> f)
    let evector = listBetween "[" "]" evalue EVector
    let eset = listBetween "#{" "}" evalue (Set.ofList >> ESet)
    let keyValue = evalue .>>. (ws >>. evalue)
    let emap = listBetween "{" "}" keyValue (Map.ofList >> EMap)

    do evalueRef
       := ws
          >>. choice
                  [ ebool; enull; efloat; emap; evector; eset; ekeyword; estring ]
          .>> ws

    //-------------- Interface ---------------
    let Parse = run evalue
    let seqToString sep seq =
        (String.concat sep (List.map (fun i -> i.ToString()) seq))
