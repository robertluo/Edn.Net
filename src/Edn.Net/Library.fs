namespace Robertluo

module Edn =
    open FParsec

    let test p str =
        match run p str with
        | Success(result, _, _) -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Error: %A" errorMsg

    type Keyword = {
        ns: string option
        symbol: string
    }

    type Edn = EString of string
             | EFloat of float
             | EInt of int64
             | ENull
             | EBool of bool
             | EComment of string
             | EKeyword of Keyword
             | EVector of Edn list
             | EMap of Map<Edn, Edn>

    type EdnParser = Parser<Edn, unit>

    /// null parser
    let enull: EdnParser = stringReturn "nil" ENull

    /// bool parser
    let ebool: EdnParser = (stringReturn "true" (EBool true))
                                    <|> (stringReturn "false" (EBool false))

    let efloat: EdnParser = pfloat |>> EFloat

    let enumber: EdnParser = pint64 |>> EInt

    // -------------- string ------------------------
    let str s = pstring s
    let stringLiteral: Parser<string, unit> =
        let escape = anyOf "\"\\/bfnrt"
                     |>> function
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

    // forward declare
    let evalue, evalueRef = createParserForwardedToRef<Edn, unit>()

    let ws = (many (anyOf " ,\t\n\r")) |>> ignore

    let listBetween sopen sclose pElement f =
        between (str sopen) (str sclose)
                (ws >>. many (pElement .>> ws) |>> f)

    let evector = listBetween "[" "]" evalue EVector           

    let keyValue = evalue .>>. (ws >>. evalue)

    let emap = listBetween "{" "}" keyValue (Map.ofList >> EMap)

    do evalueRef := choice [emap
                            evector
                            ebool
                            enull
                            enumber
                            efloat
                            estring]
