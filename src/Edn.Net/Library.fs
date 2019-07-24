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
             | ENull
             | EBool of bool
             | EKeyword of Keyword
             | EVector of Edn list
             | ESet of Set<Edn>
             | EMap of Map<Edn, Edn>

    type EdnParser = Parser<Edn, unit>

    /// null parser
    let enull: EdnParser = stringReturn "nil" ENull

    /// bool parser
    let ebool: EdnParser = (stringReturn "true" (EBool true))
                                    <|> (stringReturn "false" (EBool false))

    let efloat: EdnParser = pfloat |>> EFloat

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

    let comment = str ";" >>. skipRestOfLine true

    //--------------- keyword -----------------
    let name = many (letter <|> anyOf "-_*?!$%&=><") |>> (List.toArray >> (System.String))
    let ekeyword :EdnParser = ((str ":") >>. name .>> optional (str "/")) .>>. name 
                                |>> (fun p ->
                                        match p with
                                        | (sym, "") -> EKeyword {ns = None; symbol = sym}
                                        | (ns, sym) -> EKeyword {ns = Some ns; symbol = sym})

    // forward declare
    let evalue, evalueRef = createParserForwardedToRef<Edn, unit>()

    let ws = skipSepBy (many (anyOf " ,\t\n\r")) comment  

    let listBetween sopen sclose pElement f =
        between (str sopen) (str sclose)
                (ws >>. many (pElement .>> ws) |>> f)

    let evector = listBetween "[" "]" evalue EVector           

    let eset = listBetween "#{" "}" evalue (Set.ofList >> ESet)

    let keyValue = evalue .>>. (ws >>. evalue)

    let emap = listBetween "{" "}" keyValue (Map.ofList >> EMap)

    do evalueRef := ws >>. choice [ebool
                                   enull
                                   efloat
                                   emap
                                   evector
                                   eset
                                   ekeyword
                                   estring] .>> ws

    let Parse = run evalue
