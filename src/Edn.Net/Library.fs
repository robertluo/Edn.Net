namespace Robertluo

type Keyword =
    { Ns : string option
      Name : string }
    override this.ToString() =
        match this with
        | { Ns = None; Name = name } -> name.ToString()
        | { Ns = Some ns; Name = name } -> ns.ToString() + "/" + name.ToString()

type Edn =
    | EString of string
    | EInteger of int64
    | EFloat of float
    | ENull
    | EBool of bool
    | EKeyword of Keyword
    | ESymbol of Keyword
    | EVector of Edn list
    | ESet of Set<Edn>
    | EMap of Map<Edn, Edn>
    | EUuid of System.Guid //to support builtin uuid
    | EInstant of System.DateTime //to support builtin instant
    override this.ToString() =
        match this with
        | ENull -> "nil"
        | EBool v ->
            if v then "true"
            else "false"
        | EString s -> "\"" + s + "\""
        | EInteger v -> v.ToString()
        | EFloat f -> f.ToString()
        | EKeyword v -> ":" + v.ToString()
        | ESymbol v -> "'" + v.ToString()
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
        | EUuid uuid -> "#uuid \"" + uuid.ToString() + "\""
        | EInstant dt -> "#instant \"" + dt.ToString() + "\""

[<RequireQualifiedAccessAttribute>]
module Edn =
    open FParsec
    open System.Xml

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

    let charListToStr charList =
        charList
        |> List.toArray
        |> System.String

    let enumber =
        let toNumber ((sign, s1), s2) =
            let sign =
                match sign with
                | Some v -> v
                | None -> '+'

            let s1 = (string sign) + (charListToStr s1)
            match s2 with
            | None -> EInteger(int64 s1)
            | Some v ->
                s1 + "." + (charListToStr v)
                |> float
                |> EFloat

        let digits = anyOf "01234567890"
        opt (anyOf "+-") .>>. (many1 digits)
        .>>. opt ((pstring ".") >>. (many digits)) |>> toNumber

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
        many (letter <|> anyOf ".*+!-_?$%&=<>0123456789")
        |>> (List.toArray >> (System.String))

    let symbol =
        name .>> optional (str "/") .>>. name |>> (fun p ->
        match p with
        | (sym, "") ->
            { Ns = None
              Name = sym }
        | (ns, sym) ->
            { Ns = Some ns
              Name = sym })

    let ekeyword = (str ":") >>. symbol |>> EKeyword
    let esymbol = (str "'") >>. symbol |>> ESymbol
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

    //assoc a default ns to map m if a key does not have a ns
    let assocNsToMap ns m : Map<Edn, Edn> =
        m
        |> Map.fold (fun rst k v ->
               match k with
               | EKeyword { Ns = None; Name = name } ->
                   rst.Add(EKeyword { Ns = Some ns
                                      Name = name }, v)
               | _ -> rst.Add(k, v)) Map.empty

    //tagged element support
    let etagged =
        (str "#") >>. (ekeyword <|> (symbol |>> ESymbol)) .>>. evalue |>> function
        | EKeyword { Ns = None; Name = name }, EMap m ->
            EMap(assocNsToMap name m)
        | ESymbol { Ns = None; Name = "uuid" }, EString s ->
            EUuid(System.Guid.Parse s)
        | ESymbol { Ns = None; Name = "inst" }, EString s ->
            EInstant(XmlConvert.ToDateTime(s, XmlDateTimeSerializationMode.Utc))
        | k, v -> failwithf "Not supported: %A, %A" k v

    do evalueRef
       := ws
          >>. choice
                  [ ebool; enull; enumber; emap; evector; eset; ekeyword;
                    esymbol; estring; etagged ] .>> ws

    //-------------- Interface ---------------
    let Parse = run evalue
