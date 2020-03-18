// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace DirListing

[<AutoOpen>]
module PowershellDirParser =

    open System
    open System.IO 

    open FParsec

    open DirListing.Base
    
    
    type DateStamp = 
        { Day: int
          Mon: int
          Year: int}

    type TimeStamp = 
        { Hrs: int 
          Mins: int }

    let makeDateTime (ds: DateStamp) (ts: TimeStamp) : DateTime = 
        try 
            new DateTime(year = ds.Year, month = ds.Mon, day  = ds.Day, hour = ts.Hrs, minute = ts.Mins, second= 0)
        with
        | ex -> 
            printfn "%O %O" ds ts
            reraise ()

    type DateTimeConfig = 
        { DateParser: Parser<DateStamp, unit> 
          TimeParser: Parser<TimeStamp, unit>
        }
    
    type DateTimeReader = Parser<DateTime, unit>

    let private dateTimeReader (config: DateTimeConfig) : DateTimeReader = 
        pipe2 config.DateParser
              config.TimeParser
              makeDateTime

    type DiretoryParser<'ans> = DateTimeReader -> Parser<'ans, unit>

    // *************************************
    // PARSER

    // Parsing output of "dir" or "dir -Recurse" (Windows)

    // Utility combinators
    let private ws : Parser<string,unit> = manyChars (pchar ' ' <|> pchar '\t')
    let private ws1 : Parser<string,unit> = many1Chars (pchar ' ' <|> pchar '\t')

    let private symbol (p:Parser<'a,unit>)      : Parser<'a,unit> = p .>> ws

    let private keyword (s:string) : Parser<string,unit> = pstring s .>> ws
    let private keyword1 (s:string) : Parser<string,unit> = pstring s .>> ws1


    let private emptyLine : Parser<unit,unit> = newline >>. preturn ()

    // Names may span multiple lines
    let private pName : Parser<Name,unit> = 
        let line1 = restOfLine true
        let linesK = many1 (pchar ' ') >>. restOfLine true
        parse { 
            let! s = line1 
            let! ss = many linesK 
            let name1 = String.concat "" (s::ss)
            return name1.Trim()
            }

    type Period = AM | PM

    let pPeriod : Parser<Period, unit> = 
        let am = keyword "AM" >>. preturn AM
        let pm = keyword "PM" >>. preturn PM
        choice [am; pm]

    let time12HHMM : Parser<TimeStamp, unit> = 
        pipe3 pint32 
                (pchar ':' >>. symbol pint32)
                (symbol pPeriod)
                (fun th tm merid -> 
                    let hrs = if merid = PM then (th + 12) % 24  else th
                    { Hrs = hrs; Mins = tm })

    let time24HHMM : Parser<TimeStamp, unit> = 
        pipe2 pint32 
                (pchar ':' >>. symbol pint32)
                (fun th tm -> { Hrs = th; Mins = tm })

    let dateDDMMYear : Parser<DateStamp, unit> = 
        pipe3   pint32 
                (pchar '/' >>. pint32) 
                (pchar '/' >>. symbol pint32) 
                (fun dd mm yy -> { Day = dd; Mon = mm; Year = yy })

    let dateMMDDYear : Parser<DateStamp, unit> = 
        pipe3   pint32 
                (pchar '/' >>. pint32) 
                (pchar '/' >>. symbol pint32) 
                (fun mm dd yy -> { Day = dd; Mon = mm; Year = yy })

    //// Note this is UK centric    
    //let pUkDateTime : Parser<DateTime,unit> = 
    //    pipe5   pint32 
    //            (pchar '/' >>. pint32) 
    //            (pchar '/' >>. symbol pint32) 
    //            pint32 
    //            (pchar ':' >>. pint32)
    //            (fun dd dm dy th tm -> makeDateTime dy dm dd th tm 0)

    //let pUsDateTime : Parser<DateTime,unit> = 
    //    pipe5   pint32 
    //            (pchar '/' >>. pint32) 
    //            (pchar '/' >>. symbol pint32) 
    //            pint32 
    //            (pchar ':' >>. pint32)
    //            (fun dm dd dy th tm -> makeDateTime dy dm dd th tm 0)
    
    let private pMode : Parser<string,unit> = many1Chars (lower <|> pchar '-') 

    let private isDir (mode:string) : bool = mode.StartsWith("d")



    let private pDirectoryDirective : Parser<Name,unit> = 
        let indent = manyChars (pchar ' ')
        indent >>. keyword1 "Directory:" >>. pName

    let private pHeadings : Parser<string list,unit> = 
        let columns = pipe4 (keyword "Mode")
                            (keyword "LastWriteTime")
                            (keyword "Length")
                            (keyword "Name")
                            (fun a b c d -> [a;b;c;d])
        let underline = restOfLine false
        columns .>> newline .>> underline


    let private pFolder (pathTo: string) 
                        (mode: string) : DiretoryParser<DirListingItem> = 
        fun pDateTime -> 
            parse { 
                let! timeStamp = pDateTime 
                let! name = pName 
                return (DirectoryItem (name, { Mode = mode; ModificationTime = timeStamp}, pathTo))
                }

    let private pFile (pathTo:string) (mode:string) : DiretoryParser<DirListingItem> = 
        fun pDateTime -> 
            parse { 
                let! timeStamp = pDateTime
                let! size = symbol pint64
                let! name = pName 
                return (FileItem (name, { Mode = mode; ModificationTime = timeStamp}, size, pathTo))
                }

    // Note - file store is flat at parse time (represented as a "Row")
    // It needs postprocessing to build.
    let private pRow (pathTo:string) : DiretoryParser<DirListingItem> = 
        fun pDateTime -> 
            let parseK mode = 
                if isDir mode then pFolder pathTo mode pDateTime else pFile pathTo mode pDateTime
            (symbol pMode) >>= parseK





    let private pDirFolder : DiretoryParser<DirListingFolder> = 
        fun pDateTime -> 
            parse { 
                let! parent = (spaces >>. pDirectoryDirective) 
                do! emptyLine
                do! emptyLine
                let! _ = pHeadings .>> newline
                let! rows = many1 (pRow parent pDateTime)
                return { Path = parent; FolderContents = rows }
                }



    let private pListing : DiretoryParser<DirListingFolder list> = 
        fun pDateTime -> many (pDirFolder pDateTime .>> spaces)

    let readDirListing (config: DateTimeConfig) (inputPath:string) : Result<DirListingFolder list, string> = 
        let source = File.ReadAllText(inputPath)
        let dateTimeParser = dateTimeReader config
        match runParserOnString (pListing dateTimeParser) () inputPath source with
        | Success(a,_,_) -> Result.Ok a
        | Failure(s,_,_) -> Result.Error s




        

