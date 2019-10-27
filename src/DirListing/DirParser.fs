// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace DirListing

module DirParser =

    open System
    open System.IO 

    open FParsec


    
    type Name = string
    type FilePath = string
    type Size = int64


    // Mode is not currently interpreted
    type FsoProperties = 
        { Mode : string
          ModificationTime : DateTime
        }
 
    type DirListingRow = 
        | FolderRow of name : Name * props: FsoProperties * FilePath
        | FileRow of name : Name * props : FsoProperties * length : Size * FilePath
        member x.Name 
            with get () : string = 
                match x with
                | FolderRow(name,_,_) -> name
                | FileRow(name,_,_,_) -> name

        member x.Properties 
            with get () : FsoProperties = 
                match x with
                | FolderRow(_,props,_) -> props
                | FileRow(_,props,_,_) -> props

        member x.Path
            with get () : string = 
                match x with
                | FolderRow(_,_,path) -> path
                | FileRow(_,_,_,path) -> path


    type DirListingFolder = 
        { Path : FilePath 
          FolderContents : DirListingRow list }
        




    let makeDateTime (year:int) (month:int) (day:int) (hour:int) (minute:int) (second:int) : DateTime = 
        new DateTime(year=year, month=month, day=day, hour=hour, minute=minute, second=second)


    
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


    // Note this is UK centric    
    let private pDateTime : Parser<DateTime,unit> = 
        pipe5   pint32 
                (pchar '/' >>. pint32) 
                (pchar '/' >>. symbol pint32) 
                pint32 
                (pchar ':' >>. pint32)
                (fun dd dm dy th tm -> makeDateTime dy dm dd th tm 0)
    
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
                        (mode: string) : Parser<DirListingRow, unit> = 
        parse { 
            let! timeStamp = symbol pDateTime 
            let! name = pName 
            return (FolderRow (name, { Mode = mode; ModificationTime = timeStamp}, pathTo))
            }

    let private pFile (pathTo:string) (mode:string) : Parser<DirListingRow, unit> = 
        parse { 
            let! timeStamp = symbol pDateTime
            let! size = symbol pint64
            let! name = pName 
            return (FileRow (name, { Mode = mode; ModificationTime = timeStamp}, size, pathTo))
            }

    // Note - file store is flat at parse time (represented as a "Row")
    // It needs postprocessing to build.
    let private pRow (pathTo:string) : Parser<DirListingRow,unit> = 
        let parseK mode = 
            if isDir mode then pFolder pathTo mode else pFile pathTo mode
        (symbol pMode) >>= parseK





    let private pDirFolder : Parser<DirListingFolder, unit> = 
        parse { 
            let! parent = (spaces >>. pDirectoryDirective) 
            do! emptyLine
            do! emptyLine
            let! _ = pHeadings .>> newline
            let! rows = many1 (pRow parent)
            return { Path = parent; FolderContents = rows }
            }



    let private pListing : Parser<DirListingFolder list,unit> = 
        many (pDirFolder .>> spaces)

    let readDirListing (inputPath:string) : Result<DirListingFolder list, string> = 
        let source = File.ReadAllText(inputPath)
        match runParserOnString pListing () inputPath source with
        | Success(a,_,_) -> Result.Ok a
        | Failure(s,_,_) -> Result.Error s




        

