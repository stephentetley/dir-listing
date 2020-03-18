// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Text.Encoding.dll"
open System.IO
open System.Text.RegularExpressions

// Use FSharp.Data for CSV reading and writing
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

open FSharp.Core

#load @"..\src\DirListing\Base.fs"
#load @"..\src\DirListing\PowershellDirParser.fs"
open DirListing


let outputPath (filename: string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, "..\data", filename)


let facts01 () = 
    let input = @"G:\work\Projects\edm2\march_18_2020_file_logs\cso_sps-dir.txt"
    // DateTime is "11/28/2019   9:23 AM"
    let dtConfig = { DateParser = dateMMDDYear; TimeParser = time12HHMM }
    readDirListing dtConfig input

let printNames (ans : Result<DirListingFolder list, string>): Unit = 
    match ans with
    | Ok xs -> xs |> List.iter (fun (x:DirListingFolder) -> printfn "%s" x.FolderName)
    | Error msg -> printfn "%s" msg

type SiteWork = 
    CsvProvider<Schema = "Name (string), File (string)",
                Sample = """SITE NAME_XYZ, SITE NAME_XYZ Calibration Settings.docx""",
                HasHeaders = true >



let isSiteWorkDirectory (path: string): bool = endsWith "2.Site_Work"  path false


let isSurveyDirectory(path: string): bool = endsWith "1.Site_Work"  path false

let collectSiteWork1 (folder: DirListingFolder) : SiteWork.Row list = 
    let rmatch (patt: string) (input: string) : bool = 
        Regex.IsMatch( input = input, pattern = patt, options = RegexOptions.IgnoreCase)

    let pred (x: DirListingItem) : SiteWork.Row option = 
        match x with
        | DirectoryItem(_) -> None
        | FileItem(_) -> 
            if rmatch "\.md$" x.Name || rmatch ".*Calibration.*\.docx$" x.Name then
                Some (SiteWork.Row("TODO", x.Name))
            else
                None

    if isSiteWorkDirectory(folder.Path) then
        folder.FolderContents |> List.choose pred
    else 
        []

let collectSiteWork (dtConfig: DateTimeConfig) (path: string) : Result<SiteWork, string> = 
    match readDirListing dtConfig path with
    | Error msg -> Error msg
    | Ok dirs -> 
        dirs 
            |> List.collect collectSiteWork1 
            |> fun rows -> new SiteWork(rows)
            |> Ok
    
let generateSiteWorkFacts (dtconfig: DateTimeConfig) (srcpath: string) (outpath: string): Result<unit, string> = 
    collectSiteWork dtconfig srcpath
        |> Result.map (fun facts -> facts.Save(path = outpath, separator = ',', quote = '"'))

let temp01 () = 
    let input = @"G:\work\Projects\edm2\march_18_2020_file_logs\cso_sps-dir.txt"
    // DateTime is "11/28/2019   9:23 AM"
    let dtConfig = { DateParser = dateMMDDYear; TimeParser = time12HHMM }
    let outpath = outputPath "sitework.csv"
    generateSiteWorkFacts dtConfig input outpath
