// License: BSD 3 Clause

#r "netstandard"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

open FSharp.Core

#load @"..\src\DirListing\DirParser.fs"
open DirListing.DirParser


let facts01 () = 
    let input = @"G:\work\Projects\edm2\march_18_2020_file_logs\cso_sps-dir.txt"
    // DateTime is "11/28/2019   9:23 AM"
    let dateTime = { DateParser = dateMMDDYear; TimeParser = time12HHMM }
    readDirListing dateTime input


let printNames (ans : Result<DirListingFolder list, string>): Unit = 
    match ans with
    | Ok xs -> xs |> List.iter (fun (x:DirListingFolder) -> printfn "%s" x.Path)
    | Error msg -> printfn "%s" msg

    