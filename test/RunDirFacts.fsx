// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Text.Encoding.dll"
open System.IO

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
#load @"DirFacts.fs"
open DirListing
open DirListingExtra.DirFacts


let outputPath (filename: string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, "..\data", filename)


let facts01 () = 
    let input = @"G:\work\Projects\edm2\march_18_2020_file_logs\cso_sps-dir.txt"
    // DateTime is "11/28/2019   9:23 AM"
    let dtConfig = { DateParser = dateMMDDYear; TimeParser = time12HHMM }
    readDirListing dtConfig input


let genSurveyFacts01 () = 
    let input = @"G:\work\Projects\edm2\march_18_2020_file_logs\cso_sps-dir.txt"
    // DateTime is "11/28/2019   9:23 AM"
    let dtConfig = { DateParser = dateMMDDYear; TimeParser = time12HHMM }
    let outpath = outputPath "surveys.csv"
    generateSurveyFacts dtConfig input outpath

let genSiteWorkFacts01 () = 
    let input = @"G:\work\Projects\edm2\march_18_2020_file_logs\cso_sps-dir.txt"
    // DateTime is "11/28/2019   9:23 AM"
    let dtConfig = { DateParser = dateMMDDYear; TimeParser = time12HHMM }
    let outpath = outputPath "sitework.csv"
    generateSiteWorkFacts dtConfig input outpath
