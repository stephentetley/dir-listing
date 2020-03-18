﻿// Copyright (c) Stephen Tetley 2019
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


let readDirListing01 () = 
    let input = Path.Combine(__SOURCE_DIRECTORY__, "..\data", "project-sample.dir.txt")
    // DateTime is "30/07/2018     15:20"
    let dateTime = { DateParser = dateDDMMYear; TimeParser = time24HHMM }
    readDirListing dateTime input