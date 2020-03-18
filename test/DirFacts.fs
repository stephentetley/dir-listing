// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace DirListingExtra

module DirFacts = 
    
    open FSharp.Data
    open System.Text.RegularExpressions

    open DirListing

    type SiteWork = 
        CsvProvider<Schema = "Site Name (string), File (string)",
                    Sample = """Site Name, File""",
                    HasHeaders = true >
    
    
    
    let isSiteWorkDirectory (path: string): bool = endsWith "2.Site_Work"  path false
    
    
    let isSurveyDirectory(path: string): bool = endsWith "1.Site_Work"  path false
    
    let collectSiteWork1 (folder: DirListingFolder) : SiteWork.Row list = 
        let rmatch (patt: string) (input: string) : bool = 
            Regex.IsMatch( input = input, pattern = patt, options = RegexOptions.IgnoreCase)
    
        let pred (parent: string) (x: DirListingItem) : SiteWork.Row option = 
            match x with
            | DirectoryItem(_) -> None
            | FileItem(_) -> 
                if rmatch "\.md$" x.Name || rmatch ".*Calibration.*\.docx$" x.Name then
                    Some (SiteWork.Row(parent,  x.Name))
                else
                    None
    
        if isSiteWorkDirectory(folder.Path) then
            let parent = parentDirectory(folder.Path)
            folder.FolderContents |> List.choose (pred parent)
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
    
