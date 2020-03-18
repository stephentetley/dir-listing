// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause


namespace DirListing

[<AutoOpen>]
module Base =

    open System
    open System.IO 
    
    type Name = string
    type FilePath = string
    type Size = int64


    // Mode is not currently interpreted
    type FsoProperties = 
        { Mode : string
          ModificationTime : DateTime
        }
 
    type DirListingItem = 
        | DirectoryItem of name : Name * props: FsoProperties * FilePath
        | FileItem of name : Name * props : FsoProperties * length : Size * FilePath
        member x.Name 
            with get () : string = 
                match x with
                | DirectoryItem(name,_,_) -> name
                | FileItem(name,_,_,_) -> name

        member x.Properties 
            with get () : FsoProperties = 
                match x with
                | DirectoryItem(_,props,_) -> props
                | FileItem(_,props,_,_) -> props

        member x.Path
            with get () : string = 
                match x with
                | DirectoryItem(_,_,path) -> path
                | FileItem(_,_,_,path) -> path


    type DirListingFolder = 
        { Path : FilePath 
          FolderContents : DirListingItem list }

        member x.FolderName 
            with get () : string = 
                // Folder name is the rightmost element of the Path (.Net Path module considers this a file)
                Path.GetFileName x.Path
        
    let parentDirectory(path: string): string = 
        new FileInfo(path) 
            |> fun x -> x.Directory.Name

    let pathSegments(path: string) : string list = 
        let rec work (info: DirectoryInfo) cont = 
            if info.Name = info.Root.Name
            then cont [info.Name]
            else work info.Parent (fun xs -> cont (info.Name :: xs))
        try 
            work (new DirectoryInfo(path = path)) (fun xs -> List.rev xs)
        with
        | _ -> []

    let hasSegment(segment: string) (path: string): bool = 
        pathSegments path |> List.contains(segment)

    let endsWith(segment: string) (path: string) (caseSensitive: bool): bool = 
        let info = new DirectoryInfo(path = path)
        if caseSensitive then 
            info.Name = segment
        else
            String.Equals(info.Name, segment, StringComparison.InvariantCultureIgnoreCase)

