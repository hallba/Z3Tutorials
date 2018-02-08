#r "System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression
open System.Net

module getZ3 =

    let wc = new WebClient()

    type OS =
            | OSX            
            | Windows
            | Linux

    let getOS = 
            match int Environment.OSVersion.Platform with
            | 4 | 128 -> Linux
            | 6       -> OSX
            | _       -> Windows

    if  true <> System.IO.File.Exists("platform/z3/LICENSE.txt") then 
        match getOS with
        | Linux ->  wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-ubuntu-16.04.zip", @"z3.zip")
                    //This will take a while
                    ZipFile.ExtractToDirectory("z3.zip", ".") 
                    System.IO.Directory.Move("z3-4.6.0-x64-ubuntu-16.04","platform/z3")
        | Windows ->wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-win.zip", @"z3.zip")
                    //This will take a while
                    ZipFile.ExtractToDirectory("z3.zip", ".") 
                    System.IO.Directory.Move("z3-4.6.0-x64-win","platform/z3")
        | OSX ->    wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-osx-10.11.6.zip", @"z3.zip")
                    //This will take a while
                    ZipFile.ExtractToDirectory("z3.zip", ".") 
                    System.IO.Directory.Move("z3-4.6.0-x64-osx-10.11.6","platform/z3")
        | _ -> ()
