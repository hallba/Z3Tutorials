#r "System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression
open System.Net

module getZ3 =

    let wc: WebClient = new WebClient()

    type OS =
            | OSXx64            
            | Windowsx64
            | Linuxx64
            | OSXx86           
            | Windowsx86
            | Linuxx86
            | OSXarm64

    let getOS = 
            match int Environment.OSVersion.Platform, Environment.Is64BitProcess with
            | ((4 | 128),true) -> 
                if File.Exists("/System/Library/CoreServices/SystemVersion.plist") then 
                // Check for Apple Silicon
                        if Runtime.InteropServices.RuntimeInformation.ProcessArchitecture = 
                                Runtime.InteropServices.Architecture.Arm64 then
                                OSXarm64
                        else 
                                OSXx64
                else 
                        Linuxx64

            | (6,true)       -> 
                if Runtime.InteropServices.RuntimeInformation.ProcessArchitecture = Runtime.InteropServices.Architecture.Arm64 then
                        OSXarm64
                else 
                        OSXx64

            | (_,true)       -> Windowsx64
            | ((4 | 128),false) -> 
                if File.Exists("/System/Library/CoreServices/SystemVersion.plist") then 
                                OSXx86 
                        else 
                                Linuxx86
            | (6,false)       -> OSXx86
            | _     -> Windowsx86

    if  true <> System.IO.File.Exists("platform/z3/LICENSE.txt") then 
        match getOS with
        | Linuxx64 ->       wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-ubuntu-16.04.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.6.0-x64-ubuntu-16.04","platform/z3")
        | Windowsx64 ->     wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-win.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.6.0-x64-win","platform/z3")
        | OSXx64 ->         wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-osx-10.11.6.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.6.0-x64-osx-10.11.6","platform/z3")
        | Linuxx86 ->       wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x64-ubuntu-16.04.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.6.0-x86-ubuntu-16.04","platform/z3")
        | Windowsx86 ->     wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x86-win.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.6.0-x86-win","platform/z3")
        | OSXx86 ->         wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.6.0/z3-4.6.0-x86-osx-10.11.6.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.6.0-x86-osx-10.11.6","platform/z3")  
        | OSXarm64 ->       wc.DownloadFile("https://github.com/Z3Prover/z3/releases/download/z3-4.15.1/z3-4.15.1-arm64-osx-13.7.6.zip", @"z3.zip")
                            //This will take a while
                            ZipFile.ExtractToDirectory("z3.zip", ".", true) 
                            System.IO.Directory.Move("z3-4.15.1-arm64-osx-13.7.6","platform/z3")                  
        | _ -> ()