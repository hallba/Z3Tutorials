
# Z3 tutorial @ Compose Conference 

## Intro 

I've built several static analysis tools in F# and OCaml that pass the heavy arithmetic and logical work to the SMT solver Z3. 
In this tutorial, I want to show you how to use Z3 from your favourite functional programming language (I'll be running
with F#, but you can ride along in OCaml too). We'll write small programs to solve puzzles like Sudoku, algebra problems, etc.
The emphasis will be on how to represent your problem to Z3. We'll code as we go.

Giving this tutorial at Spotify's offices in New York, Dec 2015
https://twitter.com/brendanadamson/status/561943250773487616


## What you need to have ready

We will be using Z3 from F#'s repl (fsi on Windows, fsharpi on Mac OS X and Linux). 
Starting from a fully-fledged dev machine 
(Windows + Visual Studio 2015, OS X with XCode dev tools, Linux with g++, etc), 
you now need to have both F# and Z3, with it's .NET bindings, installed on your machine: 


### Using Windows

#### Compiler etc 

Visual Studio 2015 will already give you F# (fsi, fsc). 

#### Z3 

You can get Z3 in several ways:
Download the latest binaries from https://github.com/Z3Prover/z3/releases; 
If you want to build it yourself, download the source from https://github.com/Z3Prover/z3 and follow the instructions from there. 
	
	
### Using Mono (for Linux and MacOS)

08-03-2016: https://github.com/wintersteiger tells me that you can pass --dotnet to mk_make.py. So you might not have to do the following,
but I haven't tried it yet myself.

(Thank you to https://github.com/mmjb for these instrutions, in particular for his perl wisdom.)  

#### Compiler etc 

Install the software needed for the build process:

  * g++
  * python
  * mono for .NET 4.0
  * xbuild
  * fsharp

On a Debian (>> squeezy) or Ubuntu (>= 14.04 LTS) system, this suffices:
```
$ sudo apt-get install build-essential python mono-complete mono-xbuild fsharp
```

On OS X, install the Mono MDK for Mac OS from
       http://www.mono-project.com/download/
and install the XCode development tools (e.g., by executing "gcc" in
a Terminal -- if it's not there yet, OS X will offer to install XCode).

#### Z3  

You need the Z3 dll and it's .NET wrapper. You can either get a slightly old version of these pre-built from platform/{osx-mono,linux-ubuntu-x64} and skip this section. 

If you want to build it yourself, read on. 

First, get the Z3 sources for z3/unstable (4.3.2 is known to work) from https://github.com/Z3Prover/z3. 
Set `Z3DIR` to point to where you download the source:

```
	$ export Z3DIR=/path/to/z3/
```

On Linux, this suffices to build:

```
	$ pushd "$Z3DIR"
	$ ./configure
	$ pushd "$Z3DIR/build"
	$ make
	$ popd && popd
```

On OS X, you need to enforce a 32-bit build for compatibility with Mono:

```
	$ pushd "$Z3DIR"
	$ ./configure
	$ pushd "$Z3DIR/build"
	$ perl -i -pe 's/-D_AMD64_/-arch i386/; s/LINK_EXTRA_FLAGS=/$&-arch i386 /' config.mk
	$ make
	$ popd && popd
```

After you've got Z3 built, then build the .NET bindings for z3:

```
	$ pushd "$Z3DIR/src/api/dotnet/"
	$ echo -e '<configuration>\n <dllmap dll="libz3.dll" target="libz3.dylib" os="osx"/>\n <dllmap dll="libz3.so" target="libz3.dylib" os="linux"/>\n</configuration>\n' > Microsoft.Z3.config
	$ xbuild Microsoft.Z3.csproj
	$ popd
```

Copy the z3 binary and it's .NET wrapper over to a directory where you want to run from, like ~/my-z3-bin
```
	cp $Z3DIR/src/api/dotnet/obj/Debug/Microsoft.Z3.* ~/my-z3-bin
	cp $Z3DIR/build/libz3.* ~/my-z3-bin
```

### Rock n'roll
 
Here, we're starting fsharpi on OS X. The -I flag is assuming the Z3 dlls are in ../platform/osx-mono. 
Where-ever your dlls are (~/my-z3-bin), make sure both the native z3 dll and it's .NET wrapper are in the same directory. 

```
$ fsharpi -I:../platform/osx-mono -r:Microsoft.Z3
> open Microsoft.Z3 ;;
> let ctx = new Context ();;

val ctx : Context

> 
```
