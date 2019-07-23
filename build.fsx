#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.IO
open Fake.DotNet
open Fake.Core

Target.initEnvironment()

// Properties
let libProj = "src/Edn.Net/Edn.Net.fsproj"

// Targets
Target.create "BuildApp" (fun _ ->
  DotNet.build id libProj
)

Target.create "Pack" (fun _ ->
  DotNet.pack id libProj
  )

open Fake.Core.TargetOperators

"BuildApp"
  ==> "Pack"

// start build
Target.runOrDefault "BuildApp"