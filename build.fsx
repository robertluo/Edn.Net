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
let testProj = "test/Edn.Net.Test/Edn.Net.Test.fsproj"

// Targets
Target.create "BuildApp" (fun _ ->
  DotNet.build id libProj
)

Target.create "Test" (fun _ ->
  DotNet.test id testProj
)

Target.create "Pack" (fun _ ->
  DotNet.pack id libProj
  )

open Fake.Core.TargetOperators

"BuildApp"
  ==> "Test"
    ==> "Pack"

// start build
Target.runOrDefault "Test"