#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Testing.Expecto
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.DotNet
open Fake.Core
open Fake.DotNet.Testing

Target.initEnvironment()

// Properties
let libProj = "src/Edn.Net/Edn.Net.fsproj"
let testProj = "test/Edn.Net.Test/Edn.Net.Test.fsproj"

// Targets
Target.create "BuildApp" (fun _ ->
  DotNet.build id libProj
)

Target.create "Test" (fun _ ->
  DotNet.exec id "run" (sprintf "-p %s" testProj) |> ignore
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