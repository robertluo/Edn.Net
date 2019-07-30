# Robertluo.Edn.Net
[![CircleCI](https://circleci.com/gh/robertluo/Edn.Net.svg?style=svg)](https://circleci.com/gh/robertluo/Edn.Net)

My attempt of implementing [Edn format](https://github.com/edn-format/edn) for .Net.

## Usage

```F#
open Robertluo

Edn.Parse "{:hello \"Edn\"}"
```

## Design

The `Edn` type in `Robertluo.Edn` is the basic abstraction, it can be:

 - Null value `nil`
 - Boolean
 - String
 - a number
   - as integer (int64)
   - as bigint
   - as float
   - as decimal
 - Keyword
 - Symbol.
 - Set. 
 - Vector.
 - Map.

 It also can contains tagged element. (TODO Customizable tag parsing)

 ## Develop

 - [DotNote Core](https://dotnet.microsoft.com/download)
 - [Fake build tool](https://fake.build)
 - [Paket packaging manager](https://fsprojects.github.io/Paket/)

So you should install all above tools, then:

 1. `paket install` to install all dependencies
 1. `dotnet watch -p Test/Edn.Net.Test/Edn.Net.Test.fsproj run` to automatically build and run tests.