# Robertluo.Edn.Net
[![CircleCI](https://circleci.com/gh/robertluo/Edn.Net.svg?style=svg)](https://circleci.com/gh/robertluo/Edn.Net)

My attempt of implementing [Edn format](https://github.com/edn-format/edn) for .Net.

## Usage

### Parse

```F#
open Robertluo

Edn.parse "{:hello \"Edn\"}"
```

```C#
var v = Robertluo.Edn.Parse("{:foo/bar 3}");

//to use the value
var m = v.IsEMap ? ((Edn.EMap) v).Item : null;

// to create EDN value {:foo 3} in program
var a = Edn.MapOf(new Tuple<Edn, Edn>[] {new Tuple<Edn, Edn>(Edn.Kw(null, "foo"), Edn.NewEInteger(3L))});
// or simpler and clearer
var a = Edn.Parse("{:foo 3}");

// create a vector edn
var b = Edn.VecOf(new Edn[] { Edn.Kw("foo", "bar"), Edn.NewEBool(false), Edn.NewEString("hello")});
// or simpler
var b = Edn.Parse("[:foo/bar, false, \"Hello\"]");
```

### GetIn

`GetIn` can quickly pull value from a EDN value.

```c#
/// Get in can 
var c = Edn.Parse("[{:a 3 :b \"ok\"} {:a 15, :b nil}]");
WriteLine($"Get in: {c.GetIn(new Edn[] {Edn.NewEInteger(1L), Edn.Kw(null, "a")})}");
```

## Releases

 - 0.2.0 with all standard features implemented
 - 0.3.0 with some constructor for c# programmer convinience
 - 0.4.0 introduce `GetIn`

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