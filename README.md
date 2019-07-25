# Robertluo.Edn.Net

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
 - a number. TODO now can only be float, should introduce integer
 - Keyword
 - Symbol. TODO not implemnted yet.
 - Set. 
 - Vector.
 - Map.

 It also can contains tagged element. (TODO not implemnted yet.)