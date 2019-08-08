using System;
using Robertluo;
using static System.Console;

namespace Trial
{
    class Program
    {
        static void Main(string[] args)
        {
            var v = Edn.Parse("{:foo/bar 35 :name \"Hello\"}");
            var u = v.IsEMap ? ((Edn.EMap) v).Item : null;

            var a = Edn.MapOf(new Tuple<Edn, Edn>[] {new Tuple<Edn, Edn>(Edn.Kw(null, "foo"), Edn.NewEInteger(3L))});
            WriteLine($"a should be: {a}");
            WriteLine($"equals to: {Edn.Parse("{:foo 3}")}");

            var b = Edn.VecOf(new Edn[] {Edn.Kw("foo", "bar"), Edn.NewEBool(false), Edn.NewEString("hello")});
            WriteLine($"b should be: {b}");
            WriteLine($"equals to: {Edn.Parse("[:foo/bar, false, \"hello\"]")}");

            var c = Edn.Parse("[{:a 3 :b \"ok\"} {:a 15, :b nil}]");
            WriteLine($"Get in: {c.GetIn(new Edn[] {Edn.NewEInteger(1L), Edn.Kw(null, "a")})}");
        }
    }
}