using Robertluo;
using static System.Console;

namespace Trial
{
    class Program
    {
        static void Main(string[] args)
        {
            var v = Edn.Parse("{:foo/bar 35 :name \"Hello\"}");
            WriteLine("Hello World! " + v.IsEBigInt);
        }
    }
}
