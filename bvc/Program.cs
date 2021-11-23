using bvc.Compiler;
using bvc.Support;
using System.Reflection;

using var stream = new StringStream(@"
enum Meep { A, B, C, D = 10, E, F }
enum Arf { A = 9, B = 4, D = 0 }

class D(val s: String);

class C(var a: Integer, var b: String, val c: Double, val d: D) {
    fun Lst() {
        val lst0 = List<Integer>();
        val lst1 = List(1);
        val lst2 = List(1, 2);
        val lst3 = List(1, 2, 3);
        val lst4 = List(1, 2, 3, 4);

        lst0.Add(1);

        var lstString = List(""a"", ""b"");
        lstString.Add(""c"");
        var cntLstString = lstString.Count;
    }

    fun F(x: Integer, y: Integer): Void {
        var fld: Integer;
        var fld2: Integer = (25 + 5) / 2 + -6;
        var fld3 = 25 + 1;
        var fld4 = 5 * 2.1;
        
        var a = fld + fld2;
    }

    fun Add(x: Double, y: Double): Double {
        return x + y;
    }
    fun ShortAdd(x: Double, y: Double) = x + y;

    var meep = 10;
    fun Meep() { return 10; }
    fun ShortMeep() = 10;

    fun Fancy() = Add(ShortMeep(), Meep());
    fun Fancy2() = Add(ShortAdd(1.0 / 2.0, 0.5), Meep());

    fun D() = D(""meep"");

    fun DNameFn() = d.s;
    val DNameVal = d.s;
    val DNameGet get = d.s;

    val P get {
        val c = 10 / 2;
        return a * a + c * c;
    }

    class IC {
        enum Meep { X }
    }
}

class Program {
    fun static Main() {
        Console.WriteLine(""1 + 2 / 5 = ${1.0 + 2.0 / 5.0}"");
    
        for(i in [1, 2, 3, 4, 5, 10])
            Console.WriteLine(""Found item: ${i}."");

        for(s in [""Dark Souls"", ""Bloodborne"", ""Sekiro""])
            Console.WriteLine(""So, ${s} is great!"");

        for(i in 0..40)
            Console.Write("" ${i} "");

        Console.WriteLine();

        var r = 1..10;
        Console.WriteLine(""1..10 is between ${r.Start} and ${r.End}."");

        var c = C(1, ""moop"", 5.1, D(""d instance string thingie""));
        Console.WriteLine(""c.Add(1.1, 5.5) = ${c.Add(1.1, 5.5)}"");
        Console.WriteLine(""c.ShortAdd(1.1, 5.5) = ${c.ShortAdd(1.1, 5.5)}"");
        Console.WriteLine(""c.Meep() = ${c.Meep()}"");
        Console.WriteLine(""c.ShortMeep() = ${c.ShortMeep()}"");
        Console.WriteLine(""c.Fancy() = ${c.Fancy()}"");
        Console.WriteLine(""c.Fancy2() = ${c.Fancy2()}"");
        Console.WriteLine(""c.D() = ${c.D()}"");
        Console.WriteLine(""c.DNameFn() = ${c.DNameFn()}"");
        Console.WriteLine(""c.DNameVal = ${c.DNameVal}"");
        Console.WriteLine(""c.DNameGet = ${c.DNameGet}"");
        Console.WriteLine(""c.P = ${c.P}"");
    }
}");

var lexer = new Lexer(stream);

var parser = new Parser(lexer);
var rootNode = parser.Parse();

using (var outputStream = File.OpenWrite("out.dll"))
    CodeGeneration.Generate(rootNode!, outputStream, "out");

var outAssembly = Assembly.LoadFile(Path.GetFullPath("out.dll"));
var mainMethod = outAssembly.EntryPoint!;
mainMethod.Invoke(null, null);