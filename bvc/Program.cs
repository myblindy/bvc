using bvc.Compiler;
using bvc.Support;

using var stream = new StringStream(@"
enum Meep { A, B, C, D = 10, E, F }
enum Arf { A = 9, B = 4, D = 0 }

class D(val s: String);

class C(var a: Integer, var b: String, val c: Double, val arf: Arf, val d: D) {
    fun Lst() {
        val lst0 = List<Integer>();
        val lst1 = List<Integer>(1);
        val lst2 = List<Integer>(1, 2);
        val lst3 = List<Integer>(1, 2, 3);
        val lst4 = List<Integer>(1, 2, 3, 4);
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
}");

var lexer = new Lexer(stream);

var parser = new Parser(lexer);
var rootNode = parser.Parse();

using var outputStream = File.OpenWrite("out.dll");
CodeGeneration.Generate(rootNode!, outputStream, "out");