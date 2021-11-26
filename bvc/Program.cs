using bvc.Compiler;
using bvc.Support;
using System.Reflection;

using var stream = new StringStream(@"
class Program {
    fun static Main() {
        for(d in [D(""moop"", 15), D(""maep"", 25), D(""meep"", -1)])
            Console.WriteLine(""d.A = ${d.A}, d.B = ${d.B}"");
        
        val l = [1, 3, 5];
        Console.Write(""l ="");
        for(v in l) Console.Write("" ${v}"");
        Console.WriteLine();
        Console.WriteLine(""l[1] = ${l[1]}"");
    }
}

class D(var A: String, var B: Integer);");

var lexer = new Lexer(stream);

var parser = new Parser(lexer);
var rootNode = parser.Parse();

using (var outputStream = File.OpenWrite("out.dll"))
    CodeGeneration.Generate(rootNode!, outputStream, "out");

var outAssembly = Assembly.LoadFile(Path.GetFullPath("out.dll"));
var mainMethod = outAssembly.EntryPoint!;
mainMethod.Invoke(null, null);