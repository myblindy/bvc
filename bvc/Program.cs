using bvc.Compiler;
using bvc.Support;
using System.Reflection;

using var stream = new StringStream(@"
class Program {
    fun static Main() {
        var d = D(""moop"", 15);
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