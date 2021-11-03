using bvc.Compiler;
using bvc.Support;

using var stream = new StringStream(@"
enum Meep { A, B, C, D = 10, E, F }
enum Arf { A = 9, B = 4, D = 0 }

class C { }");

var lexer = new Lexer(stream);

var parser = new Parser(lexer);
var rootNode = parser.Parse();

var codeGen = new CodeGeneration(rootNode!);
using var outputStream = File.OpenWrite("out.dll");
codeGen.Write(outputStream, "out");