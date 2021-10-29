using bvc.Compiler;
using bvc.Support;

using var stream = new StringStream("(\"me\" + \"ep\") eq \"meep\"");
var lexer = new Lexer(stream);
var parser = new Parser(lexer);
var node = parser.Parse();
;