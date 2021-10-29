using System.Text;

namespace bvc.Compiler;

#region Tokens
enum TokenType
{
    StringLiteral,
    Identifier,

    OpenParentheses,
    CloseParentheses,
    OpenBrace,
    CloseBrace,
    SemiColon,
    Plus,
    Minus,
    Star,
    Slash,

    IfKeyword,
    EqualsKeyword,
    LessThanKeyword,
    LessThanEqualKeyword,
    GreaterThanKeyword,
    GreaterThanEqualKeyword,
    NotEqualsKeyword,
    NotKeyword,
    TrueKeyword,
    FalseKeyword,

    Error = int.MaxValue
}

record Token(string Text, TokenType Type);

record SymbolToken : Token
{
    public SymbolToken(string text, TokenType tokenType) : base(text, tokenType) { }
}

record StringLiteralToken : Token
{
    public StringLiteralToken(string text) : base(text, TokenType.StringLiteral) { }
}

record IdentifierToken : Token
{
    public IdentifierToken(string text) : base(text, TokenType.Identifier) { }
}

record KeywordToken : Token
{
    public KeywordToken(string text, TokenType tokenType) : base(text, tokenType) { }
}
#endregion 

class Lexer
{
    private readonly StreamReader reader;
    private readonly Queue<Token> previewTokens = new();

    public Lexer(Stream stream)
    {
        reader = new(stream, Encoding.Unicode);
    }

    static readonly Dictionary<string, TokenType> keywords = new()
    {
        ["if"] = TokenType.IfKeyword,
        ["eq"] = TokenType.EqualsKeyword,
        ["neq"] = TokenType.NotEqualsKeyword,
        ["lt"] = TokenType.LessThanKeyword,
        ["lte"] = TokenType.LessThanEqualKeyword,
        ["gt"] = TokenType.GreaterThanKeyword,
        ["gte"] = TokenType.GreaterThanEqualKeyword,
        ["not"] = TokenType.NotKeyword,
        ["true"] = TokenType.TrueKeyword,
        ["false"] = TokenType.FalseKeyword,
    };

    readonly StringBuilder tokenSB = new();
    Token? GetNextToken()
    {
        const char eof = unchecked((char)-1);
        while (true)
        {
            var ch = (char)reader.Read();

            if (char.IsWhiteSpace(ch)) continue;
            switch (ch)
            {
                case eof: return null;
                case '"':
                    tokenSB.Clear();
                    while (true)
                    {
                        ch = (char)reader.Read();
                        if (ch is '"' or eof)
                            return new StringLiteralToken(tokenSB.ToString());
                        else
                            tokenSB.Append(ch);
                    }
                case var letterCh when char.IsLetter(ch):
                    {
                        tokenSB.Clear();
                        tokenSB.Append(letterCh);
                        while (true)
                        {
                            ch = (char)reader.Peek();
                            if (ch is eof || !char.IsLetterOrDigit(ch))
                            {
                                var text = tokenSB.ToString();
                                return keywords.TryGetValue(text, out var tokenType) ? new KeywordToken(text, tokenType) : new IdentifierToken(text);
                            }
                            else
                            {
                                tokenSB.Append(ch);
                                reader.Read();
                            }
                        }
                    }
                case '(': return new SymbolToken(ch.ToString(), TokenType.OpenParentheses);
                case ')': return new SymbolToken(ch.ToString(), TokenType.CloseParentheses);
                case '{': return new SymbolToken(ch.ToString(), TokenType.OpenBrace);
                case '}': return new SymbolToken(ch.ToString(), TokenType.CloseBrace);
                case ';': return new SymbolToken(ch.ToString(), TokenType.SemiColon);
                case '+': return new SymbolToken(ch.ToString(), TokenType.Plus);
                case '-': return new SymbolToken(ch.ToString(), TokenType.Minus);
                case '*': return new SymbolToken(ch.ToString(), TokenType.Star);
                case '/': return new SymbolToken(ch.ToString(), TokenType.Slash);
                default: throw new NotImplementedException();
            }
        }
    }

    public Token? Consume(int n = 0)
    {
        if (n >= previewTokens.Count)
        {
            n -= previewTokens.Count;
            previewTokens.Clear();

            Token? token = default;
            while (n-- >= 0) token = GetNextToken();
            return token;
        }
        else
        {
            Token? token = default;
            while (n-- >= 0) token = previewTokens.Dequeue();
            return token;
        }
    }

    public Token? Peek(int n = 0)
    {
        if (n >= previewTokens.Count)
        {
            n -= previewTokens.Count;

            Token? token = default;
            while (n-- >= 0) { token = GetNextToken(); if (token is null) return null; previewTokens.Enqueue(token); }
            return token;
        }
        else if (n == 0)
            return previewTokens.Peek();
        else
            return previewTokens.ElementAt(n);
    }
}
