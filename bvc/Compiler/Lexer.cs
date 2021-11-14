using System.Text;

namespace bvc.Compiler;

#region Tokens
enum TokenType
{
    None,

    StringLiteral,
    IntegerLiteral,
    DoubleLiteral,
    Identifier,

    OpenParentheses,
    CloseParentheses,
    OpenBrace,
    CloseBrace,
    SemiColon,
    Colon,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Equals,
    NotEquals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    EqualsEquals,
    Not,
    Dot,

    IfKeyword,
    TrueKeyword,
    FalseKeyword,
    VarKeyword,
    ValKeyword,
    EnumKeyword,
    ClassKeyword,
    FunKeyword,
    ReturnKeyword,
    GetKeyword,
    SetKeyword,
    VarArgKeyword,

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

abstract record NumericLiteralToken(string Text, TokenType Type) : Token(Text, Type);
record IntegerLiteralToken(string Text, long Value) : NumericLiteralToken(Text, TokenType.IntegerLiteral)
{
    public IntegerLiteralToken(long Value) : this(Value.ToString(), Value) { }
}
record DoubleLiteralToken(string Text, double Value) : NumericLiteralToken(Text, TokenType.DoubleLiteral);

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
    private readonly List<Token> previewTokens = new();
    Transaction? transaction;

    public Lexer(Stream stream) =>
        reader = new(stream, Encoding.Unicode);

    public class Transaction
    {
        readonly Lexer lexer;
        List<Token>? tokens = new();
        public Transaction(Lexer lexer) => this.lexer = lexer;

        public void Add(Token token) => tokens?.Add(token);
        public void AddRange(IEnumerable<Token> token) => tokens?.AddRange(token);

        public void Reset()
        {
            if (tokens is not null)
                lexer.previewTokens.InsertRange(0, Enumerable.Reverse(tokens));
            Commit();
        }

        public void Commit() => (lexer.transaction, tokens) = (null, null);
    }

    static readonly Dictionary<string, TokenType> keywords = new()
    {
        ["if"] = TokenType.IfKeyword,
        ["true"] = TokenType.TrueKeyword,
        ["false"] = TokenType.FalseKeyword,
        ["var"] = TokenType.VarKeyword,
        ["val"] = TokenType.ValKeyword,
        ["enum"] = TokenType.EnumKeyword,
        ["class"] = TokenType.ClassKeyword,
        ["fun"] = TokenType.FunKeyword,
        ["set"] = TokenType.SetKeyword,
        ["get"] = TokenType.GetKeyword,
        ["return"] = TokenType.ReturnKeyword,
        ["vararg"] = TokenType.VarArgKeyword,
    };

    public Transaction? StartTransaction() => transaction is null ? transaction = new(this) : null;

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
                case ',': return new SymbolToken(ch.ToString(), TokenType.Comma);
                case ':': return new SymbolToken(ch.ToString(), TokenType.Colon);
                case '.': return new SymbolToken(ch.ToString(), TokenType.Dot);
                case '<' or '>' or '=':
                    {
                        var nextCh = (char)reader.Peek();
                        if (nextCh is not '=')
                            return new SymbolToken(ch.ToString(), ch switch
                            {
                                '<' => TokenType.LessThan,
                                '>' => TokenType.GreaterThan,
                                '=' => TokenType.Equals,
                                _ => throw new NotImplementedException()
                            });
                        else
                        {
                            reader.Read();
                            return new SymbolToken($"{ch}{nextCh}", ch switch
                            {
                                '<' => TokenType.LessThanEqual,
                                '>' => TokenType.GreaterThanEqual,
                                '=' => TokenType.EqualsEquals,
                                _ => throw new NotImplementedException()
                            });
                        }
                    }
                case '!':
                    {
                        var nextCh = reader.Peek();
                        if (nextCh == '=')
                        {
                            reader.Read();
                            return new SymbolToken("!=", TokenType.NotEquals);
                        }
                        else
                            return new SymbolToken("!", TokenType.Not);
                    }
                case >= '0' and <= '9':
                    {
                        tokenSB.Clear();
                        tokenSB.Append(ch);

                        var hasDot = false;

                        while (true)
                        {
                            var nextCh = (char)reader.Peek();

                            if (char.IsDigit(nextCh) || nextCh == '.')
                            {
                                reader.Read();
                                tokenSB.Append(nextCh);
                                hasDot |= nextCh == '.';
                            }
                            else
                                break;
                        }

                        var token = tokenSB.ToString();
                        if (hasDot && double.TryParse(token, out var doubleValue))
                            return new DoubleLiteralToken(token, doubleValue);
                        else if (!hasDot && long.TryParse(token, out var intValue))
                            return new IntegerLiteralToken(token, intValue);
                        else
                            throw new NotImplementedException();
                    }
                default: throw new NotImplementedException();
            }
        }
    }

    public Token? Consume(int n = 0)
    {
        if (n >= previewTokens.Count)
        {
            n -= previewTokens.Count;
            transaction?.AddRange(previewTokens);
            previewTokens.Clear();

            Token? token = default;
            while (n-- >= 0) token = GetNextToken();
            return token;
        }
        else
        {
            Token? token = default;
            while (n-- >= 0)
            {
                token = previewTokens[0];
                previewTokens.RemoveAt(0);
                transaction?.Add(token);
            }
            return token;
        }
    }

    public Token? Peek(int n = 0)
    {
        if (n >= previewTokens.Count)
        {
            n -= previewTokens.Count;

            Token? token = default;
            while (n-- >= 0) { token = GetNextToken(); if (token is null) return null; previewTokens.Add(token); }
            return token;
        }
        else if (n == 0)
            return previewTokens[0];
        else
            return previewTokens[n];
    }
}
