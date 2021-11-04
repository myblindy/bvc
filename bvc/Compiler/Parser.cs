namespace bvc.Compiler;

abstract record Node;
abstract record NodeWithMembers : Node
{
    public List<Node> Members { get; } = new();
}
record RootNode : NodeWithMembers;
record EnumDeclarationNode(string Name, (string Name, long? Value)[] Members) : Node;
record ClassDeclarationNode(string Name) : NodeWithMembers;
record FunctionDeclarationNode(string Name, string? ReturnType, (TokenType Modifier, string Name, string Type)[] Arguments) : Node
{
    public const string PrimaryConstructorName = ".ctor";
    public bool IsPrimaryConstructor => Name == PrimaryConstructorName;
}
abstract record ExpressionNode : Node;
record BinaryExpressionNode(ExpressionNode Left, TokenType Operator, ExpressionNode Right) : ExpressionNode;
record UnaryExpressionNode(TokenType Operator, ExpressionNode Right) : ExpressionNode;
record LiteralExpressionNode(object Value) : ExpressionNode;
record GroupingExpressionNode(ExpressionNode Expression) : ExpressionNode;

class Parser
{
    readonly Lexer lexer;

    public Parser(Lexer lexer) => this.lexer = lexer;

    Token? LastMatchedToken;
    TokenType MatchTokenTypes(params TokenType[] tokenTypes)
    {
        var peekToken = lexer.Peek();
        if (peekToken is not null && tokenTypes.Contains(peekToken.Type))
        {
            lexer.Consume();
            return (LastMatchedToken = peekToken).Type;
        }
        return TokenType.Error;
    }

    TokenType ExpectTokenTypes(params TokenType[] tokenTypes)
    {
        var res = MatchTokenTypes(tokenTypes);
        if (res == TokenType.Error) throw new NotImplementedException();
        return res;
    }

    public RootNode Parse()
    {
        var rootNode = new RootNode();
        ParseMembers(rootNode);
        return rootNode;
    }

    void ParseMembers(NodeWithMembers node)
    {
        bool foundAny;
        do
        {
            foundAny = false;

            // enum
            while (MatchTokenTypes(TokenType.EnumKeyword) != TokenType.Error)
            {
                ExpectTokenTypes(TokenType.Identifier);
                var name = ((IdentifierToken)LastMatchedToken!).Text;
                ExpectTokenTypes(TokenType.OpenBrace);

                var enumMembers = new List<(string Name, long? Value)>();
                var nextValue = 0L;
                while (MatchTokenTypes(TokenType.Identifier) != TokenType.Error)
                {
                    var identifierToken = LastMatchedToken!;
                    if (MatchTokenTypes(TokenType.Equals) != TokenType.Error)
                    {
                        ExpectTokenTypes(TokenType.IntegerLiteral);
                        var numericValue = ((IntegerLiteralToken)LastMatchedToken!).Value;
                        nextValue = numericValue + 1;

                        enumMembers.Add((identifierToken.Text, numericValue));
                    }
                    else
                        enumMembers.Add((identifierToken.Text, nextValue++));

                    if (MatchTokenTypes(TokenType.Comma) == TokenType.Error)
                        break;
                }

                ExpectTokenTypes(TokenType.CloseBrace);

                node.Members.Add(new EnumDeclarationNode(name, enumMembers.ToArray()));
                foundAny = true;
            }

            // class
            // class X { }
            // class X(var v: int, var j: int) { }
            while (MatchTokenTypes(TokenType.ClassKeyword) != TokenType.Error)
            {
                ExpectTokenTypes(TokenType.Identifier);
                var name = ((IdentifierToken)LastMatchedToken!).Text;
                var classDeclarationNode = new ClassDeclarationNode(name);

                // primary constructor
                if (MatchTokenTypes(TokenType.OpenParentheses) != TokenType.Error)
                {
                    var args = new List<(TokenType Modifier, string Name, string Type)>();
                    while (true)
                    {
                        if (args.Count > 0 && MatchTokenTypes(TokenType.Comma) == TokenType.Error)
                            break;

                        if (MatchTokenTypes(TokenType.ValKeyword, TokenType.VarKeyword) is { } varTypeTokenType && varTypeTokenType == TokenType.Error)
                            break;

                        ExpectTokenTypes(TokenType.Identifier);
                        var varName = LastMatchedToken!.Text;

                        ExpectTokenTypes(TokenType.Colon);

                        ExpectTokenTypes(TokenType.Identifier);
                        var typeName = LastMatchedToken!.Text;

                        args.Add((varTypeTokenType, varName, typeName));
                    }
                    ExpectTokenTypes(TokenType.CloseParentheses);

                    classDeclarationNode.Members.Add(new FunctionDeclarationNode(FunctionDeclarationNode.PrimaryConstructorName, null, args.ToArray()));
                }

                ExpectTokenTypes(TokenType.OpenBrace);

                ParseMembers(classDeclarationNode);
                node.Members.Add(classDeclarationNode);

                foundAny = true;
                ExpectTokenTypes(TokenType.CloseBrace);
            }

            // functions
            // fun F(a: int, b: int) { }
            while (MatchTokenTypes(TokenType.FunKeyword) != TokenType.Error)
            {
                ExpectTokenTypes(TokenType.Identifier);
                var name = ((IdentifierToken)LastMatchedToken!).Text;
                ExpectTokenTypes(TokenType.OpenParentheses);

                var args = new List<(TokenType Modifier, string Name, string Type)>();
                while (true)
                {
                    if (args.Count > 0 && MatchTokenTypes(TokenType.Comma) == TokenType.Error)
                        break;

                    ExpectTokenTypes(TokenType.Identifier);
                    var varName = LastMatchedToken!.Text;

                    ExpectTokenTypes(TokenType.Colon);

                    ExpectTokenTypes(TokenType.Identifier);
                    var typeName = LastMatchedToken!.Text;

                    args.Add((TokenType.None, varName, typeName));
                }
                ExpectTokenTypes(TokenType.CloseParentheses);
                ExpectTokenTypes(TokenType.Colon);
                ExpectTokenTypes(TokenType.Identifier);
                var returnType = LastMatchedToken!.Text;

                ExpectTokenTypes(TokenType.OpenBrace);
                ExpectTokenTypes(TokenType.CloseBrace);

                foundAny = true;
                node.Members.Add(new FunctionDeclarationNode(name, returnType, args.ToArray()));
            }
        } while (foundAny);
    }

    ExpressionNode? ParseExpression() => ParseEqualityExpression();

    ExpressionNode? ParseEqualityExpression()
    {
        var left = ParseComparisonExpression();
        if (left is null) throw new NotImplementedException();

        while (MatchTokenTypes(TokenType.EqualsEquals, TokenType.NotEquals) is { } operatorTokenType && operatorTokenType != TokenType.Error)
        {
            var right = ParseComparisonExpression();
            if (right is null) throw new NotImplementedException();

            left = new BinaryExpressionNode(left, operatorTokenType, right);
        }

        return left;
    }

    ExpressionNode? ParseComparisonExpression()
    {
        var left = ParseTermExpression();
        if (left is null) throw new NotImplementedException();

        while (MatchTokenTypes(TokenType.LessThan, TokenType.LessThanEqual, TokenType.GreaterThan, TokenType.GreaterThanEqual) is { } operatorTokenType && operatorTokenType != TokenType.Error)
        {
            var right = ParseTermExpression();
            if (right is null) throw new NotImplementedException();

            left = new BinaryExpressionNode(left, operatorTokenType, right);
        }

        return left;
    }

    ExpressionNode? ParseTermExpression()
    {
        var left = ParseFactorExpression();
        if (left is null) throw new NotImplementedException();

        while (MatchTokenTypes(TokenType.Plus, TokenType.Minus) is { } operatorTokenType && operatorTokenType != TokenType.Error)
        {
            var right = ParseFactorExpression();
            if (right is null) throw new NotImplementedException();

            left = new BinaryExpressionNode(left, operatorTokenType, right);
        }

        return left;
    }

    ExpressionNode? ParseFactorExpression()
    {
        var left = ParseUnaryExpression();
        if (left is null) throw new NotImplementedException();

        while (MatchTokenTypes(TokenType.Star, TokenType.Slash) is { } operatorTokenType && operatorTokenType != TokenType.Error)
        {
            var right = ParseUnaryExpression();
            if (right is null) throw new NotImplementedException();

            left = new BinaryExpressionNode(left, operatorTokenType, right);
        }

        return left;
    }

    ExpressionNode? ParseUnaryExpression()
    {
        if (MatchTokenTypes(TokenType.Not, TokenType.Minus) is { } operatorTokenType && operatorTokenType != TokenType.Error)
        {
            var right = ParsePrimaryExpression();
            if (right is null) throw new NotImplementedException();

            return new UnaryExpressionNode(operatorTokenType, right);
        }

        return ParsePrimaryExpression();
    }

    ExpressionNode? ParsePrimaryExpression()
    {
        if (MatchTokenTypes(TokenType.TrueKeyword, TokenType.FalseKeyword) is { } operatorTokenType && operatorTokenType != TokenType.Error)
            return new LiteralExpressionNode(operatorTokenType == TokenType.TrueKeyword);

        if (MatchTokenTypes(TokenType.StringLiteral) != TokenType.Error)
            return new LiteralExpressionNode(LastMatchedToken!.Text);

        if (MatchTokenTypes(TokenType.OpenParentheses) != TokenType.Error)
        {
            var expr = ParseExpression();
            if (expr is null || lexer.Consume()!.Type != TokenType.CloseParentheses) throw new NotImplementedException();

            return new GroupingExpressionNode(expr);
        }

        throw new NotImplementedException();
    }
}
