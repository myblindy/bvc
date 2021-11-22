using Mono.Cecil;

namespace bvc.Compiler;

abstract record Node;
abstract record NodeWithMembers : Node
{
    public List<Node> Members { get; } = new();
}
record RootNode : NodeWithMembers;
record EnumDeclarationNode(string Name, (string Name, long? Value)[] Members) : Node;
record ClassDeclarationNode(string Name, string[]? GenericTypes = null) : NodeWithMembers
{
    public Action<TypeDefinition>? CustomCode { get; init; }
}
record BlockNode : NodeWithMembers;
record FunctionDeclarationNode(TokenType Modifier, string Name, IdentifierExpressionNode? ReturnType, (TokenType Modifier, string Name, string Type)[] Arguments, bool Internal = false) : BlockNode
{
    public const string PrimaryConstructorName = ".ctor";
    public bool IsPrimaryConstructor => Name == PrimaryConstructorName;
}
record VariableDeclarationNode(TokenType Modifier, string Name, IdentifierExpressionNode? ReturnType, ExpressionNode? InitialValueExpression, FunctionDeclarationNode? GetFunction) : Node;
abstract record ExpressionNode : Node;
record BinaryExpressionNode(ExpressionNode Left, TokenType Operator, ExpressionNode Right) : ExpressionNode
{
    public ExpressionNode LeftMostExpression
    {
        get
        {
            BinaryExpressionNode e = this;
            while (e.Left is BinaryExpressionNode leftBinaryExpressionNode)
                e = leftBinaryExpressionNode;

            return e.Left;
        }
    }
}
record UnaryExpressionNode(TokenType Operator, ExpressionNode Right) : ExpressionNode;
record LiteralExpressionNode(object Value) : ExpressionNode;
record StringExpressionNode(ExpressionNode[] Expressions) : ExpressionNode;
record IdentifierExpressionNode(string Identifier, ExpressionNode[]? GenericParameters = null) : ExpressionNode
{
    public CodeGeneration.TypeMember[]? InferredGenericParameters { get; set; }
}
record FunctionCallExpressionNode(ExpressionNode Expression, ExpressionNode[] Arguments) : ExpressionNode;
record GroupingExpressionNode(ExpressionNode Expression) : ExpressionNode;
abstract record StatementNode : Node;
record ReturnStatementNode(ExpressionNode Expression) : StatementNode;
record ForStatementNode(string VariableName, ExpressionNode EnumerableExpression, BlockNode Block) : StatementNode;
record ExpressionStatementNode(ExpressionNode Expression) : StatementNode;

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
        ParseMembers(rootNode, MemberType.Class);
        return rootNode;
    }

    enum MemberType { Function, Class }
    void ParseMembers(NodeWithMembers node, MemberType memberType, bool onlyOne = false)
    {
        bool foundAny;
        do
        {
            foundAny = false;

            // enum
            if (memberType is MemberType.Class)
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
                    if (onlyOne) return;
                }

            // class
            // class X { }
            // class X(var v: int, var j: int) { }
            if (memberType is MemberType.Class)
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

                        classDeclarationNode.Members.Add(new FunctionDeclarationNode(TokenType.None, FunctionDeclarationNode.PrimaryConstructorName, null, args.ToArray()));
                    }

                    if (MatchTokenTypes(TokenType.OpenBrace) != TokenType.Error)
                    {
                        ParseMembers(classDeclarationNode, MemberType.Class);
                        ExpectTokenTypes(TokenType.CloseBrace);
                    }
                    else
                        ExpectTokenTypes(TokenType.SemiColon);

                    node.Members.Add(classDeclarationNode);
                    if (onlyOne) return;
                    foundAny = true;
                }

            // functions
            // fun F(a: int, b: int) { }
            while (MatchTokenTypes(TokenType.FunKeyword) != TokenType.Error)
            {
                var @static = MatchTokenTypes(TokenType.StaticKeyword) != TokenType.Error;

                ExpectTokenTypes(TokenType.Identifier);
                var name = ((IdentifierToken)LastMatchedToken!).Text;
                ExpectTokenTypes(TokenType.OpenParentheses);

                var args = new List<(TokenType Modifier, string Name, string Type)>();
                while (true)
                {
                    if (args.Count > 0 && MatchTokenTypes(TokenType.Comma) == TokenType.Error) break;
                    if (MatchTokenTypes(TokenType.Identifier) == TokenType.Error) break;
                    var varName = LastMatchedToken!.Text;

                    ExpectTokenTypes(TokenType.Colon);

                    ExpectTokenTypes(TokenType.Identifier);
                    var typeName = LastMatchedToken!.Text;

                    args.Add((TokenType.None, varName, typeName));
                }
                ExpectTokenTypes(TokenType.CloseParentheses);

                IdentifierExpressionNode? returnType = null;
                if (MatchTokenTypes(TokenType.Colon) != TokenType.Error)
                    returnType = (IdentifierExpressionNode)ParseExpression()!;

                var functionDeclarationNode = new FunctionDeclarationNode(@static ? TokenType.StaticKeyword : TokenType.None, name, returnType, args.ToArray());

                if (MatchTokenTypes(TokenType.OpenBrace) != TokenType.Error)
                {
                    ParseMembers(functionDeclarationNode, MemberType.Function);
                    ExpectTokenTypes(TokenType.CloseBrace);
                }
                else
                {
                    ExpectTokenTypes(TokenType.Equals);
                    functionDeclarationNode.Members.Add(new ReturnStatementNode(ParseExpression()!));
                    ExpectTokenTypes(TokenType.SemiColon);
                }

                foundAny = true;
                node.Members.Add(functionDeclarationNode);
                if (onlyOne) return;
            }

            // variables
            // var/val a: A;
            while (MatchTokenTypes(TokenType.VarKeyword, TokenType.ValKeyword) is { } variableKindTokenType && variableKindTokenType != TokenType.Error)
            {
                ExpectTokenTypes(TokenType.Identifier);
                var name = LastMatchedToken!.Text;
                IdentifierExpressionNode? type = null;
                if (MatchTokenTypes(TokenType.Colon) != TokenType.Error)
                {
                    type = (IdentifierExpressionNode)ParseExpression()!;
                }

                ExpressionNode? initialValue = default;
                bool initialValueIsGet = false, needsSemiColon = true;
                FunctionDeclarationNode? functionGet = default;
                if (MatchTokenTypes(TokenType.GetKeyword) != TokenType.Error)
                {
                    if (MatchTokenTypes(TokenType.Equals) != TokenType.Error)
                        (initialValue, initialValueIsGet) = (ParseExpression(), true);
                    else if (MatchTokenTypes(TokenType.OpenBrace) != TokenType.Error)
                    {
                        ParseMembers(functionGet = new(TokenType.None, $"get_{name}", type, Array.Empty<(TokenType, string, string)>(), true), MemberType.Function);
                        ExpectTokenTypes(TokenType.CloseBrace);
                        needsSemiColon = false;
                    }
                    else
                        throw new NotImplementedException();
                }

                if (initialValue is null && MatchTokenTypes(TokenType.Equals) != TokenType.Error)
                    initialValue = ParseExpression();

                if (initialValue is null && type is null && functionGet is null)
                    throw new NotImplementedException();

                if (needsSemiColon)
                    ExpectTokenTypes(TokenType.SemiColon);

                foundAny = true;
                if (functionGet is null && initialValue is not null && initialValueIsGet)
                    functionGet = new(TokenType.None, $"get_{name}", type, Array.Empty<(TokenType, string, string)>(), true);

                if (functionGet?.Members.Count == 0)
                    functionGet.Members.Add(new ReturnStatementNode(initialValue!));
                if (functionGet is not null)
                    node.Members.Add(functionGet);

                node.Members.Add(new VariableDeclarationNode(variableKindTokenType, name, type, initialValueIsGet ? null : initialValue, functionGet));
                if (onlyOne) return;
            }

            if (memberType == MemberType.Function)
            {
                // return
                while (MatchTokenTypes(TokenType.ReturnKeyword) != TokenType.Error)
                {
                    var expression = ParseExpression();
                    if (expression is null) throw new NotImplementedException();
                    ExpectTokenTypes(TokenType.SemiColon);

                    foundAny = true;
                    node.Members.Add(new ReturnStatementNode(expression));
                    if (onlyOne) return;
                }

                // for(v in enumerable) { ... }
                while (MatchTokenTypes(TokenType.ForKeyword) != TokenType.Error)
                {
                    ExpectTokenTypes(TokenType.OpenParentheses);
                    ExpectTokenTypes(TokenType.Identifier);
                    var id = LastMatchedToken!.Text;
                    ExpectTokenTypes(TokenType.InKeyword);
                    var expr = ParseExpression()!;
                    ExpectTokenTypes(TokenType.CloseParentheses);

                    var body = new BlockNode();
                    if (MatchTokenTypes(TokenType.OpenBrace) != TokenType.Error)
                    {
                        ParseMembers(body, MemberType.Function);
                        ExpectTokenTypes(TokenType.CloseBrace);
                    }
                    else
                        ParseMembers(body, MemberType.Function, true);

                    foundAny = true;
                    node.Members.Add(new ForStatementNode(id, expr, body));
                    if (onlyOne) return;
                }

                // expression
                while (ParseExpression() is { } expression)
                {
                    ExpectTokenTypes(TokenType.SemiColon);
                    foundAny = true;
                    node.Members.Add(new ExpressionStatementNode(expression));
                    if (onlyOne) return;
                }
            }
        } while (foundAny);
    }

    #region Parse Expression
    ExpressionNode? ParseExpression() => ParseEqualityExpression();

    ExpressionNode? ParseEqualityExpression()
    {
        var left = ParseComparisonExpression();
        if (left is null) return null;

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
        if (left is null) return null;

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
        if (left is null) return null;

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
        if (left is null) return null;

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

        return ParseRangeExpression();
    }

    ExpressionNode? ParseRangeExpression()
    {
        var left = ParsePrimaryExpression();
        if (left is null) return null;

        while (MatchTokenTypes(TokenType.DotDot) is not TokenType.Error)
        {
            var right = ParsePrimaryExpression();
            if (right is null) throw new NotImplementedException();

            left = new FunctionCallExpressionNode(new IdentifierExpressionNode("Range"), new[] { left, right });
        }

        return left;
    }

    ExpressionNode? ParsePrimaryExpression()
    {
        if (MatchTokenTypes(TokenType.TrueKeyword, TokenType.FalseKeyword) is { } operatorTokenType && operatorTokenType != TokenType.Error)
            return new LiteralExpressionNode(operatorTokenType == TokenType.TrueKeyword);
        else if (MatchTokenTypes(TokenType.StringBeginning) != TokenType.Error)
        {
            var expressions = new List<ExpressionNode>();
            while (true)
                if (MatchTokenTypes(TokenType.StringEnding) != TokenType.Error)
                    return expressions.Count == 1 && expressions[0] is LiteralExpressionNode literalExpressionNode ? literalExpressionNode : new StringExpressionNode(expressions.ToArray());
                else if (MatchTokenTypes(TokenType.StringLiteral) != TokenType.Error)
                    expressions.Add(new LiteralExpressionNode(LastMatchedToken!.Text));
                else if (ParseExpression() is { } expression)
                {
                    expressions.Add(expression);
                    ExpectTokenTypes(TokenType.CloseBrace);
                    if (lexer.PopInStringToken()) throw new NotImplementedException();
                }
        }
        else if (MatchTokenTypes(TokenType.StringLiteral) != TokenType.Error)
            return new LiteralExpressionNode(LastMatchedToken!.Text);
        else if (MatchTokenTypes(TokenType.IntegerLiteral) != TokenType.Error)
            return new LiteralExpressionNode(long.Parse(LastMatchedToken!.Text));
        else if (MatchTokenTypes(TokenType.DoubleLiteral) != TokenType.Error)
            return new LiteralExpressionNode(double.Parse(LastMatchedToken!.Text));
        else if (MatchTokenTypes(TokenType.OpenParentheses) != TokenType.Error)
        {
            var expr = ParseExpression();
            if (expr is null || lexer.Consume()!.Type != TokenType.CloseParentheses) throw new NotImplementedException();

            return new GroupingExpressionNode(expr);
        }
        else if (MatchTokenTypes(TokenType.Identifier) != TokenType.Error)
        {
            ExpressionNode node = ParseIdentifierExpressionNode(true)!;
            while (MatchTokenTypes(TokenType.Dot) != TokenType.Error)
            {
                ExpectTokenTypes(TokenType.Identifier);
                node = new BinaryExpressionNode(node, TokenType.Dot, new IdentifierExpressionNode(LastMatchedToken!.Text));
            }

            if (MatchTokenTypes(TokenType.OpenParentheses) != TokenType.Error)
            {
                // function call
                var arguments = new List<ExpressionNode>();
                if (MatchTokenTypes(TokenType.CloseParentheses) == TokenType.Error)
                {
                    while (true)
                    {
                        if (arguments.Count > 0 && MatchTokenTypes(TokenType.Comma) == TokenType.Error)
                            break;

                        var expr = ParseExpression();
                        if (expr is null) throw new NotImplementedException();
                        arguments.Add(expr);
                    }
                    ExpectTokenTypes(TokenType.CloseParentheses);
                }
                return new FunctionCallExpressionNode(node, arguments.ToArray());
            }
            else
                return node;
        }
        else if (MatchTokenTypes(TokenType.OpenBracket) != TokenType.Error)
        {
            var arguments = new List<ExpressionNode>();
            while (true)
            {
                if (arguments.Count > 0 && MatchTokenTypes(TokenType.Comma) == TokenType.Error)
                    break;

                var expr = ParseExpression();
                if (expr is null) throw new NotImplementedException();
                arguments.Add(expr);
            }
            ExpectTokenTypes(TokenType.CloseBracket);
            return new FunctionCallExpressionNode(new IdentifierExpressionNode("List"), arguments.ToArray());
        }

        return null;
    }

    IdentifierExpressionNode? ParseIdentifierExpressionNode(bool identifierMatched = false)
    {
        if (!identifierMatched && MatchTokenTypes(TokenType.Identifier) == TokenType.Error)
            return null;

        var id = LastMatchedToken!.Text;
        var genericExpressions = new List<ExpressionNode>();

        // start a transaction at <
        var transaction = lexer.StartTransaction();
        try
        {
            // try to match a generic list
            if (MatchTokenTypes(TokenType.LessThan) != TokenType.Error)
            {
                var expr = ParsePrimaryExpression();
                if (expr is null) return new(id);
                genericExpressions.Add(expr);

                while (MatchTokenTypes(TokenType.Comma) != TokenType.Error)
                {
                    expr = ParsePrimaryExpression();
                    if (expr is null) return new(id);
                    genericExpressions.Add(expr);
                }

                if (MatchTokenTypes(TokenType.GreaterThan) != TokenType.Error)
                {
                    // good case
                    transaction?.Commit();
                    return new(id, genericExpressions.ToArray());
                }
            }
        }
        finally
        {
            transaction?.Reset();
        }

        return new(id);
    }
    #endregion
}
