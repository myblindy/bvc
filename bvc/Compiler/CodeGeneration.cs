using Cecilifier.Runtime;
using Mono.Cecil;
using Mono.Cecil.Cil;
using System.Diagnostics.CodeAnalysis;

namespace bvc.Compiler;

class CodeGeneration
{
    private readonly RootNode rootNode;

    const string OperatorAddName = "op_Add";
    const string OperatorSubName = "op_Sub";
    const string OperatorMulName = "op_Mul";
    const string OperatorDivName = "op_Div";

    static string TokenTypeToOperatorName(TokenType tokenType) => tokenType switch
    {
        TokenType.Plus => OperatorAddName,
        TokenType.Minus => OperatorSubName,
        TokenType.Star => OperatorMulName,
        TokenType.Slash => OperatorDivName,
        _ => throw new NotImplementedException()
    };

    readonly ClassDeclaration IntegerClassDeclaration = new("Integer", null);
    readonly ClassDeclaration DoubleClassDeclaration = new("Double", null);
    readonly ClassDeclaration StringClassDeclaration = new("String", null);
    readonly ClassDeclaration VoidClassDeclaration = new("Void", null);

    public CodeGeneration(RootNode rootNode)
    {
        this.rootNode = rootNode;

        declarations[(IntegerClassDeclaration.Name!, null)] = IntegerClassDeclaration;
        declarations[(DoubleClassDeclaration.Name!, null)] = DoubleClassDeclaration;
        declarations[(StringClassDeclaration.Name!, null)] = StringClassDeclaration;
        declarations[(VoidClassDeclaration.Name!, null)] = VoidClassDeclaration;

        static void addBinaryOperation(ClassDeclaration c, string op, ClassDeclaration ret, ClassDeclaration right) =>
            c.Members.Add((op, new[] { right }), new FunctionDeclaration(op, ret, new Parameter[] { new(null, right, "right") }, c));

        addBinaryOperation(IntegerClassDeclaration, OperatorAddName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorAddName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorSubName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorSubName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorDivName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorDivName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorMulName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(IntegerClassDeclaration, OperatorMulName, DoubleClassDeclaration, DoubleClassDeclaration);

        addBinaryOperation(DoubleClassDeclaration, OperatorAddName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorAddName, DoubleClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorSubName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorSubName, DoubleClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorDivName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorDivName, DoubleClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorMulName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(DoubleClassDeclaration, OperatorMulName, DoubleClassDeclaration, IntegerClassDeclaration);

        addBinaryOperation(StringClassDeclaration, OperatorAddName, StringClassDeclaration, StringClassDeclaration);
    }

    class DeclarationKeyComparer : IEqualityComparer<(string Name, Member[]? ParameterTypeFields)>
    {
        public bool Equals((string Name, Member[]? ParameterTypeFields) x, (string Name, Member[]? ParameterTypeFields) y) =>
            x.Name == y.Name && (x.ParameterTypeFields?.Length ?? 0) == (y.ParameterTypeFields?.Length ?? 0);

        public int GetHashCode([DisallowNull] (string Name, Member[]? ParameterTypeFields) obj)
        {
            var hash = new HashCode();
            hash.Add(obj.Name);
            if (obj.ParameterTypeFields is not null)
                foreach (var field in obj.ParameterTypeFields)
                    hash.Add(field);
            return hash.ToHashCode();
        }
    }

    abstract record Member(string? Name, DeclarationWithMembers? Parent);
    record VariableMember(string Name, Member? Type, DeclarationWithMembers? Parent, ExpressionNode? InitialValueExpression = null) : Member(Name, Parent);

    abstract record Statement(DeclarationWithMembers? Parent) : Member(null, Parent);
    record ReturnStatement(ExpressionNode ExpressionNode, DeclarationWithMembers? Parent) : Statement(Parent);

    abstract record Declaration(string Name, DeclarationWithMembers? Parent) : Member(Name, Parent);
    abstract record DeclarationWithMembers(string Name, DeclarationWithMembers? Parent) : Declaration(Name, Parent)
    {
        public Dictionary<(string Name, Member[]? ParameterTypeFields), Member> Members { get; } = new(new DeclarationKeyComparer());
    }
    record EnumDeclaration(string Name, DeclarationWithMembers? Parent) : DeclarationWithMembers(Name, Parent);
    record ClassDeclaration(string Name, DeclarationWithMembers? Parent) : DeclarationWithMembers(Name, Parent);

    record Parameter(TokenType? Modifier, Member Type, string Name);
    record FunctionDeclaration(string Name, Parameter[] Parameters, DeclarationWithMembers? Parent) : DeclarationWithMembers(Name, Parent)
    {
        public FunctionDeclaration(string Name, Member? returnType, Parameter[] Parameters, DeclarationWithMembers? Parent) : this(Name, Parameters, Parent) => ReturnType = returnType;
        public Member? ReturnType { get; set; }
    }

    readonly Dictionary<(string Name, Member[]? ParameterTypeFields), Member> declarations = new(new DeclarationKeyComparer());

    Dictionary<(string Name, Member[]? ParameterTypeFields), Member> GetFields(DeclarationWithMembers? parent) => parent?.Members ?? declarations;

    Member? FindField(string name, Member[]? argumentTypeFields = null, DeclarationWithMembers? parent = null)
    {
        while (true)
        {
            var fields = GetFields(parent);
            if (fields.TryGetValue((name, argumentTypeFields), out var field))
                return field;

            if (parent is null) return null;
            parent = parent?.Parent;
        }
    }

    public void Write(Stream stream, string assemblyName)
    {
        var assembly = AssemblyDefinition.CreateAssembly(new(assemblyName, new()), assemblyName, ModuleKind.Dll);
        var module = assembly.MainModule;

        TypeReference GetTypeReference(Member? typeField) => typeField?.Name switch
        {
            null or "Void" => module!.TypeSystem.Void,
            "Integer" => module!.TypeSystem.Int64,
            "Double" => module!.TypeSystem.Double,
            "String" => module!.TypeSystem.String,
            _ => module!.GetType(typeField.Name)
        };

        // first parse out all the types
        Member ParseExpressionNodeTypeField(ExpressionNode expressionNode, DeclarationWithMembers? parent)
        {
            switch (expressionNode)
            {
                case LiteralExpressionNode literalExpressionNode:
                    return literalExpressionNode.Value switch
                    {
                        long => FindField("Integer")!,
                        double => FindField("Double")!,
                        string => FindField("String")!,
                        _ => throw new NotImplementedException(),
                    };

                case UnaryExpressionNode unaryExpressionNode:
                    return ParseExpressionNodeTypeField(unaryExpressionNode.Right, parent);

                case BinaryExpressionNode binaryExpressionNode:
                    {
                        var leftTypeField = ParseExpressionNodeTypeField(binaryExpressionNode.Left, parent);
                        var rightTypeField = ParseExpressionNodeTypeField(binaryExpressionNode.Right, parent);
                        var fnName = TokenTypeToOperatorName(binaryExpressionNode.Operator);
                        return ((FunctionDeclaration)FindField(fnName, new[] { rightTypeField }, (DeclarationWithMembers)leftTypeField)!).ReturnType!;
                    }

                case GroupingExpressionNode groupingExpressionNode:
                    return ParseExpressionNodeTypeField(groupingExpressionNode.Expression, parent);
            }

            throw new NotImplementedException();
        }

        void ParseEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithMembers? parent)
        {
            var enumDeclaration = new EnumDeclaration(enumDeclarationNode.Name, parent);

            long nextValue = 0;
            for (int idx = 0; idx < enumDeclarationNode.Members.Length; ++idx)
                enumDeclaration.Members.Add((enumDeclarationNode.Members[idx].Name, null), new VariableMember(enumDeclarationNode.Members[idx].Name,
                    null, enumDeclaration, new LiteralExpressionNode((nextValue = (enumDeclarationNode.Members[idx].Value ?? nextValue) + 1) - 1)));
            GetFields(parent).Add((enumDeclarationNode.Name, null), enumDeclaration);
        }

        void ParseClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithMembers? parent)
        {
            var classDeclaration = new ClassDeclaration(classDeclarationNode.Name, parent);
            ParseMembers(classDeclarationNode, classDeclaration);
            GetFields(parent).Add((classDeclaration.Name!, null), classDeclaration);
        }

        Member? GetTypeField(string? name, DeclarationWithMembers? parent)
        {
            if (name is null) return null;
            if (parent is null) return declarations.TryGetValue((name, null), out var field2) ? field2 : null;
            if (parent?.Members.TryGetValue((name, null), out var field) == true) return field;
            return GetTypeField(name, parent!.Parent);
        }

        void ParseFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, DeclarationWithMembers? parent)
        {
            long unreachableIdx = 0;
            string nextUnreachableId() => $"<>{unreachableIdx++}";

            var functionDeclaration = new FunctionDeclaration(functionDeclarationNode.Name, GetTypeField(functionDeclarationNode.ReturnType, parent),
                functionDeclarationNode.Arguments.Select(w => new Parameter(w.Modifier, GetTypeField(w.Type, parent)!, w.Name)).ToArray(), parent);

            foreach (var member in functionDeclarationNode.Members)
                switch (member)
                {
                    case VariableDeclarationNode variableDeclarationNode:
                        ParseVariableDeclarationNode(variableDeclarationNode, functionDeclaration);
                        break;
                    case ReturnStatementNode returnStatementNode:
                        var inferredType = ParseExpressionNodeTypeField(returnStatementNode.Expression, parent);
                        if (functionDeclaration.ReturnType is null)
                            functionDeclaration.ReturnType = inferredType;
                        else if (functionDeclaration.ReturnType != inferredType)
                            throw new NotImplementedException();

                        functionDeclaration.Members.Add((nextUnreachableId(), null), new ReturnStatement(returnStatementNode.Expression, parent));
                        break;
                    default: throw new NotImplementedException();
                }

            GetFields(parent).Add((functionDeclaration.Name!, functionDeclaration.Parameters.Select(p => p.Type).ToArray()), functionDeclaration);
        }

        void ParseVariableDeclarationNode(VariableDeclarationNode variableDeclarationNode, DeclarationWithMembers parent)
        {
            var varTypeField = GetTypeField(variableDeclarationNode.ReturnType, parent);
            var inferredVarTypeField = variableDeclarationNode.InitialValueExpression is null ? null : ParseExpressionNodeTypeField(variableDeclarationNode.InitialValueExpression!, parent);

            if (varTypeField is null && inferredVarTypeField is not null)
                varTypeField = inferredVarTypeField;
            else if (varTypeField is null && inferredVarTypeField is null || varTypeField != inferredVarTypeField && inferredVarTypeField is not null)
                throw new NotImplementedException();

            parent.Members.Add((variableDeclarationNode.Name, null),
                new VariableMember(variableDeclarationNode.Name, varTypeField!, parent, variableDeclarationNode.InitialValueExpression));
        }

        void ParseMembers(NodeWithMembers nodeWithMembers, DeclarationWithMembers? parent = null)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    ParseEnumDeclarationNode(enumDeclarationNode, parent);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    ParseClassDeclarationNode(classDeclarationNode, parent);
                else if (node is FunctionDeclarationNode functionDeclarationNode)
                    ParseFunctionDeclarationNode(functionDeclarationNode, parent);
                else if (node is VariableDeclarationNode variableDeclarationNode)
                    ParseVariableDeclarationNode(variableDeclarationNode, parent!);
                else
                    throw new NotImplementedException();
        }
        ParseMembers(rootNode);

        // next generate the code
        void WriteEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithMembers? parentDeclaration, TypeDefinition? parentType)
        {
            var enumDeclaration = (EnumDeclaration)FindField(enumDeclarationNode.Name, parent: parentDeclaration)!;
            var enumTypeDefinition = new TypeDefinition(null, enumDeclaration.Name, (parentType is null ? TypeAttributes.Public : TypeAttributes.NestedPublic) | TypeAttributes.Sealed,
                assembly.MainModule.ImportReference(typeof(Enum)));
            enumTypeDefinition.Fields.Add(new("value__", FieldAttributes.SpecialName | FieldAttributes.RTSpecialName | FieldAttributes.Public, assembly.MainModule.TypeSystem.Int64));

            foreach (var (Name, _, _, Expression) in enumDeclaration.Members.Values.Cast<VariableMember>())
                enumTypeDefinition.Fields.Add(new(Name, FieldAttributes.Static | FieldAttributes.Literal | FieldAttributes.Public | FieldAttributes.HasDefault, enumTypeDefinition) { Constant = ((LiteralExpressionNode)Expression!).Value });

            if (parentType is not null)
                parentType.NestedTypes.Add(enumTypeDefinition);
            else
                module!.Types.Add(enumTypeDefinition);
        }

        void WriteClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithMembers? parentDeclaration, TypeDefinition? parentType)
        {
            var classDeclaration = (ClassDeclaration)FindField(classDeclarationNode.Name, parent: parentDeclaration)!;
            var classTypeDefinition = new TypeDefinition(null, classDeclaration.Name,
                TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | (parentType is null ? TypeAttributes.Public : TypeAttributes.NestedPublic), assembly.MainModule.TypeSystem.Object);
            WriteDeclarations(classDeclarationNode, classDeclaration, classTypeDefinition);

            // finalize all constructors, they were left unfinished to write field initialization
            foreach (var constructorDefinition in classTypeDefinition.Methods.Where(m => m.IsConstructor))
                constructorDefinition.Body.GetILProcessor().Emit(OpCodes.Ret);

            if (parentType is not null)
                parentType.NestedTypes.Add(classTypeDefinition);
            else
                module!.Types.Add(classTypeDefinition);
        }

        void WriteFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, DeclarationWithMembers? parentDeclaration, TypeDefinition? parentTypeDefinition)
        {
            var functionDeclaration = (FunctionDeclaration)FindField(functionDeclarationNode.Name,
                functionDeclarationNode.Arguments.Select(a => GetTypeField(a.Type, parentDeclaration)!).ToArray(), parentDeclaration)!;
            var functionDefinition = new MethodDefinition(functionDeclaration.Name,
                MethodAttributes.Public | (functionDeclarationNode.IsPrimaryConstructor
                    ? MethodAttributes.HideBySig | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName
                    : 0), GetTypeReference(functionDeclaration.ReturnType));

            // arguments
            var parameterDefinitions = new ParameterDefinition[functionDeclaration.Parameters.Length];
            int parameterIdx = 0;
            foreach (var parameter in functionDeclaration.Parameters)
            {
                var parameterDefinition = new ParameterDefinition(parameter.Name, ParameterAttributes.None, GetTypeReference(parameter.Type));
                functionDefinition.Parameters.Add(parameterDefinition);
                parameterDefinitions[parameterIdx++] = parameterDefinition;
            }

            functionDefinition.Body.InitLocals = true;
            var functionIl = functionDefinition.Body.GetILProcessor();

            if (functionDeclarationNode.IsPrimaryConstructor)
            {
                // base..ctor()
                functionIl.Emit(OpCodes.Ldarg_0);
                functionIl.Emit(OpCodes.Call, module!.ImportReference(TypeHelpers.DefaultCtorFor(parentTypeDefinition!.BaseType)));

                // create the backing properties for the primary constructor
                parameterIdx = 0;
                foreach (var parameter in functionDeclaration.Parameters)
                {
                    var returnTypeReference = GetTypeReference(parameter.Type);
                    var propertyDefinition = new PropertyDefinition(parameter.Name, PropertyAttributes.None, returnTypeReference);

                    // backing field
                    var fieldDefinition = new FieldDefinition($"<P>{parameter.Name}__BackingField", FieldAttributes.Private, returnTypeReference);
                    parentTypeDefinition!.Fields.Add(fieldDefinition);

                    // getter
                    var propertyGetterDefinition = new MethodDefinition($"get_{parameter.Name}", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig, returnTypeReference);
                    parentTypeDefinition!.Methods.Add(propertyGetterDefinition);
                    propertyGetterDefinition.Body = new(propertyGetterDefinition);
                    propertyDefinition.GetMethod = propertyGetterDefinition;
                    var propertyGetterIl = propertyGetterDefinition.Body.GetILProcessor();
                    propertyGetterIl.Emit(OpCodes.Ldarg_0);
                    propertyGetterIl.Emit(OpCodes.Ldfld, fieldDefinition);
                    propertyGetterIl.Emit(OpCodes.Ret);

                    if (parameter.Modifier == TokenType.VarKeyword)
                    {
                        // setter
                        var propertySetterDefinition = new MethodDefinition($"set_{parameter.Name}", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig,
                            module!.TypeSystem.Void);
                        propertySetterDefinition.Parameters.Add(new ParameterDefinition("value", ParameterAttributes.None, returnTypeReference));
                        parentTypeDefinition!.Methods.Add(propertySetterDefinition);
                        propertySetterDefinition.Body = new(propertySetterDefinition);
                        propertyDefinition.SetMethod = propertySetterDefinition;
                        var propertySetterIl = propertySetterDefinition.Body.GetILProcessor();
                        propertySetterIl.Emit(OpCodes.Ldarg_0);
                        propertySetterIl.Emit(OpCodes.Ldarg_1);
                        propertySetterIl.Emit(OpCodes.Stfld, fieldDefinition);
                        propertySetterIl.Emit(OpCodes.Ret);
                    }

                    parentTypeDefinition!.Properties.Add(propertyDefinition);

                    // assign the property with the corresponding constructor parameter value
                    functionIl.Emit(OpCodes.Ldarg_0);
                    functionIl.Emit(OpCodes.Ldarg, ++parameterIdx);
                    if (propertyDefinition.SetMethod is not null)
                        functionIl.Emit(OpCodes.Call, propertyDefinition.SetMethod);
                    else
                        functionIl.Emit(OpCodes.Stfld, fieldDefinition);
                }
            }
            else
            {
                var variables = new Dictionary<string, VariableDefinition>();
                foreach (var field in functionDeclaration.Members.Values)
                    switch (field)
                    {
                        case VariableMember variableField:
                            {
                                var variableDefinition = new VariableDefinition(GetTypeReference(variableField.Type));
                                variables.Add(variableField.Name!, variableDefinition);
                                functionDefinition.Body.Variables.Add(variableDefinition);

                                if (variableField.InitialValueExpression is { } expressionNode)
                                {
                                    WriteExpressionNode(expressionNode, functionIl, functionDeclaration);
                                    functionIl.Emit(OpCodes.Stloc, variableDefinition);
                                }

                                break;
                            }
                        case ReturnStatement returnStatement:
                            {
                                WriteExpressionNode(returnStatement.ExpressionNode, functionIl, functionDeclaration);
                                functionIl.Emit(OpCodes.Ret);
                                break;
                            }
                        default: throw new NotImplementedException();
                    }

                functionIl.Emit(OpCodes.Ret);
            }

            if (parentTypeDefinition is not null)
                parentTypeDefinition.Methods.Add(functionDefinition);
            else
                throw new NotImplementedException();
        }

        void WriteVariableDeclarationNode(VariableDeclarationNode variableDeclarationNode, DeclarationWithMembers parentDeclaration, TypeDefinition? parentTypeDefinition)
        {
            var variableField = (VariableMember)GetTypeField(variableDeclarationNode.Name, parentDeclaration)!;
            var variableDefinition = new FieldDefinition(variableDeclarationNode.Name, FieldAttributes.Public, GetTypeReference(variableField.Type));
            parentTypeDefinition!.Fields.Add(variableDefinition);

            if (variableDeclarationNode.InitialValueExpression is not null)
                foreach (var constructorDefinition in parentTypeDefinition.Methods.Where(m => m.IsConstructor))
                {
                    var ilProcessor = constructorDefinition.Body.GetILProcessor();
                    ilProcessor.Emit(OpCodes.Ldarg_0);
                    WriteExpressionNode(variableDeclarationNode.InitialValueExpression, ilProcessor, parentDeclaration);
                    ilProcessor.Emit(OpCodes.Stfld, variableDefinition);
                }
        }

        Member? WriteExpressionNode(ExpressionNode expressionNode, ILProcessor ilProcessor, DeclarationWithMembers parentDeclaration)
        {
            switch (expressionNode)
            {
                case LiteralExpressionNode literalExpressionNode:
                    switch (literalExpressionNode.Value)
                    {
                        case long longValue: ilProcessor.Emit(OpCodes.Ldc_I8, longValue); return FindField("Integer");
                        case double doubleValue: ilProcessor.Emit(OpCodes.Ldc_R8, doubleValue); return FindField("Double");
                        case string stringValue: ilProcessor.Emit(OpCodes.Ldstr, stringValue); return FindField("String");
                        default: throw new NotImplementedException();
                    }
                case GroupingExpressionNode groupingExpressionNode:
                    return WriteExpressionNode(groupingExpressionNode.Expression, ilProcessor, parentDeclaration);
                case UnaryExpressionNode unaryExpressionNode:
                    var type = WriteExpressionNode(unaryExpressionNode.Right, ilProcessor, parentDeclaration);
                    ilProcessor.Emit(unaryExpressionNode.Operator switch
                    {
                        TokenType.Not => OpCodes.Not,
                        TokenType.Minus => OpCodes.Neg,
                        _ => throw new NotImplementedException()
                    });
                    return type;
                case BinaryExpressionNode binaryExpressionNode:
                    var leftTypeField = WriteExpressionNode(binaryExpressionNode.Left, ilProcessor, parentDeclaration);
                    var insertionPoint = ilProcessor.Body.Instructions.Last();
                    var rightTypeField = WriteExpressionNode(binaryExpressionNode.Right, ilProcessor, parentDeclaration);

                    // auto-promotion rules
                    if (leftTypeField?.Name is "Integer" && rightTypeField?.Name is "Double")
                        ilProcessor.InsertAfter(insertionPoint, ilProcessor.Create(OpCodes.Conv_R8));
                    else if (leftTypeField?.Name is "Double" && rightTypeField?.Name is "Integer")
                        ilProcessor.Emit(OpCodes.Conv_R8);

                    ilProcessor.Emit(binaryExpressionNode.Operator switch
                    {
                        TokenType.Plus => OpCodes.Add,
                        TokenType.Minus => OpCodes.Sub,
                        TokenType.Slash => OpCodes.Div,
                        TokenType.Star => OpCodes.Mul,
                        _ => throw new NotImplementedException()
                    });

                    var fnName = TokenTypeToOperatorName(binaryExpressionNode.Operator);
                    return ((FunctionDeclaration)FindField(fnName, new[] { rightTypeField! }, (DeclarationWithMembers)leftTypeField!)!).ReturnType!;
                default: throw new NotImplementedException();
            }
        }

        void WriteDeclarations(NodeWithMembers nodeWithMembers, DeclarationWithMembers? parentDeclaration = null, TypeDefinition? parentType = null)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    WriteEnumDeclarationNode(enumDeclarationNode, parentDeclaration, parentType);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    WriteClassDeclarationNode(classDeclarationNode, parentDeclaration, parentType);
                else if (node is FunctionDeclarationNode functionDeclarationNode)
                    WriteFunctionDeclarationNode(functionDeclarationNode, parentDeclaration, parentType);
                else if (node is VariableDeclarationNode variableDeclarationNode)
                    WriteVariableDeclarationNode(variableDeclarationNode, parentDeclaration, parentType);
                else
                    throw new NotImplementedException();
        }
        WriteDeclarations(rootNode);

        PrivateCoreLibFixer.FixReferences(module);
        assembly.Write(stream);
    }
}
