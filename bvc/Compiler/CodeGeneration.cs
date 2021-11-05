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

    string TokenTypeToOperatorName(TokenType tokenType) => tokenType switch
    {
        TokenType.Plus => OperatorAddName,
        TokenType.Minus => OperatorSubName,
        TokenType.Star => OperatorMulName,
        TokenType.Slash => OperatorDivName,
        _ => throw new NotImplementedException()
    };

    public CodeGeneration(RootNode rootNode)
    {
        this.rootNode = rootNode;

        var integerClassDeclaration = new ClassDeclaration("Integer", null); declarations[("Integer", null)] = integerClassDeclaration;
        var doubleClassDeclaration = new ClassDeclaration("Double", null); declarations[("Double", null)] = doubleClassDeclaration;
        var stringClassDeclaration = new ClassDeclaration("String", null); declarations[("String", null)] = stringClassDeclaration;
        declarations[("Void", null)] = new ClassDeclaration("Void", null);

        static void addBinaryOperation(ClassDeclaration c, string op, ClassDeclaration ret, ClassDeclaration right) =>
            c.Fields.Add((op, new[] { right }), new FunctionDeclaration(op, ret, new Parameter[] { new(null, right, "right") }, c));

        addBinaryOperation(integerClassDeclaration, OperatorAddName, integerClassDeclaration, integerClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorAddName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorSubName, integerClassDeclaration, integerClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorSubName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorDivName, integerClassDeclaration, integerClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorDivName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorMulName, integerClassDeclaration, integerClassDeclaration);
        addBinaryOperation(integerClassDeclaration, OperatorMulName, doubleClassDeclaration, doubleClassDeclaration);

        addBinaryOperation(doubleClassDeclaration, OperatorAddName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorAddName, doubleClassDeclaration, integerClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorSubName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorSubName, doubleClassDeclaration, integerClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorDivName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorDivName, doubleClassDeclaration, integerClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorMulName, doubleClassDeclaration, doubleClassDeclaration);
        addBinaryOperation(doubleClassDeclaration, OperatorMulName, doubleClassDeclaration, integerClassDeclaration);

        addBinaryOperation(stringClassDeclaration, OperatorAddName, stringClassDeclaration, stringClassDeclaration);
    }

    class DeclarationKeyComparer : IEqualityComparer<(string Name, Field[]? ParameterTypeFields)>
    {
        public bool Equals((string Name, Field[]? ParameterTypeFields) x, (string Name, Field[]? ParameterTypeFields) y) =>
            x.Name == y.Name && (x.ParameterTypeFields?.Length ?? 0) == (y.ParameterTypeFields?.Length ?? 0);

        public int GetHashCode([DisallowNull] (string Name, Field[]? ParameterTypeFields) obj)
        {
            var hash = new HashCode();
            hash.Add(obj.Name);
            if (obj.ParameterTypeFields is not null)
                foreach (var field in obj.ParameterTypeFields)
                    hash.Add(field);
            return hash.ToHashCode();
        }
    }

    abstract record Field(string Name, DeclarationWithFields? Parent);
    record VariableField(string Name, Field? Type, DeclarationWithFields? Parent, object? InitialValue = null) : Field(Name, Parent);

    abstract record Declaration(string Name, DeclarationWithFields? Parent) : Field(Name, Parent);
    abstract record DeclarationWithFields(string Name, DeclarationWithFields? Parent) : Declaration(Name, Parent)
    {
        public Dictionary<(string Name, Field[]? ParameterTypeFields), Field> Fields { get; } = new(new DeclarationKeyComparer());
    }
    record EnumDeclaration(string Name, DeclarationWithFields? Parent) : DeclarationWithFields(Name, Parent);
    record ClassDeclaration(string Name, DeclarationWithFields? Parent) : DeclarationWithFields(Name, Parent);

    record Parameter(TokenType? Modifier, Field Type, string Name);
    record FunctionDeclaration(string Name, Field? ReturnType, Parameter[] Parameters, DeclarationWithFields? Parent) : DeclarationWithFields(Name, Parent);

    readonly Dictionary<(string Name, Field[]? ParameterTypeFields), Field> declarations = new(new DeclarationKeyComparer());

    Dictionary<(string Name, Field[]? ParameterTypeFields), Field> GetFields(DeclarationWithFields? parent) => parent?.Fields ?? declarations;

    Field? FindField(string name, Field[]? argumentTypeFields = null, DeclarationWithFields? parent = null)
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

        TypeReference GetTypeReference(Field? typeField) => typeField?.Name switch
        {
            null or "Void" => module!.TypeSystem.Void,
            "Integer" => module!.TypeSystem.Int64,
            "Double" => module!.TypeSystem.Double,
            "String" => module!.TypeSystem.String,
            _ => module!.GetType(typeField.Name)
        };

        // first parse out all the types
        Field ParseExpressionNodeTypeField(ExpressionNode expressionNode, DeclarationWithFields? parent)
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
                        return FindField(fnName, new[] { rightTypeField }, (DeclarationWithFields)leftTypeField)!;
                    }
            }

            throw new NotImplementedException();
        }

        void ParseEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithFields? parent)
        {
            var enumDeclaration = new EnumDeclaration(enumDeclarationNode.Name, parent);

            long nextValue = 0;
            for (int idx = 0; idx < enumDeclarationNode.Members.Length; ++idx)
                enumDeclaration.Fields.Add((enumDeclarationNode.Members[idx].Name, null), new VariableField(enumDeclarationNode.Members[idx].Name,
                    null, enumDeclaration, (nextValue = (enumDeclarationNode.Members[idx].Value ?? nextValue) + 1) - 1));
            GetFields(parent).Add((enumDeclarationNode.Name, null), enumDeclaration);
        }

        void ParseClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithFields? parent)
        {
            var classDeclaration = new ClassDeclaration(classDeclarationNode.Name, parent);
            ParseMembers(classDeclarationNode, classDeclaration);
            GetFields(parent).Add((classDeclaration.Name, null), classDeclaration);
        }

        Field? GetTypeField(string name, DeclarationWithFields? parent)
        {
            if (name is null) return null;
            if (parent is null) return declarations.TryGetValue((name, null), out var field2) ? field2 : null;
            if (parent?.Fields.TryGetValue((name, null), out var field) == true) return field;
            return GetTypeField(name, parent!.Parent);
        }

        void ParseFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, DeclarationWithFields? parent)
        {
            var functionDeclaration = new FunctionDeclaration(functionDeclarationNode.Name, GetTypeField(functionDeclarationNode.ReturnType, parent),
                functionDeclarationNode.Arguments.Select(w => new Parameter(w.Modifier, GetTypeField(w.Type, parent)!, w.Name)).ToArray(), parent);

            foreach (var member in functionDeclarationNode.Members)
                switch (member)
                {
                    case VariableDeclarationNode variableDeclarationNode:
                        functionDeclaration.Fields.Add((variableDeclarationNode.Name, null), new VariableField(variableDeclarationNode.Name, GetTypeField(variableDeclarationNode.ReturnType, parent),
                            parent, variableDeclarationNode.InitialValue));
                        break;
                    default: throw new NotImplementedException();
                }

            GetFields(parent).Add((functionDeclaration.Name, functionDeclaration.Parameters.Select(p => p.Type).ToArray()), functionDeclaration);
        }

        void ParseMembers(NodeWithMembers nodeWithMembers, DeclarationWithFields? parent = null)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    ParseEnumDeclarationNode(enumDeclarationNode, parent);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    ParseClassDeclarationNode(classDeclarationNode, parent);
                else if (node is FunctionDeclarationNode functionDeclarationNode)
                    ParseFunctionDeclarationNode(functionDeclarationNode, parent);
                else
                    throw new NotImplementedException();
        }
        ParseMembers(rootNode);

        // next generate the code
        void WriteEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithFields? parentDeclaration, TypeDefinition? parentType)
        {
            var enumDeclaration = (EnumDeclaration)FindField(enumDeclarationNode.Name, parent: parentDeclaration)!;
            var enumTypeDefinition = new TypeDefinition(null, enumDeclaration.Name, (parentType is null ? TypeAttributes.Public : TypeAttributes.NestedPublic) | TypeAttributes.Sealed,
                assembly.MainModule.ImportReference(typeof(Enum)));
            enumTypeDefinition.Fields.Add(new("value__", FieldAttributes.SpecialName | FieldAttributes.RTSpecialName | FieldAttributes.Public, assembly.MainModule.TypeSystem.Int64));

            foreach (var (Name, _, _, Value) in enumDeclaration.Fields.Values.Cast<VariableField>())
                enumTypeDefinition.Fields.Add(new(Name, FieldAttributes.Static | FieldAttributes.Literal | FieldAttributes.Public | FieldAttributes.HasDefault, enumTypeDefinition) { Constant = Value });

            if (parentType is not null)
                parentType.NestedTypes.Add(enumTypeDefinition);
            else
                module!.Types.Add(enumTypeDefinition);
        }

        void WriteClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithFields? parentDeclaration, TypeDefinition? parentType)
        {
            var classDeclaration = (ClassDeclaration)FindField(classDeclarationNode.Name, parent: parentDeclaration)!;
            var classTypeDefinition = new TypeDefinition(null, classDeclaration.Name,
                TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | (parentType is null ? TypeAttributes.Public : TypeAttributes.NestedPublic), assembly.MainModule.TypeSystem.Object);
            WriteDeclarations(classDeclarationNode, classDeclaration, classTypeDefinition);

            if (parentType is not null)
                parentType.NestedTypes.Add(classTypeDefinition);
            else
                module!.Types.Add(classTypeDefinition);
        }

        void WriteFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, DeclarationWithFields? parentDeclaration, TypeDefinition? parentTypeDefinition)
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

                // create the backing properties
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
                foreach (var field in functionDeclaration.Fields.Values)
                    switch (field)
                    {
                        case VariableField variableField:
                            {
                                var variableDefinition = new VariableDefinition(GetTypeReference(variableField.Type));
                                variables.Add(variableField.Name, variableDefinition);
                                functionDefinition.Body.Variables.Add(variableDefinition);

                                if (variableField.InitialValue is ExpressionNode expressionNode)
                                {
                                    WriteExpressionNode(expressionNode, functionIl, functionDeclaration, functionDefinition);
                                    functionIl.Emit(OpCodes.Stloc, variableDefinition);
                                }

                                break;
                            }
                    }
            }

            functionIl.Emit(OpCodes.Ret);

            if (parentTypeDefinition is not null)
                parentTypeDefinition.Methods.Add(functionDefinition);
            else
                throw new NotImplementedException();
        }

        void WriteExpressionNode(ExpressionNode expressionNode, ILProcessor ilProcessor, DeclarationWithFields parentDeclaration, MethodDefinition parentDefinition)
        {
            switch (expressionNode)
            {
                case LiteralExpressionNode literalExpressionNode:
                    switch (literalExpressionNode.Value)
                    {
                        case long longValue: ilProcessor.Emit(OpCodes.Ldc_I8, longValue); break;
                        case double doubleValue: ilProcessor.Emit(OpCodes.Ldc_R8, doubleValue); break;
                        case string stringValue: ilProcessor.Emit(OpCodes.Ldstr, stringValue); break;
                        default: throw new NotImplementedException();
                    }
                    break;
                case GroupingExpressionNode groupingExpressionNode:
                    WriteExpressionNode(groupingExpressionNode.Expression, ilProcessor, parentDeclaration, parentDefinition);
                    break;
                case UnaryExpressionNode unaryExpressionNode:
                    WriteExpressionNode(unaryExpressionNode.Right, ilProcessor, parentDeclaration, parentDefinition);
                    ilProcessor.Emit(unaryExpressionNode.Operator switch
                    {
                        TokenType.Not => OpCodes.Not,
                        TokenType.Minus => OpCodes.Neg,
                        _ => throw new NotImplementedException()
                    });
                    break;
                case BinaryExpressionNode binaryExpressionNode:
                    WriteExpressionNode(binaryExpressionNode.Left, ilProcessor, parentDeclaration, parentDefinition);
                    WriteExpressionNode(binaryExpressionNode.Right, ilProcessor, parentDeclaration, parentDefinition);
                    ilProcessor.Emit(binaryExpressionNode.Operator switch
                    {
                        TokenType.Plus => OpCodes.Add,
                        TokenType.Minus => OpCodes.Sub,
                        TokenType.Slash => OpCodes.Div,
                        TokenType.Star => OpCodes.Mul,
                        _ => throw new NotImplementedException()
                    });
                    break;
                default: throw new NotImplementedException();
            }
        }

        void WriteDeclarations(NodeWithMembers nodeWithMembers, DeclarationWithFields? parentDeclaration = null, TypeDefinition? parentType = null)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    WriteEnumDeclarationNode(enumDeclarationNode, parentDeclaration, parentType);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    WriteClassDeclarationNode(classDeclarationNode, parentDeclaration, parentType);
                else if (node is FunctionDeclarationNode functionDeclarationNode)
                    WriteFunctionDeclarationNode(functionDeclarationNode, parentDeclaration, parentType);
                else
                    throw new NotImplementedException();
        }
        WriteDeclarations(rootNode);

        PrivateCoreLibFixer.FixReferences(module);
        assembly.Write(stream);
    }
}
