using Cecilifier.Runtime;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace bvc.Compiler;

static partial class CodeGeneration
{
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

    public static void Generate(RootNode rootNode, Stream stream, string assemblyName)
    {
        var assembly = AssemblyDefinition.CreateAssembly(new(assemblyName, new()), assemblyName, ModuleKind.Dll);
        var module = assembly.MainModule!;

        StackFrame mainStackFrame = new(null);

        ClassMember IntegerClassDeclaration;
        ClassMember DoubleClassDeclaration;
        ClassMember StringClassDeclaration;
        ClassMember VoidClassDeclaration;
        var integerStackFrame = mainStackFrame.Add(IntegerClassDeclaration = new("Integer"), module.TypeSystem.Int64);
        var doubleStackFrame = mainStackFrame.Add(DoubleClassDeclaration = new("Double"), module.TypeSystem.Double);
        var stringStackFrame = mainStackFrame.Add(StringClassDeclaration = new("String"), module.TypeSystem.String);
        mainStackFrame.Add(VoidClassDeclaration = new("Void"), module.TypeSystem.Void);

        static void addBinaryOperation(StackFrame s, string op, ClassMember ret, ClassMember right) =>
            s.Add(new FunctionMember(op, ret)).Add(new ParameterVariableMember(TokenType.ValKeyword, "right", right));

        addBinaryOperation(integerStackFrame, OperatorAddName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorAddName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorSubName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorSubName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorDivName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorDivName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorMulName, IntegerClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(integerStackFrame, OperatorMulName, DoubleClassDeclaration, DoubleClassDeclaration);

        addBinaryOperation(doubleStackFrame, OperatorAddName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorAddName, DoubleClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorSubName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorSubName, DoubleClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorDivName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorDivName, DoubleClassDeclaration, IntegerClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorMulName, DoubleClassDeclaration, DoubleClassDeclaration);
        addBinaryOperation(doubleStackFrame, OperatorMulName, DoubleClassDeclaration, IntegerClassDeclaration);

        addBinaryOperation(stringStackFrame, OperatorAddName, StringClassDeclaration, StringClassDeclaration);

        rootNode.Members.Insert(0, new ClassDeclarationNode("List", new[] { "T" })
        {
            Members =
            {
                new FunctionDeclarationNode(FunctionDeclarationNode.PrimaryConstructorName, null, new[] { (TokenType.VarArgKeyword, "vals", "T") }),
                new FunctionDeclarationNode("Add", null, new[] { (TokenType.None, "obj", "T") }),
                new FunctionDeclarationNode("Get", new("T"), new[] { (TokenType.None, "index", "Integer") }),
                new VariableDeclarationNode(TokenType.ValKeyword, "Count", new("Integer"), null, null),
            }
        });

        // first parse out all the types
        TypeMember ParseExpressionNodeTypeField(ExpressionNode expressionNode, StackFrame stackFrame)
        {
            switch (expressionNode)
            {
                case LiteralExpressionNode literalExpressionNode:
                    return literalExpressionNode.Value switch
                    {
                        long => IntegerClassDeclaration,
                        double => DoubleClassDeclaration,
                        string => StringClassDeclaration,
                        _ => throw new NotImplementedException(),
                    };

                case UnaryExpressionNode unaryExpressionNode:
                    return ParseExpressionNodeTypeField(unaryExpressionNode.Right, stackFrame);

                case BinaryExpressionNode binaryExpressionNode:
                    {
                        var leftTypeField = ParseExpressionNodeTypeField(binaryExpressionNode.Left, stackFrame);
                        if (binaryExpressionNode.Operator is TokenType.Dot)
                            return leftTypeField.StackFrame.Find<Member>(((IdentifierExpressionNode)binaryExpressionNode.Right).Identifier) switch
                            {
                                VariableMember variableMember => variableMember.Type!,
                                _ => throw new NotImplementedException()
                            };
                        var rightTypeField = ParseExpressionNodeTypeField(binaryExpressionNode.Right, stackFrame);
                        var fnName = TokenTypeToOperatorName(binaryExpressionNode.Operator);
                        return leftTypeField.StackFrame.FindFunction(fnName, Array.Empty<TypeMember>(), new[] { rightTypeField })!.ReturnType!;
                    }

                case GroupingExpressionNode groupingExpressionNode:
                    return ParseExpressionNodeTypeField(groupingExpressionNode.Expression, stackFrame);

                case IdentifierExpressionNode identifierExpressionNode:
                    return stackFrame.Find<Member>(identifierExpressionNode.Identifier) switch
                    {
                        VariableMember variableMember => variableMember.Type!,
                        ParameterVariableMember parameterVariableMember => parameterVariableMember.Type!,
                        _ => throw new NotImplementedException(),
                    };

                case FunctionCallExpressionNode functionCallExpressionNode:
                    var returnType = stackFrame.FindFunction(functionCallExpressionNode.Expression, functionCallExpressionNode.Arguments.Select(a => ParseExpressionNodeTypeField(a, stackFrame)).ToArray())!.ReturnType!;
                    return returnType.AsGeneric(functionCallExpressionNode.Expression, stackFrame);
            }

            throw new NotImplementedException();
        }

        void ParseEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, StackFrame stackFrame)
        {
            var enumDeclaration = new EnumMember(enumDeclarationNode.Name);
            var enumStackFrame = stackFrame.Add(enumDeclaration);

            long nextValue = 0;
            for (int idx = 0; idx < enumDeclarationNode.Members.Length; ++idx)
                enumStackFrame.Add(new VariableMember(TokenType.None, enumDeclarationNode.Members[idx].Name,
                    null, new LiteralExpressionNode((nextValue = (enumDeclarationNode.Members[idx].Value ?? nextValue) + 1) - 1)));
        }

        void ParseClassDeclarationNode(ClassDeclarationNode classDeclarationNode, StackFrame stackFrame)
        {
            var classDeclaration = new ClassMember(classDeclarationNode.Name);
            var classStackFrame = stackFrame.Add(classDeclaration);

            if (classDeclarationNode.GenericTypes is not null)
                foreach (var typeName in classDeclarationNode.GenericTypes)
                    classStackFrame.Add(new GenericTypeMember(typeName));

            ParseMembers(classDeclarationNode, classStackFrame);
        }

        void ParseFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, StackFrame stackFrame)
        {
            var functionDeclaration = new FunctionMember(functionDeclarationNode.Name, functionDeclarationNode.IsPrimaryConstructor
                ? (ClassMember)stackFrame.FindParentMember()!
                : stackFrame.Find<TypeMember>(functionDeclarationNode.ReturnType));
            var functionStackFrame = stackFrame.Add(functionDeclaration);
            foreach (var arg in functionDeclarationNode.Arguments)
            {
                var type = stackFrame.Find<TypeMember>(arg.Type)!;
                functionStackFrame.Add(new ParameterVariableMember(arg.Modifier, arg.Name, type));
                stackFrame.Add(new VariableMember(arg.Modifier, arg.Name, type, null));
            }

            foreach (var member in functionDeclarationNode.Members)
                switch (member)
                {
                    case VariableDeclarationNode variableDeclarationNode:
                        ParseVariableDeclarationNode(variableDeclarationNode, functionStackFrame);
                        break;
                    case ReturnStatementNode returnStatementNode:
                        var inferredType = ParseExpressionNodeTypeField(returnStatementNode.Expression, functionStackFrame);
                        if (functionDeclaration.ReturnType is null)
                            functionDeclaration.ReturnType = inferredType;
                        else if (functionDeclaration.ReturnType != inferredType)
                            throw new NotImplementedException();

                        functionStackFrame.Add(new ReturnStatement(returnStatementNode.Expression));
                        break;
                    default: throw new NotImplementedException();
                }
        }

        void ParseVariableDeclarationNode(VariableDeclarationNode variableDeclarationNode, StackFrame stackFrame)
        {
            var varTypeField = stackFrame.Find<TypeMember>(variableDeclarationNode.ReturnType);
            var inferredVarTypeField = variableDeclarationNode.InitialValueExpression is null && variableDeclarationNode.GetFunction is not null
                ? stackFrame.FindFunction(variableDeclarationNode.GetFunction.Name, variableDeclarationNode.ReturnType?.GenericParameters?.Select(w => stackFrame.Find<TypeMember>(w)!).ToArray() ?? Array.Empty<TypeMember>(),
                    Array.Empty<TypeMember>(), false)!.ReturnType
                : variableDeclarationNode.InitialValueExpression is null ? null : ParseExpressionNodeTypeField(variableDeclarationNode.InitialValueExpression!, stackFrame);

            if (varTypeField is null && inferredVarTypeField is not null)
                varTypeField = inferredVarTypeField;
            else if (varTypeField is null && inferredVarTypeField is null || varTypeField != inferredVarTypeField && inferredVarTypeField is not null)
                throw new NotImplementedException();

            stackFrame.Add(new VariableMember(variableDeclarationNode.Modifier, variableDeclarationNode.Name, varTypeField!,
                variableDeclarationNode.InitialValueExpression, variableDeclarationNode.GetFunction));
        }

        void ParseMembers(NodeWithMembers nodeWithMembers, StackFrame stackFrame)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    ParseEnumDeclarationNode(enumDeclarationNode, stackFrame);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    ParseClassDeclarationNode(classDeclarationNode, stackFrame);
                else if (node is FunctionDeclarationNode functionDeclarationNode)
                    ParseFunctionDeclarationNode(functionDeclarationNode, stackFrame);
                else if (node is VariableDeclarationNode variableDeclarationNode)
                    ParseVariableDeclarationNode(variableDeclarationNode, stackFrame);
                else
                    throw new NotImplementedException();
        }
        ParseMembers(rootNode, mainStackFrame);

        // next generate the code
        void WriteEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, StackFrame stackFrame)
        {
            var enumMember = stackFrame.Find<EnumMember>(enumDeclarationNode.Name)!;
            var enumTypeDefinition = new TypeDefinition(null, enumMember.Name,
                (stackFrame.Parent is null ? TypeAttributes.Public : TypeAttributes.NestedPublic) | TypeAttributes.Sealed,
                assembly.MainModule.ImportReference(typeof(Enum)));
            enumMember.StackFrame.SetMemberReference(enumTypeDefinition, stackFrame);

            enumTypeDefinition.Fields.Add(new("value__", FieldAttributes.SpecialName | FieldAttributes.RTSpecialName | FieldAttributes.Public, assembly.MainModule.TypeSystem.Int64));
            foreach (var (_, Name, _, Expression, _) in enumMember.StackFrame.OfType<VariableMember>())
                enumTypeDefinition.Fields.Add(new(Name, FieldAttributes.Static | FieldAttributes.Literal | FieldAttributes.Public | FieldAttributes.HasDefault, enumTypeDefinition)
                {
                    Constant = ((LiteralExpressionNode)Expression!).Value
                });

            if (stackFrame.Parent is not null)
                ((TypeDefinition)stackFrame.Parent.MemberReference).NestedTypes.Add(enumTypeDefinition);
            else
                module!.Types.Add(enumTypeDefinition);
        }

        void WriteClassDeclarationNode(ClassDeclarationNode classDeclarationNode, StackFrame stackFrame)
        {
            var classDeclaration = stackFrame.Find<ClassMember>(classDeclarationNode.Name)!;
            var classTypeDefinition = new TypeDefinition(null, classDeclaration.Name,
                TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | (stackFrame.Parent is null ? TypeAttributes.Public : TypeAttributes.NestedPublic),
                assembly.MainModule.TypeSystem.Object);

            if (classDeclarationNode.GenericTypes is not null)
                foreach (var genericType in classDeclarationNode.GenericTypes)
                {
                    GenericParameter genericParameter = new(genericType, classTypeDefinition);
                    classTypeDefinition.GenericParameters.Add(genericParameter);
                    classDeclaration.StackFrame.Find<GenericTypeMember>(genericType, false)!.StackFrame.SetMemberReference(genericParameter, stackFrame);
                }

            classDeclaration.StackFrame.SetMemberReference(classTypeDefinition, stackFrame);
            WriteDeclarations(classDeclarationNode, classDeclaration.StackFrame);

            // finalize all constructors, they were left unfinished to write field initialization
            foreach (var constructorDefinition in classTypeDefinition.Methods.Where(m => m.IsConstructor))
                constructorDefinition.Body.GetILProcessor().Emit(OpCodes.Ret);

            if (stackFrame.Parent is not null)
                ((TypeDefinition)stackFrame.MemberReference).NestedTypes.Add(classTypeDefinition);
            else
                module!.Types.Add(classTypeDefinition);
        }

        void WriteFunctionBody(StackFrame functionStackFrame, MethodDefinition functionDefinition, ILProcessor functionIl)
        {
            foreach (var field in functionStackFrame)
                switch (field)
                {
                    case ParameterVariableMember:
                        break;
                    case VariableMember variableMember:
                        {
                            var variableDefinition = new VariableDefinition((TypeReference)variableMember.Type!.StackFrame.MemberReference!);
                            functionDefinition.Body.Variables.Add(variableDefinition);
                            variableMember.StackFrame.SetMemberReference(variableDefinition, functionStackFrame);

                            if (variableMember.InitialValueExpression is { } expressionNode)
                            {
                                WriteExpressionNode(expressionNode, functionIl, variableMember.StackFrame);
                                functionIl.Emit(OpCodes.Stloc, variableDefinition);
                            }

                            break;
                        }
                    case ReturnStatement returnStatement:
                        {
                            WriteExpressionNode(returnStatement.ExpressionNode, functionIl, returnStatement.StackFrame);
                            functionIl.Emit(OpCodes.Ret);
                            break;
                        }
                    default: throw new NotImplementedException();
                }

            functionIl.Emit(OpCodes.Ret);
        }

        void WriteFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, StackFrame stackFrame)
        {
            if (functionDeclarationNode.Internal) return;

            var functionMember = stackFrame.FindFunction(functionDeclarationNode.Name, stackFrame.GenericTypeMembers,
                functionDeclarationNode.Arguments.Select(a => stackFrame.Find<TypeMember>(a.Type)!).ToArray())!;
            var functionDefinition = new MethodDefinition(functionMember.Name,
                MethodAttributes.Public | (functionDeclarationNode.IsPrimaryConstructor
                    ? MethodAttributes.HideBySig | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName
                    : 0), (functionDeclarationNode.IsPrimaryConstructor ? null : (TypeReference?)functionMember.ReturnType?.StackFrame.MemberReference) ?? module!.TypeSystem.Void);
            functionMember.StackFrame.SetMemberReference(functionDefinition, stackFrame);

            // arguments
            int parameterIdx = 0;
            foreach (var parameter in functionMember.StackFrame.OfType<ParameterVariableMember>())
            {
                var parameterTypeReference = (TypeReference?)parameter.Type!.StackFrame.MemberReference!;
                if (parameter.Modifier == TokenType.VarArgKeyword)
                    parameterTypeReference = parameterTypeReference.MakeArrayType();
                var parameterDefinition = new ParameterDefinition(parameter.Name, ParameterAttributes.None, parameterTypeReference);
                functionDefinition.Parameters.Add(parameterDefinition);
                functionMember.StackFrame.Find<ParameterVariableMember>(parameter.Name!)!.StackFrame.SetMemberReference(parameterDefinition, stackFrame);
            }

            functionDefinition.Body.InitLocals = true;
            var functionIl = functionDefinition.Body.GetILProcessor();

            if (functionDeclarationNode.IsPrimaryConstructor)
            {
                var parentTypeDefinition = (TypeDefinition)stackFrame.MemberReference;

                // base..ctor()
                functionIl.Emit(OpCodes.Ldarg_0);
                functionIl.Emit(OpCodes.Call, module!.ImportReference(TypeHelpers.DefaultCtorFor(parentTypeDefinition!.BaseType)));

                // create the backing properties for the primary constructor
                parameterIdx = 0;
                foreach (var parameter in functionMember.StackFrame.OfType<ParameterVariableMember>().Where(p => p.Modifier != TokenType.VarArgKeyword))
                {
                    var (propertyDefinition, fieldDefinition) = BuildPropertyDefinition(parentTypeDefinition!, parameter.Modifier, parameter.Name!,
                        (TypeReference)parameter.Type!.StackFrame.MemberReference!, module, forceBackingFieldGet: true);

                    // assign the property with the corresponding constructor parameter value
                    functionIl.Emit(OpCodes.Ldarg_0);
                    functionIl.Emit(OpCodes.Ldarg, ++parameterIdx);
                    if (propertyDefinition.SetMethod is not null)
                        functionIl.Emit(OpCodes.Call, propertyDefinition.SetMethod);
                    else
                        functionIl.Emit(OpCodes.Stfld, fieldDefinition);

                    stackFrame.Find<VariableMember>(parameter.Name!, false)!.StackFrame.SetMemberReference(propertyDefinition, stackFrame);
                }
            }
            else
                WriteFunctionBody(functionMember.StackFrame, functionDefinition, functionIl);

            if (stackFrame.Parent is not null)
                ((TypeDefinition)stackFrame.MemberReference).Methods.Add(functionDefinition);
            else
                throw new NotImplementedException();
        }

        void WriteVariableDeclarationNode(VariableDeclarationNode variableDeclarationNode, StackFrame stackFrame)
        {
            var variableField = stackFrame.Find<VariableMember>(variableDeclarationNode.Name)!;
            var parentTypeDefinition = (TypeDefinition)stackFrame.MemberReference!;

            if (variableField.GetFunction is not null)
            {
                // this is actually a read-only property with no backing field
                var (propertyDefinition, _) = BuildPropertyDefinition(parentTypeDefinition, variableField.Modifier, variableField.Name!, (TypeReference)variableField.Type!.StackFrame.MemberReference!, module!,
                    writeGetBody: (ilProcessor, functionDefinition, fieldDefinition) => WriteFunctionBody(variableField.StackFrame.AccessorFrames.Get, functionDefinition, ilProcessor));
            }
            else
            {
                var variableDefinition = new FieldDefinition(variableDeclarationNode.Name, FieldAttributes.Public, (TypeReference)variableField.Type!.StackFrame.MemberReference!);
                variableField.StackFrame.SetMemberReference(variableDefinition, stackFrame);

                parentTypeDefinition.Fields.Add(variableDefinition);

                if (variableDeclarationNode.InitialValueExpression is not null)
                    foreach (var constructorDefinition in parentTypeDefinition.Methods.Where(m => m.IsConstructor))
                    {
                        var ilProcessor = constructorDefinition.Body.GetILProcessor();
                        ilProcessor.Emit(OpCodes.Ldarg_0);
                        WriteExpressionNode(variableDeclarationNode.InitialValueExpression, ilProcessor, variableField.StackFrame);
                        ilProcessor.Emit(OpCodes.Stfld, variableDefinition);
                    }
            }
        }

        TypeMember? WriteExpressionNode(ExpressionNode expressionNode, ILProcessor ilProcessor, StackFrame stackFrame)
        {
            switch (expressionNode)
            {
                case LiteralExpressionNode literalExpressionNode:
                    switch (literalExpressionNode.Value)
                    {
                        case long longValue: ilProcessor.Emit(OpCodes.Ldc_I8, longValue); return IntegerClassDeclaration;
                        case double doubleValue: ilProcessor.Emit(OpCodes.Ldc_R8, doubleValue); return DoubleClassDeclaration;
                        case string stringValue: ilProcessor.Emit(OpCodes.Ldstr, stringValue); return StringClassDeclaration;
                        default: throw new NotImplementedException();
                    }
                case GroupingExpressionNode groupingExpressionNode:
                    return WriteExpressionNode(groupingExpressionNode.Expression, ilProcessor, stackFrame);
                case UnaryExpressionNode unaryExpressionNode:
                    var type = WriteExpressionNode(unaryExpressionNode.Right, ilProcessor, stackFrame);
                    ilProcessor.Emit(unaryExpressionNode.Operator switch
                    {
                        TokenType.Not => OpCodes.Not,
                        TokenType.Minus => OpCodes.Neg,
                        _ => throw new NotImplementedException()
                    });
                    return type;
                case BinaryExpressionNode binaryExpressionNode:
                    {
                        var leftTypeField = WriteExpressionNode(binaryExpressionNode.Left, ilProcessor, stackFrame);
                        if (leftTypeField is null) throw new NotImplementedException();
                        if (binaryExpressionNode.Operator is TokenType.Dot)
                        {
                            // dot expression
                            var targetMember = leftTypeField!.StackFrame.Find<Member>(((IdentifierExpressionNode)binaryExpressionNode.Right).Identifier, false);

                            switch (targetMember)
                            {
                                case VariableMember variableMember:
                                    {
                                        ilProcessor.Emit(OpCodes.Callvirt, ((PropertyDefinition)variableMember.StackFrame.MemberReference!).GetMethod);
                                        return variableMember.Type!;
                                    }
                                default:
                                    throw new NotImplementedException();
                            }
                        }

                        var insertionPoint = ilProcessor.Body.Instructions.Last();
                        var rightTypeField = WriteExpressionNode(binaryExpressionNode.Right, ilProcessor, stackFrame);
                        if (rightTypeField is null) throw new NotImplementedException();

                        // auto-promotion rules
                        if (leftTypeField?.Name == IntegerClassDeclaration.Name && rightTypeField?.Name == DoubleClassDeclaration.Name)
                            ilProcessor.InsertAfter(insertionPoint, ilProcessor.Create(OpCodes.Conv_R8));
                        else if (leftTypeField?.Name == DoubleClassDeclaration.Name && rightTypeField?.Name == IntegerClassDeclaration.Name)
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
                        return leftTypeField!.StackFrame.FindFunction(fnName, Array.Empty<TypeMember>(), new[] { rightTypeField! })!.ReturnType!;
                    }
                case IdentifierExpressionNode identifierExpressionNode:
                    {
                        var member = stackFrame.Find<Member>(identifierExpressionNode.Identifier)!;
                        if (member is VariableMember variableMember)
                        {
                            if (variableMember.StackFrame.MemberReference is VariableDefinition variableDefinition)
                                ilProcessor.Emit(OpCodes.Ldloc, variableDefinition);
                            else if (variableMember.StackFrame.MemberReference is PropertyDefinition propertyDefinition)
                            {
                                ilProcessor.Emit(OpCodes.Ldarg_0);
                                ilProcessor.Emit(OpCodes.Callvirt, propertyDefinition.GetMethod);
                            }
                            else
                                throw new NotImplementedException();
                            return variableMember.Type;
                        }
                        else if (member is ParameterVariableMember parameterVariableMember)
                        {
                            ilProcessor.Emit(OpCodes.Ldarg, (ParameterDefinition)parameterVariableMember.StackFrame.MemberReference!);
                            return parameterVariableMember.Type;
                        }
                        else
                            throw new NotImplementedException();
                    }
                case FunctionCallExpressionNode functionCallExpressionNode:
                    var functionMember = stackFrame.FindFunction(functionCallExpressionNode.Expression, functionCallExpressionNode.Arguments.Select(a => ParseExpressionNodeTypeField(a, stackFrame)).ToArray());
                    if (functionMember is null) throw new NotImplementedException();

                    if (!functionMember.IsConstructor)
                        ilProcessor.Emit(OpCodes.Ldarg_0);

                    var methodReference = (MethodReference)functionMember.StackFrame.MemberReference!;
                    var classMember = (ClassMember)functionMember.StackFrame.Parent!.FindParentMember()!;
                    if (classMember.StackFrame.OfType<GenericTypeMember>().Any())
                    {
                        var newMethodReference = new MethodReference(methodReference.Name, methodReference.ReturnType)
                        {
                            HasThis = methodReference.HasThis,
                            ExplicitThis = methodReference.ExplicitThis,
                            DeclaringType = ((TypeDefinition)classMember.StackFrame.MemberReference!).MakeGenericInstanceType(
                                ((IdentifierExpressionNode)functionCallExpressionNode.Expression).GenericParameters!.Select(p => (TypeReference)stackFrame.Find<TypeMember>(p)!.StackFrame.MemberReference!).ToArray()),
                            CallingConvention = methodReference.CallingConvention,
                        };
                        foreach (var parameterDefinition in methodReference.Parameters)
                            newMethodReference.Parameters.Add(new(parameterDefinition.Name, parameterDefinition.Attributes, parameterDefinition.ParameterType));
                        methodReference = newMethodReference;
                    }

                    var parameters = functionMember.StackFrame.Parameters.ToArray();
                    for (int argIdx = 0, pIdx = 0; pIdx < parameters.Length; ++argIdx, ++pIdx)
                    {
                        var parameter = parameters[pIdx];
                        if (parameter.Modifier == TokenType.VarArgKeyword)
                        {
                            // if a generic parameter, get the real type instead
                            var destType = parameter.Type! switch
                            {
                                GenericTypeMember genericTypeMember => (TypeMember)stackFrame.Find<TypeMember>(((IdentifierExpressionNode)functionCallExpressionNode.Expression)
                                    .GenericParameters![classMember.StackFrame.OfType<GenericTypeMember>().TakeWhile(w => w.Name != genericTypeMember.Name).Count()])!,
                                EnumMember or ClassMember => parameter.Type!,
                                _ => throw new NotImplementedException()
                            };

                            int argCnt = 0;
                            for (; argCnt + argIdx < functionCallExpressionNode.Arguments.Length && ParseExpressionNodeTypeField(functionCallExpressionNode.Arguments[argIdx], stackFrame)!.IsAssignableTo(destType); ++argCnt) { }
                            ilProcessor.Emit(OpCodes.Ldc_I4, argCnt);
                            ilProcessor.Emit(OpCodes.Newarr, (TypeReference)destType.StackFrame.MemberReference!);

                            for (var argIdx2 = argIdx; argIdx2 < argCnt; ++argIdx2)
                            {
                                ilProcessor.Emit(OpCodes.Dup);
                                ilProcessor.Emit(OpCodes.Ldc_I4, argIdx2);
                                WriteExpressionNode(functionCallExpressionNode.Arguments[argIdx++], ilProcessor, stackFrame);
                                ilProcessor.Emit(OpCodes.Stelem_I4);
                            }

                            --argIdx;
                        }
                        else
                            WriteExpressionNode(functionCallExpressionNode.Arguments[argIdx], ilProcessor, stackFrame);
                    }

                    ilProcessor.Emit(functionMember.IsConstructor ? OpCodes.Newobj : OpCodes.Call, methodReference);
                    return functionMember.ReturnType;

                default: throw new NotImplementedException();
            }
        }

        void WriteDeclarations(NodeWithMembers nodeWithMembers, StackFrame stackFrame)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    WriteEnumDeclarationNode(enumDeclarationNode, stackFrame);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    WriteClassDeclarationNode(classDeclarationNode, stackFrame);
                else if (node is FunctionDeclarationNode functionDeclarationNode)
                    WriteFunctionDeclarationNode(functionDeclarationNode, stackFrame);
                else if (node is VariableDeclarationNode variableDeclarationNode)
                    WriteVariableDeclarationNode(variableDeclarationNode, stackFrame);
                else
                    throw new NotImplementedException();
        }
        WriteDeclarations(rootNode, mainStackFrame);

        PrivateCoreLibFixer.FixReferences(module);
        assembly.Write(stream);
    }

    static (PropertyDefinition, FieldDefinition?) BuildPropertyDefinition(TypeDefinition typeDefinition, TokenType modifier, string name, TypeReference type, ModuleDefinition module,
        bool forceBackingFieldGet = false, Action<ILProcessor, MethodDefinition, FieldDefinition?>? writeGetBody = null, Action<ILProcessor, MethodDefinition, FieldDefinition?>? writeSetBody = null)
    {
        var propertyDefinition = new PropertyDefinition(name, PropertyAttributes.None, type);

        // backing field
        FieldDefinition? fieldDefinition = default;
        if (forceBackingFieldGet || modifier != TokenType.ValKeyword)
        {
            fieldDefinition = new FieldDefinition($"<P>{name}__BackingField", FieldAttributes.Private, type);
            typeDefinition.Fields.Add(fieldDefinition);
        }

        // getter
        var propertyGetterDefinition = new MethodDefinition($"get_{name}", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig, type);
        typeDefinition.Methods.Add(propertyGetterDefinition);
        propertyGetterDefinition.Body = new(propertyGetterDefinition);
        propertyDefinition.GetMethod = propertyGetterDefinition;

        var propertyGetterIl = propertyDefinition.GetMethod.Body.GetILProcessor();
        if (writeGetBody is null && (forceBackingFieldGet || modifier != TokenType.ValKeyword))
        {
            propertyGetterIl.Emit(OpCodes.Ldarg_0);
            propertyGetterIl.Emit(OpCodes.Ldfld, fieldDefinition);
            propertyGetterIl.Emit(OpCodes.Ret);
        }
        else if (writeGetBody is not null)
            writeGetBody(propertyGetterIl, propertyGetterDefinition, fieldDefinition);

        if (modifier == TokenType.VarKeyword)
        {
            // setter
            var propertySetterDefinition = new MethodDefinition($"set_{name}", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig,
                module.TypeSystem.Void);
            propertySetterDefinition.Parameters.Add(new ParameterDefinition("value", ParameterAttributes.None, type));
            typeDefinition.Methods.Add(propertySetterDefinition);
            propertySetterDefinition.Body = new(propertySetterDefinition);
            propertyDefinition.SetMethod = propertySetterDefinition;

            var propertySetterIl = propertySetterDefinition.Body.GetILProcessor();
            if (writeSetBody is null)
            {
                propertySetterIl.Emit(OpCodes.Ldarg_0);
                propertySetterIl.Emit(OpCodes.Ldarg_1);
                propertySetterIl.Emit(OpCodes.Stfld, fieldDefinition);
                propertySetterIl.Emit(OpCodes.Ret);
            }
            else
                writeSetBody(propertySetterIl, propertySetterDefinition, fieldDefinition);
        }

        typeDefinition.Properties.Add(propertyDefinition);
        return (propertyDefinition, fieldDefinition);
    }

    enum Modifiers
    {
        Var = 1 << 1,
        Val = Var << 1,
        VarArgs = Val << 1,
    }

    abstract record Member(string? Name)
    {
        public StackFrame StackFrame { get; set; }
    }
    record VariableMember(TokenType Modifier, string Name, TypeMember? Type, ExpressionNode? InitialValueExpression = null, FunctionDeclarationNode? GetFunction = null) : Member(Name);
    record ParameterVariableMember(TokenType Modifier, string Name, TypeMember? Type, ExpressionNode? InitialValueExpression = null) : Member(Name);
    abstract record TypeMember(string Name) : Member(Name)
    {
        public bool IsAssignableTo(TypeMember x) => x == this || x.Name == "Integer" && Name == "Double";

        class GenericCacheComparer : IEqualityComparer<(TypeMember type, TypeMember[] typeMembers)>
        {
            public bool Equals((TypeMember type, TypeMember[] typeMembers) x, (TypeMember type, TypeMember[] typeMembers) y) =>
                x.type == y.type && x.typeMembers.SequenceEqual(y.typeMembers);

            public int GetHashCode([DisallowNull] (TypeMember type, TypeMember[] typeMembers) obj)
            {
                var hash = new HashCode();
                hash.Add(obj.type);
                foreach (var item in obj.typeMembers)
                    hash.Add(item);
                return hash.ToHashCode();
            }
        }

        static readonly Dictionary<(TypeMember type, TypeMember[] typeMembers), TypeMember> genericCache = new(new GenericCacheComparer());

        public TypeMember AsGeneric(ExpressionNode? expression, StackFrame stackFrame)
        {
            if (expression is null) return this;

            var genericTypes = ((expression switch
            {
                IdentifierExpressionNode i => i.GenericParameters,
                BinaryExpressionNode b => ((IdentifierExpressionNode)b.Right).GenericParameters,
                _ => throw new NotImplementedException()
            }) ?? Array.Empty<ExpressionNode>()).Select(e => stackFrame.Find<TypeMember>(e)!).ToArray();

            TypeMember? genericType;
            if (genericTypes.Length == 0)
                return this;
            else if (!genericCache.TryGetValue((this, genericTypes), out genericType))
                genericCache[(this, genericTypes)] = genericType = this with
                {
                    StackFrame = StackFrame.NewRelatedFrameWith(genericTypes),
                };
            return genericType;
        }
    }
    record GenericTypeMember(string Name) : TypeMember(Name);
    record EnumMember(string Name) : TypeMember(Name);
    record ClassMember(string Name) : TypeMember(Name)
    {
        public IEnumerable<FunctionMember> Constructors => StackFrame.OfType<FunctionMember>().Where(f => f.IsConstructor);
    }

    record FunctionMember(string Name) : Member(Name)
    {
        public FunctionMember(string Name, TypeMember? returnType) : this(Name) =>
            ReturnType = returnType;
        public TypeMember? ReturnType { get; set; }
        public bool IsConstructor { get; } = Name == ".ctor";
    }

    abstract record Statement() : Member((string?)null);
    record ReturnStatement(ExpressionNode ExpressionNode) : Statement;
}
