using Cecilifier.Runtime;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace bvc.Compiler;

class CodeGeneration
{
    private readonly RootNode rootNode;

    public CodeGeneration(RootNode rootNode) => this.rootNode = rootNode;

    abstract record Field(string Name, DeclarationWithFields? Parent);
    record VariableField(string Name, Field? Type, DeclarationWithFields? Parent, object? InitialValue = null) : Field(Name, Parent);

    abstract record Declaration(string Name, DeclarationWithFields? Parent) : Field(Name, Parent);
    abstract record DeclarationWithFields(string Name, DeclarationWithFields? Parent) : Declaration(Name, Parent)
    {
        public Dictionary<string, Field> Fields { get; } = new();
    }
    record EnumDeclaration(string Name, DeclarationWithFields? Parent) : DeclarationWithFields(Name, Parent);
    record ClassDeclaration(string Name, DeclarationWithFields? Parent) : DeclarationWithFields(Name, Parent);

    record Parameter(TokenType? Modifier, Field Type, string Name);
    record FunctionDeclaration(string Name, Field? ReturnType, Parameter[] Parameters, DeclarationWithFields? Parent) : DeclarationWithFields(Name, Parent);

    readonly Dictionary<string, Field> declarations = new()
    {
        ["Integer"] = new ClassDeclaration("Integer", null),
        ["Double"] = new ClassDeclaration("Double", null),
        ["String"] = new ClassDeclaration("String", null),
        ["Void"] = new ClassDeclaration("Void", null),
    };

    Dictionary<string, Field> GetFields(DeclarationWithFields? parent) => parent?.Fields ?? declarations;

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
        void ParseEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithFields? parent)
        {
            var enumDeclaration = new EnumDeclaration(enumDeclarationNode.Name, parent);

            long nextValue = 0;
            for (int idx = 0; idx < enumDeclarationNode.Members.Length; ++idx)
                enumDeclaration.Fields.Add(enumDeclarationNode.Members[idx].Name, new VariableField(enumDeclarationNode.Members[idx].Name,
                    null, enumDeclaration, (nextValue = (enumDeclarationNode.Members[idx].Value ?? nextValue) + 1) - 1));
            GetFields(parent).Add(enumDeclarationNode.Name, enumDeclaration);
        }

        void ParseClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithFields? parent)
        {
            var classDeclaration = new ClassDeclaration(classDeclarationNode.Name, parent);
            ParseMembers(classDeclarationNode, classDeclaration);
            GetFields(parent).Add(classDeclaration.Name, classDeclaration);
        }

        Field? GetTypeField(string name, DeclarationWithFields? parent)
        {
            if (name is null) return null;
            if (parent is null) return declarations.TryGetValue(name, out var field2) ? field2 : null;
            if (parent?.Fields.TryGetValue(name, out var field) == true) return field;
            return GetTypeField(name, parent!.Parent);
        }

        void ParseFunctionDeclarationNode(FunctionDeclarationNode functionDeclarationNode, DeclarationWithFields? parent)
        {
            var functionDeclaration = new FunctionDeclaration(functionDeclarationNode.Name, GetTypeField(functionDeclarationNode.ReturnType, parent),
                functionDeclarationNode.Arguments.Select(w => new Parameter(w.Modifier, GetTypeField(w.Type, parent)!, w.Name)).ToArray(), parent);
            GetFields(parent).Add(functionDeclaration.Name, functionDeclaration);
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
            var enumDeclaration = (EnumDeclaration)GetFields(parentDeclaration)[enumDeclarationNode.Name];
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
            var classDeclaration = (ClassDeclaration)GetFields(parentDeclaration)[classDeclarationNode.Name];
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
            var functionDeclaration = (FunctionDeclaration)GetFields(parentDeclaration)[functionDeclarationNode.Name];
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

            functionIl.Emit(OpCodes.Ret);

            if (parentTypeDefinition is not null)
                parentTypeDefinition.Methods.Add(functionDefinition);
            else
                throw new NotImplementedException();
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

        assembly.Write(stream);
    }
}
