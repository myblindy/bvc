using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using static Mono.Cecil.TypeAttributes;
using static Mono.Cecil.FieldAttributes;

namespace bvc.Compiler;

class CodeGeneration
{
    private readonly RootNode rootNode;

    public CodeGeneration(RootNode rootNode) => this.rootNode = rootNode;

    abstract record Field(string Name, Declaration? Parent);
    record VariableField(string Name, Declaration? Parent, object? InitialValue = null) : Field(Name, Parent);

    abstract record Declaration(string Name, Declaration? Parent) : Field(Name, Parent);
    abstract record DeclarationWithFields(string Name, Declaration? Parent) : Declaration(Name, Parent)
    {
        public Dictionary<string, Field> Fields { get; } = new();
    }
    record EnumDeclaration(string Name, Declaration? Parent) : DeclarationWithFields(Name, Parent);
    record ClassDeclaration(string Name, Declaration? Parent) : DeclarationWithFields(Name, Parent);

    readonly Dictionary<string, Field> declarations = new();

    Dictionary<string, Field> GetFields(DeclarationWithFields? parent) => parent?.Fields ?? declarations;

    public void Write(Stream stream, string assemblyName)
    {
        var assembly = AssemblyDefinition.CreateAssembly(new(assemblyName, new()), assemblyName, ModuleKind.Dll);
        var module = assembly.MainModule;

        // first parse out all the types
        void ParseEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithFields? parent)
        {
            var enumDeclaration = new EnumDeclaration(enumDeclarationNode.Name, parent);

            long nextValue = 0;
            for (int idx = 0; idx < enumDeclarationNode.Members.Length; ++idx)
                enumDeclaration.Fields.Add(enumDeclarationNode.Members[idx].Name, new VariableField(enumDeclarationNode.Members[idx].Name, enumDeclaration, (nextValue = (enumDeclarationNode.Members[idx].Value ?? nextValue) + 1) - 1));
            GetFields(parent).Add(enumDeclarationNode.Name, enumDeclaration);
        }

        void ParseClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithFields? parent)
        {
            var classDeclaration = new ClassDeclaration(classDeclarationNode.Name, parent);
            ParseMembers(classDeclarationNode, classDeclaration);
            GetFields(parent).Add(classDeclaration.Name, classDeclaration);
        }

        void ParseMembers(NodeWithMembers nodeWithMembers, DeclarationWithFields? parent = null)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    ParseEnumDeclarationNode(enumDeclarationNode, parent);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    ParseClassDeclarationNode(classDeclarationNode, parent);
                else
                    throw new NotImplementedException();
        }
        ParseMembers(rootNode);

        // next generate the code
        void WriteEnumDeclarationNode(EnumDeclarationNode enumDeclarationNode, DeclarationWithFields? parentDeclaration, TypeDefinition? parentType)
        {
            var enumDeclaration = (EnumDeclaration)GetFields(parentDeclaration)[enumDeclarationNode.Name];
            var enumTypeDefinition = new TypeDefinition(null, enumDeclaration.Name, (parentType is null ? TypeAttributes.Public : NestedPublic) | Sealed, assembly.MainModule.ImportReference(typeof(Enum)));
            enumTypeDefinition.Fields.Add(new("value__", FieldAttributes.SpecialName | FieldAttributes.RTSpecialName | FieldAttributes.Public, assembly.MainModule.TypeSystem.Int64));

            foreach (var (Name, _, Value) in enumDeclaration.Fields.Values.Cast<VariableField>())
                enumTypeDefinition.Fields.Add(new(Name, Static | Literal | FieldAttributes.Public | HasDefault, enumTypeDefinition) { Constant = Value });

            if (parentType is not null)
                parentType.NestedTypes.Add(enumTypeDefinition);
            else
                module!.Types.Add(enumTypeDefinition);
        }

        void WriteClassDeclarationNode(ClassDeclarationNode classDeclarationNode, DeclarationWithFields? parentDeclaration, TypeDefinition? parentType)
        {
            var classDeclaration = (ClassDeclaration)GetFields(parentDeclaration)[classDeclarationNode.Name];
            var classTypeDefinition = new TypeDefinition(null, classDeclaration.Name, AnsiClass | BeforeFieldInit | (parentType is null ? TypeAttributes.Public : NestedPublic), assembly.MainModule.TypeSystem.Object);
            WriteDeclarations(classDeclarationNode, classDeclaration, classTypeDefinition);

            if (parentType is not null)
                parentType.NestedTypes.Add(classTypeDefinition);
            else
                module!.Types.Add(classTypeDefinition);
        }

        void WriteDeclarations(NodeWithMembers nodeWithMembers, DeclarationWithFields? parentDeclaration = null, TypeDefinition? parentType = null)
        {
            foreach (var node in nodeWithMembers.Members)
                if (node is EnumDeclarationNode enumDeclarationNode)
                    WriteEnumDeclarationNode(enumDeclarationNode, parentDeclaration, parentType);
                else if (node is ClassDeclarationNode classDeclarationNode)
                    WriteClassDeclarationNode(classDeclarationNode, parentDeclaration, parentType);
                else
                    throw new NotImplementedException();
        }
        WriteDeclarations(rootNode);

        assembly.Write(stream);
    }
}
