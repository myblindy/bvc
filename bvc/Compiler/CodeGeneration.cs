using Mono.Cecil;

namespace bvc.Compiler;

class CodeGeneration
{
    private readonly RootNode rootNode;

    public CodeGeneration(RootNode rootNode) => this.rootNode = rootNode;

    abstract record Field(string Name);
    record VariableField(string Name, object? InitialValue = null) : Field(Name);

    abstract record Declaration(string Name);
    record EnumDeclaration(string Name, Field[] Fields) : Declaration(Name);
    record ClassDeclaration(string Name, Field[] Fields) : Declaration(Name);

    readonly Dictionary<string, Declaration> declarations = new();

    public void Write(Stream stream, string assemblyName)
    {
        var assembly = AssemblyDefinition.CreateAssembly(new(assemblyName, new()), assemblyName, ModuleKind.Dll);
        var module = assembly.MainModule;

        // first parse out all the types
        void ParseType(EnumDeclarationNode enumDeclarationNode)
        {
            var fields = new Field[enumDeclarationNode.Members.Length];
            long nextValue = 0;
            for (int idx = 0; idx < enumDeclarationNode.Members.Length; ++idx)
                fields[idx] = new VariableField(enumDeclarationNode.Members[idx].Name, (nextValue = (enumDeclarationNode.Members[idx].Value ?? nextValue) + 1) - 1);
            declarations.Add(enumDeclarationNode.Name, new EnumDeclaration(enumDeclarationNode.Name, fields));
        }
        foreach (var node in rootNode.Members)
            if (node is EnumDeclarationNode enumDeclarationNode)
                ParseType(enumDeclarationNode);
            else
                throw new NotImplementedException();

        // next generate the code
        void Write(EnumDeclarationNode enumDeclarationNode)
        {
            var enumDeclaration = (EnumDeclaration)declarations[enumDeclarationNode.Name];
            var enumTypeDefinition = new TypeDefinition(null, enumDeclaration.Name, TypeAttributes.Public, assembly.MainModule.ImportReference(typeof(Enum)));
            enumTypeDefinition.Fields.Add(new("value__", FieldAttributes.SpecialName | FieldAttributes.RTSpecialName | FieldAttributes.Public, assembly.MainModule.TypeSystem.Int64));

            foreach (var (Name, Value) in enumDeclaration.Fields.Cast<VariableField>())
                enumTypeDefinition.Fields.Add(new(Name, FieldAttributes.Static | FieldAttributes.Literal | FieldAttributes.Public | FieldAttributes.HasDefault, enumTypeDefinition) { Constant = Value });

            module.Types.Add(enumTypeDefinition);
        }

        foreach (var node in rootNode.Members)
            if (node is EnumDeclarationNode enumDeclarationNode)
                Write(enumDeclarationNode);
            else
                throw new NotImplementedException();

        assembly.Write(stream);
    }
}
