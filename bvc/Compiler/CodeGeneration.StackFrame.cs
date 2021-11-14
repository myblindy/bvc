using Mono.Cecil;
using Mono.Cecil.Rocks;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.Metrics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;

namespace bvc.Compiler;

partial class CodeGeneration
{
    class StackFrame : IEnumerable<Member>
    {
        public StackFrame? Parent { get; }
        public StackFrame(StackFrame? parent) => Parent = parent;

        readonly List<StackFrame> relatedStackFrames = new();
        public object? MemberReference { get; private set; }
        public void SetMemberReference(object? newMemberReference, StackFrame stackFrame)
        {
            MemberReference = newMemberReference;
            foreach (var s in relatedStackFrames)
                if (s.GenericTypeMembers?.Length > 0)
                    s.MemberReference = newMemberReference switch
                    {
                        TypeDefinition typeDefinition => typeDefinition.MakeGenericInstanceType(s.GenericTypeMembers.Select(t => (TypeReference)t.StackFrame.MemberReference!).ToArray()),
                        _ => throw new NotImplementedException()
                    };
                else
                    s.MemberReference = newMemberReference;
        }
        public TypeMember[]? GenericTypeMembers { get; set; }

        readonly List<(Member member, StackFrame stackFrame)> members = new();

        public StackFrame NewRelatedFrameWith(TypeMember[]? genericTypeMembers)
        {
            var newStackFrame = new StackFrame(Parent) { GenericTypeMembers = genericTypeMembers };
            newStackFrame.members.AddRange(members);
            relatedStackFrames.Add(newStackFrame);
            return newStackFrame;
        }

        public IEnumerator<Member> GetEnumerator() => members.Select(w => w.member).GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public StackFrame Add(Member member, MemberReference? memberReference = null)
        {
            StackFrame newStackFrame = new(this) { MemberReference = memberReference };
            members.Add((member, newStackFrame));
            return member.StackFrame = newStackFrame;
        }

        public T? Find<T>(string name, bool recurse = true) where T : Member
        {
            while (true)
            {
                if (members.Select(w => w.member).OfType<T>().FirstOrDefault(v => v.Name == name) is { } member)
                    return member;
                else if (recurse)
                    return Parent?.Find<T>(name, recurse);
            }
        }

        public T? Find<T>(ExpressionNode expression) where T : Member
        {
            static Member? internalFind(ExpressionNode expression, StackFrame stackFrame, bool recurse)
            {
                if (expression is IdentifierExpressionNode identifierExpression)
                    return stackFrame.Find<Member>(identifierExpression.Identifier);
                else if (expression is BinaryExpressionNode binaryExpressionNode && binaryExpressionNode.Operator == TokenType.Dot)
                {
                    var leftMember = internalFind(binaryExpressionNode.Left, stackFrame, true);
                    if (leftMember is null) return null;
                    var rightMember = leftMember.StackFrame.Find<Member>(((IdentifierExpressionNode)binaryExpressionNode.Right).Identifier, false);
                    return rightMember;
                }
                return null;
            }
            return internalFind(expression, this, true) as T;
        }

        public IEnumerable<ParameterVariableMember> Parameters => this.OfType<ParameterVariableMember>();

        public (StackFrame Get, StackFrame Set) AccessorFrames => (Parent!.FindFunction($"get_{FindParentMember()!.Name!}", Array.Empty<TypeMember>(), Array.Empty<TypeMember>(), false)!.StackFrame, null!);

        public FunctionMember? FindFunction(string name, TypeMember[]? genericParameters, TypeMember[] parameters, bool recurse = true)
        {
            bool TestFunctionMember(FunctionMember member)
            {
                var parameterVariableMembers = member.StackFrame.Parameters.ToList();

                int pvmIdx = 0, pIdx = 0;
                for (; pvmIdx < parameterVariableMembers.Count; ++pvmIdx, ++pIdx)
                    if (parameterVariableMembers[pvmIdx].Modifier == TokenType.VarArgKeyword)
                    {
                        var destType = parameterVariableMembers[pvmIdx].Type! switch
                        {
                            GenericTypeMember genericTypeMember when genericParameters is not null => genericParameters[this.OfType<GenericTypeMember>().TakeWhile(w => w.Name != genericTypeMember.Name).Count()]!,
                            _ => parameterVariableMembers[pvmIdx].Type!
                        };

                        while (pIdx < parameters.Length && parameters[pIdx].IsAssignableTo(destType)) ++pIdx;
                        --pIdx;
                    }
                    else if (!parameterVariableMembers[pvmIdx].Type!.IsAssignableTo(parameters[pIdx]))
                        return false;

                return pvmIdx == parameterVariableMembers.Count && pIdx == parameters.Length;
            }

            while (true)
            {
                foreach (var member in members.Select(w => w.member))
                {
                    if (member is FunctionMember functionMember && functionMember.Name == name && TestFunctionMember(functionMember))
                        return functionMember;
                    else if (member is ClassMember classMember && classMember.Name == name)
                        foreach (var constructorMember in classMember.Constructors)
                            if (TestFunctionMember(constructorMember))
                                return constructorMember;
                }
                return recurse ? Parent?.FindFunction(name, genericParameters, parameters, recurse) : null;
            }
        }

        public FunctionMember? FindFunction(ExpressionNode expression, TypeMember[] parameters)
        {
            var genericParameters = (expression switch
            {
                IdentifierExpressionNode identifierExpression => identifierExpression.GenericParameters ?? Array.Empty<ExpressionNode>(),
                BinaryExpressionNode binaryExpressionNode => ((IdentifierExpressionNode)binaryExpressionNode.Right).GenericParameters ?? Array.Empty<ExpressionNode>(),
                _ => throw new NotImplementedException()
            }).Select(w => Find<TypeMember>(w)!).ToArray();

            Member? internalFindFunction(ExpressionNode expression, TypeMember[] parameters, StackFrame stackFrame, bool recurse, int level = 0)
            {
                if (expression is IdentifierExpressionNode identifierExpression)
                    return level == 0
                        ? stackFrame.FindFunction(identifierExpression.Identifier, genericParameters, parameters, true)
                        : stackFrame.Find<Member>(identifierExpression.Identifier);
                else if (expression is BinaryExpressionNode binaryExpressionNode && binaryExpressionNode.Operator == TokenType.Dot)
                {
                    var leftMember = internalFindFunction(binaryExpressionNode.Left, parameters, stackFrame, true, level + 1);
                    if (leftMember is null) return null;
                    var rightMember = level == 0
                        ? leftMember.StackFrame.FindFunction(((IdentifierExpressionNode)binaryExpressionNode.Right).Identifier, genericParameters, parameters, false)
                        : leftMember.StackFrame.Find<Member>(((IdentifierExpressionNode)binaryExpressionNode.Right).Identifier, false);
                    return rightMember;
                }
                return null;
            }
            return internalFindFunction(expression, parameters, this, true) as FunctionMember;
        }

        public Member? FindParentMember() => Parent?.members.FirstOrDefault(w => w.stackFrame == this).member;
    }
}
