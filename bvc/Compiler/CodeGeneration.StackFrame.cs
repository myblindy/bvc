using Mono.Cecil;
using System;
using System.Collections;
using System.Collections.Generic;
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

        public object? MemberReference { get; set; }

        readonly List<(Member member, StackFrame stackFrame)> members = new();

        public IEnumerator<Member> GetEnumerator() => members.Select(w => w.member).GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

        public StackFrame Add(Member member, MemberReference? memberReference = null)
        {
            MemberReference = memberReference;
            StackFrame newStackFrame = new(this);
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

        public (StackFrame Get, StackFrame Set) AccessorFrames => (Parent!.FindFunction($"get_{FindParentMember()!.Name!}", Array.Empty<TypeMember>(), false)!.StackFrame, null!);

        public FunctionMember? FindFunction(string name, TypeMember[] parameters, bool recurse = true)
        {
            bool TestFunctionMember(FunctionMember member)
            {
                var parameterVariableMembers = member.StackFrame.Parameters.ToList();

                if (parameterVariableMembers.Count == parameters.Length)
                {
                    bool ok = true;
                    for (int idx = 0; idx < parameters.Length; ++idx)
                        if (!parameterVariableMembers[idx].Type!.IsAssignableTo(parameters[idx])) { ok = false; break; }
                    if (ok) return true;
                }

                return false;
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
                return recurse ? Parent?.FindFunction(name, parameters, recurse) : null;
            }
        }

        public FunctionMember? FindFunction(ExpressionNode expression, TypeMember[] parameters)
        {
            static Member? internalFindFunction(ExpressionNode expression, TypeMember[] parameters, StackFrame stackFrame, bool recurse, int level = 0)
            {
                if (expression is IdentifierExpressionNode identifierExpression)
                    return level == 0
                        ? stackFrame.FindFunction(identifierExpression.Identifier, parameters, true)
                        : stackFrame.Find<Member>(identifierExpression.Identifier);
                else if (expression is BinaryExpressionNode binaryExpressionNode && binaryExpressionNode.Operator == TokenType.Dot)
                {
                    var leftMember = internalFindFunction(binaryExpressionNode.Left, parameters, stackFrame, true, level + 1);
                    if (leftMember is null) return null;
                    var rightMember = level == 0
                        ? leftMember.StackFrame.FindFunction(((IdentifierExpressionNode)binaryExpressionNode.Right).Identifier, parameters, false)
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
