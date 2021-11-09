using Mono.Cecil;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.Metrics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

        public T? Find<T>(string name) where T : Member
        {
            while (true)
            {
                if (members.Select(w => w.member).OfType<T>().FirstOrDefault(v => v.Name == name) is { } member)
                    return member;
                else
                    return Parent?.Find<T>(name);
            }
        }

        public IEnumerable<ParameterVariableMember> Parameters => this.OfType<ParameterVariableMember>();

        public FunctionMember? FindFunction(string name, TypeMember[] parameters)
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
                return Parent?.FindFunction(name, parameters);
            }
        }

        public Member? FindParentMember() => Parent?.members.FirstOrDefault(w => w.stackFrame == this).member;
    }
}
