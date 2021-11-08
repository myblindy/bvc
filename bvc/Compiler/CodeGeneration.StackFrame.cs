using Mono.Cecil;
using System;
using System.Collections;
using System.Collections.Generic;
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

        public FunctionMember? FindFunction(string name, TypeMember[] parameters)
        {
            while (true)
            {
                foreach (var member in members.Select(w => w.member).OfType<FunctionMember>())
                    if (member.Name == name)
                    {
                        var parameterVariableMembers = member.StackFrame.OfType<ParameterVariableMember>().ToList();

                        if (parameterVariableMembers.Count == parameters.Length)
                        {
                            bool ok = true;
                            for (int idx = 0; idx < parameters.Length; ++idx)
                                if (!parameterVariableMembers[idx].Type!.IsAssignableTo(parameters[idx])) { ok = false; break; }
                            if (ok)
                                return member;
                        }
                    }
                return Parent?.FindFunction(name, parameters);
            }
        }
    }
}
