// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class ExpressionLambdaRewriter
    {
        private NamedTypeSymbol _CSharpStatementType;
        private NamedTypeSymbol CSharpStatementType
        {
            get
            {
                if ((object)_CSharpStatementType == null)
                {
                    _CSharpStatementType = _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpStatement);
                }
                return _CSharpStatementType;
            }
        }

        private NamedTypeSymbol _LabelTargetType;
        private NamedTypeSymbol LabelTargetType
        {
            get
            {
                if ((object)_LabelTargetType == null)
                {
                    _LabelTargetType = _bound.WellKnownType(WellKnownType.System_Linq_Expressions_LabelTarget);
                }
                return _LabelTargetType;
            }
        }

        private BoundExpression Visit(BoundStatement node)
        {
            if (node == null)
            {
                return null;
            }

            CSharpSyntaxNode old = _bound.Syntax;
            _bound.Syntax = node.Syntax;
            var result = VisitInternal(node);
            _bound.Syntax = old;
            return _bound.Convert(ExpressionType, result);
        }

        private BoundExpression VisitStatementWithoutStackGuard(BoundStatement node)
        {
            switch (node.Kind)
            {
                case BoundKind.SequencePoint:
                    return VisitSequencePoint((BoundSequencePoint)node);
                case BoundKind.SequencePointWithSpan:
                    return VisitSequencePointWithSpan((BoundSequencePointWithSpan)node);

                case BoundKind.Block:
                    return VisitBlock((BoundBlock)node);

                case BoundKind.ReturnStatement:
                    return VisitReturn((BoundReturnStatement)node);

                case BoundKind.NoOpStatement:
                    return VisitNoOp((BoundNoOpStatement)node);

                case BoundKind.ExpressionStatement:
                    return VisitExpressionStatement((BoundExpressionStatement)node);

                case BoundKind.LocalDeclaration:
                    return VisitLocalDeclaration((BoundLocalDeclaration)node);
                case BoundKind.MultipleLocalDeclarations:
                    return VisitMultipleLocalDeclarations((BoundMultipleLocalDeclarations)node);

                /*
                case BoundKind.IfStatement:
                    return VisitIf((BoundIfStatement)node);
                case BoundKind.SwitchStatement:
                    return VisitSwitch((BoundSwitchStatement)node);

                case BoundKind.DoStatement:
                    return VisitDo((BoundDoStatement)node);
                case BoundKind.ForStatement:
                    return VisitFor((BoundForStatement)node);
                case BoundKind.ForEachStatement:
                    return VisitForEach((BoundForEachStatement)node);
                case BoundKind.WhileStatement:
                    return VisitWhile((BoundWhileStatement)node);

                case BoundKind.LockStatement:
                    return VisitLock((BoundLockStatement)node);

                case BoundKind.UsingStatement:
                    return VisitUsing((BoundUsingStatement)node);

                case BoundKind.TryStatement:
                    return VisitTry((BoundTryStatement)node);
                case BoundKind.ThrowStatement:
                    return VisitThrow((BoundThrowStatement)node);

                case BoundKind.GotoStatement:
                    return VisitGoto((BoundGotoStatement)node);
                case BoundKind.LabeledStatement:
                    return VisitLabeled((BoundLabeledStatement)node);
                case BoundKind.BreakStatement:
                    return VisitBreak((BoundBreakStatement)node);
                case BoundKind.ContinueStatement:
                    return VisitContinue((BoundContinueStatement)node);
                */

                default:
                    throw ExceptionUtilities.UnexpectedValue(node.Kind);
            }
        }

        private BoundExpression VisitInternal(BoundStatement node)
        {
            BoundExpression result;
            _recursionDepth++;
#if DEBUG
            int saveRecursionDepth = _recursionDepth;
#endif

            if (_recursionDepth > 1)
            {
                StackGuard.EnsureSufficientExecutionStack(_recursionDepth);

                result = VisitStatementWithoutStackGuard(node);
            }
            else
            {
                result = VisitStatementWithStackGuard(node);
            }

#if DEBUG
            Debug.Assert(saveRecursionDepth == _recursionDepth);
#endif
            _recursionDepth--;
            return result;
        }

        private BoundExpression VisitStatementWithStackGuard(BoundStatement node)
        {
            try
            {
                return VisitStatementWithoutStackGuard(node);
            }
            catch (Exception ex) when (StackGuard.IsInsufficientExecutionStackException(ex))
            {
                throw new BoundTreeVisitor.CancelledByStackGuardException(ex, node);
            }
        }

        private BoundExpression Statements(ImmutableArray<BoundStatement> statements, out int count)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var arg in statements)
            {
                var stmt = Visit(arg);
                if (stmt != null)
                {
                    builder.Add(stmt);
                }
            }

            count = builder.Count;

            return _bound.Array(ExpressionType, builder.ToImmutableAndFree());
        }

        private BoundExpression CSharpStmtFactory(string name, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(CSharpStatementType, name, arguments);
        }

        private BoundExpression CSharpStmtFactory(string name, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(_ignoreAccessibility ? BinderFlags.IgnoreAccessibility : BinderFlags.None, CSharpStatementType, name, typeArgs, arguments);
        }

        private BoundExpression CSharpStmtFactory(WellKnownMember method, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            var m0 = _bound.WellKnownMethod(method);
            Debug.Assert((object)m0 != null);
            Debug.Assert(m0.ParameterCount == arguments.Length);
            var m1 = m0.Construct(typeArgs);
            return _bound.Call(null, m1, arguments);
        }

        private BoundExpression VisitAssignmentOperator(BoundAssignmentOperator node)
        {
            var lhs = Visit(node.Left);
            var rhs = Visit(node.Right);

            return CSharpStmtFactory("Assign", lhs, rhs); // NB: use stmt factory to suppress when C# expression library is not referenced
        }

        private BoundExpression VisitCompoundAssignmentOperator(BoundCompoundAssignmentOperator node)
        {
            var lhs = Visit(node.Left);
            var rhs = Visit(node.Right);

            // node.ExpressionSymbol contains the method
            // node.FinalConversion
            // node.LeftConversion
            // node.Operator

            throw new NotImplementedException();
        }

        private BoundExpression VisitSequencePoint(BoundSequencePoint node)
        {
            return Visit(node.StatementOpt);
        }

        private BoundExpression VisitSequencePointWithSpan(BoundSequencePointWithSpan node)
        {
            return Visit(node.StatementOpt);
        }

        private BoundExpression VisitNoOp(BoundNoOpStatement node)
        {
            if (node.Flavor != NoOpStatementFlavor.Default)
            {
                throw ExceptionUtilities.UnexpectedValue(node.Flavor);
            }

            return CSharpStmtFactory("Empty");
        }

        private readonly Dictionary<LocalSymbol, BoundExpression> _localMap = new Dictionary<LocalSymbol, BoundExpression>();

        private BoundExpression VisitBlock(BoundBlock node, bool isTopLevel = false)
        {
            var locals = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var local in node.Locals)
            {
                var variable = _bound.SynthesizedLocal(ParameterExpressionType);
                CurrentLambdaInfo.AddLocal(variable);
                var localReference = _bound.Local(variable);
                locals.Add(localReference);
                var parameter = CSharpStmtFactory(
                    "Variable",
                    _bound.Typeof(_typeMap.SubstituteType(local.Type).Type), _bound.Literal(local.Name));
                CurrentLambdaInfo.AddLocalInitializer(_bound.AssignmentExpression(localReference, parameter));
                _localMap[local] = localReference;
            }

            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var arg in node.Statements)
            {
                var stmt = Visit(arg);
                if (stmt != null)
                {
                    builder.Add(stmt);
                }
            }

            foreach (var local in node.Locals)
            {
                _localMap.Remove(local);
            }

            if (isTopLevel)
            {
                if (CurrentLambdaInfo.HasReturnLabel)
                {
                    builder.Add(CurrentLambdaInfo.GetReturnLabelDeclaration());
                }
            }

            var variables = locals.Count > 0 ? _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree()) : null;

            var res = default(BoundExpression);

            if (builder.Count > 0)
            {
                var statements = _bound.Array(ExpressionType, builder.ToImmutableAndFree());

                if (variables != null)
                {
                    res = CSharpStmtFactory("Block", variables, statements);
                }
                else
                {
                    res = CSharpStmtFactory("Block", statements);
                }
            }
            else
            {
                // NB: Expression factory doesn't support empty blocks; we could shadow Block in the C# expression library.
                res = CSharpStmtFactory("Empty");

                if (variables != null)
                {
                    res = CSharpStmtFactory("Block", variables, res);
                }
            }

            return res;
        }

        private BoundExpression VisitReturn(BoundReturnStatement node)
        {
            // TODO: check node.WasCompilerGenerated to omit when user didn't write it

            var exprOpt = node.ExpressionOpt;
            var returnLabel = CurrentLambdaInfo.EnsureReturnLabel(exprOpt?.Type);

            if (exprOpt != null)
            {
                var expr = Visit(exprOpt);
                return CSharpStmtFactory("Return", returnLabel, expr);
            }
            else
            {
                return CSharpStmtFactory("Return", returnLabel);
            }
        }

        private BoundExpression VisitExpressionStatement(BoundExpressionStatement node)
        {
            return Visit(node.Expression);
        }

        private BoundExpression VisitLocalDeclaration(BoundLocalDeclaration node)
        {
            throw new NotImplementedException();
        }

        private BoundExpression VisitMultipleLocalDeclarations(BoundMultipleLocalDeclarations node)
        {
            throw new NotImplementedException();
        }

        private LambdaCompilationInfo CurrentLambdaInfo
        {
            get { return _lambdas.Peek(); }
        }

        class LambdaCompilationInfo
        {
            private readonly ExpressionLambdaRewriter _parent;

            private readonly ArrayBuilder<LocalSymbol> _locals;
            private readonly ArrayBuilder<BoundExpression> _initializers;

            private BoundLocal _returnLabelTarget;

            public LambdaCompilationInfo(ExpressionLambdaRewriter parent, ArrayBuilder<LocalSymbol> locals, ArrayBuilder<BoundExpression> initializers)
            {
                _parent = parent;
                _locals = locals;
                _initializers = initializers;
            }

            public BoundLocal EnsureReturnLabel(TypeSymbol type)
            {
                if (_returnLabelTarget == null)
                {
                    var returnLabelTarget = _parent._bound.SynthesizedLocal(_parent.LabelTargetType);
                    _locals.Add(returnLabelTarget);

                    var returnLabelTargetLocal = _parent._bound.Local(returnLabelTarget);
                    _returnLabelTarget = returnLabelTargetLocal;

                    var returnLabelTargetCreation = default(BoundExpression);
                    if (type == null || type.SpecialType == SpecialType.System_Void)
                    {
                        returnLabelTargetCreation = _parent.CSharpStmtFactory("Label");
                    }
                    else
                    {
                        var labelType = _parent._bound.Typeof(_parent._typeMap.SubstituteType(type).Type);
                        returnLabelTargetCreation = _parent.CSharpStmtFactory("Label", labelType);
                    }

                    _initializers.Add(_parent._bound.AssignmentExpression(returnLabelTargetLocal, returnLabelTargetCreation));
                }

                return _returnLabelTarget;
            }

            public bool HasReturnLabel => _returnLabelTarget != null;

            public BoundExpression GetReturnLabelDeclaration()
            {
                return _parent.CSharpStmtFactory("Label", _returnLabelTarget);
            }

            internal void AddLocal(LocalSymbol local)
            {
                _locals.Add(local);
            }

            internal void AddLocalInitializer(BoundExpression initializer)
            {
                _initializers.Add(initializer);
            }
        }
    }
}
