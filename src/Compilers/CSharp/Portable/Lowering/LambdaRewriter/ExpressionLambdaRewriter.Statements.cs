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

        private NamedTypeSymbol _CatchBlockType;
        private NamedTypeSymbol CatchBlockType
        {
            get
            {
                if ((object)_CatchBlockType == null)
                {
                    _CatchBlockType = _bound.WellKnownType(WellKnownType.System_Linq_Expressions_CatchBlock);
                }
                return _CatchBlockType;
            }
        }

        private NamedTypeSymbol _CSharpSwitchCaseType;
        private NamedTypeSymbol CSharpSwitchCaseType
        {
            get
            {
                if ((object)_CSharpSwitchCaseType == null)
                {
                    _CSharpSwitchCaseType = _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpSwitchCase);
                }
                return _CSharpSwitchCaseType;
            }
        }

        private NamedTypeSymbol _CSharpConditionalReceiverType;
        private NamedTypeSymbol ConditionalReceiverType
        {
            get
            {
                if ((object)_CSharpConditionalReceiverType == null)
                {
                    _CSharpConditionalReceiverType = _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_ConditionalReceiver);
                }
                return _CSharpConditionalReceiverType;
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
                case BoundKind.StatementList:
                    return VisitStatementList((BoundStatementList)node);

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
                case BoundKind.LabelStatement:
                    return VisitLabel((BoundLabelStatement)node);

                case BoundKind.BreakStatement:
                    return VisitBreak((BoundBreakStatement)node);
                case BoundKind.ContinueStatement:
                    return VisitContinue((BoundContinueStatement)node);

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

            if (node.Left.HasDynamicType() || node.Right.HasDynamicType())
            {
                // NB: using dynamic factories to support disabling all dynamic operations in an expression tree

                // TODO: check need for dynamic convert nodes generated at compile time

                return DynamicCSharpExprFactory("DynamicAssign", lhs, rhs);
            }
            else
            {
                return CSharpStmtFactory("Assign", lhs, rhs); // NB: use stmt factory to suppress when C# expression library is not referenced
            }
        }

        private BoundExpression VisitCompoundAssignmentOperator(BoundCompoundAssignmentOperator node)
        {
            var left = Visit(node.Left);
            var right = Visit(node.Right);

            // TODO: check whether all lifting cases are properly supported by the ET API
            bool isChecked, isLifted, requiresLifted;
            string opName = GetBinaryOperatorAssignName(node.Operator.Kind, out isChecked, out isLifted, out requiresLifted);

            bool isDynamic = node.Left.HasDynamicType() || node.Right.HasDynamicType();
            if (isDynamic)
            {
                // NB: We don't have dynamic arguments or flags in this case, but the runtime library
                //     can infer it all. For the flags, the expression node type encodes the checked
                //     context flag as well as the compound nature. For the argument flags, the nature
                //     of the operands can be used to infer UseCompileTimeType when the nodes are non-
                //     dynamic in nature.

                // DESIGN: Review the above; in particular for variables for which we don't have a
                //         dynamic variant, we are not able to distinguish object from dynamic.

                opName = "Dynamic" + opName;

                // NB: Using dynamic factories to support disabling all dynamic operations in an expression tree

                // TODO: Check whether we can have any conversions in this case; also check whether
                //       we should create a final conversion lambda to pass to the factory in the case
                //       where the LHS has a static type and the RHS has a dynamic type (or should/can
                //       we infer all the required information in the runtime library?).

                // TODO: Check whether we can safely ignore a method, if any.

                return DynamicCSharpExprFactory(opName, left, right);
            }
            else
            {
                var methodSymbol = node.Operator.Method;
                var method = methodSymbol != null ? _bound.MethodInfo(methodSymbol) : _bound.Null(_bound.WellKnownType(WellKnownType.System_Reflection_MethodInfo));

                var leftType = node.Left.Type;

                var leftConversion = default(BoundExpression);
                if (node.LeftConversion.IsUserDefined)
                {
                    leftType = node.LeftConversion.Method.ReturnType;
                    leftConversion = MakeConversionLambda(node.LeftConversion, leftType, leftType);
                }

                var finalConversion = default(BoundExpression);
                if (node.FinalConversion.IsUserDefined)
                {
                    var operationResultType = leftType; // TODO: check if this is the right type to use here
                    var resultType = node.FinalConversion.Method.ReturnType;
                    finalConversion = MakeConversionLambda(node.FinalConversion, operationResultType, resultType);
                }

                var args = default(BoundExpression[]);

                if (leftConversion != null || finalConversion != null)
                {
                    leftConversion = leftConversion ?? _bound.Null(_bound.WellKnownType(WellKnownType.System_Linq_Expressions_LambdaExpression));
                    finalConversion = finalConversion ?? _bound.Null(_bound.WellKnownType(WellKnownType.System_Linq_Expressions_LambdaExpression));

                    args = new[] { left, right, method, finalConversion, leftConversion };
                }
                else
                {
                    args = new[] { left, right, method };
                }

                return CSharpExprFactory(opName, args);
            }
        }

        private string GetBinaryOperatorAssignName(BinaryOperatorKind opKind, out bool isChecked, out bool isLifted, out bool requiresLifted)
        {
            isChecked = opKind.IsChecked();
            isLifted = opKind.IsLifted();
            requiresLifted = opKind.IsComparison();

            if (opKind.IsLogical())
            {
                throw ExceptionUtilities.UnexpectedValue(opKind.Operator());
            }

            switch (opKind.Operator())
            {
                case BinaryOperatorKind.Addition: return isChecked ? "AddAssignChecked" : "AddAssign";
                case BinaryOperatorKind.Multiplication: return isChecked ? "MultiplyAssignChecked" : "MultiplyAssign";
                case BinaryOperatorKind.Subtraction: return isChecked ? "SubtractAssignChecked" : "SubtractAssign";
                case BinaryOperatorKind.Division: return "DivideAssign";
                case BinaryOperatorKind.Remainder: return "ModuloAssign";
                case BinaryOperatorKind.Xor: return "ExclusiveOrAssign";
                case BinaryOperatorKind.LeftShift: return "LeftShiftAssign";
                case BinaryOperatorKind.RightShift: return "RightShiftAssign";
                case BinaryOperatorKind.And: return "AndAssign";
                case BinaryOperatorKind.Or: return "OrAssign";
                default:
                    throw ExceptionUtilities.UnexpectedValue(opKind.Operator());
            }
        }

        private BoundExpression VisitIncrementOperator(BoundIncrementOperator node)
        {
            var isChecked = node.OperatorKind.IsChecked();

            var op = Visit(node.Operand);

            var unaryOperatorName = default(string);

            switch (node.OperatorKind & UnaryOperatorKind.OpMask)
            {
                case UnaryOperatorKind.PostfixIncrement:
                    unaryOperatorName = isChecked ? "PostIncrementAssignChecked" : "PostIncrementAssign";
                    break;
                case UnaryOperatorKind.PostfixDecrement:
                    unaryOperatorName = isChecked ? "PostDecrementAssignChecked" : "PostDecrementAssign";
                    break;
                case UnaryOperatorKind.PrefixIncrement:
                    unaryOperatorName = isChecked ? "PreIncrementAssignChecked" : "PreIncrementAssign";
                    break;
                case UnaryOperatorKind.PrefixDecrement:
                    unaryOperatorName = isChecked ? "PreDecrementAssignChecked" : "PreDecrementAssign";
                    break;
                default:
                    throw ExceptionUtilities.UnexpectedValue(node.OperatorKind);
            }

            bool isDynamic = node.Operand.HasDynamicType();
            if (isDynamic)
            {
                // NB: We don't have dynamic arguments or flags in this case, but the runtime library
                //     can infer it all. For the flags, the expression node type encodes the checked
                //     context flag as well as the compound nature. For the argument flags, the nature
                //     of the operands can be used to infer UseCompileTimeType when the nodes are non-
                //     dynamic in nature.

                // DESIGN: Review the above; in particular for variables for which we don't have a
                //         dynamic variant, we are not able to distinguish object from dynamic.

                unaryOperatorName = "Dynamic" + unaryOperatorName;

                // NB: using dynamic factories to support disabling all dynamic operations in an expression tree

                // TODO: Check whether we can safely ignore a method, if any.

                return DynamicCSharpExprFactory(unaryOperatorName, op);
            }
            else
            {
                // TODO: add support for conversions
                //
                // node.OperandConversion
                // node.ResultConversion

                var args = default(BoundExpression[]);

                if (node.MethodOpt != null)
                {
                    args = new[] { op, _bound.MethodInfo(node.MethodOpt) };
                }
                else
                {
                    args = new[] { op };
                }

                return CSharpExprFactory(unaryOperatorName, args);
            }
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

        private BoundExpression VisitLambdaBody(BoundBlock node)
        {
            return VisitBlock(node, isTopLevel: true);
        }

        private BoundExpression VisitBlock(BoundBlock node, bool isTopLevel = false)
        {
            var locals = PushLocals(node.Locals);

            var lastStmt = default(BoundStatement);

            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var stmt in Flatten(node.Statements))
            {
                var expr = Visit(stmt);
                builder.Add(expr);

                lastStmt = stmt;
            }

            PopLocals(node.Locals);

            var returnLabel = default(BoundLocal);

            if (isTopLevel)
            {
                if (lastStmt?.Kind == BoundKind.ReturnStatement)
                {
                    var lastReturn = (BoundReturnStatement)lastStmt;
                    if (lastReturn.WasCompilerGenerated)
                    {
                        builder.RemoveLast();

                        if (lastReturn.ExpressionOpt != null)
                        {
                            var expr = Visit(lastReturn.ExpressionOpt);
                            builder.Add(expr);
                        }
                    }
                }

                returnLabel = CurrentLambdaInfo.ReturnLabel;
            }

            var variables = locals.Count > 0 ? _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree()) : null;

            return ToBlock(builder, variables, returnLabel);
        }

        private BoundExpression VisitStatementList(BoundStatementList node)
        {
            return ToBlock(node.Statements);
        }

        private BoundExpression ToBlock(ImmutableArray<BoundStatement> expressions, BoundExpression variables = null)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var arg in Flatten(expressions))
            {
                var stmt = Visit(arg);
                builder.Add(stmt);
            }

            return ToBlock(builder, variables);
        }

        private BoundExpression ToBlock(ArrayBuilder<BoundExpression> builder, BoundExpression variables = null, BoundLocal returnLabel = null)
        {
            var res = default(BoundExpression);

            if (returnLabel != null)
            {
                var statements = _bound.Array(ExpressionType, builder.ToImmutableAndFree());

                if (variables != null)
                {
                    res = CSharpStmtFactory("Block", variables, statements, returnLabel);
                }
                else
                {
                    res = CSharpStmtFactory("Block", statements, returnLabel);
                }
            }
            else
            {
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
                    res = CSharpStmtFactory("Empty");

                    if (variables != null)
                    {
                        res = CSharpStmtFactory("Block", variables, res);
                    }
                }
            }

            return res;
        }

        private IEnumerable<BoundStatement> Flatten(IEnumerable<BoundStatement> statements)
        {
            foreach (var stmt in statements)
            {
                foreach (var inner in GetStatements(stmt))
                {
                    if (inner != null)
                    {
                        yield return inner;
                    }
                }
            }
        }

        private IEnumerable<BoundStatement> GetStatements(BoundStatement statement)
        {
            // TODO: flatten without recursion
            if (statement != null)
            {
                switch (statement.Kind)
                {
                    case BoundKind.StatementList:
                        {
                            foreach (var stmt in ((BoundStatementList)statement).Statements)
                            {
                                foreach (var inner in GetStatements(stmt))
                                {
                                    yield return inner;
                                }
                            }
                        }
                        break;
                    case BoundKind.SequencePointWithSpan:
                        {
                            var seq = (BoundSequencePointWithSpan)statement;
                            foreach (var inner in GetStatements(seq.StatementOpt))
                            {
                                yield return inner;
                            }
                        }
                        break;
                    case BoundKind.SequencePoint:
                        {
                            var seq = (BoundSequencePoint)statement;
                            foreach (var inner in GetStatements(seq.StatementOpt))
                            {
                                yield return inner;
                            }
                        }
                        break;
                    default:
                        {
                            yield return statement;
                        }
                        break;
                }
            }
        }

        private ArrayBuilder<BoundExpression> PushLocals(ImmutableArray<LocalSymbol> locals)
        {
            var res = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var local in locals)
            {
                var variable = _bound.SynthesizedLocal(ParameterExpressionType);
                CurrentLambdaInfo.AddLocal(variable);
                var localReference = _bound.Local(variable);
                res.Add(localReference);
                var parameter = CSharpStmtFactory(
                    "Variable",
                    _bound.Typeof(_typeMap.SubstituteType(local.Type).Type), _bound.Literal(local.Name));
                CurrentLambdaInfo.AddLocalInitializer(_bound.AssignmentExpression(localReference, parameter));
                _localMap[local] = localReference;
            }

            return res;
        }

        private void PopLocals(ImmutableArray<LocalSymbol> locals)
        {
            foreach (var local in locals)
            {
                _localMap.Remove(local);
            }
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
            if (node.Expression.Kind == BoundKind.AwaitExpression)
            {
                return VisitAwaitExpression((BoundAwaitExpression)node.Expression, resultDiscarded: true);
            }

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

        private BoundExpression VisitIf(BoundIfStatement node)
        {
            var condition = Visit(node.Condition);
            var ifThen = Visit(node.Consequence);
            var ifElse = Visit(node.AlternativeOpt);

            return ifElse != null ? CSharpStmtFactory("IfThenElse", condition, ifThen, ifElse) : CSharpStmtFactory("IfThen", condition, ifThen);
        }

        private BoundExpression VisitSwitch(BoundSwitchStatement node)
        {
            var expression = Visit(node.BoundExpression);

            CurrentLambdaInfo.PushBreak(node.BreakLabel);

            var locals = PushLocals(node.InnerLocals);

            var caseList = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var section in node.SwitchSections)
            {
                var switchLabels = section.BoundSwitchLabels.SelectAsArray(l =>
                {
                    // REVIEW: Label, Pattern, WhenClause

                    if (l.ExpressionOpt == null)
                    {
                        // default case
                        return _bound.Property(CSharpExpressionType, "SwitchCaseDefaultValue");
                    }
                    else
                    {
                        return _bound.Convert(_objectType, l.ExpressionOpt);
                    }
                });

                var testValues = _bound.Array(_objectType, switchLabels);

                var body = VisitStatements(section.Statements);

                var @case = CSharpStmtFactory("SwitchCase", testValues, body);
                caseList.Add(@case);
            }

            var cases = _bound.Array(CSharpSwitchCaseType, caseList.ToImmutableAndFree());

            var breakInfo = CurrentLambdaInfo.PopBreak();

            PopLocals(node.InnerLocals);

            var variables = _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());

            return CSharpStmtFactory("Switch", expression, breakInfo.BreakLabel, variables, cases);
        }

        private BoundExpression VisitStatements(ImmutableArray<BoundStatement> statements)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var stmt in statements)
            {
                builder.Add(Visit(stmt));
            }

            var expression = _bound.Array(ExpressionType, builder.ToImmutableAndFree());

            return expression;
        }

        private BoundExpression VisitDo(BoundDoStatement node)
        {
            var condition = Visit(node.Condition);

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            return CSharpStmtFactory("Do", body, condition, loopInfo.BreakLabel, loopInfo.ContinueLabel);
        }

        private BoundExpression VisitFor(BoundForStatement node)
        {
            var locals = PushLocals(node.OuterLocals);

            var initializers = VisitStatements(node.Initializer).ToImmutableArray();
            var condition = Visit(node.Condition);
            var increments = VisitStatements(node.Increment).ToImmutableArray();

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            PopLocals(node.OuterLocals);

            var variables = _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());
            var initializer = _bound.Array(ExpressionType, initializers);
            var increment = _bound.Array(ExpressionType, increments);

            return CSharpStmtFactory("For", variables, initializer, condition ?? _bound.Null(ExpressionType), increment, body, loopInfo.BreakLabel, loopInfo.ContinueLabel);
        }

        private BoundExpression VisitForEach(BoundForEachStatement node)
        {
            var expression = Visit(node.Expression);

            var local = new[] { node.IterationVariable }.ToImmutableArray();
            var locals = PushLocals(local);

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            var variable = locals.ToImmutableAndFree()[0];
            PopLocals(local);

            if (node.EnumeratorInfoOpt != null && node.ElementConversion.IsUserDefined)
            {
                TypeSymbol lambdaParamType = node.EnumeratorInfoOpt.ElementType;
                var conversion = MakeConversionLambda(node.ElementConversion, lambdaParamType, node.IterationVariableType.Type);
                return CSharpStmtFactory("ForEach", variable, expression, body, loopInfo.BreakLabel, loopInfo.ContinueLabel, conversion);
            }

            // TODO: add overloads that take in MethodInfo for GetEnumerator etc?
            return CSharpStmtFactory("ForEach", variable, expression, body, loopInfo.BreakLabel, loopInfo.ContinueLabel);
        }

        private IEnumerable<BoundExpression> VisitStatements(BoundStatement node)
        {
            if (node == null)
            {
                yield break;
            }

            switch (node.Kind)
            {
                case BoundKind.StatementList:
                    foreach (var stmt in ((BoundStatementList)node).Statements)
                    {
                        yield return Visit(stmt);
                    }
                    break;
                default:
                    yield return Visit(node);
                    break;
            }
        }

        private BoundExpression VisitWhile(BoundWhileStatement node)
        {
            var condition = Visit(node.Condition);

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            return CSharpStmtFactory("While", condition, body, loopInfo.BreakLabel, loopInfo.ContinueLabel);
        }

        private BoundExpression VisitBreak(BoundBreakStatement node)
        {
            return CSharpStmtFactory("Break", CurrentLambdaInfo.ClosestBreak);
        }

        private BoundExpression VisitContinue(BoundContinueStatement node)
        {
            return CSharpStmtFactory("Continue", CurrentLambdaInfo.ClosestLoopContinue);
        }

        private BoundExpression VisitLock(BoundLockStatement node)
        {
            var argument = Visit(node.Argument);
            var body = Visit(node.Body);

            return CSharpStmtFactory("Lock", argument, body);
        }

        private BoundExpression VisitUsing(BoundUsingStatement node)
        {
            // REVIEW: Locals
            // REVIEW: IDisposableConversion
            // TODO: DisposeMethodOpt (pattern-based in C# 8.0)
            // TODO: AwaitOpt (await using in C# 8.0)

            if (node.ExpressionOpt != null)
            {
                var expression = Visit(node.ExpressionOpt);
                var body = Visit(node.Body);

                return CSharpStmtFactory("Using", expression, body);
            }
            else
            {
                var decls = node.DeclarationsOpt.LocalDeclarations;
                var n = decls.Length;

                var localsBuilder = ArrayBuilder<LocalSymbol>.GetInstance(n);

                // NB: We're just flattening all locals into a single list; no need to narrow down the
                //     scopes here; all locals should be unique and will be mapped onto unique expressions.
                //     Also, for expression trees, all ParameterExpression creations precede the creation
                //     of the tree using factories, so there's no way to narrow down scopes.

                for (var i = 0; i < n; i++)
                {
                    localsBuilder.Add(decls[i].LocalSymbol);
                }

                var locals = localsBuilder.ToImmutableAndFree();

                PushLocals(locals);

                var res = Visit(node.Body);

                for (var i = n - 1; i >= 0; i--)
                {
                    var decl = decls[i];

                    var local = _localMap[decl.LocalSymbol];
                    var initializer = Visit(decl.InitializerOpt);

                    // NB: We just nest Using blocks but we could improve the node in the runtime library
                    //     to capture multiple declarations, a la For.
                    res = CSharpStmtFactory("Using", local, initializer, res);
                }

                PopLocals(locals);

                return res;
            }
        }

        private BoundExpression VisitTry(BoundTryStatement node)
        {
            var body = Visit(node.TryBlock);

            var @catch = node.CatchBlocks;
            if (@catch.Length > 0)
            {
                var catchBlocks = ArrayBuilder<BoundExpression>.GetInstance();

                foreach (var catchBlock in node.CatchBlocks)
                {
                    catchBlocks.Add(VisitCatchBlock(catchBlock));
                }

                var catches = _bound.Array(CatchBlockType, catchBlocks.ToImmutableAndFree());

                if (node.FinallyBlockOpt != null)
                {
                    var @finally = Visit(node.FinallyBlockOpt);
                    return CSharpStmtFactory("TryCatchFinally", body, @finally, catches);
                }
                else
                {
                    return CSharpStmtFactory("TryCatch", body, catches);
                }
            }
            else
            {
                var @finally = Visit(node.FinallyBlockOpt);
                return CSharpStmtFactory("TryFinally", body, @finally);
            }
        }

        private BoundExpression VisitCatchBlock(BoundCatchBlock node)
        {
            // TODO: check use of ExceptionSourceOpt

            if (node.LocalOpt != null)
            {
                var local = new[] { node.LocalOpt }.ToImmutableArray();

                var locals = PushLocals(local);

                var body = Visit(node.Body);
                var filter = Visit(node.ExceptionFilterOpt);

                var variable = locals.ToImmutableAndFree()[0];

                PopLocals(local);

                if (filter != null)
                {
                    return CSharpStmtFactory("Catch", variable, body, filter);
                }
                else
                {
                    return CSharpStmtFactory("Catch", variable, body);
                }
            }
            else
            {
                var type = node.ExceptionTypeOpt ?? _bound.WellKnownType(WellKnownType.System_Exception); // TODO: catch System.Object instead?
                type = _typeMap.SubstituteType(type).Type;

                var body = Visit(node.Body);
                var filter = Visit(node.ExceptionFilterOpt);

                var exceptionType = _bound.Typeof(type);

                if (filter != null)
                {
                    return CSharpStmtFactory("Catch", exceptionType, body, filter);
                }
                else
                {
                    return CSharpStmtFactory("Catch", exceptionType, body);
                }
            }
        }

        private BoundExpression VisitThrow(BoundThrowStatement node)
        {
            var expr = node.ExpressionOpt;

            if (expr != null)
            {
                var expression = Visit(expr);
                return CSharpStmtFactory("Throw", expression);
            }
            else
            {
                return CSharpStmtFactory("Rethrow");
            }
        }

        private BoundExpression VisitGoto(BoundGotoStatement node)
        {
            if (node.CaseExpressionOpt != null)
            {
                var @case = _bound.Convert(_objectType, node.CaseExpressionOpt);
                return CSharpStmtFactory("GotoCase", @case);
            }
            else if (node.Label.Name == "default:") // TODO: come up with a better way to detect this case
            {
                return CSharpStmtFactory("GotoDefault");
            }
            else
            {
                var label = CurrentLambdaInfo.GetOrAddLabel(node.Label);
                return CSharpStmtFactory("GotoLabel", label);
            }
        }

        private BoundExpression VisitLabel(BoundLabelStatement node)
        {
            var label = CurrentLambdaInfo.GetOrAddLabel(node.Label);
            return CSharpStmtFactory("Label", label);
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
            private readonly Stack<LoopInfo> _loops = new Stack<LoopInfo>();
            private readonly Stack<BreakInfo> _breaks = new Stack<BreakInfo>();
            private readonly Dictionary<LabelSymbol, BoundLocal> _labels = new Dictionary<LabelSymbol, BoundLocal>();
            private readonly Stack<BoundLocal> _receivers = new Stack<BoundLocal>();

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
                    var returnLabelTarget = CreateLabelTargetLocalSymbol();
                    var returnLabelTargetLocal = CreateLabelTargetLocal(returnLabelTarget);
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

                    AddLabelInitializer(returnLabelTargetLocal, returnLabelTargetCreation);
                }

                return _returnLabelTarget;
            }

            public bool HasReturnLabel => _returnLabelTarget != null;

            public BoundLocal ReturnLabel => _returnLabelTarget;

            internal void AddLocal(LocalSymbol local)
            {
                _locals.Add(local);
            }

            internal void AddLocalInitializer(BoundExpression initializer)
            {
                _initializers.Add(initializer);
            }

            internal void PushLoop(LabelSymbol breakLabel, LabelSymbol continueLabel)
            {
                // TODO: Use the parameters so we can assert?

                var breakLabelLocal = PushBreak(breakLabel);

                var continueLabelLocalSymbol = CreateLabelTargetLocalSymbol();
                var continueLabelLocal = CreateLabelTargetLocal(continueLabelLocalSymbol);
                AddLabelInitializer(continueLabelLocal, _parent.CSharpStmtFactory("Label"));

                var loopInfo = new LoopInfo
                {
                    BreakLabel = breakLabelLocal,
                    ContinueLabel = continueLabelLocal,
                };

                _loops.Push(loopInfo);
            }

            internal LoopInfo PopLoop()
            {
                PopBreak();

                return _loops.Pop();
            }

            internal BoundLocal PushBreak(LabelSymbol breakLabel)
            {
                // TODO: Use the parameters so we can assert?

                var breakLabelLocalSymbol = CreateLabelTargetLocalSymbol();
                var breakLabelLocal = CreateLabelTargetLocal(breakLabelLocalSymbol);
                AddLabelInitializer(breakLabelLocal, _parent.CSharpStmtFactory("Label"));

                var breakInfo = new BreakInfo
                {
                    BreakLabel = breakLabelLocal,
                };

                _breaks.Push(breakInfo);

                return breakLabelLocal;
            }

            internal BreakInfo PopBreak()
            {
                return _breaks.Pop();
            }

            internal BoundExpression ClosestBreak => _breaks.Peek().BreakLabel;
            internal BoundExpression ClosestLoopContinue => _loops.Peek().ContinueLabel;

            private LocalSymbol CreateLabelTargetLocalSymbol()
            {
                var symbol = _parent._bound.SynthesizedLocal(_parent.LabelTargetType);
                _locals.Add(symbol);
                return symbol;
            }

            private BoundLocal CreateLabelTargetLocal(LocalSymbol symbol)
            {
                return _parent._bound.Local(symbol);
            }

            private void AddLabelInitializer(BoundLocal labelTargetLocal, BoundExpression labelTargetCreation)
            {
                _initializers.Add(_parent._bound.AssignmentExpression(labelTargetLocal, labelTargetCreation));
            }

            internal BoundLocal GetOrAddLabel(LabelSymbol label)
            {
                var labelLocal = default(BoundLocal);
                if (!TryGetLabel(label, out labelLocal))
                {
                    var labelLocalSymbol = CreateLabelTargetLocalSymbol();
                    labelLocal = CreateLabelTargetLocal(labelLocalSymbol);
                    AddLabelInitializer(labelLocal, _parent.CSharpStmtFactory("Label", _parent._bound.Literal(label.Name)));

                    _labels.Add(label, labelLocal);
                }

                return labelLocal;
            }

            internal bool TryGetLabel(LabelSymbol label, out BoundLocal target)
            {
                return _labels.TryGetValue(label, out target);
            }

            internal BoundLocal PushConditionalReceiver(BoundConditionalReceiver node)
            {
                var symbol = _parent._bound.SynthesizedLocal(_parent.ConditionalReceiverType);
                _locals.Add(symbol);

                var receiverLocal = _parent._bound.Local(symbol);
                _receivers.Push(receiverLocal);

                var receiverType = _parent._bound.Typeof(_parent._typeMap.SubstituteType(node.Type).Type);
                var receiverCreation = _parent.CSharpStmtFactory("ConditionalReceiver", receiverType);

                _initializers.Add(_parent._bound.AssignmentExpression(receiverLocal, receiverCreation));

                return receiverLocal;
            }

            internal BoundLocal PopConditionalReceiver()
            {
                return _receivers.Pop();
            }
        }

        class LoopInfo
        {
            public BoundExpression BreakLabel { get; set; }
            public BoundExpression ContinueLabel { get; set; }
        }

        class BreakInfo
        {
            public BoundExpression BreakLabel { get; set; }
        }
    }
}
