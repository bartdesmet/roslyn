// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class ExpressionLambdaRewriter
    {
        private NamedTypeSymbol _CSharpStatementType;
        private NamedTypeSymbol CSharpStatementType => _CSharpStatementType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpStatement);

        private NamedTypeSymbol _LabelTargetType;
        private NamedTypeSymbol LabelTargetType => _LabelTargetType ??= _bound.WellKnownType(WellKnownType.System_Linq_Expressions_LabelTarget);

        private NamedTypeSymbol _CSharpCatchBlockType;
        private NamedTypeSymbol CSharpCatchBlockType => _CSharpCatchBlockType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpCatchBlock);

        private NamedTypeSymbol _CSharpSwitchCaseType;
        private NamedTypeSymbol CSharpSwitchCaseType => _CSharpSwitchCaseType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpSwitchCase);

        private NamedTypeSymbol _CSharpConditionalReceiverType;
        private NamedTypeSymbol ConditionalReceiverType => _CSharpConditionalReceiverType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_ConditionalReceiver);

        private NamedTypeSymbol _CSharpLocalDeclarationType;
        private NamedTypeSymbol CSharpLocalDeclarationType => _CSharpLocalDeclarationType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_LocalDeclaration);

        [return: NotNullIfNotNull("node")]
        private BoundExpression? Visit(BoundStatement? node)
        {
            if (node == null)
            {
                return null;
            }

            SyntaxNode old = _bound.Syntax;
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
            catch (InsufficientExecutionStackException ex)
            {
                throw new BoundTreeVisitor.CancelledByStackGuardException(ex, node);
            }
        }

        private BoundExpression CSharpStmtFactory(string name, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(CSharpStatementType, name, arguments);
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

                var conversionLeft = BoundNode.GetConversion(node.LeftConversion, node.LeftPlaceholder);

                BoundExpression? leftConversion = null;
                if (conversionLeft.Method is not null)
                {
                    leftType = conversionLeft.Method.ReturnType;
                    leftConversion = MakeConversionLambda(conversionLeft, leftType, leftType);
                }

                var conversionFinal = BoundNode.GetConversion(node.FinalConversion, node.FinalPlaceholder);

                BoundExpression? finalConversion = null;
                if (conversionFinal.Method is not null)
                {
                    var operationResultType = leftType; // TODO: check if this is the right type to use here
                    var resultType = conversionFinal.Method.ReturnType;
                    finalConversion = MakeConversionLambda(conversionFinal, operationResultType, resultType);
                }

                BoundExpression[] args;

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

            string unaryOperatorName;

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

                BoundExpression[] args;

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
            // REVIEW: Nullability.
            return Visit(node.StatementOpt!);
        }

        private BoundExpression VisitSequencePointWithSpan(BoundSequencePointWithSpan node)
        {
            // REVIEW: Nullability.
            return Visit(node.StatementOpt!);
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

            BoundStatement? lastStmt = null;

            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var stmt in Flatten(node.Statements))
            {
                var expr = Visit(stmt);
                builder.Add(expr);

                lastStmt = stmt;
            }

            BoundLocal? returnLabel = null;

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

            PopLocals(node.Locals);

            var variables = locals.Count > 0 ? _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree()) : null;

            return ToBlock(builder, variables, returnLabel);
        }

        private BoundExpression VisitStatementList(BoundStatementList node)
        {
            return ToBlock(node.Statements);
        }

        private BoundExpression ToBlock(ImmutableArray<BoundStatement> expressions, BoundExpression? variables = null)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var arg in Flatten(expressions))
            {
                var stmt = Visit(arg);
                builder.Add(stmt);
            }

            return ToBlock(builder, variables);
        }

        private BoundExpression ToBlock(ArrayBuilder<BoundExpression> builder, BoundExpression? variables = null, BoundLocal? returnLabel = null)
        {
            BoundExpression res;

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

        private IEnumerable<BoundStatement> GetStatements(BoundStatement? statement)
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
                res.Add(PushLocal(local));
            }

            return res;
        }

        private BoundExpression PushLocal(LocalSymbol local)
        {
            var variable = _bound.SynthesizedLocal(ParameterExpressionType);
            CurrentLambdaInfo.AddLocal(variable);
            var localReference = _bound.Local(variable);
            var parameter = CSharpStmtFactory(
                "Variable",
                _bound.Typeof(_typeMap.SubstituteType(local.Type).Type), _bound.Literal(local.Name));
            CurrentLambdaInfo.AddLocalInitializer(_bound.AssignmentExpression(localReference, parameter));
            _localMap[local] = localReference;
            return localReference;
        }

        private void PopLocals(ImmutableArray<LocalSymbol> locals)
        {
            foreach (var local in locals)
            {
                PopLocal(local);
            }
        }

        private void PopLocal(LocalSymbol local)
        {
            _localMap.Remove(local);
        }

        private BoundExpression VisitReturn(BoundReturnStatement node)
        {
            // TODO: Check node.WasCompilerGenerated to omit separate node when user didn't write a return statement.
            // REVIEW: The use of a return label is unnatural for C# expression trees (unlike for the reduction/lowering target).

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
            var local = _localMap[node.LocalSymbol];
            var initializer = Visit(node.InitializerOpt);

            return CSharpStmtFactory("LocalDeclaration", local, initializer);
        }

        private BoundExpression VisitMultipleLocalDeclarations(BoundMultipleLocalDeclarations node)
        {
            var declarations = node.LocalDeclarations.SelectAsArray(decl => VisitLocalDeclaration(decl));

            return _bound.Array(CSharpLocalDeclarationType, declarations);
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
            var expression = Visit(node.Expression);

            CurrentLambdaInfo.PushBreak(node.BreakLabel);
            CurrentLambdaInfo.PushSwitchDefaultLabel(node.DefaultLabel);

            var locals = PushLocals(node.InnerLocals);

            var caseList = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var section in node.SwitchSections)
            {
                var switchLabels = section.SwitchLabels.SelectAsArray(l =>
                {
                    var label = CurrentLambdaInfo.GetOrAddLabel(l.Label); // REVIEW
                    var labelExpr = CSharpStmtFactory("Label", label);

                    switch (l.Pattern)
                    {
                        case BoundConstantPattern c:
                            // TODO: c.ConvertedType
                            return _bound.Convert(_objectType, c.Value);
                        case BoundDiscardPattern _:
                            // REVIEW: Takes the place of default for now; switch needs rework to support patterns
                            return _bound.Property(WellKnownMember.Microsoft_CSharp_Expressions_CSharpExpression__SwitchCaseDefaultValue);
                        default:
                            // TODO: Other pattern types
                            throw new NotImplementedException("TODO");
                    }
                });

                // TODO: section.Locals

                var testValues = _bound.Array(_objectType, switchLabels);

                var body = VisitStatements(section.Statements);

                var @case = CSharpStmtFactory("SwitchCase", testValues, body);
                caseList.Add(@case);
            }

            var cases = _bound.Array(CSharpSwitchCaseType, caseList.ToImmutableAndFree());

            CurrentLambdaInfo.PopSwitchDefaultLabel();
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
            var locals = PushLocals(node.Locals);

            var condition = Visit(node.Condition);

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            PopLocals(node.Locals);

            var variables = _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());

            return CSharpStmtFactory("Do", body, condition, loopInfo.BreakLabel, loopInfo.ContinueLabel, variables);
        }

        private BoundExpression VisitFor(BoundForStatement node)
        {
            var outerLocals = PushLocals(node.OuterLocals);

            var initializers = VisitStatements(node.Initializer).ToImmutableArray();
            
            var innerLocals = PushLocals(node.InnerLocals);

            var condition = Visit(node.Condition);
            var increments = VisitStatements(node.Increment).ToImmutableArray();

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            PopLocals(node.InnerLocals);

            PopLocals(node.OuterLocals);

            var variables = _bound.Array(ParameterExpressionType, outerLocals.ToImmutableAndFree());
            var locals = _bound.Array(ParameterExpressionType, innerLocals.ToImmutableAndFree());
            var initializer = _bound.Array(ExpressionType, initializers);
            var increment = _bound.Array(ExpressionType, increments);

            return CSharpStmtFactory("For", variables, initializer, condition ?? _bound.Null(ExpressionType), increment, body, loopInfo.BreakLabel, loopInfo.ContinueLabel, locals);
        }

        private BoundExpression VisitForEach(BoundForEachStatement node)
        {
            var expression = Visit(node.Expression);

            var locals = PushLocals(node.IterationVariables);

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var awaitInfo = getAwaitInfo();
            var conversion = getConversionLambda();
            var deconstruction = getDeconstructionLambda();
            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            var variables = _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());
            PopLocals(node.IterationVariables);

            // TODO: node.EnumeratorInfoOpt - add overloads that take in MethodInfo for GetEnumerator etc?

            if (awaitInfo is not null)
            {
                return CSharpStmtFactory("AwaitForEach", variables, expression, body, loopInfo.BreakLabel, loopInfo.ContinueLabel, conversion, deconstruction, awaitInfo);
            }
            else
            {
                return CSharpStmtFactory("ForEach", variables, expression, body, loopInfo.BreakLabel, loopInfo.ContinueLabel, conversion, deconstruction);
            }

            BoundExpression getConversionLambda()
            {
                var elementConversion = BoundNode.GetConversion(node.ElementConversion, node.ElementPlaceholder);

                if (node.EnumeratorInfoOpt != null && elementConversion.IsUserDefined)
                {
                    TypeSymbol lambdaParamType = node.EnumeratorInfoOpt.ElementType;
                    return MakeConversionLambda(elementConversion, lambdaParamType, node.IterationVariableType.Type);
                }

                return _bound.Null(LambdaExpressionType);
            }

            BoundExpression getDeconstructionLambda()
            {
                if (node.DeconstructionOpt is { DeconstructionAssignment: var assignment, TargetPlaceholder: var placeholder })
                {
                    var inputParameterSymbol = _bound.SynthesizedParameter(placeholder.Type, "t");
                    var inputParameter = _bound.Parameter(inputParameterSymbol);
                    var inputParamExprSymbol = _bound.SynthesizedLocal(ParameterExpressionType);
                    var inputParamExpr = _bound.Local(inputParamExprSymbol);
                    var inputParam = ExprFactory("Parameter", _bound.Typeof(placeholder.Type), _bound.Literal("t"));

                    var replacements = new Dictionary<BoundDeconstructValuePlaceholder, BoundExpression>
                    {
                        { placeholder, inputParameter }
                    };

                    _parameterMap.Add(inputParameterSymbol, inputParamExpr);

                    var rewrittenAssignment = (BoundExpression)new DeconstructValuePlaceholderSubstitutor(replacements).Visit(assignment);
                    var rewrittenAssignmentExpr = Visit(rewrittenAssignment);

                    _parameterMap.Remove(inputParameterSymbol);

                    return _bound.Sequence(
                        ImmutableArray.Create(inputParamExprSymbol),
                        ImmutableArray.Create<BoundExpression>(
                            _bound.AssignmentExpression(inputParamExpr, inputParam)
                        ),
                        ExprFactory(
                            "Lambda",
                            rewrittenAssignmentExpr,
                            _bound.ArrayOrEmpty(ParameterExpressionType, ImmutableArray.Create<BoundExpression>(inputParamExpr))));
                }

                return _bound.Null(LambdaExpressionType);
            }

            BoundExpression? getAwaitInfo()
            {
                if (node.AwaitOpt is not null)
                {
                    return VisitAwaitInfo(node.AwaitOpt);
                }

                return null;
            }
        }

        private IEnumerable<BoundExpression> VisitStatements(BoundStatement? node)
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
            var locals = PushLocals(node.Locals);

            var condition = Visit(node.Condition);

            CurrentLambdaInfo.PushLoop(node.BreakLabel, node.ContinueLabel);

            var body = Visit(node.Body);

            var loopInfo = CurrentLambdaInfo.PopLoop();

            PopLocals(node.Locals);

            var variables = _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());

            return CSharpStmtFactory("While", condition, body, loopInfo.BreakLabel, loopInfo.ContinueLabel, variables);
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
            BoundExpression createPatternDisposeLambda()
            {
                if (node.PatternDisposeInfoOpt is QuotedMethodArgumentInfo { Receiver: var receiver, Call: var call })
                {
                    var inputParamExprSymbol = _bound.SynthesizedLocal(ParameterExpressionType);
                    var inputParamExpr = _bound.Local(inputParamExprSymbol);
                    var inputParam = ExprFactory("Parameter", _bound.Typeof(receiver.Type), _bound.Literal(receiver.ParameterSymbol.Name));

                    _parameterMap.Add(receiver.ParameterSymbol, inputParamExpr);

                    var callExpr = Visit(call);

                    _parameterMap.Remove(receiver.ParameterSymbol);

                    return _bound.Sequence(
                        ImmutableArray.Create(inputParamExprSymbol),
                        ImmutableArray.Create<BoundExpression>(
                            _bound.AssignmentExpression(inputParamExpr, inputParam)
                        ),
                        ExprFactory(
                            "Lambda",
                            callExpr,
                            _bound.ArrayOrEmpty(ParameterExpressionType, ImmutableArray.Create<BoundExpression>(inputParamExpr))));
                }

                return _bound.Null(LambdaExpressionType);
            }

            var locals = PushLocals(node.Locals);

            BoundExpression resource;

            if (node.ExpressionOpt is not null)
            {
                resource = Visit(node.ExpressionOpt);
            }
            else
            {
                Debug.Assert(node.DeclarationsOpt != null); // REVIEW

                resource = VisitMultipleLocalDeclarations(node.DeclarationsOpt);
            }

            var body = Visit(node.Body);

            PopLocals(node.Locals);

            var variables =_bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());

            var patternDispose = createPatternDisposeLambda();

            if (node.AwaitOpt is not null)
            {
                var awaitInfo = VisitAwaitInfo(node.AwaitOpt);

                return CSharpStmtFactory("AwaitUsing", variables, resource, body, awaitInfo, patternDispose);
            }
            else
            {
                return CSharpStmtFactory("Using", variables, resource, body, patternDispose);
            }
        }

        private BoundExpression VisitTry(BoundTryStatement node)
        {
            var tryBlock = Visit(node.TryBlock);

            var catchBlockExprs = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var catchBlock in node.CatchBlocks)
            {
                catchBlockExprs.Add(VisitCatchBlock(catchBlock));
            }

            var catchBlocks = _bound.Array(CSharpCatchBlockType, catchBlockExprs.ToImmutableAndFree());

            var finallyBlock = node.FinallyBlockOpt != null ? Visit(node.FinallyBlockOpt) :_bound.Null(ExpressionType);

            return CSharpStmtFactory("Try", tryBlock, catchBlocks, finallyBlock);
        }

        private BoundExpression VisitCatchBlock(BoundCatchBlock node)
        {
            var args = ArrayBuilder<BoundExpression>.GetInstance();

            var locals = PushLocals(node.Locals);

            var variables = _bound.Array(ParameterExpressionType, locals.ToImmutableAndFree());
            args.Add(variables);

            if (node.ExceptionSourceOpt is BoundExpression source)
            {
                // catch (T e)

                Debug.Assert(source is BoundLocal);

                var local = (BoundLocal)source;
                Debug.Assert(local.LocalSymbol.DeclarationKind == LocalDeclarationKind.CatchVariable);

                // NB: We want the local to come out as ParameterExpression to pick the right overload of Catch.
                args.Add(Visit(local, convertToExpressionType: false));
            }
            else if (node.ExceptionTypeOpt is TypeSymbol type)
            {
                // catch (T)

                type = _typeMap.SubstituteType(type).Type;

                var exceptionType = _bound.Typeof(type);

                args.Add(exceptionType);
            }
            else
            {
                // catch
            }

            args.Add(Visit(node.Body));
            
            if (node.ExceptionFilterOpt is BoundExpression filter)
            {
                args.Add(Visit(filter));
            }

            PopLocals(node.Locals);

            // Catch(IEnumerable<ParameterExpression> variables, ParameterExpression variable, Expression body[, Expression filter])
            // Catch(IEnumerable<ParameterExpression> variables, Type type, Expression body[, Expression filter])
            // Catch(IEnumerable<ParameterExpression> variables, Expression body[, Expression filter])
            
            return CSharpStmtFactory("Catch", args.ToArrayAndFree());
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
            else if (node.Label == CurrentLambdaInfo.ClosestSwitchDefaultLabel)
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
            private readonly Stack<LabelSymbol?> _defaultLabels = new Stack<LabelSymbol?>();
            private readonly Dictionary<LabelSymbol, BoundLocal> _labels = new Dictionary<LabelSymbol, BoundLocal>();
            private readonly Stack<ReceiverInfo> _receivers = new Stack<ReceiverInfo>();

            private BoundLocal? _returnLabelTarget;

            public LambdaCompilationInfo(ExpressionLambdaRewriter parent, ArrayBuilder<LocalSymbol> locals, ArrayBuilder<BoundExpression> initializers)
            {
                _parent = parent;
                _locals = locals;
                _initializers = initializers;
            }

            public BoundLocal EnsureReturnLabel(TypeSymbol? type)
            {
                if (_returnLabelTarget == null)
                {
                    var returnLabelTarget = CreateLabelTargetLocalSymbol();
                    var returnLabelTargetLocal = CreateLabelTargetLocal(returnLabelTarget);
                    _returnLabelTarget = returnLabelTargetLocal;

                    BoundExpression returnLabelTargetCreation;
                    if (type is null || type.SpecialType == SpecialType.System_Void)
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

            public BoundLocal? ReturnLabel => _returnLabelTarget;

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

                var loopInfo = new LoopInfo(breakLabelLocal, continueLabelLocal);

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

                var breakInfo = new BreakInfo(breakLabelLocal);

                _breaks.Push(breakInfo);

                return breakLabelLocal;
            }

            internal BreakInfo PopBreak()
            {
                return _breaks.Pop();
            }

            // TODO: Revamp switch and parameterize it by the label expression (rather than having "GotoDefault")

            internal void PushSwitchDefaultLabel(BoundSwitchLabel? defaultLabel)
            {
                _defaultLabels.Push(defaultLabel?.Label);
            }

            internal void PopSwitchDefaultLabel()
            {
                _defaultLabels.Pop();
            }

            internal BoundExpression ClosestBreak => _breaks.Peek().BreakLabel;
            internal BoundExpression ClosestLoopContinue => _loops.Peek().ContinueLabel;
            internal LabelSymbol? ClosestSwitchDefaultLabel => _defaultLabels.Count > 0 ? _defaultLabels.Peek() : null;

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
                if (!TryGetLabel(label, out var labelLocal))
                {
                    var labelLocalSymbol = CreateLabelTargetLocalSymbol();
                    labelLocal = CreateLabelTargetLocal(labelLocalSymbol);
                    AddLabelInitializer(labelLocal, _parent.CSharpStmtFactory("Label", _parent._bound.Literal(label.Name)));

                    _labels.Add(label, labelLocal);
                }

                return labelLocal;
            }

            internal bool TryGetLabel(LabelSymbol label, [NotNullWhen(true)] out BoundLocal? target)
            {
                return _labels.TryGetValue(label, out target);
            }

            internal void PushConditionalReceiver()
            {
                _receivers.Push(new ReceiverInfo());
            }

            internal BoundLocal BindConditionalReceiver(BoundConditionalReceiver node)
            {
                var symbol = _parent._bound.SynthesizedLocal(_parent.ConditionalReceiverType);
                _locals.Add(symbol);

                var receiverLocal = _parent._bound.Local(symbol);
                _receivers.Peek().Local = receiverLocal;

                var receiverType = _parent._bound.Typeof(_parent._typeMap.SubstituteType(node.Type).Type);
                var receiverCreation = _parent.CSharpStmtFactory("ConditionalReceiver", receiverType);

                _initializers.Add(_parent._bound.AssignmentExpression(receiverLocal, receiverCreation));

                return receiverLocal;
            }

            internal BoundLocal PopConditionalReceiver()
            {
                return _receivers.Pop().Local;
            }
        }

        record LoopInfo(BoundExpression BreakLabel, BoundExpression ContinueLabel);

        record BreakInfo(BoundExpression BreakLabel);

        class ReceiverInfo
        {
            public BoundLocal Local
            {
                get => _local ?? throw new InvalidOperationException(); // TODO

                set
                {
                    if (_local != null)
                        throw new InvalidOperationException(); // TODO

                    _local = value;
                }
            }

            private BoundLocal? _local;
        }
    }
}
