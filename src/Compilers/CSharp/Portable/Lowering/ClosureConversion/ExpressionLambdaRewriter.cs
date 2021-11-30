// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal partial class ExpressionLambdaRewriter // this is like a bound tree rewriter, but only handles a small subset of node kinds
    {
        private readonly TypeCompilationState _compilationState;
        private readonly SyntheticBoundNodeFactory _bound;
        private readonly TypeMap _typeMap;
        private readonly Dictionary<ParameterSymbol, BoundExpression> _parameterMap = new Dictionary<ParameterSymbol, BoundExpression>();
        private readonly Dictionary<BoundAwaitableValuePlaceholder, BoundExpression> _awaitableValuePlaceholderMap = new Dictionary<BoundAwaitableValuePlaceholder, BoundExpression>();
        private readonly bool _ignoreAccessibility;
        private readonly Stack<LambdaCompilationInfo> _lambdas = new Stack<LambdaCompilationInfo>();
        private int _recursionDepth;

        private NamedTypeSymbol _ExpressionType;
        private NamedTypeSymbol ExpressionType
        {
            get
            {
                if ((object)_ExpressionType == null)
                {
                    _ExpressionType = _bound.WellKnownType(WellKnownType.System_Linq_Expressions_Expression);
                }
                return _ExpressionType;
            }
        }

        private NamedTypeSymbol _ExpressionTypeType;
        private NamedTypeSymbol ExpressionTypeType => _ExpressionTypeType ??= _bound.WellKnownType(WellKnownType.System_Linq_Expressions_ExpressionType);

        private NamedTypeSymbol _LambdaExpressionType;
        private NamedTypeSymbol LambdaExpressionTypeType => _LambdaExpressionType ??= _bound.WellKnownType(WellKnownType.System_Linq_Expressions_LambdaExpression);

        private NamedTypeSymbol _CSharpExpressionType;
        private NamedTypeSymbol CSharpExpressionType => _CSharpExpressionType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpExpression);

        private NamedTypeSymbol _CSharpParameterAssignmentType;
        private NamedTypeSymbol CSharpParameterAssignmentType => _CSharpParameterAssignmentType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_ParameterAssignment);

        private NamedTypeSymbol _DynamicCSharpExpressionType;
        private NamedTypeSymbol DynamicCSharpExpressionType => _DynamicCSharpExpressionType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_DynamicCSharpExpression);

        private NamedTypeSymbol _DynamicCSharpArgumentType;
        private NamedTypeSymbol DynamicCSharpArgumentType => _DynamicCSharpArgumentType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_DynamicCSharpArgument);

        private NamedTypeSymbol _CSharpArgumentInfoFlagsType;
        private NamedTypeSymbol CSharpArgumentInfoFlagsType => _CSharpArgumentInfoFlagsType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_RuntimeBinder_CSharpArgumentInfoFlags);

        private NamedTypeSymbol _CSharpBinderFlagsType;
        private NamedTypeSymbol CSharpBinderFlagsType => _CSharpBinderFlagsType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_RuntimeBinder_CSharpBinderFlags);

        private NamedTypeSymbol _CSharp_Expressions_InterpolationType;
        private NamedTypeSymbol CSharp_Expressions_InterpolationType
        {
            get
            {
                if ((object)_CSharp_Expressions_InterpolationType == null)
                {
                    _CSharp_Expressions_InterpolationType = _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_Interpolation);
                }
                return _CSharp_Expressions_InterpolationType;
            }
        }

        private NamedTypeSymbol _CSharp_Expressions_ConversionType;
        private NamedTypeSymbol CSharp_Expressions_ConversionType => _CSharp_Expressions_ConversionType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_Conversion);

        private NamedTypeSymbol _ParameterExpressionType;
        private NamedTypeSymbol ParameterExpressionType
        {
            get
            {
                if ((object)_ParameterExpressionType == null)
                {
                    _ParameterExpressionType = _bound.WellKnownType(WellKnownType.System_Linq_Expressions_ParameterExpression);
                }
                return _ParameterExpressionType;
            }
        }

        private NamedTypeSymbol _ElementInitType;
        private NamedTypeSymbol ElementInitType
        {
            get
            {
                if ((object)_ElementInitType == null)
                {
                    _ElementInitType = _bound.WellKnownType(WellKnownType.System_Linq_Expressions_ElementInit);
                }
                return _ElementInitType;
            }
        }

        private NamedTypeSymbol _MemberBindingType;

        public NamedTypeSymbol MemberBindingType
        {
            get
            {
                if ((object)_MemberBindingType == null)
                {
                    _MemberBindingType = _bound.WellKnownType(WellKnownType.System_Linq_Expressions_MemberBinding);
                }
                return _MemberBindingType;
            }
        }

        private readonly NamedTypeSymbol _int32Type;

        private readonly NamedTypeSymbol _stringType;

        private readonly NamedTypeSymbol _objectType;

        private readonly NamedTypeSymbol _nullableType;

        private NamedTypeSymbol _MemberInfoType;
        private NamedTypeSymbol MemberInfoType
        {
            get
            {
                if ((object)_MemberInfoType == null)
                {
                    _MemberInfoType = _bound.WellKnownType(WellKnownType.System_Reflection_MemberInfo);
                }
                return _MemberInfoType;
            }
        }

        private readonly NamedTypeSymbol _IEnumerableType;

        private BindingDiagnosticBag Diagnostics { get { return _bound.Diagnostics; } }

        private ExpressionLambdaRewriter(TypeCompilationState compilationState, TypeMap typeMap, SyntaxNode node, int recursionDepth, BindingDiagnosticBag diagnostics)
        {
            _compilationState = compilationState;
            _bound = new SyntheticBoundNodeFactory(null, compilationState.Type, node, compilationState, diagnostics);
            _ignoreAccessibility = compilationState.ModuleBuilderOpt.IgnoreAccessibility;
            _int32Type = _bound.SpecialType(SpecialType.System_Int32);
            _stringType = _bound.SpecialType(SpecialType.System_String);
            _objectType = _bound.SpecialType(SpecialType.System_Object);
            _nullableType = _bound.SpecialType(SpecialType.System_Nullable_T);
            _IEnumerableType = _bound.SpecialType(SpecialType.System_Collections_Generic_IEnumerable_T);

            _typeMap = typeMap;
            _recursionDepth = recursionDepth;
        }

        internal static BoundNode RewriteLambda(BoundLambda node, TypeCompilationState compilationState, TypeMap typeMap, int recursionDepth, BindingDiagnosticBag diagnostics)
        {
            try
            {
                var r = new ExpressionLambdaRewriter(compilationState, typeMap, node.Syntax, recursionDepth, diagnostics);
                var result = r.VisitLambdaInternal(node);
                if (!node.Type.Equals(result.Type, TypeCompareKind.IgnoreNullableModifiersForReferenceTypes))
                {
                    diagnostics.Add(ErrorCode.ERR_MissingPredefinedMember, node.Syntax.Location, r.ExpressionType, "Lambda");
                }
                return result;
            }
            catch (SyntheticBoundNodeFactory.MissingPredefinedMember ex)
            {
                diagnostics.Add(ex.Diagnostic);
                return node;
            }
        }

        private BoundExpression TranslateLambdaBodyCore(BoundNode node, ArrayBuilder<LocalSymbol> locals, ArrayBuilder<BoundExpression> initializers)
        {
            var info = new LambdaCompilationInfo(this, locals, initializers);
            _lambdas.Push(info);

            try
            {
                if (node is BoundBlock block)
                {
                    return VisitLambdaBody(block);
                }
                else
                {
                    return Visit((BoundExpression)node);
                }
            }
            finally
            {
                _lambdas.Pop();
            }
        }

        private BoundExpression TranslateLambdaBody(BoundBlock block, ArrayBuilder<LocalSymbol> locals, ArrayBuilder<BoundExpression> initializers)
        {
            // NB: Compat with prior versions of C# where we only supported expression-bodied lambdas.
            if (block.Locals.IsEmpty)
            {
                var expressionLambda = true;

                var stmts = block.Statements;
                for (var i = 0; i < stmts.Length; i++)
                {
                    for (var stmt = stmts[i]; stmt != null;)
                    {
                        switch (stmt.Kind)
                        {
                            case BoundKind.ReturnStatement:
                                if (EnsureLastStatement(stmts, i))
                                {
                                    var result = TranslateLambdaBodyCore(((BoundReturnStatement)stmt).ExpressionOpt, locals, initializers);
                                    if (result != null)
                                    {
                                        return result;
                                    }
                                }
                                else
                                {
                                    goto default;
                                }
                                stmt = null;
                                break;
                            case BoundKind.ExpressionStatement:
                                if (EnsureLastStatement(stmts, i))
                                {
                                    return VisitExpressionStatement((BoundExpressionStatement)stmt);
                                }
                                goto default;
                            case BoundKind.SequencePoint:
                                stmt = ((BoundSequencePoint)stmt).StatementOpt;
                                break;
                            case BoundKind.SequencePointWithSpan:
                                stmt = ((BoundSequencePointWithSpan)stmt).StatementOpt;
                                break;
                            default:
                                expressionLambda = false;
                                stmt = null;
                                break;
                        }
                    }

                    if (!expressionLambda)
                    {
                        break;
                    }
                }
            }

            var res = TranslateLambdaBodyCore(block, locals, initializers);

            return res;
        }

        private bool EnsureLastStatement(ImmutableArray<BoundStatement> statements, int index)
        {
            for (var i = index + 1; i < statements.Length; i++)
            {
                var stmt = statements[i];

                switch (stmt.Kind)
                {
                    case BoundKind.SequencePoint:
                        if (((BoundSequencePoint)stmt).StatementOpt != null)
                        {
                            return false;
                        }
                        break;
                    case BoundKind.SequencePointWithSpan:
                        if (((BoundSequencePointWithSpan)stmt).StatementOpt != null)
                        {
                            return false;
                        }
                        break;
                    case BoundKind.ReturnStatement:
                        if (((BoundReturnStatement)stmt).ExpressionOpt != null)
                        {
                            return false;
                        }
                        break;
                    default:
                        return false;
                }
            }

            return true;
        }

        private BoundExpression Visit(BoundExpression node, bool convertToExpressionType = true)
        {
            if (node == null)
            {
                return null;
            }

            SyntaxNode old = _bound.Syntax;
            _bound.Syntax = node.Syntax;
            var result = VisitInternal(node);
            _bound.Syntax = old;

            if (result.HasAnyErrors)
            {
                // Handle the case where a factory method is missing.
                return result;
            }

            return convertToExpressionType ? _bound.Convert(ExpressionType, result) : result;
        }

        private BoundExpression VisitExpressionWithoutStackGuard(BoundExpression node)
        {
            switch (node.Kind)
            {
                case BoundKind.ArrayAccess:
                    return VisitArrayAccess((BoundArrayAccess)node);
                case BoundKind.ArrayCreation:
                    return VisitArrayCreation((BoundArrayCreation)node);
                case BoundKind.ArrayLength:
                    return VisitArrayLength((BoundArrayLength)node);
                case BoundKind.AsOperator:
                    return VisitAsOperator((BoundAsOperator)node);
                case BoundKind.AwaitExpression:
                    return VisitAwaitExpression((BoundAwaitExpression)node);
                case BoundKind.AwaitableValuePlaceholder:
                    return VisitAwaitableValuePlaceholder((BoundAwaitableValuePlaceholder)node);
                case BoundKind.BaseReference:
                    return VisitBaseReference((BoundBaseReference)node);
                case BoundKind.BinaryOperator:
                    var binOp = (BoundBinaryOperator)node;
                    return VisitBinaryOperator(binOp.OperatorKind, binOp.Method, binOp.Type, binOp.Left, binOp.Right);
                case BoundKind.UserDefinedConditionalLogicalOperator:
                    var userDefCondLogOp = (BoundUserDefinedConditionalLogicalOperator)node;
                    return VisitBinaryOperator(userDefCondLogOp.OperatorKind, userDefCondLogOp.LogicalOperator, userDefCondLogOp.Type, userDefCondLogOp.Left, userDefCondLogOp.Right);
                case BoundKind.Call:
                    return VisitCall((BoundCall)node);
                case BoundKind.ConditionalAccess:
                    return VisitConditionalAccess((BoundConditionalAccess)node);
                case BoundKind.ConditionalReceiver:
                    return VisitConditionalReceiver((BoundConditionalReceiver)node);
                case BoundKind.ConditionalOperator:
                    return VisitConditionalOperator((BoundConditionalOperator)node);
                case BoundKind.Conversion:
                    return VisitConversion((BoundConversion)node);
                case BoundKind.PassByCopy:
                    return Visit(((BoundPassByCopy)node).Expression);
                case BoundKind.DelegateCreationExpression:
                    return VisitDelegateCreationExpression((BoundDelegateCreationExpression)node);
                case BoundKind.FieldAccess:
                    var fieldAccess = (BoundFieldAccess)node;
                    if (fieldAccess.FieldSymbol.IsCapturedFrame)
                    {
                        return Constant(fieldAccess);
                    }
                    return VisitFieldAccess(fieldAccess);
                case BoundKind.IndexerAccess:
                    return VisitIndexerAccess((BoundIndexerAccess)node);
                case BoundKind.IsOperator:
                    return VisitIsOperator((BoundIsOperator)node);
                case BoundKind.Lambda:
                    return VisitLambda((BoundLambda)node);
                case BoundKind.NewT:
                    return VisitNewT((BoundNewT)node);
                case BoundKind.NullCoalescingOperator:
                    return VisitNullCoalescingOperator((BoundNullCoalescingOperator)node);
                case BoundKind.NullCoalescingAssignmentOperator:
                    return VisitNullCoalescingAssignmentOperator((BoundNullCoalescingAssignmentOperator)node);
                case BoundKind.ObjectCreationExpression:
                    return VisitObjectCreationExpression((BoundObjectCreationExpression)node);
                case BoundKind.Parameter:
                    return VisitParameter((BoundParameter)node);
                case BoundKind.PointerIndirectionOperator:
                    return VisitPointerIndirectionOperator((BoundPointerIndirectionOperator)node);
                case BoundKind.PointerElementAccess:
                    return VisitPointerElementAccess((BoundPointerElementAccess)node);
                case BoundKind.PropertyAccess:
                    return VisitPropertyAccess((BoundPropertyAccess)node);
                case BoundKind.QuotedDynamicMemberAccess:
                    return VisitDynamicMemberAccess((BoundQuotedDynamicMemberAccess)node);
                case BoundKind.QuotedDynamicIndexAccess:
                    return VisitDynamicIndexAccess((BoundQuotedDynamicIndexAccess)node);
                case BoundKind.QuotedDynamicInvocation:
                    return VisitDynamicInvoke((BoundQuotedDynamicInvocation)node);
                case BoundKind.QuotedDynamicNew:
                    return VisitDynamicNew((BoundQuotedDynamicNew)node);
                case BoundKind.QuotedDynamicCall:
                    return VisitDynamicCall((BoundQuotedDynamicCall)node);
                case BoundKind.QuotedDynamicUnary:
                    return VisitDynamicUnary((BoundQuotedDynamicUnary)node);
                case BoundKind.QuotedDynamicBinary:
                    return VisitDynamicBinary((BoundQuotedDynamicBinary)node);
                case BoundKind.QuotedDynamicConvert:
                    return VisitDynamicConvert((BoundQuotedDynamicConvert)node);
                case BoundKind.SizeOfOperator:
                    return VisitSizeOfOperator((BoundSizeOfOperator)node);
                case BoundKind.UnaryOperator:
                    return VisitUnaryOperator((BoundUnaryOperator)node);

                case BoundKind.DefaultExpression:
                case BoundKind.HostObjectMemberReference:
                case BoundKind.Literal:
                case BoundKind.MethodInfo:
                case BoundKind.PreviousSubmissionReference:
                case BoundKind.ThisReference:
                case BoundKind.TypeOfOperator:
                    return Constant(node);

                case BoundKind.Local:
                    return VisitLocal((BoundLocal)node);

                case BoundKind.AssignmentOperator:
                    return VisitAssignmentOperator((BoundAssignmentOperator)node);
                case BoundKind.CompoundAssignmentOperator:
                    return VisitCompoundAssignmentOperator((BoundCompoundAssignmentOperator)node);
                case BoundKind.IncrementOperator:
                    return VisitIncrementOperator((BoundIncrementOperator)node);

                case BoundKind.SequencePointExpression:
                    return Visit(((BoundSequencePointExpression)node).Expression);

                case BoundKind.ThrowExpression:
                    return VisitThrowExpression((BoundThrowExpression)node);

                case BoundKind.DiscardExpression:
                    return VisitDiscardExpression((BoundDiscardExpression)node);

                case BoundKind.InterpolatedString:
                    return VisitInterpolatedString((BoundInterpolatedString)node);

                case BoundKind.FromEndIndexExpression:
                    return VisitFromEndIndex((BoundFromEndIndexExpression)node);
                case BoundKind.RangeExpression:
                    return VisitRange((BoundRangeExpression)node);

                // TODO: Revisit due to changes made in https://github.com/dotnet/roslyn/pull/57318.
                //case BoundKind.IndexOrRangePatternIndexerAccess:
                //    return VisitIndexOrRangePatternIndexerAccess((BoundIndexOrRangePatternIndexerAccess)node);

                case BoundKind.TupleLiteral:
                    return VisitTupleLiteral((BoundTupleLiteral)node);
                case BoundKind.ConvertedTupleLiteral:
                    return VisitConvertedTupleLiteral((BoundConvertedTupleLiteral)node);
                case BoundKind.TupleBinaryOperator:
                    return VisitTupleBinaryOperator((BoundTupleBinaryOperator)node);

                case BoundKind.DeconstructionAssignmentOperator:
                    return VisitDeconstructionAssignmentOperator((BoundDeconstructionAssignmentOperator)node);

                default:
                    throw ExceptionUtilities.UnexpectedValue(node.Kind);
            }
        }

        private BoundExpression VisitInternal(BoundExpression node)
        {
            BoundExpression result;
            _recursionDepth++;
#if DEBUG
            int saveRecursionDepth = _recursionDepth;
#endif

            if (_recursionDepth > 1)
            {
                StackGuard.EnsureSufficientExecutionStack(_recursionDepth);

                result = VisitExpressionWithoutStackGuard(node);
            }
            else
            {
                result = VisitExpressionWithStackGuard(node);
            }

#if DEBUG
            Debug.Assert(saveRecursionDepth == _recursionDepth);
#endif
            _recursionDepth--;
            return result;
        }

        private BoundExpression VisitExpressionWithStackGuard(BoundExpression node)
        {
            try
            {
                return VisitExpressionWithoutStackGuard(node);
            }
            catch (InsufficientExecutionStackException ex)
            {
                throw new BoundTreeVisitor.CancelledByStackGuardException(ex, node);
            }
        }

        private BoundExpression VisitArrayAccess(BoundArrayAccess node)
        {
            return VisitArrayAccess(Visit(node.Expression), node);
        }

        private BoundExpression VisitArrayAccess(BoundExpression receiverOpt, BoundArrayAccess node)
        {
            var array = receiverOpt;
            if (node.Indices.Length == 1)
            {
                var arg = node.Indices[0];
                var index = Visit(arg);

                if (TypeSymbol.Equals(arg.Type, _bound.WellKnownType(WellKnownType.System_Index), TypeCompareKind.ConsiderEverything2) ||
                    TypeSymbol.Equals(arg.Type, _bound.WellKnownType(WellKnownType.System_Range), TypeCompareKind.ConsiderEverything2))
                {
                    return CSharpExprFactory("ArrayAccess", array, index);
                }
                else
                {
                    // REVIEW: It seems the following type check can never be false, because `index` is of type `Expression` (unlike `arg`).
                    //         See commit https://github.com/dotnet/roslyn/commit/7b81dc50a709a3fff6177561917b04cd0bcf5e8f where this was tweaked.

                    if (!TypeSymbol.Equals(index.Type, _int32Type, TypeCompareKind.ConsiderEverything2))
                    {
                        index = ConvertIndex(index, arg.Type, _int32Type);
                    }

                    return ExprFactory("ArrayIndex", array, index);
                }
            }
            else
            {
                return ExprFactory("ArrayIndex", array, Indices(node.Indices));
            }
        }

        private BoundExpression Indices(ImmutableArray<BoundExpression> expressions)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var arg in expressions)
            {
                var index = Visit(arg);

                // REVIEW: It seems the following type check can never be false, because `index` is of type `Expression` (unlike `arg`).
                //         See commit https://github.com/dotnet/roslyn/commit/7b81dc50a709a3fff6177561917b04cd0bcf5e8f where this was tweaked.

                if (!TypeSymbol.Equals(index.Type, _int32Type, TypeCompareKind.ConsiderEverything2))
                {
                    index = ConvertIndex(index, arg.Type, _int32Type);
                }

                builder.Add(index);
            }

            return _bound.ArrayOrEmpty(ExpressionType, builder.ToImmutableAndFree());
        }

        private BoundExpression Expressions(ImmutableArray<BoundExpression> expressions)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var arg in expressions)
            {
                builder.Add(Visit(arg));
            }

            return _bound.ArrayOrEmpty(ExpressionType, builder.ToImmutableAndFree());
        }

        private BoundExpression RowMajorExpressions(ImmutableArray<BoundExpression> expressions)
        {
            var queue = new Queue<BoundExpression>(expressions.Length);

            foreach (var arg in expressions)
            {
                queue.Enqueue(arg);
            }

            var builder = ArrayBuilder<BoundExpression>.GetInstance();

            while (queue.Count > 0)
            {
                var arg = queue.Dequeue();

                if (arg is BoundArrayInitialization init)
                {
                    foreach (var elem in init.Initializers)
                    {
                        queue.Enqueue(elem);
                    }
                }
                else
                {
                    builder.Add(Visit(arg));
                }
            }

            return _bound.Array(ExpressionType, builder.ToImmutableAndFree());
        }

        private BoundExpression VisitArrayCreation(BoundArrayCreation node)
        {
            var arrayType = (ArrayTypeSymbol)node.Type;
            var boundType = _bound.Typeof(arrayType.ElementType);
            if (node.InitializerOpt != null)
            {
                if (arrayType.IsSZArray)
                {
                    return ExprFactory("NewArrayInit", boundType, Expressions(node.InitializerOpt.Initializers));
                }
                else
                {
                    var bounds = _bound.Array(_int32Type, node.Bounds);
                    var elements = RowMajorExpressions(node.InitializerOpt.Initializers);
                    return CSharpExprFactory("NewMultidimensionalArrayInit", boundType, bounds, elements);
                }
            }
            else
            {
                return ExprFactory("NewArrayBounds", boundType, Expressions(node.Bounds));
            }
        }

        private BoundExpression VisitArrayLength(BoundArrayLength node)
        {
            return ExprFactory("ArrayLength", Visit(node.Expression));
        }

        private BoundExpression VisitAsOperator(BoundAsOperator node)
        {
            if (node.Operand.IsLiteralNull() && (object)node.Operand.Type == null)
            {
                var operand = _bound.Null(_bound.SpecialType(SpecialType.System_Object));
                Debug.Assert(node.OperandPlaceholder is null);
                Debug.Assert(node.OperandConversion is null);
                node = node.Update(operand, node.TargetType, node.OperandPlaceholder, node.OperandConversion, node.Type);
            }

            return ExprFactory("TypeAs", Visit(node.Operand), _bound.Typeof(node.Type));
        }

        private BoundExpression VisitAwaitExpression(BoundAwaitExpression node, bool resultDiscarded = false)
        {
            BoundExpression info;

            if (node.AwaitableInfo.IsDynamic)
            {
                var ctx = _bound.TypeofDynamicOperationContextType();

                info = DynamicCSharpExprFactory("DynamicAwaitInfo", ctx, _bound.Literal(resultDiscarded));
            }
            else
            {
                var getAwaiter = MakeGetAwaiterLambda(node.AwaitableInfo);
                var getIsCompleted = _bound.MethodInfo(node.AwaitableInfo.IsCompleted.GetOwnOrInheritedGetMethod());
                var getResult = _bound.MethodInfo(node.AwaitableInfo.GetResult);

                info = CSharpExprFactory("AwaitInfo", getAwaiter, getIsCompleted, getResult);
            }

            return CSharpExprFactory("Await", Visit(node.Expression), info);
        }

        private BoundExpression VisitAwaitableValuePlaceholder(BoundAwaitableValuePlaceholder node)
        {
            return _awaitableValuePlaceholderMap[node];
        }

        private BoundExpression MakeGetAwaiterLambda(BoundAwaitableInfo info)
        {
            var awaitableType = info.AwaitableInstancePlaceholder.Type;
            string parameterName = "p";
            ParameterSymbol lambdaParameter = _bound.SynthesizedParameter(awaitableType, parameterName);
            var param = _bound.SynthesizedLocal(ParameterExpressionType);
            var parameterReference = _bound.Local(param);
            var parameter = ExprFactory("Parameter", _bound.Typeof(awaitableType), _bound.Literal(parameterName));
            _awaitableValuePlaceholderMap[info.AwaitableInstancePlaceholder] = parameterReference;
            var getAwaiter = Visit(info.GetAwaiter);
            _awaitableValuePlaceholderMap.Remove(info.AwaitableInstancePlaceholder);
            var result = _bound.Sequence(
                ImmutableArray.Create(param),
                ImmutableArray.Create<BoundExpression>(_bound.AssignmentExpression(parameterReference, parameter)),
                ExprFactory(
                    "Lambda",
                    getAwaiter,
                    _bound.ArrayOrEmpty(ParameterExpressionType, ImmutableArray.Create<BoundExpression>(parameterReference))));
            return result;
        }

        private BoundExpression VisitBaseReference(BoundBaseReference node)
        {
            // should have been reported earlier.
            // Diagnostics.Add(ErrorCode.ERR_ExpressionTreeContainsBaseAccess, node.Syntax.Location);
            return new BoundBadExpression(node.Syntax, 0, ImmutableArray<Symbol>.Empty, ImmutableArray.Create<BoundExpression>(node), ExpressionType);
        }

        private static string GetBinaryOperatorName(BinaryOperatorKind opKind, MethodSymbol methodOpt, out bool isChecked, out bool isLifted, out bool requiresLifted)
        {
            isChecked = opKind.IsChecked();
            isLifted = opKind.IsLifted();
            requiresLifted = opKind.IsComparison();

            switch (opKind.Operator())
            {
                case BinaryOperatorKind.Addition: return useCheckedFactory(isChecked, methodOpt) ? "AddChecked" : "Add";
                case BinaryOperatorKind.Multiplication: return useCheckedFactory(isChecked, methodOpt) ? "MultiplyChecked" : "Multiply";
                case BinaryOperatorKind.Subtraction: return useCheckedFactory(isChecked, methodOpt) ? "SubtractChecked" : "Subtract";
                case BinaryOperatorKind.Division: return "Divide";
                case BinaryOperatorKind.Remainder: return "Modulo";
                case BinaryOperatorKind.And: return opKind.IsLogical() ? "AndAlso" : "And";
                case BinaryOperatorKind.Xor: return "ExclusiveOr";
                case BinaryOperatorKind.Or: return opKind.IsLogical() ? "OrElse" : "Or";
                case BinaryOperatorKind.LeftShift: return "LeftShift";
                case BinaryOperatorKind.RightShift: return "RightShift";
                case BinaryOperatorKind.Equal: return "Equal";
                case BinaryOperatorKind.NotEqual: return "NotEqual";
                case BinaryOperatorKind.LessThan: return "LessThan";
                case BinaryOperatorKind.LessThanOrEqual: return "LessThanOrEqual";
                case BinaryOperatorKind.GreaterThan: return "GreaterThan";
                case BinaryOperatorKind.GreaterThanOrEqual: return "GreaterThanOrEqual";
                default:
                    throw ExceptionUtilities.UnexpectedValue(opKind.Operator());
            }

            static bool useCheckedFactory(bool isChecked, MethodSymbol methodOpt)
            {
                return isChecked || (methodOpt is { Name: string name } && SyntaxFacts.IsCheckedOperator(name));
            }
        }

        private BoundExpression VisitBinaryOperator(BinaryOperatorKind opKind, MethodSymbol methodOpt, TypeSymbol type, BoundExpression left, BoundExpression right)
        {
            bool isChecked, isLifted, requiresLifted;
            string opName = GetBinaryOperatorName(opKind, methodOpt, out isChecked, out isLifted, out requiresLifted);

            // Fix up the null value for a nullable comparison vs null
            if ((object)left.Type == null && left.IsLiteralNull())
            {
                left = _bound.Default(right.Type);
            }
            if ((object)right.Type == null && right.IsLiteralNull())
            {
                right = _bound.Default(left.Type);
            }


            // Enums are handled as per their promoted underlying type
            switch (opKind.OperandTypes())
            {
                case BinaryOperatorKind.EnumAndUnderlying:
                case BinaryOperatorKind.UnderlyingAndEnum:
                case BinaryOperatorKind.Enum:
                    {
                        var enumOperand = (opKind.OperandTypes() == BinaryOperatorKind.UnderlyingAndEnum) ? right : left;
                        var promotedType = PromotedType(enumOperand.Type.StrippedType().GetEnumUnderlyingType());
                        if (opKind.IsLifted())
                        {
                            promotedType = _nullableType.Construct(promotedType);
                        }

                        var loweredLeft = VisitAndPromoteEnumOperand(left, promotedType, isChecked);
                        var loweredRight = VisitAndPromoteEnumOperand(right, promotedType, isChecked);

                        var result = MakeBinary(methodOpt, type, isLifted, requiresLifted, opName, loweredLeft, loweredRight);
                        return Demote(result, type, isChecked);
                    }
                default:
                    {
                        var loweredLeft = Visit(left);
                        var loweredRight = Visit(right);
                        return MakeBinary(methodOpt, type, isLifted, requiresLifted, opName, loweredLeft, loweredRight);
                    }
            }
        }

        private static BoundExpression DemoteEnumOperand(BoundExpression operand)
        {
            if (operand.Kind == BoundKind.Conversion)
            {
                var conversion = (BoundConversion)operand;
                if (!conversion.ConversionKind.IsUserDefinedConversion() &&
                    conversion.ConversionKind.IsImplicitConversion() &&
                    conversion.ConversionKind != ConversionKind.NullLiteral &&
                    conversion.Type.StrippedType().IsEnumType())
                {
                    operand = conversion.Operand;
                }
            }

            return operand;
        }

        private BoundExpression VisitAndPromoteEnumOperand(BoundExpression operand, TypeSymbol promotedType, bool isChecked)
        {
            var literal = operand as BoundLiteral;
            if (literal != null)
            {
                // for compat reasons enum literals are directly promoted into underlying values
                return Constant(literal.Update(literal.ConstantValue, promotedType));
            }
            else
            {
                // COMPAT: if we have an operand converted to enum, we should unconvert it first
                //         Otherwise we will have an extra conversion in the tree: op -> enum -> underlying
                //         where native compiler would just directly convert to underlying
                var demotedOperand = DemoteEnumOperand(operand);
                var loweredOperand = Visit(demotedOperand);
                return Convert(loweredOperand, operand.Type, promotedType, isChecked, false);
            }
        }

        private BoundExpression MakeBinary(MethodSymbol methodOpt, TypeSymbol type, bool isLifted, bool requiresLifted, string opName, BoundExpression loweredLeft, BoundExpression loweredRight)
        {
            return
                ((object)methodOpt == null) ? ExprFactory(opName, loweredLeft, loweredRight) :
                    requiresLifted ? ExprFactory(opName, loweredLeft, loweredRight, _bound.Literal(isLifted && !TypeSymbol.Equals(methodOpt.ReturnType, type, TypeCompareKind.ConsiderEverything2)), _bound.MethodInfo(methodOpt)) :
                        ExprFactory(opName, loweredLeft, loweredRight, _bound.MethodInfo(methodOpt));
        }

        private TypeSymbol PromotedType(TypeSymbol underlying)
        {
            if (underlying.SpecialType == SpecialType.System_Boolean)
            {
                return underlying;
            }

            var possiblePromote = Binder.GetEnumPromotedType(underlying.SpecialType);

            if (possiblePromote == underlying.SpecialType)
            {
                return underlying;
            }
            else
            {
                return _bound.SpecialType(possiblePromote);
            }
        }

        private BoundExpression Demote(BoundExpression node, TypeSymbol type, bool isChecked)
        {
            var e = type as NamedTypeSymbol;
            if ((object)e != null)
            {
                if (e.StrippedType().TypeKind == TypeKind.Enum)
                {
                    return Convert(node, type, isChecked);
                }

                var promotedType = e.IsNullableType() ? _nullableType.Construct(PromotedType(e.GetNullableUnderlyingType())) : PromotedType(e);
                if (!TypeSymbol.Equals(promotedType, type, TypeCompareKind.ConsiderEverything2))
                {
                    return Convert(node, type, isChecked);
                }
            }

            return node;
        }

        private BoundExpression ConvertIndex(BoundExpression expr, TypeSymbol oldType, TypeSymbol newType)
        {
            var useSiteInfo = new CompoundUseSiteInfo<AssemblySymbol>(Diagnostics, _bound.Compilation.Assembly);
            var kind = _bound.Compilation.Conversions.ClassifyConversionFromType(oldType, newType, isChecked: false, ref useSiteInfo).Kind;
            Debug.Assert(useSiteInfo.Diagnostics.IsNullOrEmpty());
            Diagnostics.AddDependencies(useSiteInfo);

            switch (kind)
            {
                case ConversionKind.Identity:
                    return expr;
                case ConversionKind.ExplicitNumeric:
                    return Convert(expr, newType, true);
                default:
                    return Convert(expr, _int32Type, false);
            }
        }

        private BoundExpression VisitCall(BoundCall node)
        {
            var receiver = node.Method.IsStatic ? null : Visit(node.ReceiverOpt);
            return VisitCall(receiver, node);
        }

        private BoundExpression VisitCall(BoundExpression receiverOpt, BoundCall node)
        {
            if (node.IsDelegateCall)
            {
                if (HasNamedOrOptionalParameters(node.ArgumentNamesOpt, node.Method, node.Arguments))
                {
                    var method = node.Method;
                    return CSharpExprFactory(
                        "Invoke",
                        receiverOpt,
                        ParameterBindings(node.Arguments, method, node.ArgsToParamsOpt));
                }
                else
                {
                    var args = Expressions(node.Arguments);

                    if (HasByRefArrayAccessUsingSystemIndexParameters(node.Method, node.Arguments))
                    {
                        // Generate CSharpExpression.Invoke(Receiver, arguments)
                        return CSharpExprFactory("Invoke", receiverOpt, args);
                    }
                    else
                    {
                        // Generate Expression.Invoke(Receiver, arguments)
                        return ExprFactory("Invoke", receiverOpt, args);
                    }
                }
            }
            else if (HasNamedOrOptionalParameters(node.ArgumentNamesOpt, node.Method, node.Arguments))
            {
                var method = node.Method;
                return CSharpExprFactory(
                    "Call",
                    method.RequiresInstanceReceiver ? receiverOpt : _bound.Null(ExpressionType),
                    _bound.MethodInfo(method),
                    ParameterBindings(node.Arguments, method, node.ArgsToParamsOpt));
            }
            else
            {
                var method = node.Method;

                var obj = method.RequiresInstanceReceiver ? receiverOpt : _bound.Null(ExpressionType);
                var mtd = _bound.MethodInfo(method);
                var args = Expressions(node.Arguments);

                if (HasByRefArrayAccessUsingSystemIndexParameters(method, node.Arguments))
                {
                    // Generate CSharpExpression.Call(Receiver, Method, arguments)
                    return CSharpExprFactory("Call", obj, mtd, args);
                }
                else
                {
                    // Generate Expression.Call(Receiver, Method, arguments)
                    return ExprFactory("Call", obj, mtd, args);
                }
            }
        }

        private static bool HasNamedOrOptionalParameters(ImmutableArray<string> argumentNamesOpt, MethodSymbol method, ImmutableArray<BoundExpression> arguments)
        {
            // Checks whether we have any named arguments or missing arguments.
            return !argumentNamesOpt.IsDefaultOrEmpty || method.ParameterCount != arguments.Length;
        }

        private bool HasByRefArrayAccessUsingSystemIndexParameters(MethodSymbol method, ImmutableArray<BoundExpression> arguments)
        {
            // NB: When an ArrayAccess node using System.Index occurs in a by-ref position, we need to guarantee we can
            //     reduce the generated expression tree node such that a reference to the array slot is passed. This
            //     requires reduction of the enclosing node (Call, New, or Invoke) such that all requires temporaries for
            //     the array and index computation end up in a Block, while allowing for an IndexExpression to be passed
            //     to the by-ref parameter. See Microsoft.CSharp.Expressions for more info.

            var parameters = method.GetParameters();

            for (var i = 0; i < arguments.Length; i++)
            {
                var arg = arguments[i];
                var par = parameters[i];

                if (par.RefKind == RefKind.Out || par.RefKind == RefKind.Ref)
                {
                    if (arg is BoundArrayAccess arrayAccess)
                    {
                        if (arrayAccess.Indices.Length == 1)
                        {
                            var index = arrayAccess.Indices[0];

                            if (TypeSymbol.Equals(index.Type, _bound.WellKnownType(WellKnownType.System_Index), TypeCompareKind.ConsiderEverything2))
                            {
                                return true;
                            }
                        }
                    }
                }
            }

            return false;
        }

        private BoundExpression ParameterBindings(ImmutableArray<BoundExpression> arguments, MethodSymbol method, ImmutableArray<int> argsToParamsOpt)
        {
            var parameters = method.GetParameters();

            var builder = ArrayBuilder<BoundExpression>.GetInstance();

            var argsToParamsCount = argsToParamsOpt.IsDefaultOrEmpty ? 0 : argsToParamsOpt.Length;

            for (var i = 0; i < arguments.Length; i++)
            {
                var arg = arguments[i];
                var par = parameters[i];
                var idx = i;

                if (i < argsToParamsCount)
                {
                    idx = argsToParamsOpt[i];
                }

                builder.Add(ParameterBinding(method, idx, arg));
            }

            return _bound.Array(CSharpParameterAssignmentType, builder.ToImmutableAndFree());
        }

        private BoundExpression ParameterBinding(MethodSymbol method, int parameterIndex, BoundExpression argument)
        {
            var arg = Visit(argument);

            return CSharpExprFactory("Bind", _bound.MethodInfo(method, useMethodBase: true), _bound.Literal(parameterIndex), arg);
        }

        private BoundExpression VisitConditionalAccess(BoundConditionalAccess node)
        {
            var receiver = Visit(node.Receiver);

            CurrentLambdaInfo.PushConditionalReceiver();

            var access = Visit(node.AccessExpression);

            var conditionalReceiver = CurrentLambdaInfo.PopConditionalReceiver();

            return CSharpExprFactory("ConditionalAccess", receiver, conditionalReceiver, access);
        }

        private BoundExpression VisitConditionalReceiver(BoundConditionalReceiver node)
        {
            return CurrentLambdaInfo.BindConditionalReceiver(node);
        }

        private BoundExpression VisitConditionalOperator(BoundConditionalOperator node)
        {
            var condition = Visit(node.Condition);
            var consequence = VisitExactType(node.Consequence);
            var alternative = VisitExactType(node.Alternative);
            return ExprFactory("Condition", condition, consequence, alternative);
        }

        /// <summary>
        /// Visit the expression, but do so in a way that ensures that its type is precise.  That means that any
        /// sometimes-unnecessary conversions (such as an implicit reference conversion) are retained.
        /// </summary>
        private BoundExpression VisitExactType(BoundExpression e)
        {
            var conversion = e as BoundConversion;
            if (conversion != null && !conversion.ExplicitCastInCode)
            {
                e = conversion.Update(
                    conversion.Operand,
                    conversion.Conversion,
                    isBaseConversion: conversion.IsBaseConversion,
                    @checked: conversion.Checked,
                    explicitCastInCode: true,
                    conversionGroupOpt: conversion.ConversionGroupOpt,
                    constantValueOpt: conversion.ConstantValueOpt,
                    type: conversion.Type);
            }

            return Visit(e);
        }

        private BoundExpression VisitConversion(BoundConversion node)
        {
            switch (node.ConversionKind)
            {
                case ConversionKind.MethodGroup:
                    {
                        var mg = (BoundMethodGroup)node.Operand;
                        return DelegateCreation(mg.ReceiverOpt, node.SymbolOpt, node.Type, !node.SymbolOpt.RequiresInstanceReceiver && !node.IsExtensionMethod);
                    }
                case ConversionKind.ExplicitUserDefined:
                case ConversionKind.ImplicitUserDefined:
                case ConversionKind.IntPtr:
                    {
                        var method = node.SymbolOpt;
                        var operandType = node.Operand.Type;
                        var strippedOperandType = operandType.StrippedType();
                        var conversionInputType = method.Parameters[0].Type;
                        var isLifted = !TypeSymbol.Equals(operandType, conversionInputType, TypeCompareKind.ConsiderEverything2) && TypeSymbol.Equals(strippedOperandType, conversionInputType, TypeCompareKind.ConsiderEverything2);
                        bool requireAdditionalCast =
                            !TypeSymbol.Equals(strippedOperandType, ((node.ConversionKind == ConversionKind.ExplicitUserDefined) ? conversionInputType : conversionInputType.StrippedType()), TypeCompareKind.ConsiderEverything2);
                        var resultType = (isLifted && method.ReturnType.IsNonNullableValueType() && node.Type.IsNullableType()) ?
                                            _nullableType.Construct(method.ReturnType) : method.ReturnType;
                        var e1 = requireAdditionalCast
                            ? Convert(Visit(node.Operand), node.Operand.Type, method.Parameters[0].Type, node.Checked, false)
                            : Visit(node.Operand);
                        var e2 = ExprFactory(node.Checked && SyntaxFacts.IsCheckedOperator(method.Name) ? "ConvertChecked" : "Convert", e1, _bound.Typeof(resultType), _bound.MethodInfo(method));
                        return Convert(e2, resultType, node.Type, node.Checked, false);
                    }
                case ConversionKind.ImplicitReference:
                case ConversionKind.Identity:
                    {
                        var operand = Visit(node.Operand);
                        return node.ExplicitCastInCode ? Convert(operand, node.Type, false) : operand;
                    }
                case ConversionKind.ImplicitTuple:
                case ConversionKind.ExplicitTuple:
                case ConversionKind.ImplicitNullable when IsLiftedTupleConversion(node):
                case ConversionKind.ExplicitNullable when IsLiftedTupleConversion(node):
                    return TupleConvert(node);
                case ConversionKind.ImplicitNullable:
                    if (node.Operand.Type.IsNullableType())
                    {
                        return Convert(Visit(node.Operand), node.Operand.Type, node.Type, node.Checked, node.ExplicitCastInCode);
                    }
                    else
                    {
                        // the native compiler performs this conversion in two steps, so we follow suit
                        var nullable = (NamedTypeSymbol)node.Type;
                        var intermediate = nullable.TypeArgumentsWithAnnotationsNoUseSiteDiagnostics[0].Type;
                        var e1 = Convert(Visit(node.Operand), node.Operand.Type, intermediate, node.Checked, false);
                        return Convert(e1, intermediate, node.Type, node.Checked, false);
                    }
                case ConversionKind.NullLiteral:
                    return Convert(Constant(_bound.Null(_objectType)), _objectType, node.Type, false, node.ExplicitCastInCode);
                case ConversionKind.InterpolatedString:
                    return VisitInterpolatedString((BoundInterpolatedString)node.Operand, node.Type);
                case ConversionKind.ImplicitTupleLiteral:
                case ConversionKind.ExplicitTupleLiteral:
                    throw ExceptionUtilities.UnexpectedValue(node.ConversionKind); // REVIEW: LocalRewriter turns this into identity. Should we preserve it in expession trees?
                default:
                    return Convert(Visit(node.Operand), node.Operand.Type, node.Type, node.Checked, node.ExplicitCastInCode);
            }
        }

        private static bool IsLiftedTupleConversion(BoundConversion node)
        {
            var fromType = node.Operand.Type;
            var toType = node.Type;

            // T -> T
            if (TypeSymbol.Equals(fromType, toType, TypeCompareKind.ConsiderEverything2))
            {
                return false;
            }

            if (fromType.IsNullableType())
            {
                var fromTypeNonNull = fromType.GetNullableUnderlyingType();

                if (!fromTypeNonNull.IsTupleType)
                {
                    return false;
                }

                // (T, U)? -> (T', U')
                if (TypeSymbol.Equals(fromTypeNonNull, toType, TypeCompareKind.ConsiderEverything2))
                {
                    return false;
                }
            }
            else if (!fromType.IsTupleType)
            {
                return false;
            }

            if (toType.IsNullableType())
            {
                var toTypeNonNull = toType.GetNullableUnderlyingType();

                if (!toTypeNonNull.IsTupleType)
                {
                    return false;
                }

                // (T, U) -> (T', U')?
                if (TypeSymbol.Equals(fromType, toTypeNonNull, TypeCompareKind.ConsiderEverything2))
                {
                    return false;
                }
            }
            else if (!toType.IsTupleType)
            {
                return false;
            }

            // (T, U) -> (T', U')
            // (T, U) -> (T', U')?
            // (T, U)? -> (T', U')
            // (T, U)? -> (T', U')?
            return true;
        }

        private BoundExpression Convert(BoundExpression operand, TypeSymbol oldType, TypeSymbol newType, bool isChecked, bool isExplicit)
        {
            return (TypeSymbol.Equals(oldType, newType, TypeCompareKind.ConsiderEverything2) && !isExplicit) ? operand : Convert(operand, newType, isChecked);
        }

        private BoundExpression Convert(BoundExpression expr, TypeSymbol type, bool isChecked)
        {
            return ExprFactory(isChecked ? "ConvertChecked" : "Convert", expr, _bound.Typeof(type));
        }

        private BoundExpression TupleConvert(BoundConversion node)
        {
            return TupleConvert(Visit(node.Operand), node.Operand.Type, node.Type, node.Conversion);
        }

        private BoundExpression TupleConvert(BoundExpression operand, TypeSymbol oldType, TypeSymbol newType, Conversion conversion)
        {
            Debug.Assert(isTupleOrNullableTuple(oldType));
            Debug.Assert(isTupleOrNullableTuple(newType));

            static bool isTupleOrNullableTuple(TypeSymbol type)
            {
                if (type.IsNullableType())
                {
                    type = type.GetNullableUnderlyingType();
                }

                return type.IsTupleType;
            }

            BoundExpression getElementConversions()
            {
                var fromType = oldType;

                if (fromType.IsNullableType())
                {
                    fromType = fromType.GetNullableUnderlyingType();
                }

                Debug.Assert(fromType.IsTupleType);

                var toType = newType;

                if (toType.IsNullableType())
                {
                    toType = toType.GetNullableUnderlyingType();
                }

                Debug.Assert(toType.IsTupleType);

                var elementConversions = conversion.UnderlyingConversions;

                if (conversion.Kind == ConversionKind.ImplicitNullable || conversion.Kind == ConversionKind.ExplicitNullable)
                {
                    var tupleConversion = elementConversions[0];

                    Debug.Assert(tupleConversion.Kind == ConversionKind.ImplicitTuple || tupleConversion.Kind == ConversionKind.ExplicitTuple);

                    elementConversions = tupleConversion.UnderlyingConversions;
                }

                var sourceTypes = fromType.TupleElementTypesWithAnnotations;
                var destTypes = toType.TupleElementTypesWithAnnotations;

                var builder = ArrayBuilder<BoundExpression>.GetInstance(elementConversions.Length);

                for (int i = 0, n = elementConversions.Length; i < n; i++)
                {
                    builder.Add(MakeConversionLambda(elementConversions[i], sourceTypes[i].Type, destTypes[i].Type));
                }

                return _bound.Array(LambdaExpressionTypeType, builder.ToImmutableAndFree());
            }

            var conversions = getElementConversions();

            return CSharpExprFactory("TupleConvert", operand, _bound.Typeof(newType), conversions);
        }

        private BoundExpression DelegateCreation(BoundExpression receiver, MethodSymbol method, TypeSymbol delegateType, bool requiresInstanceReceiver)
        {
            var nullObject = _bound.Null(_objectType);
            receiver = requiresInstanceReceiver ? nullObject : receiver.Type.IsReferenceType ? receiver : _bound.Convert(_objectType, receiver);

            var createDelegate = _bound.WellKnownMethod(WellKnownMember.System_Reflection_MethodInfo__CreateDelegate, isOptional: true);
            BoundExpression unquoted;
            if ((object)createDelegate != null)
            {
                // beginning in 4.5, we do it this way
                unquoted = _bound.Call(_bound.MethodInfo(method), createDelegate, _bound.Typeof(delegateType), receiver);
            }
            else
            {
                // 4.0 and earlier we do it this way
                //createDelegate = (MethodSymbol)Bound.WellKnownMember(WellKnownMember.System_Delegate__CreateDelegate);
                //operand = Bound.Call(nullObject, createDelegate, Bound.Typeof(node.Type), receiver, Bound.MethodInfo(method));
                unquoted = _bound.StaticCall(_bound.SpecialType(SpecialType.System_Delegate), "CreateDelegate", _bound.Typeof(delegateType), receiver, _bound.MethodInfo(method));
            }

            // NOTE: we visit the just-built node, which has not yet been visited.  This is not the usual order
            // of operations.  The above code represents Dev10's pre-expression-tree lowering, and producing
            // the expanded lowering by hand is very cumbersome.
            return Convert(Visit(unquoted), delegateType, false);
        }

        private BoundExpression VisitDelegateCreationExpression(BoundDelegateCreationExpression node)
        {
            if (node.Argument.Kind == BoundKind.MethodGroup)
            {
                throw ExceptionUtilities.UnexpectedValue(BoundKind.MethodGroup);
            }

            if ((object)node.MethodOpt != null)
            {
                bool staticMember = !node.MethodOpt.RequiresInstanceReceiver && !node.IsExtensionMethod;
                return DelegateCreation(node.Argument, node.MethodOpt, node.Type, staticMember);
            }

            var d = node.Argument.Type as NamedTypeSymbol;
            if ((object)d != null && d.TypeKind == TypeKind.Delegate)
            {
                return DelegateCreation(node.Argument, d.DelegateInvokeMethod, node.Type, false);
            }

            // there should be no other cases.  Have we missed one?
            throw ExceptionUtilities.UnexpectedValue(node.Argument);
        }

        private BoundExpression VisitDynamicMemberAccess(BoundQuotedDynamicMemberAccess node)
        {
            var receiver = Visit(node.Receiver);
            var name = node.Name;
            var args = _bound.Array(DynamicCSharpArgumentType, ImmutableArray<BoundExpression>.Empty); // REVIEW: remove altogether in runtime library?
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            return DynamicCSharpExprFactory("DynamicGetMember", receiver, name, args, flags, context);
        }

        private BoundExpression VisitDynamicIndexAccess(BoundQuotedDynamicIndexAccess node)
        {
            var receiver = Visit(node.Receiver);
            var args = VisitDynamicArguments(node.Arguments);
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            return DynamicCSharpExprFactory("DynamicGetIndex", receiver, args, flags, context);
        }

        private BoundExpression VisitDynamicInvoke(BoundQuotedDynamicInvocation node)
        {
            var receiver = Visit(node.Receiver);
            var args = VisitDynamicArguments(node.Arguments);
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            return DynamicCSharpExprFactory("DynamicInvoke", receiver, args, flags, context);
        }

        private BoundExpression VisitDynamicCall(BoundQuotedDynamicCall node)
        {
            var receiver = node.TypeReceiver != null ? node.TypeReceiver : Visit(node.Receiver);
            var name = node.Name;
            var typeArguments = node.TypeArguments;
            var args = VisitDynamicArguments(node.Arguments);
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            return DynamicCSharpExprFactory("DynamicInvokeMember", receiver, name, typeArguments, args, flags, context);
        }

        private BoundExpression VisitDynamicNew(BoundQuotedDynamicNew node)
        {
            var receiver = node.TypeReceiver;
            var args = VisitDynamicArguments(node.Arguments);
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            return DynamicCSharpExprFactory("DynamicInvokeConstructor", receiver, args, flags, context);
        }

        private BoundExpression VisitDynamicUnary(BoundQuotedDynamicUnary node)
        {
            var expressionType = _bound.Convert(ExpressionTypeType, node.ExpressionType);
            var operand = VisitDynamicArgument(node.Operand);
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            // DESIGN: Emit calls to specific factories? Could be beneficial for bind time validation.
            return DynamicCSharpExprFactory("MakeDynamicUnary", expressionType, operand, flags, context);
        }

        private BoundExpression VisitDynamicBinary(BoundQuotedDynamicBinary node)
        {
            var expressionType = _bound.Convert(ExpressionTypeType, node.ExpressionType);
            var left = VisitDynamicArgument(node.Left);
            var right = VisitDynamicArgument(node.Right);
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            // DESIGN: Emit calls to specific factories? Could be beneficial for bind time validation.
            return DynamicCSharpExprFactory("MakeDynamicBinary", expressionType, left, right, flags, context);
        }

        private BoundExpression VisitDynamicConvert(BoundQuotedDynamicConvert node)
        {
            var operand = Visit(node.Operand);
            var type = node.TargetType;
            var flags = _bound.Convert(CSharpBinderFlagsType, node.Flags);
            var context = node.Context;

            // DESIGN: Emit calls to specific factories? Could be beneficial for bind time validation.
            return DynamicCSharpExprFactory("DynamicConvert", operand, type, flags, context);
        }

        private BoundExpression VisitDynamicArguments(ImmutableArray<BoundQuotedDynamicArgument> arguments)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();

            foreach (var argument in arguments)
            {
                var arg = VisitDynamicArgument(argument);
                builder.Add(arg);
            }

            return _bound.Array(DynamicCSharpArgumentType, builder.ToImmutableAndFree());
        }

        private BoundExpression VisitDynamicArgument(BoundQuotedDynamicArgument argument)
        {
            return DynamicCSharpExprFactory("DynamicArgument", Visit(argument.Expression), argument.Name, _bound.Convert(CSharpArgumentInfoFlagsType, argument.Flags));
        }

        private BoundExpression VisitFieldAccess(BoundFieldAccess node)
        {
            var receiver = node.FieldSymbol.IsStatic ? null : Visit(node.ReceiverOpt);
            return VisitFieldAccess(receiver, node);
        }

        private BoundExpression VisitFieldAccess(BoundExpression receiverOpt, BoundFieldAccess node)
        {
            var receiver = node.FieldSymbol.IsStatic ? _bound.Null(ExpressionType) : receiverOpt;
            return ExprFactory(
                "Field",
                receiver, _bound.FieldInfo(node.FieldSymbol));
        }

        private BoundExpression VisitIndexerAccess(BoundIndexerAccess node)
        {
            var indexer = node.Indexer;
            var method = indexer.GetOwnOrInheritedGetMethod() ?? indexer.GetOwnOrInheritedSetMethod();
            var receiver = method.IsStatic ? null : Visit(node.ReceiverOpt);

            return VisitIndexerAccess(receiver, node);
        }

        private BoundExpression VisitIndexerAccess(BoundExpression receiverOpt, BoundIndexerAccess node)
        {
            var indexer = node.Indexer;
            var method = indexer.GetOwnOrInheritedGetMethod() ?? indexer.GetOwnOrInheritedSetMethod();

            var receiver = method.IsStatic ? _bound.Null(ExpressionType) : receiverOpt;

            if (!node.ArgumentNamesOpt.IsDefaultOrEmpty)
            {
                return CSharpExprFactory(
                    "Index",
                    receiver,
                    _bound.MethodInfo(method),
                    ParameterBindings(node.Arguments, method, node.ArgsToParamsOpt)
                );
            }
            else
            {
                return CSharpExprFactory(
                    "Index",
                    receiver,
                    _bound.MethodInfo(method),
                    Expressions(node.Arguments)
                );
            }
        }

        private BoundExpression VisitIsOperator(BoundIsOperator node)
        {
            var operand = node.Operand;
            if ((object)operand.Type == null && operand.ConstantValue != null && operand.ConstantValue.IsNull)
            {
                operand = _bound.Null(_objectType);
            }

            return ExprFactory("TypeIs", Visit(operand), _bound.Typeof(node.TargetType.Type));
        }

        private BoundExpression VisitLambda(BoundLambda node)
        {
            var result = VisitLambdaInternal(node);
            return node.Type.IsExpressionTree() ? ExprFactory("Quote", result) : result;
        }

        private BoundExpression VisitLambdaInternal(BoundLambda node)
        {
            // prepare parameters so that they can be seen later
            var locals = ArrayBuilder<LocalSymbol>.GetInstance();
            var initializers = ArrayBuilder<BoundExpression>.GetInstance();
            var parameters = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var p in node.Symbol.Parameters)
            {
                var param = _bound.SynthesizedLocal(ParameterExpressionType);
                locals.Add(param);
                var parameterReference = _bound.Local(param);
                parameters.Add(parameterReference);
                var parameter = ExprFactory(
                    "Parameter",
                    _bound.Typeof(_typeMap.SubstituteType(p.Type).Type), _bound.Literal(p.Name));
                initializers.Add(_bound.AssignmentExpression(parameterReference, parameter));
                _parameterMap[p] = parameterReference;
            }

            var underlyingDelegateType = node.Type.GetDelegateType();

            var underlyingDelegateTypeSymbol = ImmutableArray.Create<TypeSymbol>(underlyingDelegateType);

            var body = TranslateLambdaBody(node.Body, locals, initializers);

            var parameterArray = _bound.ArrayOrEmpty(ParameterExpressionType, parameters.ToImmutableAndFree());

            var lambda =
                node.IsAsync ?
                CSharpExprFactory("Lambda", underlyingDelegateTypeSymbol, _bound.Literal(true), body, parameterArray) :
                ExprFactory("Lambda", underlyingDelegateTypeSymbol, body, parameterArray);

            var result = _bound.Sequence(locals.ToImmutableAndFree(), initializers.ToImmutableAndFree(), lambda);

            foreach (var p in node.Symbol.Parameters)
            {
                _parameterMap.Remove(p);
            }

            return result;
        }

        private BoundExpression VisitNewT(BoundNewT node)
        {
            return VisitObjectCreationContinued(ExprFactory("New", _bound.Typeof(node.Type)), node.InitializerExpressionOpt);
        }

        private BoundExpression VisitNullCoalescingOperator(BoundNullCoalescingOperator node)
        {
            var left = Visit(node.LeftOperand);
            var right = Visit(node.RightOperand);
            if (BoundNode.GetConversion(node.LeftConversion, node.LeftPlaceholder) is { IsUserDefined: true } leftConversion)
            {
                Debug.Assert(node.LeftPlaceholder is not null);
                TypeSymbol lambdaParamType = node.LeftPlaceholder.Type;
                return ExprFactory("Coalesce", left, right, MakeConversionLambda(leftConversion, lambdaParamType, node.LeftConversion.Type));
            }
            else
            {
                return ExprFactory("Coalesce", left, right);
            }
        }

        private BoundExpression VisitNullCoalescingAssignmentOperator(BoundNullCoalescingAssignmentOperator node)
        {
            var left = Visit(node.LeftOperand);
            var right = Visit(node.RightOperand);

            if (node.LeftOperand.HasDynamicType() || node.RightOperand.HasDynamicType())
            {
                // NB: using dynamic factories to support disabling all dynamic operations in an expression tree

                return DynamicCSharpExprFactory("DynamicNullCoalescingAssign", left, right);
            }
            else
            {
                return CSharpExprFactory("NullCoalescingAssign", left, right);
            }
        }

        private BoundExpression MakeConversionLambda(Conversion conversion, TypeSymbol fromType, TypeSymbol toType)
        {
            string parameterName = "p";
            ParameterSymbol lambdaParameter = _bound.SynthesizedParameter(fromType, parameterName);
            var param = _bound.SynthesizedLocal(ParameterExpressionType);
            var parameterReference = _bound.Local(param);
            var parameter = ExprFactory("Parameter", _bound.Typeof(fromType), _bound.Literal(parameterName));
            _parameterMap[lambdaParameter] = parameterReference;
            var convertedValue = Visit(_bound.Convert(toType, _bound.Parameter(lambdaParameter), conversion));
            _parameterMap.Remove(lambdaParameter);
            var result = _bound.Sequence(
                ImmutableArray.Create(param),
                ImmutableArray.Create<BoundExpression>(_bound.AssignmentExpression(parameterReference, parameter)),
                ExprFactory(
                    "Lambda",
                    convertedValue,
                    _bound.ArrayOrEmpty(ParameterExpressionType, ImmutableArray.Create<BoundExpression>(parameterReference))));
            return result;
        }

        private BoundExpression InitializerMemberSetter(Symbol symbol)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Field:
                    return _bound.Convert(MemberInfoType, _bound.FieldInfo((FieldSymbol)symbol));
                case SymbolKind.Property:
                    return _bound.MethodInfo(((PropertySymbol)symbol).GetOwnOrInheritedSetMethod());
                case SymbolKind.Event:
                    return _bound.Convert(MemberInfoType, _bound.FieldInfo(((EventSymbol)symbol).AssociatedField));
                default:
                    throw ExceptionUtilities.UnexpectedValue(symbol.Kind);
            }
        }

        private BoundExpression InitializerMemberGetter(Symbol symbol)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Field:
                    return _bound.Convert(MemberInfoType, _bound.FieldInfo((FieldSymbol)symbol));
                case SymbolKind.Property:
                    return _bound.MethodInfo(((PropertySymbol)symbol).GetOwnOrInheritedGetMethod());
                case SymbolKind.Event:
                    return _bound.Convert(MemberInfoType, _bound.FieldInfo(((EventSymbol)symbol).AssociatedField));
                default:
                    throw ExceptionUtilities.UnexpectedValue(symbol.Kind);
            }
        }

        private enum InitializerKind { Expression, MemberInitializer, CollectionInitializer };

        private BoundExpression VisitInitializer(BoundExpression node, out InitializerKind kind)
        {
            switch (node.Kind)
            {
                case BoundKind.ObjectInitializerExpression:
                    {
                        var oi = (BoundObjectInitializerExpression)node;
                        var builder = ArrayBuilder<BoundExpression>.GetInstance();
                        foreach (BoundAssignmentOperator a in oi.Initializers)
                        {
                            var sym = ((BoundObjectInitializerMember)a.Left).MemberSymbol;

                            // An error is reported in diagnostics pass when a dynamic object initializer is encountered in an ET:
                            Debug.Assert((object)sym != null);

                            InitializerKind elementKind;
                            var value = VisitInitializer(a.Right, out elementKind);
                            switch (elementKind)
                            {
                                case InitializerKind.CollectionInitializer:
                                    {
                                        var left = InitializerMemberGetter(sym);
                                        builder.Add(ExprFactory("ListBind", left, value));
                                        break;
                                    }
                                case InitializerKind.Expression:
                                    {
                                        var left = InitializerMemberSetter(sym);
                                        builder.Add(ExprFactory("Bind", left, value));
                                        break;
                                    }
                                case InitializerKind.MemberInitializer:
                                    {
                                        var left = InitializerMemberGetter(sym);
                                        builder.Add(ExprFactory("MemberBind", left, value));
                                        break;
                                    }
                                default:
                                    throw ExceptionUtilities.UnexpectedValue(elementKind);
                            }
                        }

                        kind = InitializerKind.MemberInitializer;
                        return _bound.ArrayOrEmpty(MemberBindingType, builder.ToImmutableAndFree());
                    }

                case BoundKind.CollectionInitializerExpression:
                    {
                        var ci = (BoundCollectionInitializerExpression)node;
                        Debug.Assert(ci.Initializers.Length != 0);
                        kind = InitializerKind.CollectionInitializer;

                        var builder = ArrayBuilder<BoundExpression>.GetInstance();

                        // The method invocation must be a static call. 
                        // Dynamic calls are not allowed in ETs, an error is reported in diagnostics pass.
                        foreach (BoundCollectionElementInitializer i in ci.Initializers)
                        {
                            BoundExpression elementInit = ExprFactory("ElementInit", _bound.MethodInfo(i.AddMethod), Expressions(i.Arguments));
                            builder.Add(elementInit);
                        }

                        return _bound.ArrayOrEmpty(ElementInitType, builder.ToImmutableAndFree());
                    }

                default:
                    {
                        kind = InitializerKind.Expression;
                        return Visit(node);
                    }
            }
        }

        private BoundExpression VisitObjectCreationExpression(BoundObjectCreationExpression node)
        {
            return VisitObjectCreationContinued(VisitObjectCreationExpressionInternal(node), node.InitializerExpressionOpt);
        }

        private BoundExpression VisitObjectCreationContinued(BoundExpression creation, BoundExpression initializerExpressionOpt)
        {
            var result = creation;
            if (initializerExpressionOpt == null) return result;
            InitializerKind initializerKind;
            var init = VisitInitializer(initializerExpressionOpt, out initializerKind);
            switch (initializerKind)
            {
                case InitializerKind.CollectionInitializer:
                    return ExprFactory("ListInit", result, init);
                case InitializerKind.MemberInitializer:
                    return ExprFactory("MemberInit", result, init);
                default:
                    throw ExceptionUtilities.UnexpectedValue(initializerKind); // no other options at the top level of an initializer
            }
        }

        private BoundExpression VisitObjectCreationExpressionInternal(BoundObjectCreationExpression node)
        {
            if (node.ConstantValue != null)
            {
                // typically a decimal constant.
                return Constant(node);
            }

            var hasNamedOrOptionalParameters = HasNamedOrOptionalParameters(node.ArgumentNamesOpt, node.Constructor, node.Arguments);

            if (!hasNamedOrOptionalParameters)
            {
                if ((object)node.Constructor == null ||
                    (node.Arguments.Length == 0 && !node.Type.IsStructType()) ||
                    node.Constructor.IsDefaultValueTypeConstructor())
                {
                    return ExprFactory("New", _bound.Typeof(node.Type));
                }
            }

            var ctor = _bound.ConstructorInfo(node.Constructor);
            var args = _bound.Convert(_IEnumerableType.Construct(ExpressionType), Expressions(node.Arguments));
            if (node.Type.IsAnonymousType && node.Arguments.Length != 0)
            {
                var anonType = (NamedTypeSymbol)node.Type;
                var membersBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                for (int i = 0; i < node.Arguments.Length; i++)
                {
                    membersBuilder.Add(_bound.MethodInfo(AnonymousTypeManager.GetAnonymousTypeProperty(anonType, i).GetMethod));
                }

                return ExprFactory("New", ctor, args, _bound.ArrayOrEmpty(MemberInfoType, membersBuilder.ToImmutableAndFree()));
            }
            else
            {
                if (hasNamedOrOptionalParameters)
                {
                    var constructor = node.Constructor;
                    return CSharpExprFactory(
                        "New",
                        ctor,
                        ParameterBindings(node.Arguments, constructor, node.ArgsToParamsOpt));
                }
                else
                {
                    if (HasByRefArrayAccessUsingSystemIndexParameters(node.Constructor, node.Arguments))
                    {
                        return CSharpExprFactory("New", ctor, args);
                    }
                    else
                    {
                        return ExprFactory("New", ctor, args);
                    }
                }
            }
        }

        private BoundExpression VisitParameter(BoundParameter node)
        {
            return _parameterMap[node.ParameterSymbol];
        }

        private static BoundExpression VisitPointerIndirectionOperator(BoundPointerIndirectionOperator node)
        {
            // error should have been reported earlier
            // Diagnostics.Add(ErrorCode.ERR_ExpressionTreeContainsPointerOp, node.Syntax.Location);
            return new BoundBadExpression(node.Syntax, default(LookupResultKind), ImmutableArray<Symbol>.Empty, ImmutableArray.Create<BoundExpression>(node), node.Type);
        }

        private static BoundExpression VisitPointerElementAccess(BoundPointerElementAccess node)
        {
            // error should have been reported earlier
            // Diagnostics.Add(ErrorCode.ERR_ExpressionTreeContainsPointerOp, node.Syntax.Location);
            return new BoundBadExpression(node.Syntax, default(LookupResultKind), ImmutableArray<Symbol>.Empty, ImmutableArray.Create<BoundExpression>(node), node.Type);
        }

        private BoundExpression VisitPropertyAccess(BoundPropertyAccess node)
        {
            var receiver = node.PropertySymbol.IsStatic ? null : Visit(node.ReceiverOpt);
            return VisitPropertyAccess(receiver, node);
        }

        private BoundExpression VisitPropertyAccess(BoundExpression receiverOpt, BoundPropertyAccess node)
        {
            var receiver = node.PropertySymbol.IsStatic ? _bound.Null(ExpressionType) : receiverOpt;
            var getMethod = node.PropertySymbol.GetOwnOrInheritedGetMethod();

            // COMPAT: see https://github.com/dotnet/roslyn/issues/4471
            //         old compiler used to insert casts like this and 
            //         there are known dependencies on this kind of tree shape.
            //
            //         While the casts are semantically incorrect, the conditions
            //         under which they are observable are extremely narrow:
            //         We would have to deal with a generic T receiver which is actually a struct
            //         that implements a property form an interface and 
            //         the implementation of the getter must make observable mutations to the instance.
            //
            //         At this point it seems more appropriate to continue adding these casts.
            if (node.ReceiverOpt?.Type.IsTypeParameter() == true &&
                !node.ReceiverOpt.Type.IsReferenceType)
            {
                receiver = this.Convert(receiver, getMethod.ReceiverType, isChecked: false);
            }

            return ExprFactory("Property", receiver, _bound.MethodInfo(getMethod));
        }

        private static BoundExpression VisitSizeOfOperator(BoundSizeOfOperator node)
        {
            // error should have been reported earlier
            // Diagnostics.Add(ErrorCode.ERR_ExpressionTreeContainsPointerOp, node.Syntax.Location);
            return new BoundBadExpression(node.Syntax, default(LookupResultKind), ImmutableArray<Symbol>.Empty, ImmutableArray.Create<BoundExpression>(node), node.Type);
        }

        private BoundExpression VisitUnaryOperator(BoundUnaryOperator node)
        {
            var arg = node.Operand;
            var loweredArg = Visit(arg);
            var opKind = node.OperatorKind;
            var op = opKind & UnaryOperatorKind.OpMask;
            var isChecked = (opKind & UnaryOperatorKind.Checked) != 0;

            string opname;
            switch (op)
            {
                case UnaryOperatorKind.UnaryPlus:
                    if ((object)node.MethodOpt == null)
                    {
                        return loweredArg;
                    }
                    opname = "UnaryPlus";
                    break;
                case UnaryOperatorKind.UnaryMinus:
                    opname = isChecked || (node.MethodOpt is { Name: string name } && SyntaxFacts.IsCheckedOperator(name)) ? "NegateChecked" : "Negate";
                    break;
                case UnaryOperatorKind.BitwiseComplement:
                case UnaryOperatorKind.LogicalNegation:
                    opname = "Not";
                    break;
                default:
                    throw ExceptionUtilities.UnexpectedValue(op);
            }

            if (node.OperatorKind.OperandTypes() == UnaryOperatorKind.Enum && (opKind & UnaryOperatorKind.Lifted) != 0)
            {
                Debug.Assert((object)node.MethodOpt == null);
                var promotedType = PromotedType(arg.Type.StrippedType().GetEnumUnderlyingType());
                promotedType = _nullableType.Construct(promotedType);
                loweredArg = Convert(loweredArg, arg.Type, promotedType, isChecked, false);
                var result = ExprFactory(opname, loweredArg);
                return Demote(result, node.Type, isChecked);
            }

            return ((object)node.MethodOpt == null)
                ? ExprFactory(opname, loweredArg)
                : ExprFactory(opname, loweredArg, _bound.MethodInfo(node.MethodOpt));
        }

        // ======================================================

        private BoundExpression ExprFactory(string name, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(ExpressionType, name, arguments);
        }

        private BoundExpression ExprFactory(string name, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(_ignoreAccessibility ? BinderFlags.IgnoreAccessibility : BinderFlags.None, ExpressionType, name, typeArgs, arguments);
        }

        private BoundExpression ExprFactory(WellKnownMember method, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            var m0 = _bound.WellKnownMethod(method);
            Debug.Assert((object)m0 != null);
            Debug.Assert(m0.ParameterCount == arguments.Length);
            var m1 = m0.Construct(typeArgs);
            return _bound.Call(null, m1, arguments);
        }

        private BoundExpression CSharpExprFactory(string name, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(CSharpExpressionType, name, arguments);
        }

        private BoundExpression CSharpExprFactory(string name, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(_ignoreAccessibility ? BinderFlags.IgnoreAccessibility : BinderFlags.None, CSharpExpressionType, name, typeArgs, arguments);
        }

        private BoundExpression CSharpExprFactory(WellKnownMember method, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            var m0 = _bound.WellKnownMethod(method);
            Debug.Assert((object)m0 != null);
            Debug.Assert(m0.ParameterCount == arguments.Length);
            var m1 = m0.Construct(typeArgs);
            return _bound.Call(null, m1, arguments);
        }

        private BoundExpression DynamicCSharpExprFactory(string name, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(DynamicCSharpExpressionType, name, arguments);
        }

        private BoundExpression DynamicCSharpExprFactory(string name, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(_ignoreAccessibility ? BinderFlags.IgnoreAccessibility : BinderFlags.None, DynamicCSharpExpressionType, name, typeArgs, arguments);
        }

        private BoundExpression DynamicCSharpExprFactory(WellKnownMember method, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            var m0 = _bound.WellKnownMethod(method);
            Debug.Assert((object)m0 != null);
            Debug.Assert(m0.ParameterCount == arguments.Length);
            var m1 = m0.Construct(typeArgs);
            return _bound.Call(null, m1, arguments);
        }

        private BoundExpression Constant(BoundExpression node)
        {
            return ExprFactory(
                "Constant",
                _bound.Convert(_objectType, node),
                _bound.Typeof(node.Type));
        }

        private BoundExpression VisitLocal(BoundLocal node)
        {
            if (node.ConstantValueOpt != null)
            {
                return Constant(node);
            }
            else
            {
                return _localMap[node.LocalSymbol];
            }
        }

        private BoundExpression VisitThrowExpression(BoundThrowExpression node)
        {
            var expr = node.Expression;

            var expression = Visit(expr);
            return CSharpStmtFactory("Throw", expression, _bound.Typeof(node.Type));
        }

        private BoundExpression VisitDiscardExpression(BoundDiscardExpression node)
        {
            return CSharpStmtFactory("Discard", _bound.Typeof(node.Type));
        }

        private BoundExpression VisitInterpolatedString(BoundInterpolatedString node, TypeSymbol type = null)
        {
            type ??= _stringType;

            // TODO: Cache these.
            var nullableInt32Type = _nullableType.Construct(_int32Type);

            var parts = node.Parts;
            var n = parts.Length;
            var expressions = new BoundExpression[n];

            for (int i = 0; i < n; i++)
            {
                var part = parts[i];

                var fillin = part as BoundStringInsert;
                if (fillin == null)
                {
                    Debug.Assert(part is BoundLiteral && part.ConstantValue != null);

                    // this is one of the literal parts
                    expressions[i] = CSharpExprFactory("InterpolationStringLiteral", _bound.StringLiteral(part.ConstantValue));
                }
                else
                {
                    // this is one of the expression holes

                    BoundExpression alignment, format;

                    if (fillin.Alignment != null && !fillin.Alignment.HasErrors)
                    {
                        if (!Binder.TryGetSpecialTypeMember<MethodSymbol>(_compilationState.Compilation, SpecialMember.System_Nullable_T__ctor, fillin.Alignment.Syntax, Diagnostics, out var ctor))
                        {
                            Debug.Assert(false);
                        }

                        alignment = _bound.New(ctor.AsMember(nullableInt32Type), _bound.Literal(fillin.Alignment.ConstantValue.Int32Value));
                    }
                    else
                    {
                        alignment = _bound.Default(nullableInt32Type);
                    }

                    if (fillin.Format != null && !fillin.Format.HasErrors)
                    {
                        format = _bound.StringLiteral(fillin.Format.ConstantValue.StringValue);
                    }
                    else
                    {
                        format = _bound.Null(_stringType);
                    }

                    var value = Visit(fillin.Value);

                    expressions[i] = CSharpExprFactory("InterpolationStringInsert", value, format, alignment);
                }
            }

            var interpolations = _bound.ArrayOrEmpty(CSharp_Expressions_InterpolationType, expressions);

            return CSharpExprFactory("InterpolatedString", _bound.Typeof(type), interpolations);
        }

        private BoundExpression VisitFromEndIndex(BoundFromEndIndexExpression node)
        {
            return CSharpExprFactory("FromEndIndex", Visit(node.Operand), _bound.MethodInfo(node.MethodOpt, useMethodBase: true), _bound.Typeof(node.Type));
        }

        private BoundExpression VisitRange(BoundRangeExpression node)
        {
            return CSharpExprFactory("Range", Visit(node.LeftOperandOpt) ?? _bound.Null(ExpressionType), Visit(node.RightOperandOpt) ?? _bound.Null(ExpressionType), _bound.MethodInfo(node.MethodOpt, useMethodBase: true), _bound.Typeof(node.Type));
        }

        // TODO: Revisit due to changes made in https://github.com/dotnet/roslyn/pull/57318.

        // private BoundExpression VisitIndexOrRangePatternIndexerAccess(BoundIndexOrRangePatternIndexerAccess node)
        // {
        //     MethodSymbol GetAccessor(PropertySymbol p)
        //     {
        //         return p.GetOwnOrInheritedGetMethod() ?? p.GetOwnOrInheritedSetMethod();
        //     }

        //     var countProperty = node.LengthOrCountProperty;
        //     var countAccessor = GetAccessor(countProperty);

        //     var receiver = Visit(node.Receiver);
        //     var argument = Visit(node.Argument);
        //     var lengthOrCount = _bound.MethodInfo(countAccessor);

        //     var indexer = node.PatternSymbol switch
        //     {
        //         PropertySymbol p => GetAccessor(p),
        //         MethodSymbol m => m,
        //         _ => throw ExceptionUtilities.UnexpectedValue(node.PatternSymbol)
        //     };

        //     return CSharpExprFactory("IndexerAccess", receiver, argument, lengthOrCount, _bound.MethodInfo(indexer));
        // }

        private BoundExpression VisitTupleBinaryOperator(BoundTupleBinaryOperator node)
        {
            //
            // NB: node.Left and/or node.Right may not have a type, which happens for cases like
            //
            //       (1, null) == (2, null)
            //
            //     We reject such cases during the expression tree diagnostic pass because we don't want to deal with
            //     untyped nodes in expression trees. This is not without precedent, cf. null-coalescing with a null
            //     or default literal as the left hand side
            //
            //       null ?? "bar"
            //

            Debug.Assert(node.Left.Type is { });
            Debug.Assert(node.Right.Type is { });

            var boolType = node.Type;
            var operatorKind = node.OperatorKind;

            var name = operatorKind.Operator() switch
            {
                BinaryOperatorKind.Equal => "TupleEqual",
                BinaryOperatorKind.NotEqual => "TupleNotEqual",
                _ => throw ExceptionUtilities.UnexpectedValue(operatorKind)
            };

            var left = Visit(node.Left);
            var right = Visit(node.Right);

            var equalityCheckLambdas = makeEqualityCheckLambdas(node.Operators);

            return CSharpExprFactory(name, left, right, equalityCheckLambdas);

            BoundExpression makeEqualityCheckLambdas(TupleBinaryOperatorInfo.Multiple info)
            {
                // CONSIDER: We could reuse parameter symbols across lambdas, or even reuse lambdas if they represent the same equality test, which is very common.

                var builder = ArrayBuilder<BoundExpression>.GetInstance(info.Operators.Length);

                for (int i = 0, n = info.Operators.Length; i < n; i++)
                {
                    var operatorInfo = info.Operators[i];
                    var leftElementType = operatorInfo.LeftConvertedTypeOpt;
                    var rightElementType = operatorInfo.RightConvertedTypeOpt;

                    var (leftParameterSymbol, leftParameter, leftParameterExpressionLocalSymbol, leftParameterExpressionLocal, leftAssignParameterExpression) = pushParameter(leftElementType, "left");
                    var (rightParameterSymbol, rightParameter, rightParameterExpressionLocalSymbol, rightParameterExpressionLocal, rightAssignParameterExpression) = pushParameter(rightElementType, "right");

                    var expr = operatorInfo.InfoKind switch
                    {
                        TupleBinaryOperatorInfoKind.Single => getElementEqualityCheckSingle((TupleBinaryOperatorInfo.Single)operatorInfo, leftParameter, rightParameter),
                        TupleBinaryOperatorInfoKind.Multiple => getElementEqualityCheckMultiple((TupleBinaryOperatorInfo.Multiple)operatorInfo, leftParameter, rightParameter),
                        TupleBinaryOperatorInfoKind.NullNull => getElementEqualityCheckNullNull(),
                        _ => throw ExceptionUtilities.UnexpectedValue(info.InfoKind)
                    };

                    var bodyExpr = Visit(expr);

                    popParameter(rightParameterSymbol);
                    popParameter(leftParameterSymbol);

                    var result = _bound.Sequence(
                            ImmutableArray.Create(leftParameterExpressionLocalSymbol, rightParameterExpressionLocalSymbol),
                            ImmutableArray.Create<BoundExpression>(
                                leftAssignParameterExpression,
                                rightAssignParameterExpression
                            ),
                            ExprFactory(
                                "Lambda",
                                bodyExpr,
                                _bound.ArrayOrEmpty(ParameterExpressionType, ImmutableArray.Create<BoundExpression>(leftParameterExpressionLocal, rightParameterExpressionLocal))));

                    builder.Add(result);
                }

                return _bound.Array(LambdaExpressionTypeType, builder.ToImmutableAndFree());

                (ParameterSymbol parameterSymbol, BoundParameter parameter, LocalSymbol parameterExpressionLocalSymbol, BoundLocal parameterExpressionLocal, BoundAssignmentOperator assignParameterExpression) pushParameter(TypeSymbol type, string name)
                {
                    var parameterSymbol = _bound.SynthesizedParameter(type, name);
                    var parameterParameter = _bound.Parameter(parameterSymbol);
                    var parameterExpressionLocalSymbol = _bound.SynthesizedLocal(ParameterExpressionType);
                    var parameterExpressionLocal = _bound.Local(parameterExpressionLocalSymbol);
                    var parameterExpressionValue = ExprFactory("Parameter", _bound.Typeof(type), _bound.Literal(name));
                    var assignParameterExpression = _bound.AssignmentExpression(parameterExpressionLocal, parameterExpressionValue);

                    _parameterMap[parameterSymbol] = parameterExpressionLocal;

                    return (parameterSymbol, parameterParameter, parameterExpressionLocalSymbol, parameterExpressionLocal, assignParameterExpression);
                }

                void popParameter(ParameterSymbol lambdaParameter)
                {
                    _parameterMap.Remove(lambdaParameter);
                }

                BoundExpression getElementEqualityCheckSingle(TupleBinaryOperatorInfo.Single operatorInfo, BoundParameter leftParameter, BoundParameter rightParameter)
                {
                    if (operatorInfo.Kind.IsDynamic())
                    {
                        var dyn = new LoweredDynamicOperationFactory(_bound, methodOrdinal: 0);

                        var dynamicResult = dyn.MakeDynamicBinaryOperatorExpression(operatorKind, leftParameter, rightParameter, isCompoundAssignment: false, _bound.Compilation.DynamicType);

                        if (operatorKind == BinaryOperatorKind.Equal)
                        {
                            return _bound.Not(dyn.MakeDynamicUnaryOperatorExpression(UnaryOperatorKind.DynamicFalse, dynamicResult, boolType));
                        }
                        else
                        {
                            return dyn.MakeDynamicUnaryOperatorExpression(UnaryOperatorKind.DynamicTrue, dynamicResult, boolType);
                        }
                    }

                    BoundExpression expr = _bound.Binary(operatorKind, operatorInfo.MethodSymbolOpt?.ReturnType ?? boolType, leftParameter, rightParameter, operatorInfo.MethodSymbolOpt);

                    var conversion = BoundNode.GetConversion(operatorInfo.ConversionForBool, operatorInfo.ConversionForBoolPlaceholder);

                    if (operatorInfo.BoolOperator.Kind != UnaryOperatorKind.Error)
                    {
                        var initialConvert = _bound.Convert(operatorInfo.BoolOperator.OperandType, expr, conversion);

                        expr = _bound.Unary(operatorInfo.BoolOperator.Kind, boolType, initialConvert, operatorInfo.BoolOperator.Method);

                        if (operatorKind == BinaryOperatorKind.Equal)
                        {
                            expr = _bound.Not(expr);
                        }
                    }
                    else if (!conversion.IsIdentity)
                    {
                        expr = _bound.Convert(boolType, expr, conversion);
                    }

                    return expr;
                }

                BoundExpression getElementEqualityCheckMultiple(TupleBinaryOperatorInfo.Multiple operatorInfo, BoundParameter leftParameter, BoundParameter rightParameter)
                {
                    return _bound.TupleBinary(operatorKind, boolType, leftParameter, rightParameter, operatorInfo);
                }

                BoundExpression getElementEqualityCheckNullNull()
                {
                    return _bound.Literal(operatorKind == BinaryOperatorKind.Equal);
                }
            }
        }

        private BoundExpression VisitConvertedTupleLiteral(BoundConvertedTupleLiteral node)
        {
            //
            // BUG: node may not have a type, e.g. for (1, null) == (2, null).
            //

            var (args, argNames) = GetArgsAndNames(node);

            return CSharpExprFactory("TupleLiteral", _bound.Typeof(node.Type), args, argNames);
        }

        private BoundExpression VisitTupleLiteral(BoundTupleLiteral node)
        {
            throw ExceptionUtilities.Unreachable;
        }

        private (BoundExpression args, BoundExpression names) GetArgsAndNames(BoundTupleExpression node)
        {
            var stringType = _bound.SpecialType(SpecialType.System_String);

            var args = Expressions(node.Arguments);

            BoundExpression argNames;

            if (!node.ArgumentNamesOpt.IsDefault)
            {
                var names = ImmutableArray.CreateBuilder<BoundExpression>(node.ArgumentNamesOpt.Length);

                foreach (var argName in node.ArgumentNamesOpt)
                {
                    if (argName != null)
                    {
                        names.Add(_bound.StringLiteral(argName));
                    }
                    else
                    {
                        names.Add(_bound.Null(stringType));
                    }
                }

                argNames = _bound.ArrayOrEmpty(stringType, names.ToImmutableArray());
            }
            else
            {
                argNames = _bound.Null(_bound.ArrayType(stringType));
            }

            // REVIEW: InferredNamesOpt

            return (args, argNames);
        }

        private BoundExpression VisitDeconstructionAssignmentOperator(BoundDeconstructionAssignmentOperator node)
        {
            var left = Visit(node.Left, convertToExpressionType: false);
            var right = Visit(node.Right.Operand);
            var conversion = VisitDeconstructionConversion(node.Right.Conversion, node.Right.Operand.Type, node.Left.Type);

            return CSharpExprFactory("DeconstructionAssignment", _bound.Typeof(node.Type), left, right, conversion);
        }

        private BoundExpression VisitDeconstructionConversion(Conversion conversion, TypeSymbol fromType, TypeSymbol toType)
        {
            Debug.Assert(conversion.Kind == ConversionKind.Deconstruction);

            //
            // E.g. (int a, (long x, double y)) = t where t is (int, Point)
            //
            // Deconstruction
            //   ElementConversions:
            //     Conversion
            //       Lambda: (int x) => x  // identity for a
            //     Deconstruction
            //       Method: Point.Deconstruct(out int x, out int y)
            //       ElementConversions: 
            //         Conversion
            //           Lambda: (int x) => (long)x    // convert x
            //         Conversion
            //           Lambda: (int x) => (double)x  // convert y
            //

            BoundExpression makeConversions(ImmutableArray<Conversion> elementConversions, ImmutableArray<TypeSymbol> sourceTypes, ImmutableArray<TypeSymbol> destTypes)
            {
                var builder = ArrayBuilder<BoundExpression>.GetInstance(elementConversions.Length);

                for (int i = 0, n = elementConversions.Length; i < n; i++)
                {
                    var elementConversion = elementConversions[i];

                    BoundExpression conversionExpr;

                    if (elementConversion.Kind == ConversionKind.Deconstruction)
                    {
                        conversionExpr = VisitDeconstructionConversion(elementConversion, sourceTypes[i], destTypes[i]);
                    }
                    else
                    {
                        conversionExpr = CSharpExprFactory("Conversion", MakeConversionLambda(elementConversion, sourceTypes[i], destTypes[i]));
                    }

                    builder.Add(conversionExpr);
                }

                return _bound.Array(CSharp_Expressions_ConversionType, builder.ToImmutableAndFree());
            }

            if (conversion.DeconstructionInfo.IsDefault)
            {
                Debug.Assert(fromType.IsTupleType);
                Debug.Assert(toType.IsTupleType);

                var sourceTypes = fromType.TupleElementTypesWithAnnotations.SelectAsArray(t => t.Type);
                var destTypes = toType.TupleElementTypesWithAnnotations.SelectAsArray(t => t.Type);

                var conversions = makeConversions(conversion.UnderlyingConversions, sourceTypes, destTypes);

                return CSharpExprFactory("Deconstruction", _bound.Typeof(fromType), _bound.Typeof(toType), conversions);
            }
            else
            {
                Debug.Assert(toType.IsTupleType);

                var sourceTypes = conversion.DeconstructionInfo.OutputPlaceholders.SelectAsArray(t => t.Type);
                var destTypes = toType.TupleElementTypesWithAnnotations.SelectAsArray(t => t.Type);

                var deconstruct = makeDeconstructLambda(conversion.DeconstructionInfo);

                var conversions = makeConversions(conversion.UnderlyingConversions, sourceTypes, destTypes);

                return CSharpExprFactory("Deconstruction", _bound.Typeof(fromType), _bound.Typeof(toType), deconstruct, conversions);

                BoundExpression makeDeconstructLambda(DeconstructMethodInfo info)
                {
                    var replacements = new Dictionary<BoundDeconstructValuePlaceholder, BoundExpression>();
                    var locals = ImmutableArray.CreateBuilder<LocalSymbol>(1 + info.OutputPlaceholders.Length);
                    var lambdaParams = ImmutableArray.CreateBuilder<BoundExpression>(1 + info.OutputPlaceholders.Length);
                    var assignments = ImmutableArray.CreateBuilder<BoundExpression>(1 + info.OutputPlaceholders.Length);
                    var parameterMapKeys = new List<ParameterSymbol>(1 + info.OutputPlaceholders.Length);

                    var inputParameterSymbol = _bound.SynthesizedParameter(info.InputPlaceholder.Type, "i");
                    var inputParameter = _bound.Parameter(inputParameterSymbol);
                    replacements.Add(info.InputPlaceholder, inputParameter);

                    var inputParamExprSymbol = _bound.SynthesizedLocal(ParameterExpressionType);
                    locals.Add(inputParamExprSymbol);

                    var inputParamExpr = _bound.Local(inputParamExprSymbol);
                    lambdaParams.Add(inputParamExpr);

                    _parameterMap.Add(inputParameterSymbol, inputParamExpr);
                    parameterMapKeys.Add(inputParameterSymbol);

                    var inputParam = ExprFactory("Parameter", _bound.Typeof(info.InputPlaceholder.Type), _bound.Literal("i"));
                    assignments.Add(_bound.AssignmentExpression(inputParamExpr, inputParam));

                    for (int i = 0, n = info.OutputPlaceholders.Length; i < n; i++)
                    {
                        var outputPlaceholder = info.OutputPlaceholders[i];
                        var name = "o" + i;

                        var outputParameterSymbol = _bound.SynthesizedParameter(outputPlaceholder.Type, name, refKind: RefKind.Out);
                        var outputParameter = _bound.Parameter(outputParameterSymbol);
                        replacements.Add(outputPlaceholder, outputParameter);

                        var outputParamExprSymbol = _bound.SynthesizedLocal(ParameterExpressionType);
                        locals.Add(outputParamExprSymbol);

                        var outputParamExpr = _bound.Local(outputParamExprSymbol);
                        lambdaParams.Add(outputParamExpr);

                        _parameterMap.Add(outputParameterSymbol, outputParamExpr);
                        parameterMapKeys.Add(outputParameterSymbol);

                        var outputParam = ExprFactory("Parameter", _bound.InstanceCall(_bound.Typeof(outputPlaceholder.Type), "MakeByRefType"), _bound.Literal(name));
                        assignments.Add(_bound.AssignmentExpression(outputParamExpr, outputParam));
                    }

                    var rewrittenInvocation = (BoundExpression)new DeconstructValuePlaceholderSubstitutor(replacements).Visit(info.Invocation);
                    var body = Visit(rewrittenInvocation);

                    foreach (var parameterMapKey in parameterMapKeys)
                    {
                        _parameterMap.Remove(parameterMapKey);
                    }

                    var result = _bound.Sequence(
                        locals.ToImmutable(),
                        assignments.ToImmutable(),
                        CSharpExprFactory(
                            "DeconstructLambda",
                            body,
                            _bound.ArrayOrEmpty(ParameterExpressionType, lambdaParams.ToImmutable())));

                    return result;
                }
            }
        }

        private sealed class DeconstructValuePlaceholderSubstitutor : BoundTreeRewriterWithStackGuard
        {
            private readonly Dictionary<BoundDeconstructValuePlaceholder, BoundExpression> _replacements;

            public DeconstructValuePlaceholderSubstitutor(Dictionary<BoundDeconstructValuePlaceholder, BoundExpression> replacements)
            {
                _replacements = replacements;
            }

            public override BoundNode VisitDeconstructValuePlaceholder(BoundDeconstructValuePlaceholder node)
            {
                return _replacements[node];
            }
        }
    }
}
