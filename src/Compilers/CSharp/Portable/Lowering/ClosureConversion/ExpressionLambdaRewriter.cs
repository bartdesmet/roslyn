// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.PooledObjects;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    internal class ExpressionLambdaRewriter // this is like a bound tree rewriter, but only handles a small subset of node kinds
    {
        private readonly SyntheticBoundNodeFactory _bound;
        private readonly TypeMap _typeMap;
        private readonly Dictionary<ParameterSymbol, BoundExpression> _parameterMap = new Dictionary<ParameterSymbol, BoundExpression>();
        private readonly bool _ignoreAccessibility;
        private int _recursionDepth;
        private bool _usingOriginalExpressionType;

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

        private ExpressionLambdaRewriter(TypeCompilationState compilationState, TypeMap typeMap, SyntaxNode node, int recursionDepth, BindingDiagnosticBag diagnostics, NamedTypeSymbol expressionType)
        {
            if (expressionType is null)
            {
                _usingOriginalExpressionType = true;
            }

            _ExpressionType = expressionType;

            _bound = new SyntheticBoundNodeFactory(null, compilationState.Type, node, compilationState, diagnostics);
            _ignoreAccessibility = compilationState.ModuleBuilderOpt.IgnoreAccessibility;
            _int32Type = _bound.SpecialType(SpecialType.System_Int32);
            _objectType = _bound.SpecialType(SpecialType.System_Object);
            _nullableType = _bound.SpecialType(SpecialType.System_Nullable_T);
            _IEnumerableType = _bound.SpecialType(SpecialType.System_Collections_Generic_IEnumerable_T);

            _typeMap = typeMap;
            _recursionDepth = recursionDepth;
        }

        internal static BoundNode RewriteLambda(BoundLambda node, TypeCompilationState compilationState, TypeMap typeMap, int recursionDepth, BindingDiagnosticBag diagnostics, Func<BoundExpression, BoundExpression> lower)
        {
            var type = (NamedTypeSymbol)node.Type.OriginalDefinition;

            if (!GetExpressionBuilderType(type, out var expressionType))
            {
                // TODO-ETLIKE: Add better error; borrowing an error from similar code for builders now.
                diagnostics.Add(ErrorCode.ERR_BadAsyncReturn, node.Syntax.Location);
                return node;
            }

            try
            {
                var r = new ExpressionLambdaRewriter(compilationState, typeMap, node.Syntax, recursionDepth, diagnostics, expressionType);
                var result = r.VisitLambdaInternal(node);
                if (!node.Type.Equals(result.Type, TypeCompareKind.IgnoreNullableModifiersForReferenceTypes))
                {
                    // TODO-ETLIKE: Review this error; if the expression tree factories are misbehaved, we should provide better error than "can't find Lambda method".

                    diagnostics.Add(ErrorCode.ERR_MissingPredefinedMember, node.Syntax.Location, r.ExpressionType, "Lambda");
                }
                if (expressionType is not null && !result.HasErrors) // NB-ETLIKE: Custom factories rely on "params".
                {
                    // TODO-ETLIKE: This is an initial approach to allow for more lowering to take place as part of emitting factory calls to params
                    //              array methods. We should likely revisit the overall layering or do a more narrow rewrite to deal with expanded
                    //              forms here (or in the synthetic bound node factory).
                    result = lower(result);
                }
                return result;
            }
            catch (SyntheticBoundNodeFactory.MissingPredefinedMember ex)
            {
                diagnostics.Add(ex.Diagnostic);
                return node;
            }
        }

        internal static bool GetExpressionBuilderType(TypeSymbol type, out NamedTypeSymbol expressionBuilderType)
        {
            if (type.IsCustomExpressionType(out var builderType))
            {
                expressionBuilderType = ValidateBuilderType(builderType, type.DeclaredAccessibility);

                if (expressionBuilderType is null)
                {
                    return false;
                }

                return true;
            }

            expressionBuilderType = null; // NB: Indicates original System.Linq.Expressions.Expression factory.
            return true;
        }

        private static NamedTypeSymbol ValidateBuilderType(NamedTypeSymbol builderType, Accessibility declaredAccessibility)
        {
            Debug.Assert(builderType is not null);

            if (!builderType.IsErrorType() &&
                 builderType.SpecialType != SpecialType.System_Void &&
                 builderType.DeclaredAccessibility == declaredAccessibility && // TODO-ETLIKE: Review accessibility requirements.
                 !builderType.IsGenericType)
            {
                return builderType;
            }

            return null;
        }

        private BoundExpression TranslateLambdaBody(BoundBlock block)
        {
            Debug.Assert(block.Locals.IsEmpty);
            foreach (var s in block.Statements)
            {
                for (var stmt = s; stmt != null;)
                {
                    switch (stmt.Kind)
                    {
                        case BoundKind.ReturnStatement:
                            var result = Visit(((BoundReturnStatement)stmt).ExpressionOpt);
                            if (result != null)
                            {
                                return result;
                            }
                            stmt = null;
                            break;
                        case BoundKind.ExpressionStatement:
                            return Visit(((BoundExpressionStatement)stmt).Expression);
                        case BoundKind.SequencePoint:
                            stmt = ((BoundSequencePoint)stmt).StatementOpt;
                            break;
                        case BoundKind.SequencePointWithSpan:
                            stmt = ((BoundSequencePointWithSpan)stmt).StatementOpt;
                            break;
                        default:
                            throw ExceptionUtilities.UnexpectedValue(stmt.Kind);
                    }
                }
            }

            return null;
        }

        private BoundExpression Visit(BoundExpression node)
        {
            if (node == null)
            {
                return null;
            }

            SyntaxNode old = _bound.Syntax;
            _bound.Syntax = node.Syntax;
            var result = VisitInternal(node);
            _bound.Syntax = old;

            // NB-ETLIKE: When using factory methods for expression tree like types, we don't insert any conversions. We just emit the calls.
            //            I.e. we don't require the type containing the factory methods to be the base type of the expressions returned.

            if (result.HasErrors || !_usingOriginalExpressionType)
            {
                return result;
            }

            return _bound.Convert(ExpressionType, result);
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
                case BoundKind.IndexerAccess: // NB-ETLIKE: New for custom expression tree types.
                    return VisitIndexerAccess((BoundIndexerAccess)node);
                case BoundKind.IsOperator:
                    return VisitIsOperator((BoundIsOperator)node);
                case BoundKind.Lambda:
                    return VisitLambda((BoundLambda)node);
                case BoundKind.NewT:
                    return VisitNewT((BoundNewT)node);
                case BoundKind.NullCoalescingOperator:
                    return VisitNullCoalescingOperator((BoundNullCoalescingOperator)node);
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
                case BoundKind.SizeOfOperator:
                    return VisitSizeOfOperator((BoundSizeOfOperator)node);
                case BoundKind.UnaryOperator:
                    return VisitUnaryOperator((BoundUnaryOperator)node);

                case BoundKind.DefaultExpression:
                case BoundKind.HostObjectMemberReference:
                case BoundKind.Literal:
                case BoundKind.Local:
                case BoundKind.MethodInfo:
                case BoundKind.PreviousSubmissionReference:
                case BoundKind.ThisReference:
                case BoundKind.TypeOfOperator:
                    return Constant(node);
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
            var array = Visit(node.Expression);

            if (_usingOriginalExpressionType)
            {
                if (node.Indices.Length == 1)
                {
                    var arg = node.Indices[0];
                    var index = Visit(arg);
                    if (!TypeSymbol.Equals(index.Type, _int32Type, TypeCompareKind.ConsiderEverything2))
                    {
                        index = ConvertIndex(index, arg.Type, _int32Type);
                    }
                    return ExprFactory("ArrayIndex", array, index);
                }
                else
                {
                    return ExprFactory("ArrayIndex", array, Indices(node.Indices));
                }
            }
            else
            {
                var builder = ArrayBuilder<BoundExpression>.GetInstance();
                builder.Add(array);
                VisitIndices(node.Indices, builder);

                return ExprFactory("ArrayIndex", builder.ToArrayAndFree());
            }
        }

        private BoundExpression Indices(ImmutableArray<BoundExpression> expressions)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();
            VisitIndices(expressions, builder);
            return _bound.ArrayOrEmpty(ExpressionType, builder.ToImmutableAndFree());
        }

        private void VisitIndices(ImmutableArray<BoundExpression> expressions, ArrayBuilder<BoundExpression> builder)
        {
            foreach (var arg in expressions)
            {
                var index = Visit(arg);
                if (!TypeSymbol.Equals(index.Type, _int32Type, TypeCompareKind.ConsiderEverything2))
                {
                    index = ConvertIndex(index, arg.Type, _int32Type);
                }
                builder.Add(index);
            }
        }

        private BoundExpression Expressions(ImmutableArray<BoundExpression> expressions)
        {
            Debug.Assert(_usingOriginalExpressionType);

            return _bound.ArrayOrEmpty(ExpressionType, VisitExpressions(expressions));
        }

        private ImmutableArray<BoundExpression> VisitExpressions(ImmutableArray<BoundExpression> expressions)
        {
            var builder = ArrayBuilder<BoundExpression>.GetInstance();

            VisitExpressions(expressions, builder);
            
            return builder.ToImmutableAndFree();
        }

        private void VisitExpressions(ImmutableArray<BoundExpression> expressions, ArrayBuilder<BoundExpression> builder)
        {
            foreach (var arg in expressions)
            {
                builder.Add(Visit(arg));
            }
        }

        private BoundExpression VisitArrayCreation(BoundArrayCreation node)
        {
            var arrayType = (ArrayTypeSymbol)node.Type;
            var boundType = _bound.Typeof(arrayType.ElementType);
            if (node.InitializerOpt != null)
            {
                if (arrayType.IsSZArray)
                {
                    if (_usingOriginalExpressionType)
                    {
                        return ExprFactory("NewArrayInit", boundType, Expressions(node.InitializerOpt.Initializers));
                    }
                    else
                    {
                        var builder = ArrayBuilder<BoundExpression>.GetInstance();
                        builder.Add(boundType);
                        VisitExpressions(node.InitializerOpt.Initializers, builder);

                        return ExprFactory("NewArrayInit", builder.ToArrayAndFree());
                    }
                }
                else
                {
                    // error should have been reported earlier
                    // Bound.Diagnostics.Add(ErrorCode.ERR_ExpressionTreeContainsMultiDimensionalArrayInitializer, node.Syntax.Location);
                    return new BoundBadExpression(node.Syntax, default(LookupResultKind), ImmutableArray<Symbol>.Empty, ImmutableArray.Create<BoundExpression>(node), ExpressionType);
                }
            }
            else
            {
                if (_usingOriginalExpressionType)
                {
                    return ExprFactory("NewArrayBounds", boundType, Expressions(node.Bounds));
                }
                else
                {
                    var builder = ArrayBuilder<BoundExpression>.GetInstance();
                    builder.Add(boundType);
                    VisitExpressions(node.Bounds, builder);

                    return ExprFactory("NewArrayBounds", builder.ToArrayAndFree());
                }
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

        private BoundExpression VisitBaseReference(BoundBaseReference node)
        {
            // should have been reported earlier.
            // Diagnostics.Add(ErrorCode.ERR_ExpressionTreeContainsBaseAccess, node.Syntax.Location);
            return new BoundBadExpression(node.Syntax, 0, ImmutableArray<Symbol>.Empty, ImmutableArray.Create<BoundExpression>(node), ExpressionType);
        }

        private static string GetBinaryOperatorName(BinaryOperatorKind opKind, out bool isChecked, out bool isLifted, out bool requiresLifted)
        {
            isChecked = opKind.IsChecked();
            isLifted = opKind.IsLifted();
            requiresLifted = opKind.IsComparison();

            switch (opKind.Operator())
            {
                case BinaryOperatorKind.Addition: return isChecked ? "AddChecked" : "Add";
                case BinaryOperatorKind.Multiplication: return isChecked ? "MultiplyChecked" : "Multiply";
                case BinaryOperatorKind.Subtraction: return isChecked ? "SubtractChecked" : "Subtract";
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
        }

        private BoundExpression VisitBinaryOperator(BinaryOperatorKind opKind, MethodSymbol methodOpt, TypeSymbol type, BoundExpression left, BoundExpression right)
        {
            bool isChecked, isLifted, requiresLifted;
            string opName = GetBinaryOperatorName(opKind, out isChecked, out isLifted, out requiresLifted);

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
            // TODO-ETLIKE: Consider revisiting the stance on any introduced intermediate conversion nodes here and
            //              in general. Right now, erring on the side of compatibility.

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

                // NB-ETLIKE: See remarks on SyntheticConvert.
                return SyntheticConvert(loweredOperand, operand.Type, promotedType, isChecked);
            }
        }

        private BoundExpression MakeBinary(MethodSymbol methodOpt, TypeSymbol type, bool isLifted, bool requiresLifted, string opName, BoundExpression loweredLeft, BoundExpression loweredRight)
        {
            BoundExpression liftToNull() => _bound.Literal(isLifted && !TypeSymbol.Equals(methodOpt.ReturnType, type, TypeCompareKind.ConsiderEverything2));

            if (_usingOriginalExpressionType)
            {
                return
                    ((object)methodOpt == null) ? ExprFactory(opName, loweredLeft, loweredRight) :
                        requiresLifted ? ExprFactory(opName, loweredLeft, loweredRight, liftToNull(), _bound.MethodInfo(methodOpt)) :
                            ExprFactory(opName, loweredLeft, loweredRight, _bound.MethodInfo(methodOpt));
            }
            else
            {
                // TODO-ETLIKE: This approach reduces overload hell for expression tree factories. However, one could argue that having finer-grained
                //              overloads enables custom factories to be more selective about what they support. At the same time, it can be a pitfall
                //              because lacking an overlaod with e.g. MethodInfo would preclude e.g. the decimal + operator or the string == operator,
                //              which may look like arbitrary restrictions. For now, we just have one factory for each binary operator kind, and mirror
                //              the language syntax. (FWIW, this is consistent with the stance on overloads for Field and Property where the receiver
                //              is omitted for the static field/property access cases; the [Field|Property]Info represents the member, static or not;
                //              with some leap of imagination `T.F` where `T` is a type has no receiver, while `o.F` where `o` is an object does have
                //              a receiver; the latter is akin to `o.[T.F]` where `T.F` got quoted as a FieldInfo.)
                var method = methodOpt is {} m ?  _bound.MethodInfo(m) : _bound.Null(_bound.WellKnownType(WellKnownType.System_Reflection_MethodInfo));
                return requiresLifted
                    ? ExprFactory(opName, loweredLeft, loweredRight, liftToNull(), method)
                    : ExprFactory(opName, loweredLeft, loweredRight, method);
            }
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
                    return SyntheticConvert(node, type, isChecked); // NB-ETLIKE: See remarks on SyntheticConvert.
                }

                var promotedType = e.IsNullableType() ? _nullableType.Construct(PromotedType(e.GetNullableUnderlyingType())) : PromotedType(e);
                if (!TypeSymbol.Equals(promotedType, type, TypeCompareKind.ConsiderEverything2))
                {
                    return SyntheticConvert(node, type, isChecked); // NB-ETLIKE: See remarks on SyntheticConvert.
                }
            }

            return node;
        }

        private BoundExpression ConvertIndex(BoundExpression expr, TypeSymbol oldType, TypeSymbol newType)
        {
            // REVIEW-ETLIKE: This may introduce Convert nodes that are unexpected to some custom expression tree types.
            //                We could consider to make compiler-synthesized conversions explicit through the factory
            //                calls, so the expression tree can differentiate between what the user wrote and what the
            //                bound tree will all required info for runtime evaluation looks like.

            var useSiteInfo = new CompoundUseSiteInfo<AssemblySymbol>(Diagnostics, _bound.Compilation.Assembly);
            var kind = _bound.Compilation.Conversions.ClassifyConversionFromType(oldType, newType, ref useSiteInfo).Kind;
            Debug.Assert(useSiteInfo.Diagnostics.IsNullOrEmpty());
            Diagnostics.AddDependencies(useSiteInfo);

            switch (kind)
            {
                case ConversionKind.Identity:
                    return expr;
                case ConversionKind.ExplicitNumeric:
                    // REVIEW-ETLIKE: Checked behavior here is interesting. One could wonder whether the Boolean flag was
                    //                originally (prior to adding the named parameter to clarify) meant to indicate
                    //                'explicit' rather than 'checked'. Case in point:
                    //
                    //                  (new int[0])[long.MaxValue]
                    //
                    //                throws OverflowException when evaluated through an expression tree, but
                    //                throws IndexOutOfRangeException when evaluated otherwise.
                    //
                    //                We'll keep the quirk for now but but could consider adjusting the behavior when
                    //                targeting custom expression tree types.

                    return SyntheticConvert(expr, newType, isChecked: true); // NB-ETLIKE: See remarks on SyntheticConvert.
                default:
                    return SyntheticConvert(expr, _int32Type, isChecked: false); // NB-ETLIKE: See remarks on SyntheticConvert.
            }
        }

        private BoundExpression VisitCall(BoundCall node)
        {
            if (node.IsDelegateCall)
            {
                if (_usingOriginalExpressionType)
                {
                    // Generate Expression.Invoke(Receiver, arguments)
                    return ExprFactory(WellKnownMemberNames.DelegateInvokeName, Visit(node.ReceiverOpt), Expressions(node.Arguments));
                }
                else
                {
                    var builder = ArrayBuilder<BoundExpression>.GetInstance();
                    builder.Add(Visit(node.ReceiverOpt));
                    VisitExpressions(node.Arguments, builder);
                    return ExprFactory(WellKnownMemberNames.DelegateInvokeName, builder.ToArrayAndFree());
                }
            }
            else
            {
                var method = node.Method;

                if (_usingOriginalExpressionType)
                {
                    // Generate Expression.Call(Receiver, Method, [typeArguments,] arguments)
                    return ExprFactory(
                        "Call",
                        method.RequiresInstanceReceiver ? Visit(node.ReceiverOpt) : _bound.Null(ExpressionType),
                        _bound.MethodInfo(method),
                        Expressions(node.Arguments));
                }
                else
                {
                    var builder = ArrayBuilder<BoundExpression>.GetInstance();

                    if (method.RequiresInstanceReceiver)
                    {
                        builder.Add(Visit(node.ReceiverOpt));
                    }
                    
                    builder.Add(_bound.MethodInfo(method));
                    VisitExpressions(node.Arguments, builder);

                    return ExprFactory("Call", builder.ToArrayAndFree());
                }
            }
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
                            ? SyntheticConvert(Visit(node.Operand), node.Operand.Type, method.Parameters[0].Type, node.Checked)
                            : Visit(node.Operand);
                        // TODO-ETLIKE: Decide on the factory shape for conversions. The syntatic form is (T)expr, so an order with Type first would make
                        //              more sense (and also look much better when explicitly using these factories, not having to have some trailing
                        //              argument for the type after some big operand expression). The method info is additional info about the "how" and
                        //              could be wrapped in a ConvertInfo object (where other things like a Checked flag could be kept, or a Type for the
                        //              context in which a dynamic conversion takes place).
                        var e2 = Convert(e1, resultType, isChecked: false, method);
                        return SyntheticConvert(e2, resultType, node.Type, node.Checked);
                    }
                case ConversionKind.ImplicitReference:
                case ConversionKind.Identity:
                    {
                        var operand = Visit(node.Operand);
                        return node.ExplicitCastInCode ? Convert(operand, node.Type, isChecked: false) : operand;
                    }
                case ConversionKind.ImplicitNullable:
                    if (node.Operand.Type.IsNullableType())
                    {
                        return Convert(Visit(node.Operand), node.Operand.Type, node.Type, node.Checked, node.ExplicitCastInCode);
                    }
                    else
                    {
                        // the native compiler performs this conversion in two steps, so we follow suit
                        // REVIEW-ETLIKE: Keep this behavior (albeit pointed out through "synthetic" nodes) or reduce quirks?
                        var nullable = (NamedTypeSymbol)node.Type;
                        var intermediate = nullable.TypeArgumentsWithAnnotationsNoUseSiteDiagnostics[0].Type;
                        var e1 = SyntheticConvert(Visit(node.Operand), node.Operand.Type, intermediate, node.Checked);
                        return SyntheticConvert(e1, intermediate, node.Type, node.Checked);
                    }
                case ConversionKind.NullLiteral:
                    return Convert(Constant(_bound.Null(_objectType)), _objectType, node.Type, isChecked: false, node.ExplicitCastInCode);
                default:
                    return Convert(Visit(node.Operand), node.Operand.Type, node.Type, node.Checked, node.ExplicitCastInCode);
            }
        }

        private BoundExpression Convert(BoundExpression operand, TypeSymbol oldType, TypeSymbol newType, bool isChecked, bool explicitCastInCode)
        {
            if (_usingOriginalExpressionType)
            {
                return (TypeSymbol.Equals(oldType, newType, TypeCompareKind.ConsiderEverything2) && !explicitCastInCode) ? operand : Convert(operand, newType, isChecked);
            }
            else
            {
                // NB-ETLIKE: Only gets dropped if the user didn't write the cast in order to track the syntactic form closely.
                if (explicitCastInCode)
                {
                    return Convert(operand, newType, isChecked);
                }
                else
                {
                    if (TypeSymbol.Equals(oldType, newType, TypeCompareKind.ConsiderEverything2))
                    {
                        return operand;
                    }

                    // NB: The user didn't write it, so it ought to be synthetic.
                    return SyntheticConvert(operand, oldType, newType, isChecked);
                }
            }
        }

        private BoundExpression Convert(BoundExpression expr, TypeSymbol type, bool isChecked, MethodSymbol method = null)
        {
            // TODO-ETLIKE: We could consider (also for unary and binary operators) to represent all the relevant binding/context info as an "Info" node
            //              in the tree, which may contain flags like CompilerGenerated (for synthetic nodes that got injected), Checked, etc. For a
            //              conversion, there are two pieces that are visible to the user for an explicit conversion, i.e. the type and the expression.
            //              Anything else, such as a MethodInfo, flags, etc. is not visible and could be bundled in the info object. This also naturally
            //              adds support for dynamic where more such info is needed. (One can argue about checked(expr) versus a checked context, where
            //              the former has syntactic presence and the latter is implicit.)

            if (_usingOriginalExpressionType)
            {
                var factory = isChecked ? "ConvertChecked" : "Convert";

                return method is { } m
                    ? ExprFactory(factory, expr, _bound.Typeof(type), _bound.MethodInfo(m))
                    : ExprFactory(factory, expr, _bound.Typeof(type));
            }
            else
            {
                // NB-ETLIKE: Tracking the syntactic form (T)expr left-to-right as a stance for consistency (cf. Call where the MethodInfo is
                //            also sandwiched between the receiver and the argument list).
                //
                //            Keeping the Checked suffix for consistency with other binary and unary nodes, and not yet embarking on some form
                //            of "info" node to capture details about the conversion.
                //
                //            Always have a MethodInfo parameter that can be null to reduce overload hell (see binary and unary nodes for the
                //            same approach).

                var methodInfo = method is { } m ? _bound.MethodInfo(m) : _bound.Null(_bound.WellKnownType(WellKnownType.System_Reflection_MethodInfo));
                return ExprFactory(isChecked ? "ConvertChecked" : "Convert", _bound.Typeof(type), expr, methodInfo);
            }
        }

        // NB-ETLIKE: When the user didn't write some conversion that got injected by the rewriter, we allow for custom expression trees to distinguish
        //            between synthetic compiler-generated conversions and others. So we route everything not written by the user through SyntheticConvert,
        //            which is a no-op in the original expression tree case.

        private BoundExpression SyntheticConvert(BoundExpression operand, TypeSymbol oldType, TypeSymbol newType, bool isChecked)
        {
            // NB-ETLIKE: The user never wrote synthetic converts themselves, so we can drop them if not nedded.
            return TypeSymbol.Equals(oldType, newType, TypeCompareKind.ConsiderEverything2) ? operand : SyntheticConvert(operand, newType, isChecked);
        }

        private BoundExpression SyntheticConvert(BoundExpression expr, TypeSymbol type, bool isChecked)
        {
            if (_usingOriginalExpressionType)
            {
                return Convert(expr, type, isChecked);
            }
            else
            {
                // TODO-ETLIKE: Decide how to model these synthetic nodes. For now, to spot all cases where such nodes end up in the tree, we'll use
                //              another factory method but alternatives should be considered (e.g. a flag, or simply not distinguish and be fine with
                //              a status quo on possibly seemingly arbitrary conversion nodes ending up in trees).
                return ExprFactory(isChecked ? "SyntheticConvertChecked" : "SyntheticConvert", _bound.Typeof(type), expr);
            }
        }

        private BoundExpression DelegateCreation(BoundExpression receiver, MethodSymbol method, TypeSymbol delegateType, bool requiresInstanceReceiver)
        {
            // TODO-ETLIKE: Not touching this piece for now, but it is problematic that the delegate creation evaporates in favor
            //              of a synthesized "Call" node where the receiver and method get buried. Idealy, this reduction happens
            //              at runtime if the goal is to compile and evaluate the expression. Given the receiver, method, and
            //              delegate type, it can easily craft a MethodInfo.CreateDelegate call.

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
            return Convert(Visit(unquoted), delegateType, isChecked: false);
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
                return DelegateCreation(node.Argument, d.DelegateInvokeMethod, node.Type, requiresInstanceReceiver: false);
            }

            // there should be no other cases.  Have we missed one?
            throw ExceptionUtilities.UnexpectedValue(node.Argument);
        }

        private BoundExpression VisitFieldAccess(BoundFieldAccess node)
        {
            var field = _bound.FieldInfo(node.FieldSymbol);

            if (_usingOriginalExpressionType)
            {
                var receiver = node.FieldSymbol.IsStatic ? _bound.Null(ExpressionType) : Visit(node.ReceiverOpt);
                return ExprFactory("Field", receiver, field);
            }
            else
            {
                return node.FieldSymbol.IsStatic
                    ? ExprFactory("Field", field)
                    : ExprFactory("Field", Visit(node.ReceiverOpt), field);
            }
        }

        private BoundExpression VisitIndexerAccess(BoundIndexerAccess node)
        {
            Debug.Assert(!_usingOriginalExpressionType);

            var indexer = node.Indexer;
            var method = indexer.GetOwnOrInheritedGetMethod(); // NB-ETLIKE: In this prototype, we don't add support for e.g. Assign, so only expect getters.

            var args = ArrayBuilder<BoundExpression>.GetInstance();
            args.Add(Visit(node.ReceiverOpt!)); // TODO-ETLIKE: Can receiver be null?
            args.Add(_bound.MethodInfo(method));
            VisitExpressions(node.Arguments, args);

            return ExprFactory("Index", args.ToArrayAndFree());
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
            if (node.Type.IsExpressionTree())
            {
                if (!GetExpressionBuilderType(node.Type, out var builderType))
                {
                    // TODO-ETLIKE: Add better error; borrowing an error from similar code for builders now.
                    _bound.Diagnostics.Add(ErrorCode.ERR_BadAsyncReturn, node.Syntax.Location);
                    return node;
                }

                // NB-ETLIKE: This supports quotation across different families of expression tree types.

                var (expressionType, wasUsingOriginalExpressionType) = (_ExpressionType, _usingOriginalExpressionType);

                _ExpressionType = builderType;
                _usingOriginalExpressionType = builderType is null;

                var result = VisitLambdaInternal(node);

                (_ExpressionType, _usingOriginalExpressionType) = (expressionType, wasUsingOriginalExpressionType);

                return ExprFactory("Quote", result);
            }
            else
            {
                return VisitLambdaInternal(node);
            }
        }

        private BoundExpression VisitLambdaInternal(BoundLambda node)
        {
            // prepare parameters so that they can be seen later
            var locals = ArrayBuilder<LocalSymbol>.GetInstance();
            var initializers = ArrayBuilder<BoundExpression>.GetInstance();
            var parameters = ArrayBuilder<BoundExpression>.GetInstance();
            foreach (var p in node.Symbol.Parameters)
            {
                var parameter = ExprFactory(
                    "Parameter",
                    _bound.Typeof(_typeMap.SubstituteType(p.Type).Type), _bound.Literal(p.Name));
                var param = _bound.SynthesizedLocal(_usingOriginalExpressionType ? ParameterExpressionType : parameter.Type);
                locals.Add(param);
                var parameterReference = _bound.Local(param);
                initializers.Add(_bound.AssignmentExpression(parameterReference, parameter));
                parameters.Add(parameterReference);
                _parameterMap[p] = parameterReference;
            }

            var underlyingDelegateType = node.Type.GetDelegateType();
            var typeArgs = ImmutableArray.Create<TypeSymbol>(underlyingDelegateType);
            var body = TranslateLambdaBody(node.Body);

            BoundExpression lambda;

            if (_usingOriginalExpressionType)
            {
                lambda = ExprFactory(
                    "Lambda",
                    typeArgs,
                    body,
                    _bound.ArrayOrEmpty(ParameterExpressionType, parameters.ToImmutableAndFree()));
            }
            else
            {
                var args = ArrayBuilder<BoundExpression>.GetInstance();
                args.Add(body);
                args.AddRange(parameters); // NB: Use expanded form for the call, allowing to infer the params array element type.

                lambda = ExprFactory(
                    "Lambda",
                    typeArgs,
                    args.ToArrayAndFree());
            }

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
                // REVIEW-ETLIKE: Many new nodes use "placeholders", so it may be worth revisiting how we represent these rather than
                //                piggybacking on the "Lambda" factory. Or, we could down the route of modeling Conversion as a node
                //                itself and use it throughout. This would also be useful for e.g. compound assignments that have more
                //                than one conversion, foreach loop element conversions, etc. etc.
                Debug.Assert(node.LeftPlaceholder is not null);
                TypeSymbol lambdaParamType = node.LeftPlaceholder.Type;
                return ExprFactory("Coalesce", left, right, MakeConversionLambda(leftConversion, lambdaParamType, node.LeftConversion.Type));
            }
            else
            {
                return ExprFactory("Coalesce", left, right);
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
                MakeNonGenericLambda(convertedValue, ImmutableArray.Create<BoundExpression>(parameterReference)));
            return result;
        }

        private BoundExpression MakeNonGenericLambda(BoundExpression body, ImmutableArray<BoundExpression> parameters)
        {
            if (_usingOriginalExpressionType)
            {
                return ExprFactory("Lambda", body, _bound.ArrayOrEmpty(ParameterExpressionType, parameters));
            }
            else
            {
                var builder = ArrayBuilder<BoundExpression>.GetInstance();
                builder.Add(body);
                builder.AddRange(parameters);
                return ExprFactory("Lambda", builder.ToArrayAndFree());
            }
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

        private BoundExpression CreateInitializerArrayOrSingle(ArrayBuilder<BoundExpression> builder, InitializerKind kind)
        {
            Debug.Assert(_usingOriginalExpressionType);

            return kind switch
            {
                InitializerKind.MemberInitializer => _bound.ArrayOrEmpty(MemberBindingType, builder.ToImmutableAndFree()),
                InitializerKind.CollectionInitializer => _bound.ArrayOrEmpty(ElementInitType, builder.ToImmutableAndFree()),
                InitializerKind.Expression => builder.First(),
                _ => throw ExceptionUtilities.UnexpectedValue(kind),
            };
        }

        private void VisitInitializer(BoundExpression node, ArrayBuilder<BoundExpression> builder, out InitializerKind kind)
        {
            switch (node.Kind)
            {
                case BoundKind.ObjectInitializerExpression:
                    {
                        var oi = (BoundObjectInitializerExpression)node;
                        foreach (BoundAssignmentOperator a in oi.Initializers)
                        {
                            var sym = ((BoundObjectInitializerMember)a.Left).MemberSymbol;

                            // An error is reported in diagnostics pass when a dynamic object initializer is encountered in an ET:
                            Debug.Assert((object)sym != null);

                            var (factoryName, left) = a.Right.Kind switch
                            {
                                BoundKind.CollectionInitializerExpression => ("ListBind", InitializerMemberGetter(sym)),
                                BoundKind.ObjectInitializerExpression => ("MemberBind", InitializerMemberGetter(sym)),
                                _ => ("Bind", InitializerMemberSetter(sym)),
                            };

                            var innerBuilder = ArrayBuilder<BoundExpression>.GetInstance();

                            if (!_usingOriginalExpressionType)
                            {
                                innerBuilder.Add(left);
                            }

                            VisitInitializer(a.Right, innerBuilder, out var elementKind);

                            var expr = _usingOriginalExpressionType
                                ? ExprFactory(factoryName, left, CreateInitializerArrayOrSingle(innerBuilder, elementKind))
                                : ExprFactory(factoryName, innerBuilder.ToArrayAndFree());

                            builder.Add(expr);
                        }

                        kind = InitializerKind.MemberInitializer;
                        break;
                    }

                case BoundKind.CollectionInitializerExpression:
                    {
                        var ci = (BoundCollectionInitializerExpression)node;
                        Debug.Assert(ci.Initializers.Length != 0);
                        kind = InitializerKind.CollectionInitializer;

                        // The method invocation must be a static call. 
                        // Dynamic calls are not allowed in ETs, an error is reported in diagnostics pass.
                        foreach (BoundCollectionElementInitializer i in ci.Initializers)
                        {
                            BoundExpression elementInit;
                            if (_usingOriginalExpressionType)
                            {
                                elementInit = ExprFactory("ElementInit", _bound.MethodInfo(i.AddMethod), Expressions(i.Arguments));
                            }
                            else
                            {
                                var elementInitArgsBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                                elementInitArgsBuilder.Add(_bound.MethodInfo(i.AddMethod));
                                VisitExpressions(i.Arguments, elementInitArgsBuilder);
                                elementInit = ExprFactory("ElementInit", elementInitArgsBuilder.ToArrayAndFree());
                            }
                            builder.Add(elementInit);
                        }
                        break;
                    }

                default:
                    {
                        kind = InitializerKind.Expression;
                        builder.Add(Visit(node));
                        break;
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

            if (_usingOriginalExpressionType)
            {
                var builder = ArrayBuilder<BoundExpression>.GetInstance();
                VisitInitializer(initializerExpressionOpt, builder, out var initializerKind);
                var init = CreateInitializerArrayOrSingle(builder, initializerKind);

                return ExprFactory(getFactoryName(initializerKind), result, init);
            }
            else
            {
                var builder = ArrayBuilder<BoundExpression>.GetInstance();
                builder.Add(result);
                VisitInitializer(initializerExpressionOpt, builder, out var initializerKind);

                return ExprFactory(getFactoryName(initializerKind), builder.ToArrayAndFree());
            }

            string getFactoryName(InitializerKind initializerKind)
            {
                return initializerKind switch
                {
                    InitializerKind.CollectionInitializer => "ListInit",
                    InitializerKind.MemberInitializer => "MemberInit",
                    _ => throw ExceptionUtilities.UnexpectedValue(initializerKind) // no other options at the top level of an initializer
                };
            }
        }

        private BoundExpression VisitObjectCreationExpressionInternal(BoundObjectCreationExpression node)
        {
            if (node.ConstantValue != null)
            {
                // typically a decimal constant.
                return Constant(node);
            }

            if ((object)node.Constructor == null ||
                (node.Arguments.Length == 0 && !node.Type.IsStructType()) ||
                node.Constructor.IsDefaultValueTypeConstructor())
            {
                return ExprFactory("New", _bound.Typeof(node.Type));
            }

            var ctor = _bound.ConstructorInfo(node.Constructor);

            BoundExpression getAnonynousTypeMembersArray()
            {
                var anonType = (NamedTypeSymbol)node.Type;
                var membersBuilder = ArrayBuilder<BoundExpression>.GetInstance();
                for (int i = 0; i < node.Arguments.Length; i++)
                {
                    membersBuilder.Add(_bound.MethodInfo(AnonymousTypeManager.GetAnonymousTypeProperty(anonType, i).GetMethod));
                }
                return _bound.ArrayOrEmpty(MemberInfoType, membersBuilder.ToImmutableAndFree());
            }

            if (_usingOriginalExpressionType)
            {
                var args = _bound.Convert(_IEnumerableType.Construct(ExpressionType), Expressions(node.Arguments));
                if (node.Type.IsAnonymousType && node.Arguments.Length != 0)
                {
                    return ExprFactory("New", ctor, args, getAnonynousTypeMembersArray());
                }
                else
                {
                    return ExprFactory("New", ctor, args);
                }
            }
            else
            {
                var builder = ArrayBuilder<BoundExpression>.GetInstance();
                builder.Add(ctor);

                if (node.Type.IsAnonymousType && node.Arguments.Length != 0)
                {
                    // NB-ETLIKE: We put the array of MemberInfo first because it's "info" just like the constructor. In
                    //            addition, it enables passing the arguments as a params array. Though we likely want to
                    //            have an ArgumentList node going forward to model argument lists for New, Call, Invoke,
                    //            and Index with support for named/optional parameters. If that's the case, the New node
                    //            factory would take the "info" (either a Type or a ConstructorInfo with optional list of
                    //            members in case of anonymous types) and the argument list as two parameters. It is then
                    //            debatable whether the members array is passed separately or gets "zipped" with the list
                    //            of arguments. (FWIW, record types share some traits with anonymous types where there's
                    //            some form of referential transparency across constructor parameters and members being
                    //            initialized.)
                    builder.Add(getAnonynousTypeMembersArray());
                }

                VisitExpressions(node.Arguments, builder);

                return ExprFactory("New", builder.ToArrayAndFree());
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
            var getMethod = node.PropertySymbol.GetOwnOrInheritedGetMethod();
            var getMethodInfo = _bound.MethodInfo(getMethod);

            if (_usingOriginalExpressionType)
            {
                var receiver = node.PropertySymbol.IsStatic ? _bound.Null(ExpressionType) : Visit(node.ReceiverOpt);

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

                return ExprFactory("Property", receiver, getMethodInfo);
            }
            else
            {
                return node.PropertySymbol.IsStatic
                    ? ExprFactory("Property", getMethodInfo)
                    : ExprFactory("Property", Visit(node.ReceiverOpt), getMethodInfo);
            }
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
                    // NB-ETLIKE: Track the syntatic form closer, even if `+` is a no-op.
                    if ((object)node.MethodOpt == null && !_usingOriginalExpressionType)
                    {
                        return loweredArg;
                    }
                    opname = "UnaryPlus";
                    break;
                case UnaryOperatorKind.UnaryMinus:
                    opname = isChecked ? "NegateChecked" : "Negate";
                    break;
                case UnaryOperatorKind.BitwiseComplement:
                    opname = _usingOriginalExpressionType ? "Not" : "BitwiseComplement";
                    break;
                case UnaryOperatorKind.LogicalNegation:
                    opname = "Not";
                    break;
                default:
                    throw ExceptionUtilities.UnexpectedValue(op);
            }

            if (_usingOriginalExpressionType)
            {
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
            else
            {
                // NB-ETLIKE: See remarks on Binary about reduction of overloads by always having a MethodInfo that can be null.
                //            We can consider alternatives, including having the overloads, or (better) have an "info" node that
                //            captures additional info about the operation (checked context, method implementing the operation,
                //            dynamic context type, etc.).
                var method = node.MethodOpt is {} m ? _bound.MethodInfo(m) : _bound.Null(_bound.WellKnownType(WellKnownType.System_Reflection_MethodInfo));
                return ExprFactory(opname, loweredArg, method);
            }
        }

        // ======================================================

        private BoundExpression ExprFactory(string name, params BoundExpression[] arguments)
        {
            // TODO-ETLIKE: Do we want to inspect errors (such as method not found) here and turn them into a more specific error with wording hinting at missing members on a builder type?

            return _bound.StaticCall(ExpressionType, name, arguments);
        }

        private BoundExpression ExprFactory(string name, ImmutableArray<TypeSymbol> typeArgs, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(_ignoreAccessibility ? BinderFlags.IgnoreAccessibility : BinderFlags.None, ExpressionType, name, typeArgs, arguments);
        }

        private BoundExpression Constant(BoundExpression node)
        {
            return ExprFactory(
                "Constant",
                _bound.Convert(_objectType, node),
                _bound.Typeof(node.Type));
        }
    }
}
