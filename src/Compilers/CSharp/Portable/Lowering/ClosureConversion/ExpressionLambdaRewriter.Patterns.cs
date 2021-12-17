// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
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
        private NamedTypeSymbol _CSharpPatternType, _PositionalCSharpSubpatternType, _PropertyCSharpSubpatternType;
        private NamedTypeSymbol CSharpPatternType => _CSharpPatternType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_CSharpPattern);
        private NamedTypeSymbol PositionalCSharpSubpatternType => _PositionalCSharpSubpatternType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_PositionalCSharpSubpattern);
        private NamedTypeSymbol PropertyCSharpSubpatternType => _PropertyCSharpSubpatternType ??= _bound.WellKnownType(WellKnownType.Microsoft_CSharp_Expressions_PropertyCSharpSubpattern);

        [return: NotNullIfNotNull("node")]
        private BoundExpression? Visit(BoundPattern? node)
        {
            if (node == null)
            {
                return null;
            }

            SyntaxNode old = _bound.Syntax;
            _bound.Syntax = node.Syntax;
            var result = VisitInternal(node);
            _bound.Syntax = old;
            return _bound.Convert(CSharpPatternType, result);
        }

        private BoundExpression VisitInternal(BoundPattern node)
        {
            BoundExpression result;
            _recursionDepth++;
#if DEBUG
            int saveRecursionDepth = _recursionDepth;
#endif

            if (_recursionDepth > 1)
            {
                StackGuard.EnsureSufficientExecutionStack(_recursionDepth);

                result = VisitPatternWithoutStackGuard(node);
            }
            else
            {
                result = VisitPatternWithStackGuard(node);
            }

#if DEBUG
            Debug.Assert(saveRecursionDepth == _recursionDepth);
#endif
            _recursionDepth--;
            return result;
        }

        private BoundExpression VisitPatternWithStackGuard(BoundPattern node)
        {
            try
            {
                return VisitPatternWithoutStackGuard(node);
            }
            catch (InsufficientExecutionStackException ex)
            {
                throw new BoundTreeVisitor.CancelledByStackGuardException(ex, node);
            }
        }

        private BoundExpression VisitPatternWithoutStackGuard(BoundPattern node)
        {
            switch (node.Kind)
            {
                case BoundKind.ConstantPattern:
                    return VisitConstantPattern((BoundConstantPattern)node);
                case BoundKind.DiscardPattern:
                    return VisitDiscardPattern((BoundDiscardPattern)node);
                case BoundKind.ITuplePattern:
                    return VisitITuplePattern((BoundITuplePattern)node);
                case BoundKind.TypePattern:
                    return VisitTypePattern((BoundTypePattern)node);
                case BoundKind.BinaryPattern:
                    return VisitBinaryPattern((BoundBinaryPattern)node);
                case BoundKind.NegatedPattern:
                    return VisitNegatedPattern((BoundNegatedPattern)node);
                case BoundKind.RelationalPattern:
                    return VisitRelationalPattern((BoundRelationalPattern)node);
                case BoundKind.DeclarationPattern:
                    return VisitDeclarationPattern((BoundDeclarationPattern)node);
                case BoundKind.RecursivePattern:
                    return VisitRecursivePattern((BoundRecursivePattern)node);

                /*
                 * NB: List and slice patterns are vNext; ignore for now.
                case BoundKind.SlicePattern:
                    // BoundSlicePattern { Pattern, IndexerAccess, ReceiverPlaceholder, ArgumentPlaceholder }
                case BoundKind.ListPattern:
                    // BoundListPattern { Variable, Subpatterns, HasSlice, LengthAccess, IndexerAccess, ReceiverPlaceholder, ArgumentPlaceholder }
                 */
                
                default:
                    throw ExceptionUtilities.UnexpectedValue(node.Kind);
            }
        }

        private BoundExpression VisitConstantPattern(BoundConstantPattern node) =>
            CSharpPatternFactory(
                "Constant",
                PatternInfo(node),
                PatternConstant(node.Value)
            );

        private BoundExpression VisitDiscardPattern(BoundDiscardPattern node) =>
            CSharpPatternFactory(
                "Discard",
                PatternInfo(node)
            );

        private BoundExpression VisitITuplePattern(BoundITuplePattern node) =>
            CSharpPatternFactory(
                "ITuple",
                PatternInfo(node),
                _bound.MethodInfo(node.GetLengthMethod),
                _bound.MethodInfo(node.GetItemMethod),
                VisitSubpatterns(node.Subpatterns, VisitPositionalSubpattern, PositionalCSharpSubpatternType)
            );

        private BoundExpression VisitTypePattern(BoundTypePattern node) =>
            CSharpPatternFactory(
                "Type",
                PatternInfo(node),
                _bound.Typeof(node.DeclaredType.Type) // REVIEW: Check if we can just fish Type from BoundTypeExpression?
            );

        private BoundExpression VisitBinaryPattern(BoundBinaryPattern node) =>
            CSharpPatternFactory(
                node.Disjunction ? "Or" : "And",
                PatternInfo(node),
                Visit(node.Left),
                Visit(node.Right)
            );

        private BoundExpression VisitNegatedPattern(BoundNegatedPattern node) =>
            CSharpPatternFactory(
                "Not",
                PatternInfo(node),
                Visit(node.Negated)
            );

        private BoundExpression VisitRelationalPattern(BoundRelationalPattern node) =>
            CSharpPatternFactory(
                GetBinaryOperatorName(node.Relation, out var _, out var _, out var _),
                PatternInfo(node),
                PatternConstant(node.Value)
            );

        private BoundExpression VisitDeclarationPattern(BoundDeclarationPattern node) =>
            node.IsVar
                ?
                    CSharpPatternFactory(
                        "Var",
                        ObjectPatternInfo(node)
                    )
                :
                    CSharpPatternFactory(
                        "Declaration",
                        ObjectPatternInfo(node),
                        _bound.Typeof(node.DeclaredType.Type) // REVIEW: Check if we can just fish Type from BoundTypeExpression?
                    );

        private BoundExpression VisitRecursivePattern(BoundRecursivePattern node) =>
            CSharpPatternFactory(
                "Recursive",
                ObjectPatternInfo(node),
                _bound.TypeofOrNull(node.DeclaredType?.Type), // REVIEW: Check if we can just fish Type from BoundTypeExpression?
                _bound.MethodInfoOrNull(node.DeconstructMethod),
                VisitSubpatterns(node.Deconstruction, VisitPositionalSubpattern, PositionalCSharpSubpatternType),
                VisitSubpatterns(node.Properties, VisitPropertySubpattern, PropertyCSharpSubpatternType)
            );

        private BoundExpression VisitPositionalSubpattern(BoundPositionalSubpattern node) =>
            node.Symbol switch
            {
                FieldSymbol f =>
                    CSharpPatternFactory(
                        "PositionalSubpattern",
                        Visit(node.Pattern),
                        TupleFieldInfo(f)
                    ),
                ParameterSymbol p =>
                    CSharpPatternFactory(
                        "PositionalSubpattern",
                        Visit(node.Pattern),
                        _bound.MethodInfo((MethodSymbol)p.ContainingSymbol),
                        _bound.Literal(((MethodSymbol)p.ContainingSymbol).GetParameters().IndexOf(p))
                    ),
                _ =>
                    CSharpPatternFactory(
                        "PositionalSubpattern",
                        Visit(node.Pattern)
                    )
            };

        private BoundExpression TupleFieldInfo(FieldSymbol f) =>
            CSharpExprFactory(
                "TupleFieldInfo",
                _bound.Literal(f.Name),
                _bound.Literal(f.TupleElementIndex),
                _bound.Typeof(f.Type)
            );

        private BoundExpression VisitSubpatterns<T>(ImmutableArray<T> nodes, Func<T, BoundExpression> visit, TypeSymbol type)
            where T : BoundSubpattern
        {
            var patterns = ArrayBuilder<BoundExpression>.GetInstance();

            if (!nodes.IsDefaultOrEmpty)
            {
                foreach (var node in nodes)
                {
                    patterns.Add(visit(node));
                }
            }

            return _bound.Array(type, patterns.ToImmutableAndFree());
        }

        private BoundExpression VisitPropertySubpattern(BoundPropertySubpattern node) =>
            CSharpPatternFactory(
                "PropertySubpattern",
                Visit(node.Pattern),
                VisitPropertySubpatternMember(node.Member!), // REVIEW: Only null on error. Do we reach here?
                _bound.Literal(node.IsLengthOrCount)
            );

        private BoundExpression VisitPropertySubpatternMember(BoundPropertySubpatternMember node) =>
            node.Receiver switch
            {
                null =>
                    CSharpPatternFactory(
                        "PropertySubpatternMember",
                        FieldOrPropertyMemberInfo(node.Symbol)
                    ),
                BoundPropertySubpatternMember receiver =>
                    CSharpPatternFactory(
                        "PropertySubpatternMember",
                        VisitPropertySubpatternMember(receiver),
                        FieldOrPropertyMemberInfo(node.Symbol)
                    ),
            };

        private BoundExpression FieldOrPropertyMemberInfo(Symbol? symbol) =>
            symbol switch
            {
                FieldSymbol f when f.IsVirtualTupleField => TupleFieldInfo(f),
                FieldSymbol f => _bound.FieldInfo(f),
                PropertySymbol p => _bound.MethodInfo(p.GetOwnOrInheritedGetMethod()!), // REVIEW: Always has an accessible getter?
                _ => _bound.Null(MemberInfoType)
            };

        private BoundExpression PatternInfo(BoundPattern node) =>
            CSharpPatternFactory(
                "PatternInfo",
                _bound.Typeof(node.InputType),
                _bound.Typeof(node.NarrowedType)
            );

        private BoundExpression ObjectPatternInfo(BoundObjectPattern node) =>
            CSharpPatternFactory(
                "ObjectPatternInfo",
                PatternInfo(node),
                LookupPatternSymbol(node.Variable)
            );

        private BoundExpression PatternConstant(BoundExpression node) =>
            (node.ConstantValue?.IsNull ?? false) ? ConstantNull(node.Type) : Constant(node);

        private BoundExpression LookupPatternSymbol(Symbol? symbol)
        {
            // NB: See BindPatternDesignation in Binder_Patterns on how the symbol is constructed. In an expression tree lambda,
            //     we only expect to see LocalSymbol.

            if (symbol == null)
            {
                return _bound.Null(ParameterExpressionType);
            }

            Debug.Assert(symbol is LocalSymbol);

            return _localMap[(LocalSymbol)symbol];
        }

        private BoundExpression CSharpPatternFactory(string name, params BoundExpression[] arguments)
        {
            return _bound.StaticCall(CSharpPatternType, name, arguments);
        }
    }
}
