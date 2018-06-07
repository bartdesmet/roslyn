# Expression Types

Extend language support for expression trees to support *expression types* that support language-defined factory methods to construct expression nodes.

## Introduction

C# has supported quotations starting with C# 3.0 using the `System.Linq.Expressions` types. An example is shown below:

```csharp
Expression<Func<int, int>> f = x => x + 42;
```

This code compiles to the following equivalent:

```csharp
ParameterExpression p0 = Expression.Parameter(typeof(int), "x");
Expression<Func<int, int>> f =
    Expression.Lambda<Func<int, int>>(
        Expression.Add(
            x,
            Expression.Constant(42, typeof(int))
        )
    );
```

Various language constructs can be used within expression trees, each having a corresponding factory method to construct an expression tree node. Right now, the C# compiler looks for these factory methods on `System.Linq.Expressions.Expression`.

The language feature discussed in this document introduces *expression types* which enable the use of the C# expression tree language feature to produce expression types different from `System.Linq.Expressions.Expression`.

## Rationale

Supporting custom *expression types* enables various scenarios:

* Building LINQ providers or other libraries that leverage expression trees but restrict the language constructs that can be captured during lambda expression to expression tree conversion, by omitting various factory methods from the custom expression tree type's factory type.
* Introducing support for new language constructs that can be captured in expression trees whereby new node types can be introduced in a library other than `System.Linq.Expressions.dll` (or `System.Core.dll`).

## Design

An *expression type* is defined as either the existing `System.Linq.Expressions.Expression<TDelegate>` type or any generic type with exactly one type parameter, annotated with an `ExpressionBuilder` attribute, as shown below:

```csharp
[ExpressionBuilder(typeof(Quote))]
public class Quote<T>
{
    // ...
}

namespace System.Runtime.CompilerServices
{
    [AttributeUsage(AttributeTargets.Class)]
    public class ExpressionBuilderAttribute : Attribute
    {
        public ExpressionBuilderAttribute(Type type) { }
    }
}
```

In this example, `Quote<T>` is treated as an `expression type`, which can be used for assignment by a lambda expression. The definition shown above enables users to write:

```csharp
Quote<Func<int, int>> f = x => x + 42;
```

If the target type of the lambda expression conversion is not an `expression type`, the following (existing) error occurs:

```
error CS1660: Cannot convert lambda expression to type 'Quote<Func<int, int>>' because it is not a delegate type
```

With the introduction of *expression types*, this error no longer occurs, and the compiler attempts to emit factory calls against the type specified in `ExpressionBuilder`, i.e. `Quote` in our example above:

```csharp
var p0 = Quote.Parameter(typeof(int), "x");
Quote<Func<int, int>> f =
    Quote.Lambda<Func<int, int>>(
        Quote.Add(
            x,
            Quote.Constant(42, typeof(int))
        )
    );
```

If any of these methods is not found, the following types of errors occur:

```
error CS0117: 'Quote' does not contain a definition for 'Parameter'
error CS0117: 'Quote' does not contain a definition for 'Constant'
error CS0117: 'Quote' does not contain a definition for 'Add'
error CS0117: 'Quote' does not contain a definition for 'Lambda'
```

An example of a `Quote` type supporting these expression nodes is shown below:

```csharp
public class Quote
{
    public static ConstantExpression Constant(object value, Type type) => Expression.Constant(value, type);
    public static ParameterExpression Parameter(Type type, string name) => Expression.Parameter(type, name);
    public static BinaryExpression Add(Expression left, Expression right) => Expression.Add(left, right);
    public static Quote<T> Lambda<T>(Expression body, ParameterExpression[] parameters) => new Quote<T>(Expression.Lambda<T>(body, parameters));
}

[ExpressionBuilder(typeof(Quote))]
public class Quote<T>
{
    public Quote(Expression<T> expression) => Expression = expression;

    public Expression<T> Expression { get; }
}
```

In this case, we simply subset the available expressions in `System.Linq.Expressions` and introduce a wrapper for `Expression<T>` to build the top-level `Quote<T>`. However, it's equally valid for the user to provide alternative implementations of these node types by returning different types from these factory methods.