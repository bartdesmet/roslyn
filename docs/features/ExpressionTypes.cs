using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace System.Runtime.CompilerServices
{
    [AttributeUsage(AttributeTargets.Class)]
    public sealed class ExpressionBuilderAttribute : Attribute
    {
        public ExpressionBuilderAttribute(Type type) => Type = type;

        public Type Type { get; }
    }
}

//
// The following type documents all of the factory methods that compiler binds to. It's largely isomorphic to
// factory methods in System.Linq.Expressions.Expression, but without convenience overloads of all kinds (which
// aren't used by the compiler anyway) and with a few small differences that are outlined below.
//
// Note that the "expression tree types" prototype does not aim to add new nodes at this time (that's done in a
// different prototype altogether) but rather focuses on the problem of supporting custom expression tree types
// using custom builder types, while also enabling the code in the compiler to take different paths depending
// on target expression tree type (i.e. "classic" System.Linq.Expressions versus "custom", where the latter can
// enable new factories, different lowering strategies, reduction of odd compat quirks, etc.).
//

[ExpressionBuilder(typeof(MyExpression))] // NB: Enables discovery of Lambda<T> for "natural delegate types".
class MyExpression
{
    //
    // Unary nodes.
    //
    // NB: Factories always bind to overloads with method, which can be null. This differs from ET.
    //

    public static MyExpression UnaryPlus(MyExpression operand, MethodInfo method) => null;
    public static MyExpression Negate(MyExpression operand, MethodInfo method) => null;
    public static MyExpression NegateChecked(MyExpression operand, MethodInfo method) => null;
    public static MyExpression Not(MyExpression operand, MethodInfo method) => null;
    public static MyExpression BitwiseComplement(MyExpression operand, MethodInfo method) => null; // NB: New compared to ET.


    //
    // Binary nodes.
    //
    // NB: Factories always bind to overloads with (liftToNull and) method, which can be (false and) null. This differs from ET.
    //

    public static MyExpression Add(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression AddChecked(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression Subtract(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression SubtractChecked(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression Multiply(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression MultiplyChecked(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression Divide(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression Modulo(MyExpression left, MyExpression right, MethodInfo method) => null;

    public static MyExpression And(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression AndAlso(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression Or(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression OrElse(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression ExclusiveOr(MyExpression left, MyExpression right, MethodInfo method) => null;

    public static MyExpression LeftShift(MyExpression left, MyExpression right, MethodInfo method) => null;
    public static MyExpression RightShift(MyExpression left, MyExpression right, MethodInfo method) => null;

    public static MyExpression Equal(MyExpression left, MyExpression right, bool liftToNull, MethodInfo method) => null;
    public static MyExpression NotEqual(MyExpression left, MyExpression right, bool liftToNull, MethodInfo method) => null;
    public static MyExpression LessThan(MyExpression left, MyExpression right, bool liftToNull, MethodInfo method) => null;
    public static MyExpression LessThanOrEqual(MyExpression left, MyExpression right, bool liftToNull, MethodInfo method) => null;
    public static MyExpression GreaterThan(MyExpression left, MyExpression right, bool liftToNull, MethodInfo method) => null;
    public static MyExpression GreaterThanOrEqual(MyExpression left, MyExpression right, bool liftToNull, MethodInfo method) => null;


    //
    // Coalesce.
    //

    public static MyExpression Coalesce(MyExpression left, MyExpression right) => null;


    //
    // Condition.
    //

    public static MyExpression Condition(MyExpression test, MyExpression ifTrue, MyExpression ifFalse) => null;


    //
    // Convert.
    //
    // NB: Different parameter order compared to ET. Factories also always bind to overload with method which can be null.
    //
    // REVIEW: Rationalize classification of implicit/explicit and compiler-generated conversions ("synthetic" factories below).
    //

    public static MyExpression Convert(Type type, MyExpression operand, MethodInfo method) => null;
    public static MyExpression ConvertChecked(Type type, MyExpression operand, MethodInfo method) => null;

    public static MyExpression SyntheticConvert(Type type, MyExpression operand) => null;
    public static MyExpression SyntheticConvertChecked(Type type, MyExpression operand) => null;


    //
    // Array operations.
    //

    public static MyExpression ArrayLength(MyExpression array) => null;

    public static MyExpression ArrayIndex(MyExpression array, params MyExpression[] indices) => null;

    public static MyExpression NewArrayBounds(Type elementType, params MyExpression[] bounds) => null;
    public static MyExpression NewArrayInit(Type elementType, params MyExpression[] elements) => null;


    //
    // Constant.
    //

    public static MyExpression Constant(object value, Type type) => null;


    //
    // Lambda, parameter, quote.
    //

    public static MyExpression Lambda(MyExpression body, params MyParameterExpression[] parameters) => null; // NB: This supports Coalesce with a conversion (same as ET).
    public static MyExpression<T> Lambda<T>(MyExpression body, params MyParameterExpression[] parameters) => null;

    public static MyParameterExpression Parameter(Type type, string name) => null;

    public static MyExpression Quote(MyExpression expression) => null;
    public static MyExpression Quote(TheirExpression expression) => null; // NB: Also supports cross-family quotation.


    //
    // Type operations.
    //

    public static MyExpression TypeAs(MyExpression expression, Type type) => null;
    public static MyExpression TypeIs(MyExpression expression, Type type) => null;


    //
    // Member access.
    //

    public static MyExpression Property(MethodInfo getMethod) => null; // NB: New compared to ET.
    public static MyExpression Property(MyExpression expression, MethodInfo getMethod) => null;
    public static MyExpression Field(FieldInfo field) => null; // NB: New compared to ET.
    public static MyExpression Field(MyExpression expression, FieldInfo field) => null;
    public static MyExpression Index(MyExpression expression, MethodInfo getMethod, params MyExpression[] arguments) => null; // NB: New compared to ET.


    //
    // Invoke.
    //

    public static MyExpression Invoke(MyExpression function, params MyExpression[] arguments) => null;


    //
    // Call.
    //

    public static MyExpression Call(MethodInfo method, params MyExpression[] arguments) => null; // NB: New compared to ET.
    public static MyExpression Call(MyExpression expression, MethodInfo method, params MyExpression[] arguments) => null;


    //
    // New.
    //

    public static MyNewExpression New(Type type) => null;
    public static MyNewExpression New(ConstructorInfo constructor, params MyExpression[] arguments) => null;
    public static MyNewExpression New(ConstructorInfo constructor, MemberInfo[] members, params MyExpression[] arguments) => null; // NB: Different order compared to ET.


    //
    // ListInit.
    //

    public static MyExpression ListInit(MyNewExpression newExpression, params MyElementInit[] initializers) => null;
    public static MyElementInit ElementInit(MethodInfo add, params MyExpression[] arguments) => null;


    //
    // MemberInit.
    //

    public static MyExpression MemberInit(MyNewExpression newExpression, params MyMemberBinding[] bindings) => null;
    public static MyMemberAssignment Bind(MemberInfo member, MyExpression value) => null;
    public static MyMemberMemberBinding MemberBind(MemberInfo member, params MyMemberBinding[] bindings) => null;
    public static MyMemberListBinding ListBind(MemberInfo member, params MyElementInit[] initializers) => null;
}

class MyMemberBinding
{
}

class MyMemberAssignment : MyMemberBinding
{
}

class MyMemberMemberBinding : MyMemberBinding
{
}

class MyMemberListBinding : MyMemberBinding
{
}

class MyParameterExpression : MyExpression
{
}

class MyNewExpression : MyExpression
{
}

class MyElementInit
{
}

[ExpressionBuilder(typeof(MyExpression))] // REVIEW-ETLIKE: Should we walk the base class hierarchy to find the attribute? Then we expect Lambda<T> to return MyExpression<T>.
class MyExpression<T> : MyExpression
{
}

//
// Another minimal expression tree type to show cross-family quotation.
//

class TheirExpression
{
    public static TheirExpression<T> Lambda<T>(TheirExpression body, params TheirParameterExpression[] parameters) => null;
    public static TheirParameterExpression Parameter(Type type, string name) => null;
}

class TheirParameterExpression : TheirExpression
{
}

[ExpressionBuilder(typeof(TheirExpression))]
class TheirExpression<T> : TheirExpression
{
}

static class KeySelectorExpressionFactory // NB: No common base type for this mini-DSL.
{
    public static KeySelectorParameter Parameter(Type type, string name) => null;
    public static KeySelectorProperty Property(KeySelectorParameter obj, MethodInfo property) => null;
    public static KeySelectorExpression<T> Lambda<T>(KeySelectorProperty property, KeySelectorParameter obj) => null;
}

class KeySelectorParameter {}
class KeySelectorProperty {}

[ExpressionBuilder(typeof(KeySelectorExpressionFactory))]
class KeySelectorExpression<T>
{
}

class Program
{
    static void Main()
    {
        // Constant
        MyExpression<Func<int>> constant = () => 42;

        // Parameter
        MyExpression<Func<int, int>> parameter = x => x;

        // ArrayLength
        MyExpression<Func<int[], int>> arrayLength = xs => xs.Length;

        // ArrayIndex
        MyExpression<Func<int[], int, int>> arrayIndex = (xs, i) => xs[i];

        // TypeAs
        MyExpression<Func<object, string>> typeAs = o => o as string;

        // TypeIs
        MyExpression<Func<object, bool>> typeIs = o => o is string;

        // NewArrayInit
        MyExpression<Func<int[]>> newArrayInit = () => new int[] { 2, 3, 5 };

        // NewArrayBounds
        MyExpression<Func<int[,]>> newArrayBounds = () => new int[2, 3];

        // Add
        MyExpression<Func<int, int, int>> add = (x, y) => x + y;
        MyExpression<Func<decimal, decimal, decimal>> addCustom = (x, y) => x + y;

        // Negate
        MyExpression<Func<int, int>> negate = x => -x;
        MyExpression<Func<decimal, decimal>> negateCustom = x => -x;

        // Property
        MyExpression<Func<DateTime>> propertyStatic = () => DateTime.Now;
        MyExpression<Func<string, int>> propertyInstance = s => s.Length;

        // Field
        MyExpression<Func<string>> fieldStatic = () => string.Empty;
        MyExpression<Func<StrongBox<int>, int>> fieldInstance = b => b.Value;

        // Index
        MyExpression<Func<List<int>, int, int>> index = (xs, i) => xs[i];

        // Lambda
        MyExpression<Func<int, Func<int, int>>> lambda = x => y => x + y;

        // Quote
        MyExpression<Action> quote = () => Quote(x => x);

        // Condition
        MyExpression<Func<bool, int>> condition = b => b ? 1 : 2;

        // New
        MyExpression<Func<ValueTuple>> newValueType = () => new ValueTuple();
        MyExpression<Func<string>> newClassType = () => new string('*', 1);
        MyExpression<Func<object>> newAnonymous = () => new { x = 1, y = 2 };

        // Call
        MyExpression<Action> callStatic = () => Console.WriteLine("Hello");
        MyExpression<Func<string, string>> callInstance = s => s.Substring(1, 2);

        // Coalesce
        MyExpression<Func<string, string>> coalesce = s => s ?? "";

        // Convert
        MyExpression<Func<int, long>> convertImplicit = x => x; // NB: Introduces an implicit conversion marked as "synthetic" for now.
        MyExpression<Func<int, long>> convertExplicit = x => (long)x;
        MyExpression<Func<long, int>> convertChecked = x => checked((int)x);
        MyExpression<Func<DateTime, DateTimeOffset>> convertCustom = x => x; // REVIEW: Doesn't go through "synthetic" at the moment.

        // ListInit
        MyExpression<Func<List<int>>> listInit = () => new List<int> { 2, 3, 5 };
        MyExpression<Func<Dictionary<int, int>>> listInitManyArgs = () => new Dictionary<int, int> { { 1, 2 }, { 3, 4 } };

        // MemberInit
        MyExpression<Func<Person>> memberInit = () => new Person { Name = "Bart", Age = 21 };
        MyExpression<Func<Person>> memberInitNested = () => new Person { Name = "Bart", Age = 21, Address = { City = "Springfield" } };
    }

    static void NaturalType()
    {
        MyExpression e = (int x) => x + 1;
        Console.WriteLine(e is MyExpression<Func<int, int>>);
    }

    static void KeySelectorDemo()
    {
        KeySelectorExpression<Func<Person, string>> e1 = p => p.Name;

        // KeySelectorExpression<Func<Person, int>> e2 = p => p.Name.Length; // ERROR - Argument 1: cannot convert from 'KeySelectorProperty' to 'KeySelectorParameter'
        // KeySelectorExpression<Func<Person, Person>> e3 = p => p; // ERROR - Argument 1: cannot convert from 'KeySelectorParameter' to 'KeySelectorProperty'
        // KeySelectorExpression<Func<Person, Person, int>> e4 = (p1, p2) => p1.Age; // ERROR - No overload for method 'Lambda' takes 3 arguments
    }

    static void Quote(TheirExpression<Func<int, int>> e) {}

    class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }

        public Address Address { get; }

        public List<Person> Relatives { get; }
    }

    class Address
    {
        public string City { get; set; }
    }
}
