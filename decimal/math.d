// Written in the D programming language

/**
 *	A D programming language implementation of the
 *	General Decimal Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 *	http://www.speleotrove.com/decimal/decarith.pdf)
 *
 *	Copyright Paul D. Anderson 2009 - 2015.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.decimal.math;

import eris.integer.extended;
//import std.bigint;

import eris.decimal;
import eris.decimal.context;
import eris.decimal.arithmetic;
import eris.decimal.rounding;

unittest {
	writeln("==========================");
	writeln("decimal math.........begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.test.assertion;
	import eris.decimal.test;
}

/*
	TODO: For each function --
		1. Ensure algorithm is properly implemented.
		2. Ensure context flags are being set properly.
		3. Ensure function passes GDA tests.
		4. Determine additional tests needed and implement them.
		5. Ensure all special cases are enumerated.
		6. Automate all tests, if possible.
		7. Determine what effect the context has on the function and
			if it should be explained.
		8. Ensure all documentation is complete:
			a. Header - description, inputs, return value(s)
			b. Code - variables, control statements, branches, return points.
		9. Move most tests to the test module.
*/

/*
mixin template checkNaN() {
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
}*/

//--------------------------------
// ROUNDING
//--------------------------------

/// Rounds the argument to an integer using the specified rounding mode.
/// The default rounding mode is the current context mode. //FIXTHIS
public T round(T)(T x, Rounding mode = T.mode) {
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	T value = roundToIntegralExact(x, mode);
	return value;
}

/// Rounds the argument to the nearest integer. If the argument is exactly
/// half-way between two integers the even integer is returned.
public T rint(T)(T x) {
	return round(x, HALF_EVEN);
}

/// Returns the nearest integer less than or equal to the argument.
/// Rounds toward negative infinity.
public T floor(T)(T x) {
	return round(x, ROUND_FLOOR);
}

/// Returns the nearest integer greater than or equal to the argument.
/// Rounds toward positive infinity.
public T ceil(T)(T x) {
	return round(x, ROUND_CEILING);
}

/// Returns the truncated argument.
/// Rounds toward zero.
public T trunc(T)(T x) {
	return round(x, ROUND_DOWN);
}

/// Returns the nearest integer value. If the value is greater (less) than
/// the maximum (minimum) int value the maximum (minimum) value is returned.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public int toInt(T)(T x, Rounding mode = HALF_EVEN)
{
	if (x.isNaN)
	{
		throw new InvalidOperationException("NaN cannot be converted to int");
	}
	if (x.isInfinite)
	{
		return x.isNegative ? int.min : int.max;
	}
	return toBigInt(x, mode).toInt;
}

/// Returns the nearest long value. If the value is greater (less) than
/// the maximum (minimum) long value the maximum (minimum) value is returned.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public long toLong(T)(T x,
		Rounding mode = HALF_EVEN) if (isDecimal!T)
{
	if (x.isNaN)
	{
		throw new InvalidOperationException("NaN cannot be converted to long");
	}
	if (x.isInfinite)
	{
		return x.isNegative ? long.min : long.max;
	}
	return toBigInt(x, mode).toLong;
}

/// Returns the nearest extended integer value.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public xint toBigInt(T)(T x, Rounding mode = HALF_EVEN)
{
	if (x.isNaN)
	{
		throw new InvalidOperationException("NaN cannot be converted to bigint");
	}
	if (x.isInfinite)
	{
		return x.isNegative ? -T.max.coefficient : T.max.coefficient;
	}
	if (x.exponent != 0)
	{
		// FIXTHIS: what does this actually do?
		return round(x, mode).coefficient;
	}
	return x.coefficient;
}

unittest {	// rounding
	write("-- rounding.........");
	TD num;
	num = TD("2.1");
	assertEqual(rint(num) , TD("2"));
	assertEqual(floor(num), TD("2"));
	assertEqual(ceil(num) , TD("3"));
	assertEqual(trunc(num), TD("2"));
	num = TD("2.5");
	assertEqual(rint(num) , TD("2"));
	assertEqual(floor(num), TD("2"));
	assertEqual(ceil(num) , TD("3"));
	assertEqual(trunc(num), TD("2"));
	num = TD("3.5");
	assertEqual(rint(num) , TD("4"));
	assertEqual(floor(num), TD("3"));
	assertEqual(ceil(num) , TD("4"));
	assertEqual(trunc(num), TD("3"));
	num = TD("2.9");
	assertEqual(rint(num) , TD("3"));
	assertEqual(floor(num), TD("2"));
	assertEqual(ceil(num) , TD("3"));
	assertEqual(trunc(num), TD("2"));
	num = TD("-2.1");
	assertEqual(rint(num) , TD("-2"));
	assertEqual(floor(num), TD("-3"));
	assertEqual(ceil(num) , TD("-2"));
	assertEqual(trunc(num), TD("-2"));
	num = TD("-2.9");
	assertEqual(rint(num) , TD("-3"));
	assertEqual(floor(num), TD("-3"));
	assertEqual(ceil(num) , TD("-2"));
	assertEqual(trunc(num), TD("-2"));
	num = TD("-2.5");
	assertEqual(rint(num) , TD("-2"));
	assertEqual(floor(num), TD("-3"));
	assertEqual(ceil(num) , TD("-2"));
	assertEqual(trunc(num), TD("-2"));
	writeln("passed");
}

//--------------------------------
//	string mixins
//--------------------------------

template Constant(string name)
{
const char[] Constant =
	"/// Returns the value of the constant at the specified precision.
	public T " ~ name ~ "(T)(int precision = T.precision) if (isDecimal!T)
	{
		Context context = Context(precision, T.maxExpo, T.mode);
		static T value;
		static int lastPrecision = 0;
		if (precision == lastPrecision) return value;
		if (precision > lastPrecision) {
			value = eris.decimal.math." ~ name ~ "!T(context);
			lastPrecision = precision;
		}
		return roundToPrecision(value, precision);
	}";
}

/*template UnaryFunction(string name)
{
const char[] UnaryFunction =

	"/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T, U:int)(T x, U precision = T.precision) if (isDecimal!T)
	{
		if (x.isNaN) {
			contextFlags.set(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, T.maxExpo, HALF_EVEN);
		return " ~ name ~ "!T(x, context);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,S:string)(S str, int precision = T.precision) if (isDecimal!T) {
		T x = T(str);
		return " ~ name ~ "!T(x, precision);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,L:long)(L n, int precision = T.precision) if (isDecimal!T) {
		T x = T(n);
		return " ~ name ~ "!T(x, precision);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,R:real)(R a, int precision = T.precision)  if (isDecimal!T){
		T x = T(a);
		return " ~ name ~ "!T(x, precision);
	}";
}
*/

template UnaryFunction(string name)
{
const char[] UnaryFunction =

	"/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T)(T x, int precision = T.precision) if (isDecimal!T)
	{
		if (x.isNaN) {
			contextFlags.set(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, T.maxExpo, HALF_EVEN);
		return " ~ name ~ "!T(x, context);
	}

/*/// Returns the value of the function at the specified precision.
public T reciprocal(T, U)(in U u, int precision)
	if (isDecimal!T && isConvertible!U)
{
	T x = T(u);
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	Context context = Context(precision, T.maxExpo, HALF_EVEN);
	return reciprocal!T(T(x), context);
}*/

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,U)(U num, int precision)
		if (isDecimal!T && isConvertible!U)
	{
		T x = T(num);
		return " ~ name ~ "!T(x, precision);
	}

/*	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,L:long)(L n, int precision = T.precision) if (isDecimal!T) {
		T x = T(n);
		return " ~ name ~ "!T(x, precision);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,R:real)(R a, int precision = T.precision)  if (isDecimal!T){
		T x = T(a);
		return " ~ name ~ "!T(x, precision);
	}*/";
}

template BinaryFunction(string name)
{
const char[] BinaryFunction =
	"/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T)(T x, T y, int precision = T.precision)
	if (isDecimal!T)
	{
		if (x.isNaN || y.isNaN) {
			contextFlags.set(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, T.maxExpo, HALF_EVEN);
		return " ~ name ~ "!T(x, y, context);
	}";
}

//--------------------------------
//	CONSTANTS
//--------------------------------

/// Adds guard digits to the context precision and sets rounding to HALF_EVEN.
private Context guard(Context context, int guardDigits = 2) {
	return Context(context.precision + guardDigits, context.maxExpo, HALF_EVEN);
}

/// Calculates the value of pi to the specified precision.
mixin (Constant!("pi"));
package T pi(T)(Context inContext) if (isDecimal!T)
{
	// TODO: (behavior) if only 2 guard digits are used, function doesn't return
	auto context = guard(inContext, 3);
	// AGM algorithm
	long k = 1;
	T a0 = T.one;
	T b0 = sqrt1_2!T(context);
	T s0 = T.half;
	T a1, b1, s1;
	// loop until the arithmetic mean equals the geometric mean
	while (!equals(a0, b0, context))
	{
		// arithmetic mean: a1 = (a0+bo)/2))
		a1 = mul(T.half, add(a0, b0, context), context);
		// geometric mean: b1 = sqrt(a0*b0)
		b1 = sqrt(mul(a0, b0, context), context);
		k *= 2;
		s1 = sub(s0, mul(sub(sqr(a1, context), sqr(b1, context), context), k, context), context);
		a0 = a1;
		b0 = b1;
		s0 = s1;
	}
	T pi = mul(div(sqr(a1, context), s1, context), 2, context);
	return roundToPrecision(pi, inContext);
}

unittest
{	// pi(precision)
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "3.14159265" },
		{ 10, "3.141592654" },
		{ 12, "3.14159265359" },
		{ 14, "3.1415926535898" },
		{ 16, "3.141592653589793" },
		{ 18, "3.14159265358979324" },
		{ 20, "3.1415926535897932385" },
		{ 22, "3.141592653589793238463" },
		{ 23, "3.1415926535897932384626" },
		{ 24, "3.14159265358979323846264" },
		{ 25, "3.141592653589793238462643" },
		{ 26, "3.1415926535897932384626434" },
	];
	auto f = FunctionTest!(S,TD)("pi");
	foreach (t; s) f.test(t, TD.pi(t.n), t.n);
    writefln(f.report);
}

mixin (Constant!("pi_2"));
package T pi_2(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T halfPi = mul(pi!T(context), T.half, context);
	return roundToPrecision(halfPi, inContext);
}

unittest
{	// pi_2(precision)
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "1.57079633" },
		{ 25, "1.570796326794896619231322" },
		{  5, "1.5708234" },	// note extra incorrect digits
	];
	auto f = FunctionTest!(S,TD)("pi/2");
	foreach (t; s) f.test(t, TD.pi_2(t.n), t.n);
    writefln(f.report);
}

mixin (Constant!("invPi"));
// TODO: (efficiency) Need to ensure that previous version of pi isn't reset.
// TODO: shouldn't this be a calculation without a division?
/// Calculates the value of 1/pi in the specified context.
package T invPi(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 4);
	T alpha =  div(T.one, pi!T(context), context);
	return roundToPrecision(alpha, inContext);
}

unittest
{	// invPi(precision)
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "0.318309886" },
		{ 25, "0.3183098861837906715377675" },
		{  5, "0.3183144449" },	// note extra incorrect digits
	];
	auto f = FunctionTest!(S,TD)("1/pi");
	foreach (t; s) f.test(t, TD.invPi(t.n), t.n);
    writefln(f.report);
}

mixin (Constant!("twoInvPi"));
// TODO: (efficiency) Need to ensure that previous version of pi isn't reset.
// TODO: shouldn't this be a calculation without a division?
/// Calculates the value of 1/pi in the specified context.
package T twoInvPi(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 4);
	T alpha =  div(T.two, pi!T(context), context);
	return roundToPrecision(alpha, inContext);
}

unittest
{	// twoInvPi(precision)
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "0.636619772" },
		{ 25, "0.6366197723675813430755351" },
		{  5, "0.63662" },
	];
	auto f = FunctionTest!(S,TD)("2/pi");
	foreach (t; s) f.test(t, TD.twoInvPi(t.n), t.n);
    writefln(f.report);
}

mixin (Constant!("e"));
/// Returns the value of e in the specified context.
package T e(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	// initialize Taylor series.
	long n = 2;
	T term = T.one;
	T sum  = T.one;
	// loop until the term is too small to affect the sum.
	while (term > T.epsilon(context)) {
		sum  = add(sum, term, context);
		term = div!T(term, n, context);
		n++;
	}
	return roundToPrecision(sum, inContext);
}

unittest
{	// e(precision)
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "2.71828183" },
		{ 25, "2.7182818284590452353602874713526625" },
		{  5, "2.71828183" },
	];
	auto f = FunctionTest!(S,TD)("e");
	foreach (t; s) f.test(t, TD.e(t.n), t.n);
    writefln(f.report);
}

mixin (Constant!("ln10"));
package enum T ln10(T)(Context context) if (isDecimal!T)
{
	return log(T.Ten, context, false);
}

mixin (Constant!("ln2"));
package enum T ln2(T)(Context context) if (isDecimal!T)
{
	return log(T.Two, context, false);
}

mixin (Constant!("log2_e"));
package enum T log2_e(T)(Context context) if (isDecimal!T)
{
	return div(T.one, log(T.Two, context, false), context);
}

mixin (Constant!("log2_10"));
package enum T log2_10(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T log2T = div(log(T.Ten, context, false), log(T.Two, context, false), context);
	return roundToPrecision(log2T, inContext);
//	return roundToPrecision(TD("18690473486004564289165545643685440097"), inContext);
//	return roundToPrecision(TD("18690473486004564245643685440097"), inContext);
}

mixin (Constant!("log10_e"));
package enum T log10_e(T)(Context context) {
	return T.one/log(T.Ten, context, false);
}

mixin (Constant!("log10_2"));
package enum T log10_2(T)(Context context) {
	return log(T.Two, context)/log(T.Two, context, false);
}

mixin (Constant!("sqrt2"));
package enum T sqrt2(T)(Context context) if (isDecimal!T)
{
	return sqrt(T.Two, context);
}

mixin (Constant!("sqrt1_2"));
package enum T sqrt1_2(T)(Context context) if (isDecimal!T)
{
	return sqrt(T.Half, context);
}

mixin (Constant!("phi"));
package enum T phi(T)(Context context) if (isDecimal!T)
{
	return mul(add(T(1) , sqrt(T(5), context), context), T.half, context);
}

mixin (Constant!("invSqrtPi"));
package enum T invSqrtPi(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 4);
	T alpha =  div(T.one, sqrt(pi!T(context), context), context);
	return roundToPrecision(alpha, inContext);
}
bool verbose = false;

/*unittest
{	// e(precision)
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "2.71828183" },
		{ 25, "2.7182818284590452353602874713526625" },
		{  5, "2.71828183" },
	];
	auto f = FunctionTest!(S,TD)("e");
	foreach (t; s) f.test(t, TD.e(t.n), t.n);
    writefln(f.report);
}*/

// TODO: (testing) Test these at higher precisions
unittest {
	write("-- constants........");
	assertEqual(TD.ln10,    "2.30258509");
	assertEqual(TD.ln2,     "0.693147181");
	assertEqual(TD.log2_e,  "1.44269504");
	assertEqual(TD.log2_10, "3.32192809");
	assertEqual(TD.log2_e,  "1.44269504");
	assertEqual(TD.log2_10, "3.32192809");
	assertEqual(TD.log2_10(8), "3.3219281");
//	assertPrecisionEqual(TD.log2_10(15), "3.32192809488736", 15);
	assertEqual(TD.sqrt2,   "1.41421356");
	assertEqual(TD.sqrt1_2, "0.707106781");
	assertEqual(TD.phi,     "1.61803399");
//	assertPrecisionEqual(TD.phi(25), "1.618033988749894848204587", 25);
	writeln("passed");
}
//--------------------------------
//	ALGEBRAIC FUNCTIONS
//--------------------------------

/// Returns true if n is odd, false otherwise.
private bool isOdd(int n) {
	return n & 1;
}

unittest {	// isOdd
	assertTrue(isOdd(3));
	assertTrue(!isOdd(8));
	assertTrue(isOdd(-1));
}

// TODO: (behavior) add bitshift function?

//mixin (UnaryFunction!("reciprocal"));
mixin (UnaryFunction!("invSqrt"));
mixin (UnaryFunction!("sqrt"));

/// Returns the value of the function at the specified precision.
public T reciprocal(T, U:int)(T x, U precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	Context context = Context(precision, T.maxExpo, HALF_EVEN);
	return reciprocal!T(x, context);
}

/*/// Returns the value of the function at the specified precision.
public T reciprocal(T, U)(in U u, int precision)
	if (isDecimal!T && isConvertible!U)
{
	T x = T(u);
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	Context context = Context(precision, T.maxExpo, HALF_EVEN);
	return reciprocal!T(T(x), context);
}*/

// TODO: what happens if x is very close to zero or one?
// Does it try to work with the numbers anyway??
public T reciprocal(T)(in T x, Context inContext = T.context) if (isDecimal!T)
{
	// special values
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.copyAbs.isOne) return x.copy;
	if (x.isInfinite) return T.zero(x.sign);

	// extend working precision
	auto context = guard(inContext);

	// initial estimate
	T a = x.reduce;
	T r1 = T(2, -ilogb(a)-1);

	// Newton's method
	while (true) {
		T r0 = r1;
		r1 = mul(r0, sub(T.two, mul(a, r0, context), context), context);
		if (equals(r0, r1 ,context)) break;
	}
	// round to the original precision
	return roundToPrecision(r1, context);
}

unittest
{	// reciprocal
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1234567890123456789", TD(1)/TD("1234567890123456789") },
		{ "1234567890678900000", TD(1)/TD("1234567890123456789") },
		{ "125", "0.008" },
		{ "0.008", "125" },
	];
	auto f = FunctionTest!(S,TD)("reciprocal");
	foreach (t; s) f.test(t, reciprocal(t.x));
    writefln(f.report);
}

// TODO: see note at reciprocal
public T invSqrt(T)(T x, Context inContext) if (isDecimal!T)
{
	// special values
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.isOne) return x;
	if (x.isInfinite) return T.zero(x.sign);

	// increase the working precision
	auto context = guard(inContext);

	// initial estimate
	T a;
	int k = ilogb(x);
	if (isOdd(k)) {
		a = T(2, -1);
	}
	else {
		a = T(5, -1);
		k++;
	}
	const t = T(3);
	// reduce the exponent
	x.exponent = x.exponent - k - 1;
	// Newton's method
	while (true) {
		T b = a;
		a = mul(b, mul(T.half, sub(t, mul(x, sqr(b, context), context), context), context), context);
		if (equals(b, a, context)) break;
	}
	// restore the exponent
	a.exponent = a.exponent - k/2 - 1;
	// round to the original precision
	return roundToPrecision(a, inContext);
}

unittest
{	// invSqrt
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "2", "0.707106781" },
		{ "2", "0.70710678118655" },
		{ "20", "0.223606798" },
		{ "300", "0.0577350269" },
		{ "4000", "0.0158113883" },
		{ "98763", "0.00318201969" },
		{ "98763098", "0.000100624248" },
		{ "0.008", "1.11803399" },
		{ "98763098", "0.000100624248" },
		{ "9876387982347", "3.18200552E-7" },
	];
	auto f = FunctionTest!(S,TD)("invSqrt");
	foreach (t; s) f.test(t, invSqrt(t.x));
    writefln(f.report);
}

unittest
{	// invSqrt
	static struct S { TD x; int n; TD expect; }
	S[] s =
	[
		{ "2", 14, "0.70710678118655" },
		{ "4000", 11, "0.0158113883" },
	];
	auto f = FunctionTest!(S,TD)("invSqt(x,n)");
	foreach (t; s) f.test(t, invSqrt(t.x,t.n));
    writefln(f.report);
}

/// Returns the square root of the argument to the type precision.
/// Uses Newton's method.
public T sqrt(T)(T x, Context context) if (isDecimal!T)
{
//	auto context = guard(inContext, 3);
	// special values
	if (x.isNegative) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	// TODO: what if x is very close to one or zero??
	if (x.isOne) return T.one;
	if (x.isZero) return T.zero;
	if (x.isInfinite) return T.infinity;

	// reduce the exponent and estimate the result
	T a;
	int k = ilogb(x);
	if (isOdd(k)) {
		a = T(6, -1);
	}
	else {
		a = T(2, -1);
		k++;
	}
	x.exponent = x.exponent - k - 1;

	// Newton's method
	while (true) {
		T b = a;
		a = mul(T.half, add(b, div(x, b, context), context), context);
		if (equals(a, b, context)) break;
	}
	// restore the exponent
	a.exponent = a.exponent + (k+1)/2;
	// round the result
	return roundToPrecision(a, context);
}

unittest
{	// sqrt
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "2", "1.41421356" },
		{ TD(TD.one/sqrt(TD(2))), "0.840896415" },
		{ "200", "14.1421356" },
		{ "25", "5.0" },
		{ "2E-5", "0.00447213596" },
		{ "1E-15", "3.16227766E-8" },
		{ "1E-16", "1.00000000E-8" },
	];
	auto f = FunctionTest!(S,TD)("sqrt");
	foreach (t; s) f.test(t, sqrt(t.x));
    writefln(f.report);
}

//--------------------------------
// EXPONENTIAL AND LOGARITHMIC FUNCTIONS
//--------------------------------

mixin (UnaryFunction!("exp"));
/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
package T exp(T)(T x, Context inContext) if (isDecimal!T)
{
	if (x.isInfinite) {
		return x.isNegative ? T.zero : x;
	}
	bool negative = x.isNegative;
	if (negative) x = x.copyAbs;
	auto context = guard(inContext);
	T sqrx = sqr(x, context);
	long n = 1;
	T fact = 1;
	T t1   = T.one;
	T t2   = x;
	T term = add(t1, t2, context);
	T sum  = term;
	while (term > T.epsilon(context)) {
		n   += 2;
		t1   = mul(t2, mul(x, n, context), context);
		t2   = mul(t2, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(add(t1, t2, context), fact, context);
		sum  = add(sum, term, context);
	}
	if (negative) sum = div(T.one, sum, context);
	return roundToPrecision(sum, inContext);
}

unittest
{	// exp
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1", "2.71828183" },
		{ "2", "7.3890560989306502272" },
		{ "-2", "0.13533528324" },
	];
	auto f = FunctionTest!(S,TD)("exp");
	foreach (t; s) f.test(t, exp(t.x));
    writefln(f.report);
}

unittest {
	write("-- exp..............");
	assertEqual(exp(TD.one), TD("2.71828183"));
	assertEqual(exp(TD.one, 11), TD("2.7182818285"));
	assertEqual(exp(TD.one, 15), TD("2.71828182845905"));
	assertEqual(exp(TD.two, 15), TD("7.3890560989306502272"));
	assertEqual(exp(TD.two, 11), TD("7.3890560989306502272"));
	assertEqual(exp(-TD.two, 11), TD("0.13533528324"));
	writeln("passed");
}

/+
/**
 * Decimal version of std.math function.
 * 2^x
 */
public Decimal exp2T)( arg) {
	Decimal result;
	return result;
}

unittest {
	write("exp2...........");
	writeln("test missing");
}
+/

mixin (UnaryFunction!("expm1"));

/// expm1(x) will be more accurate than exp(x) - 1 for x << 1.
/// Decimal version of std.math function.
/// Reference: Beebe, Nelson H. F., "Computation of expm1(x) = exp(x) - 1".
public T expm1(T)(T x, Context inContext) if (isDecimal!T)
{
	// special values
	if (x.isZero) return x;
	if (x.isInfinite) return x.isNegative ? -T.one : T.infinity;

	auto context = guard(inContext);
	T sum = T.zero;

	// if too large return exp(x) - 1.
	const T lower = T("-0.7");
	const T upper = T("0.5");
	// ??? what is this --> if (x.copyAbs < lower || x.copyAbs > upper) {
	if (x < lower || x > upper) {
		sum = sub(exp(x, context), 1, context);
		return roundToPrecision(sum, inContext);
	}

	bool negative = x.isNegative;
	if (negative) x = x.copyAbs;
/*	// if too large return exp(x) - 1.
	if (x < lower || x > upper) {
		sum = sub(exp(x, context), 1, context);
		return roundToPrecision(sum, inContext);
	}*/

	// otherwise return expm1(x)
	T term = x;
	long n = 1;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n++;
		term = mul(term, div(x, n, context), context);
	}
	if (negative) sum = div(T.one, sum, context);
	return roundToPrecision(sum, inContext);
}

// TODO: (testing) unittest this
unittest {
	write("-- expm1............");
	TD num;
	num = "0.1";
	assertEqual(expm1(num), TD("0.105170918"));
	num = "-0.4";
	assertEqual(expm1(num), TD("-0.329679954"));
	// FIXTHIS: incorrect result for negative numbers.
	num = "-2";
	assertEqual(expm1(num), TD("-0.864664717"));
	writeln("passed");
}

mixin (UnaryFunction!("log"));
mixin (UnaryFunction!("log1p"));
mixin (UnaryFunction!("log10"));
mixin (UnaryFunction!("log2"));

// TODO: (behavior) add log(number, base) function (will have to have a different name -- logBase or something
/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
// TODO: efficiency) see Natural Logarithm, Wikipedia.
package T log(T)(T x, Context inContext,
		bool reduceArg = true) if (isDecimal!T)
{
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return -T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	if (x.isInfinite) {
		return T.infinity;
	}
	auto context = guard(inContext);
	int k;
	if (reduceArg) {
		k = ilogb(x) + 1;
		x.exponent = x.exponent - k;
	}
	T a = div(sub(x, 1, context), add(x, 1, context), context);
	T b = sqr(a, context);
	T c = a;
	long n = 3;
	while (true) {
		c = mul(c, b, context);
		T d = add(a, div(c, n, context), context);
		if (equals(a, d, context)) {
			T ln = mul(a, 2, context);
			if (reduceArg) {
				ln = add(ln, mul(ln10!T(context), k, context));
			}
			return roundToPrecision(ln, inContext);
		}
		a = d;
		n += 2;
	}
}

unittest {
	write("-- log..............");
	TD one = TD.one;
	assertEqual(log(exp(one)), one);
	assertEqual(log(TD(10)), "2.30258509");
	assertEqual(log(TD(123.45)), "4.81583622");
	assertEqual(log(TD("99.999E+8")), "23.0258409");
	writeln("passed");
}

/**
 * log1p (== log(1 + x)).
 * Decimal version of std.math function.
 */
public T log1p(T)(T x, Context inContext) if (isDecimal!T)
{
	// special cases
	if (x.isNaN || x < T.negOne) 	// use compare(x, T.negOne) == -1?
	{
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	// check for infinite argument
	if (x.isInfinite) return T.infinity;

	if (x.isZero) return T.zero(x.sign);

	if (equals(x, T.negOne, inContext)) return T.infinity(true);

	// TODO: There's probably a better breakeven point
	if (x.copyAbs >= T.one) return log(add(T.one, x, inContext), inContext);

	T term = x;
	T pwr  = x;
	T sum  = T.zero;
	T n    = T.one;
	auto context = guard(inContext);
	while (term.copyAbs >= T.epsilon(context))
	{
		sum = add(term, sum, context);
		pwr = mul(-pwr, x, context);// * x;
		n++;
		term = div(pwr, n, context);
	}
	sum = add(term, sum, context);
	return roundToPrecision(sum, inContext);
}

// TODO: (testing) unittest this.
unittest {
	write("-- log1p............");
	TD x = "0.1";
	assertEqual(log1p(x), "0.095310179804");
	writeln("passed");
}

/// Decimal version of std.math.log10.
/// Required by General Decimal Arithmetic Specification
public T log10(T)(T x, Context inContext) if (isDecimal!T)
{
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	auto context = guard(inContext);
	int k = ilogb(x) + 1;
	x.exponent = x.exponent - k;
//	x.exponent -= k;
	T lg10 = add(div(log(x, context), ln10!T(context)), k);
	return roundToPrecision(lg10, inContext);
}

unittest {
	write("-- log10............");
	TD x = TD("2.55");
	assertEqual(log10(x), TD("0.40654018"));
	x = 123.456;
	assertEqual(log10(x), TD("2.09151220"));
	x = 10.0;
	assertEqual(log10(x), 1);
	writeln("passed");
}

/**
 * Decimal version of std.math.log2.
 * Required by General Decimal Arithmetic Specification
 */
public T log2(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T lg2 = div(log(x, context), ln2!T(context), context);
	return roundToPrecision(lg2, inContext);
}

unittest {
	write("-- log2.............");
	assertEqual(log2(TD(10)), TD("3.32192809"));
	assertEqual(log2(TD.e), TD("1.44269504"));
	writeln("passed");
}

/**
 * Decimal version of std.math.pow.
 * Required by General Decimal Arithmetic Specification
 */
 // TODO: (behavior) add context
public T pow(T)(T x, T y) if (isDecimal!T)
{
	return exp(x*log(y));
}

unittest {
	write("pow..........");
	writeln("test missing");
}

mixin (BinaryFunction!("hypot"));

/// Returns the square root of the sum of the squares in the specified context.
/// Decimal version of std.math function.
public T hypot(T)(T x, T y, Context context) if (isDecimal!T)
{
	// special values
	if (x.isInfinite || y.isInfinite) return T.infinity();
    if (x.isZero) return y;
	if (y.isZero) return x;
	if (x.isNaN || y.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}

	T a = x.copyAbs;
    T b = y.copyAbs;
	if (a < b) {
		//swap operands
		T t = a;
		a = b;
		b = t;
	}
    b = div(b, a, context);
    return mul(a, sqrt(add(T.one, sqr(b, context), context), context));
}

// TODO: (testing) Need to test operation near precisions where this operation is really useful.
unittest {
	write("-- hypot............");
	TD x = 3;
	TD y = 4;
	TD expect = 5;
	TD actual = hypot(x,y);
	assertTrue(actual == expect);
	assertEqual(actual, expect);
	writeln("passed");
}

//--------------------------------
// TRIGONOMETRIC FUNCTIONS
//--------------------------------

//mixin (UnaryFunction!("sin"));
//mixin (UnaryFunction!("cos"));


// Returns the reduced argument and the quadrant.
// Reduced argument |x| <= pi/4.
private T reduceAngle(T)(in T x,
	out int n, in Context inContext = T.context) if (isDecimal!T)
{
	auto context = guard(inContext);
	T twoInvPi = twoInvPi!T(context);
	T pi_2 = pi_2!T(context);
	T y = mul(x, twoInvPi, context);
	int k = rint(y).toInt;
	n = k % 4;
	T f = sub(y, k, context);
	T r = mul(f, pi_2, context);
	return r;
}

/// Decimal version of std.math function.
public T sin(T)(in T x, int precision = T.precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}

	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int k;
	T red = reduceAngle(x, k, context);
	switch (k) {
		case 0: return( sin( red, context));
		case 1: return( cos( red, context));
		case 2: return(-sin( red, context));
		case 3: return(-cos( red, context));
		default: return T.nan;
	}
//	return T.nan;
}

// Decimal version of std.math function.
// Precondition: x is in 1st quadrant.
package T sin(T)(in T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T sum = 0;
	int n = 1;
	T powx = x.copy;
	T sqrx = sqr(x, context);
	T fact = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n += 2;
		powx = mul(powx.copyNegate, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(powx, fact, context);
	}
	return roundToPrecision(sum, inContext);
}

unittest
{	// sin
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ "0.1",   9, "0.0998334166" },
		{ "0.333", 9, "0.326879693" },
		{ "5.0",   9, "-0.958924275" },
		{ "1.0",   9, "0.8414709848" },
		{ "1.0",  16, "0.8414709848078965" },
		{ "2.0",  16, "0.9092974268256817" },
	];
	auto f = FunctionTest!(S,TD)("sin");
	foreach (t; s) f.test(t, sin(t.x, t.p), t.p);
    writefln(f.report);
}

/*unittest {
	write("-- sin..............");
//	TD difficult = TD(5678900000);
	TD difficult = TD(5);
writefln("difficult = %s", difficult);
// FIXTHIS: throws div by zero exception...
writefln("sin(difficult) = %s", sin(difficult));
//	assertEqual(sin(difficult), TD(0));
	// TODO: (testing) one value from each quadrant, reduced value.
	// TODO: (behavior) this is a notoriously difficult value "sin(10^^22)"
	writeln("passed");
}*/

/// Decimal version of std.math function.
public T cos(T)(in T x, int precision = T.precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int quadrant;
	T red = reduceAngle(x, quadrant, context);
	switch (quadrant) {
		case 0: return( cos(red, context));
		case 1: return(-sin(red, context));
		case 2: return(-cos(red, context));
		case 3: return( sin(red, context));
		default: return T.nan;
	}
}

/// Decimal version of std.math function.
/// Precondition: x is in 1st quadrant.
package T cos(T)(in T x, Context inContext) {
	auto context = guard(inContext);
//writefln("\nx = %s", x);
	T sum = 0;
	int n = 0;
	T powx = 1;
	T sqrx = sqr(x, context);
	T fact = 1;
	T term = powx;
//writefln("sum = %s", sum);
//writefln("term = %s", term);
//writefln("fact = %s", fact);
//writefln("powx = %s", powx);
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n += 2;
		powx = mul(powx.copyNegate, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(powx, fact, context);
//writefln("sum = %s", sum);
//writefln("term = %s", term);
//writefln("fact = %s", fact);
//writefln("powx = %s", powx);
	}
	return roundToPrecision(sum, inContext);
}

unittest
{	// cos
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ "1.0",   9, "0.5403023058681397174009" },
		{ "0.333", 9, "0.945065959" },
		{ "5.0",   9, "0.283662185" },
		{ "1.0",  23, "0.540302306" },
		{ "2.0",  16, "-0.416146837" },
	];
	auto f = FunctionTest!(S,TD)("cos");
	foreach (t; s) f.test(t, cos(t.x, t.p), t.p);
    writefln(f.report);
}

public void sincos(T)(T x, out T sine, out T cosine, int precision = T.precision) {
	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int quadrant;
	T red = reduceAngle(x, quadrant, context);
	sincos(red, sine, cosine, context);
/*	switch (quadrant) {
//sin:
		case 0: break;
		case 1:
			sine = cosine;
			cosine = -sine;
			break;
		case 2:
			sine = -sine;
			cosine = -cosine;
			break;
		case 3:
			sine = -cosine;
			cosine = sine;
			break;
		default:
			sine = T.nan;
			cosine = T.nan;
	}*/
}
/**
 * Replaces std.math function expi
 *
 */
// TODO: (behavior) context, angle reduction
public void sincos(T)(T x, out T sine, out T cosine,
		Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T csum, cterm, cx;
	T ssum, sterm, sx;
	T sqrx = sqr(x, context);
	long n = 2;
	T fact = 1;
	cx = 1;	cterm = cx;	csum = cterm;
	sx = x;	sterm = sx;	ssum = sterm;
	while (sterm.copyAbs > T.epsilon) {
		cx = mul(cx.copyNegate, sqrx, context);
		fact = mul(fact, n++, context);
		cterm = div(cx, fact, context);
		csum = add(csum, cterm, context);
		sx = mul(sx.copyNegate, sqrx, context);
		fact = mul(fact, n++, context);
		sterm = div(sx, fact, context);
		ssum = add(ssum, sterm, context);
	}
    sine   = roundToPrecision(ssum, inContext);
	cosine = roundToPrecision(csum, inContext);
}

unittest {
	write("sincos.......");
	TD sine;
	TD cosine;
	sincos(TD("1.0"), sine, cosine);
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
// TODO: (efficiency) compare divTan with tan.
public T divTan(T)(T x) if (isDecimal!T)
{
	T sine;
	T cosine;
	sincos(x, sine, cosine);
	if (sine == T.zero) return T.infinity;
	return sine/cosine;
}

public T tan(T)(T x, int precision = T.precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int quadrant;
	T red = reduceAngle(x, quadrant, context);
	T sine;
	T cosine;
	sincos(red, sine, cosine, context);
	switch (quadrant) {
		case 0: return( sine/cosine);
		case 1: return(-cosine/sine);
		case 2: return( sine/cosine);
		case 3: return(-sine/cosine);
		default: return T.nan;
	}
}

/*version(unittest)
{
	private T cost1(T)(in T x)
	{
		return cos!T(x);
	}

	private T cost2(T)(in T x, int precision)
	{
		return cos!T(x, precision);
	}
}

unittest
{	// cos
// TODO: (testing) one value from each quadrant, reduced value.
}
*/
unittest {
	write("-- tan..............");
	// TODO: (testing) one value from each quadrant, reduced value.
	assertEqual(tan(TD.one), TD("1.55740772465490223"));
	assertEqual(tan(TD.one, 14), TD("1.55740772465490223"));
	assertEqual(tan(TD("0.333")), TD("0.345880295"));
	writeln("passed");
}

/// Calculates the value of pi in the specified context.
package T arctan(T)(T x, Context inContext = T.context) if (isDecimal!T)
{
	auto context = guard(inContext, 3);
	int k = 1;
	sqrt1_2!T(context);
	T a1 = sqrt(T.one + sqr(x));
	T b1 = T.one;
	T s1 = T.half;
	T a2, b2, s2;
	// AGM
	while (!equals(a1, b1, context)) {
		a2 = mul(T.half, add(a1, b1, context),context);
		b2 = sqrt(mul(a1, b1, context), context);
		k *= 2;
		s2 = sub(s1, mul(sub(sqr(a2, context), sqr(b2, context), context), k, context), context);
		a1 = a2;
		b1 = b2;
		s1 = s2;
	}
	T pi = mul(div(sqr(a2, context), s2, context), 2, context);
	// round the result in the original context
	return roundToPrecision(pi, inContext);
}


//arcsin x = 2 * arctan(x/(1+sqrt{1-x^^2))
//\arccos x = 2 \arctan \frac{\sqrt{1-x^2}}{1+x},\text{ if }-1 < x \leq +1
//\arctan x = 2 \arctan \frac{x}{1+\sqrt{1+x^2}}

/// Decimal version of std.math function.
// TODO: (behavior) convert to std unary function.
public T asin(T)(T x) if (isDecimal!T)
{
	T result = 2 * atan!T(x/(1+sqrt(1-sqr(x)))); //^^2)));
	return result;
}

unittest {
	write("-- asin.............");
	assertEqual(asin(TD.half), TD("0.523598776"));
//	assertEqual(asin(TD.one, 14), TD("1.55740772465490223"));
	assertEqual(asin(TD("0.333")), TD("0.339483378"));
	writeln("passed");
}

/// Decimal version of std.math function.
// TODO: (behavior) convert to std unary function.
public T acos(T)(T x) if (isDecimal!T)
{
	T result = 2 * atan(sqrt(1-sqr(x))/(1 + x));
	return result;
}

unittest {
	write("-- acos.............");
	assertEqual(acos(TD.half), TD("1.0471975511965977461542144610932"));
//	assertEqual(acos(TD.one, 14), TD("1.55740772465490223"));
	assertEqual(acos(TD("0.333")), TD("1.23131295"));
	writeln("passed");
}

mixin (UnaryFunction!("atan"));

/// Returns the arctangent of the argument in the specified context.
/// Algorithm uses Taylor's theorem for arctangent.
public T atan(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 3);
	long k = 1;
	// reduce the input angle
	while (x > T(0.5)) {
		T a = sqr(x, context);
		T b = add(a, 1, context);
		T c = sqrt(b, context);
		T d = add(c, 1, context);
		x = div(x, d, context);
		k *= 2;
	}
	T sum = 0;
	T powx = x;
	T sqrx = sqr(x, context);
	long dvsr = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		powx = mul(powx.copyNegate, sqrx, context);
		dvsr = dvsr + 2;
		term = div!T(powx, dvsr, context);
	}
	return roundToPrecision(mul(sum, k, context), inContext);
}


unittest {
	write("-- atan.............");
	assertEqual(atan(TD.half), TD("0.463647609"));
	assertEqual(atan(TD.one, 14), TD("0.78539816339745"));
	assertEqual(atan(TD("0.333")), TD("0.321450524"));
	assertEqual(atan(TD("0.1")), TD("0.099668652491162038065120043484057532623410224914551"));
	assertEqual(atan(TD("0.9")), TD("0.73281510178650655085164089541649445891380310058594"));
	writeln("passed");
}

/**
 * Decimal version of std.math function.
 *
 */
public TD atan2(T)(T y, TD x) if (isDecimal!T)
{
	TD result;
	return result;
}

unittest {
	write("atan2........");
	writeln("..failed");
}

//--------------------------------
// HYPERBOLIC TRIGONOMETRIC FUNCTIONS
//--------------------------------

mixin (UnaryFunction!("sinh"));
mixin (UnaryFunction!("cosh"));
mixin (UnaryFunction!("tanh"));
//mixin (UnaryFunction!("atanh"));

/// Decimal version of std.math function.
public T sinh(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	long n = 1;
	T sum = 0;
	T powx = x;
	T sqrx = sqr(x, context);
	T fact = n;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum  = add(sum, term, context);
		n += 2;
		fact = mul(fact, n*(n-1), context);
		powx = mul(powx, sqrx, context);
		term = div(powx, fact, context);
	}
	return roundToPrecision(sum, inContext);
}


/// Decimal version of std.math function.
// TODO: why does this perform so poorly?
public T sinh1(T)(T x) if (isDecimal!T)
{
	return (exp(x) - exp(-x))*T.half;
}


unittest {
	write("-- sinh.............");
	assertEqual(sinh(TD("1.0")), TD("1.1752011936438014568823818505956008151557179813341"));
//	assertEqual(sinh1(TD("1.0")), TD("1.1752011936438014568823818505956008151557179813341"));
	writeln("passed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T cosh(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	long n = 0;
	T sum = 0;
	T powx = 1;
	T sqrx = sqr(x, context);
	T fact = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum  = add(sum, term, context);
		n += 2;
		fact = mul(fact, n*(n-1), context);
		powx = mul(powx, sqrx, context);
		term = div(powx, fact, context);
	}
	return roundToPrecision(sum, inContext);
}

unittest {
	write("-- cosh.............");
	assertEqual(cosh(TD("1.0")), TD("1.5430806348152437784779056207570616826015291123659"));
	writeln("passed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T tanh(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T tan = div(sinh(x, context), cosh(x, context), context);
	return roundToPrecision(tan, inContext);
}

unittest {
	write("-- tanh.............");
	assertEqual(tanh(TD("1.0")), TD("0.76159415595576488811945828260479"));
	writeln("passed");
}

mixin (UnaryFunction!("asinh"));
mixin (UnaryFunction!("acosh"));
mixin (UnaryFunction!("atanh"));

/// Decimal version of std.math function.
public T asinh(T)(T x, Context inContext) if (isDecimal!T)
{
	// TODO: (behavior) special values
	auto context = guard(inContext);
	T arg = add(x, sqrt(add(sqr(x, context), T.one, context), context), context);
	return roundToPrecision(log(arg, context), inContext);
}

unittest {
	write("-- asinh............");
	assertEqual(asinh(TD("1.0")), TD("0.88137358701954302523260932497979"));
	writeln("passed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T acosh(T)(T x, Context inContext) if (isDecimal!T)
{
	// TODO: (behavior) special values
	auto context = guard(inContext);
	T sqp = sqrt(add(x, T.one, context));
	T sqm = sqrt(sub(x, T.one, context));
	T arg = add(x, mul(sqp, sqm, context), context);
	return roundToPrecision(log(arg, context), inContext);
}

unittest {
	write("-- acosh............");
	assertEqual(acosh(TD("1.5")), TD("0.96242365011920689499551782684874"));
	writeln("passed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T atanh(T)(T x, Context inContext) if (isDecimal!T)
{
	// TODO: (behavior) special values
	auto context = guard(inContext);
	T sqp = add(x, T.one, context);
	T sqm = sub(x, T.one, context);
	T arg = div(sqp, sqm, context);
	return mul(T.half, log(arg, context), context);
//	return sqm;
	// also atanh(x) = x + x^3/3 + x^5/5 + x^7/7 + ... (speed of convergence?)
}

unittest {
	write("-- atanh............");
	assertEqual(atanh(TD("0.5")), TD("0.54930614433405484569762261846126"));
	writeln("passed");
}

unittest {
	writeln("==========================");
	writeln("decimal math...........end");
	writeln("==========================");
}



