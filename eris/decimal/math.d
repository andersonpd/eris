// Written in the D programming language

/**
 *	A D programming language implementation of the
 *	General Decimal Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 *	http://www.speleotrove.com/decimal/decarith.pdf)
 *
 *	Copyright Paul D. Anderson 2009 - 2014.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.decimal.math;

import eris.integer.extended;
//import std.bigint;

import eris.decimal.context;
import eris.decimal.decimal;
import eris.decimal.arithmetic;
import eris.decimal.rounding;

unittest {
	writeln("==========================");
	writeln("decimal math.........begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
}

/*mixin template Foo() {
  int x = 5;
}*/

/*mixin template checkNaN() {
	if (x.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
}*/

//--------------------------------
// ROUNDING
//--------------------------------

/// Rounds the argument to an integer using the specified rounding mode.
/// The default rounding mode is the current context mode.
public T round(T)(T x, Rounding mode = Rounding.HALF_EVEN) {
	if (x.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	T value = roundToIntegralExact(x, mode);
	return value;
}

/// Rounds the argument to the nearest integer. If the argument is exactly
/// half-way between two integers the even integer is returned.
public T rint(T)(T x) {
	return round(x, Rounding.HALF_EVEN);
}

/// Returns the nearest integer less than or equal to the argument.
/// Rounds toward negative infinity.
public T floor(T)(T x) {
	return round(x, Rounding.FLOOR);
}

/// Returns the nearest integer greater than or equal to the argument.
/// Rounds toward positive infinity.
public T ceil(T)(T x) {
	return round(x, Rounding.CEILING);
}

/// Returns the truncated argument.
/// Rounds toward zero.
public T trunc(T)(T x) {
	return round(x, Rounding.DOWN);
}

/// Returns the nearest integer value. If the value is greater (less) than
/// the maximum (minimum) int value the maximum (minimum) value is returned.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public int toInt(T)(T x, Rounding mode = Rounding.HALF_EVEN) {
	if (x.isNaN) return 0;	// TODO: (behavior) should throw.
	if (x.isInfinite) return x.isNegative ? int.min : int.max;
	return toBigInt(x, mode).toInt;
}

/// Returns the nearest long value. If the value is greater (less) than
/// the maximum (minimum) long value the maximum (minimum) value is returned.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public long toLong(T)(T x, Rounding mode = Rounding.HALF_EVEN) {
	if (x.isNaN) return 0;	// TODO: (behavior) should throw.
	if (x.isInfinite) return x.isNegative ? long.min : long.max;
	return toBigInt(x, mode).toLong;
}

/// Returns the nearest extended integer value.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public xint toBigInt(T)(T x,
		Rounding mode = Rounding.HALF_EVEN) {
	if (x.isNaN) return xint(0);	// TODO: (behavior) should throw
	if (x.isInfinite) {			// TODO: (behavior) should throw
		return x.isNegative ? T.min.coefficient : T.max.coefficient;
	}
	if (x.exponent != 0) {
		return round(x, mode).coefficient;
	}
	return x.coefficient;
}

unittest {	// rounding
	write("-- rounding.........");
	dec9 num;
	num = dec9("2.1");
	assertEqual(rint(num) , dec9("2"));
	assertEqual(floor(num), dec9("2"));
	assertEqual(ceil(num) , dec9("3"));
	assertEqual(trunc(num), dec9("2"));
	num = dec9("2.5");
	assertEqual(rint(num) , dec9("2"));
	assertEqual(floor(num), dec9("2"));
	assertEqual(ceil(num) , dec9("3"));
	assertEqual(trunc(num), dec9("2"));
	num = dec9("3.5");
	assertEqual(rint(num) , dec9("4"));
	assertEqual(floor(num), dec9("3"));
	assertEqual(ceil(num) , dec9("4"));
	assertEqual(trunc(num), dec9("3"));
	num = dec9("2.9");
	assertEqual(rint(num) , dec9("3"));
	assertEqual(floor(num), dec9("2"));
	assertEqual(ceil(num) , dec9("3"));
	assertEqual(trunc(num), dec9("2"));
	num = dec9("-2.1");
	assertEqual(rint(num) , dec9("-2"));
	assertEqual(floor(num), dec9("-3"));
	assertEqual(ceil(num) , dec9("-2"));
	assertEqual(trunc(num), dec9("-2"));
	num = dec9("-2.9");
	assertEqual(rint(num) , dec9("-3"));
	assertEqual(floor(num), dec9("-3"));
	assertEqual(ceil(num) , dec9("-2"));
	assertEqual(trunc(num), dec9("-2"));
	num = dec9("-2.5");
	assertEqual(rint(num) , dec9("-2"));
	assertEqual(floor(num), dec9("-3"));
	assertEqual(ceil(num) , dec9("-2"));
	assertEqual(trunc(num), dec9("-2"));
	writeln("passed");
}

//--------------------------------
//	string mixins
//--------------------------------

template GenConstant(string name)
{
const char[] GenConstant =
	"/// Returns the value of the constant at the specified precision.\n" ~
   	"public T " ~ name ~ "(T)(int precision = T.precision) {
		Context context = Context(precision, Rounding.HALF_EVEN);
		static T value;
		static int lastPrecision = 0;
		if (precision == lastPrecision) return value;
		if (precision > lastPrecision) {
			value = " ~ name ~ "!T(context);
			lastPrecision = precision;
		}
		return roundToPrecision(value, precision);
	}";
}

template GenUnaryFunction(string name)
{
const char[] GenUnaryFunction =
	"/// Returns the value of the function at the specified precision.\n" ~
   	"public T " ~ name ~ "(T)(T x, int precision = T.precision) {
		if (x.isNaN) {
			contextFlags.setFlags(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, Rounding.HALF_EVEN);
		return " ~ name ~ "!T(x, context);
	}";
}

//--------------------------------
//	CONSTANTS
//--------------------------------

mixin (GenConstant!("pi"));
mixin (GenConstant!("invPi"));
mixin (GenConstant!("e"));

/// Adds guard digits to the context precision and sets rounding to HALF_EVEN.
private Context guard(Context context, int guardDigits = 2) {
	return Context(context.precision + guardDigits, context.rounding);
}

/// Calculates the value of pi in the specified context.
private T pi(T)(Context inContext) {
	// increase the working precision
	auto context = guard(inContext, 3);
	// initialize AGM algorithm
	long k = 1;
	T a0 = T.one;
	T b0 = sqrt1_2!T(context);
	T s0 = T.half;
	T a1, b1, s1;
	// loop until the arithmetic mean equals the geometric mean
	while (!equals(a0, b0, context)) {
		a1 = mul(T.half, add(a0, b0, context),context);
		b1 = sqrt(mul(a0, b0, context), context);
		k *= 2;
		s1 = sub(s0, mul(sub(sqr(a1, context), sqr(b1, context), context), k, context), context);
		a0 = a1;
		b0 = b1;
		s0 = s1;
	}
	T pi = mul(div(sqr(a1, context), s1, context), 2, context);
	// round the result in the original context
	return roundToPrecision(pi, inContext);
}

unittest {
	write("-- pi...............");
	assertStringEqual(dec9("3.14159265358979"), pi!dec9(15));
	assertStringEqual(dec9("3.14159265"), pi!dec9);
	assertStringEqual(dec9("3.141592653589793238462643"), pi!dec9(25));
	assertStringEqual(dec9("3.1416"), pi!dec9(5));
	writeln("passed");
}
//	immutable decimal PI = roundString("3.141592653589793238462643 3832795028841"
//		"9716939937510582097494459230781640628620899862803482534211707");

// TODO: (efficiency) Need to ensure that previous version of pi isn't reset.
// TODO: (behavior) Reciprocal is way worse than one over division.
/// Calculates the value of pi in the specified context.
private T invPi(T)(Context inContext) {
	auto context = guard(inContext, 4);
	T alpha =  div(T.one, pi!T(context), context);
	return roundToPrecision(alpha, inContext);
}

unittest {
	write("-- invPi............");
	assertStringEqual(invPi!dec9, dec9("0.318309886"));
	assertStringEqual(invPi!dec9(25), dec9("0.3183098861837906715377675"));
	writeln("passed");
}

/// Returns the value of e in the specified context.
private T e(T)(Context inContext) {
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

unittest {
	write("-- e................");
	assertStringEqual(dec9("2.71828183"), e!dec9);
	assertStringEqual(dec9("2.7182818284590452353602874713526625"), e!dec9(35));
	writeln("passed");
}

mixin (GenConstant!("ln10"));
mixin (GenConstant!("ln2"));
mixin (GenConstant!("sqrt2"));
mixin (GenConstant!("sqrt1_2"));

private T ln10(T)(Context context) {
	return log(T.TEN, context, false);
}

private T ln2(T)(Context context) {
	return log(T.TWO, context, false);
}

private T sqrt2(T)(Context context) {
	return sqrt(T.TWO, context);
}

private enum T sqrt1_2(T)(Context context) {
	return sqrt(T.HALF, context);
}

// TODO: (testing) Test these at higher precisions
unittest {
	write("-- constants........");
	assertEqual("2.30258509", ln10!dec9);
	assertEqual("0.693147181", ln2!dec9);
	assertEqual("1.41421356", sqrt2!dec9);
	assertEqual("0.707106780", sqrt1_2!dec9);
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

mixin (GenUnaryFunction!("reciprocal"));
mixin (GenUnaryFunction!("invSqrt"));
mixin (GenUnaryFunction!("sqrt"));

private T reciprocal(T)(T x, Context inContext) {

	// special values
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.copyAbs.isOne) return x;
	if (x.isInfinite) return T.zero(x.sign);

	// extend working precision
	auto context = guard(inContext);

	// initial estimate
	x = x.reduce;
	T a = T(2, -ilogb(x)-1);

	// Newton's method
	while (true) {
		T b = a;
		a = mul(b, sub(T.two, mul(x, b, context), context), context);
		if (equals(b, a ,context)) break;
	}
	// round to the original precision
	return roundToPrecision(a, inContext);
}

unittest {	// reciprocal
	write("-- reciprocal.......");
	dec9 one = dec9.one;
	dec9 num = dec9("1234567890123456789");
	dec9 a = one/num;
	dec9 b = reciprocal(num,10);
	assertEqual(b, a);
	num = dec9("12345678906789");
	a = one/num;
	b = reciprocal(num);
	assertEqual(b, a);
	writeln("passed");
}

// TODO: (behavior) sometimes fails to break out of loop (e.g. reduce guard digits to +2)
public T invSqrt(T)(T x, Context inContext) {
	// special values
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
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

unittest {
	write("-- inverse sqrt.....");
	assertEqual(invSqrt(dec9(2))/*),11)*/, dec9("0.707106781"));
	assertEqual(invSqrt(dec9(2), 14)/*),11)*/, dec9("0.70710678118655"));
	assertEqual(invSqrt(dec9(20))/*),11)*/, dec9("0.223606798"));
	assertEqual(invSqrt(dec9(300))/*,11)*/, dec9("0.0577350269"));
	assertEqual(invSqrt(dec9(4000),11), dec9("0.0158113883"));
	assertEqual(invSqrt(dec9(98763)), dec9("0.00318201969"));
	assertEqual(invSqrt(dec9(98763098)), dec9("0.000100624248"));
	assertEqual(invSqrt(dec9(9876387982347)), dec9("3.18200552E-7"));
	writeln("passed");
}


/// Returns the square root of the argument to the type precision.
/// Uses Newton's method.
public T sqrt(T)(T x, Context context) {
	// special values
	if (x.isNegative) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
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
	while(true) {
		T b = a;
		a = mul(T.half, add(b, div(x, b, context), context), context);
		if (equals(a, b, context)) break;
	}
	// restore the exponent
	a.exponent = a.exponent + (k+1)/2;
	// round the result
	return roundToPrecision(a, context);
}

unittest {
	write("-- square root......");
	assertEqual(sqrt(dec9(2)), dec9("1.41421356"));
	assertEqual(sqrt(dec9(dec9.one/sqrt(dec9(2)))), dec9("0.840896415"));
	assertEqual(sqrt(dec9(200)), dec9("14.1421356"));
	assertEqual(sqrt(dec9(25)), dec9("5.00000000"));
	assertEqual(sqrt(dec9(2E-5)), dec9("0.00447213596"));
	assertEqual(sqrt(dec9(1E-15)), dec9("3.16227766E-8"));
	assertEqual(sqrt(dec9(1E-16)), dec9("1.00000000E-8"));
	writeln("passed");
}

//--------------------------------
// EXPONENTIAL AND LOGARITHMIC FUNCTIONS
//--------------------------------

mixin (GenUnaryFunction!("exp"));
mixin (GenUnaryFunction!("expm1"));

// FIXTHIS: incorrect results for negative numbers.
/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
private T exp(T)(T x, Context inContext) {
	if (x.isInfinite) {
		return x.isNegative ? T.zero : x;
	}
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
		term = div(add(t1, t2, context), fact, context);;
		sum  = add(sum, term, context);
	}
	return roundToPrecision(sum, inContext);
}

unittest {
	write("-- exp..............");
	assertEqual(exp(dec9.one), dec9("2.71828183"));
	assertEqual(exp(dec9.one, 11), dec9("2.7182818285"));
	assertEqual(exp(dec9.one, 15), dec9("2.71828182845905"));
	assertEqual(exp(dec9.two, 15), dec9("7.3890560989306502272"));
	assertEqual(exp(dec9.two, 11), dec9("7.3890560989306502272"));
	writeln("passed");
}

/+
/**
 * Decimal version of std.math function.
 * 2^x
 */
public Decimal exp2T)(in T arg) {
	Decimal result;
	return result;
}

unittest {
	write("exp2...........");
	writeln("test missing");
}
+/

/// expm1(x) will be more accurate than exp(x) - 1 for x near 1.
/// Decimal version of std.math function.
/// Reference: Beebe, Nelson H. F., "Computation of expm1(x) = exp(x) - 1".
public T expm1(T)(T x, Context inContext) {
	// special values
	if (x.isZero) return x;
	if (x.isInfinite) return x.isNegative ? -T.one : T.infinity;

	auto context = guard(inContext);
	T sum = T.zero;
	// this function is only useful near zero
	const T lower = T("-0.7");
	const T upper = T("0.5");
	if (x < lower || x > upper) {
		sum = sub(exp(x, context), 1, context);
//writefln("sum = %s", sum);
		return roundToPrecision(sum, inContext);
	}

//	auto context = guard(inContext);
	T term = x;
//	T sum = T.zero;
	long n = 1;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n++;
		term = mul(term, div(x, n, context), context);
	}
	return roundToPrecision(sum, inContext);
}

// TODO: (testing) unittest this
unittest {
	write("-- expm1............");
	dec9 num;
	num = "0.1";
	assertEqual(expm1(num), dec9("0.105170918"));
	num = "-0.4";
	assertEqual(expm1(num), dec9("-0.329679954"));
	// FIXTHIS: incorrect result for negative numbers.
//	num = "-2";
//	assertEqual(expm1(num), dec9("-0.864664717"));
	writeln("passed");
}

mixin (GenUnaryFunction!("log"));
mixin (GenUnaryFunction!("log1p"));
mixin (GenUnaryFunction!("log10"));
mixin (GenUnaryFunction!("log2"));

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
// TODO: efficiency) see Natural Logarithm, Wikipedia.
private T log(T)(T x, Context inContext, bool reduceArg = true) {
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
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
	dec9 one = dec9.one;
	assertEqual(log(exp(one)), one);
	assertEqual(log(dec9(10)), "2.30258509");
	assertEqual(log(dec9(123.45)), "4.81583622");
	assertEqual(log(dec9("99.999E+8")), "23.0258409");
	writeln("passed");
}

/**
 * log1p (== log(1 + x)).
 * Decimal version of std.math function.
 */
// TODO: (behavior) use bounds like expm1?
public T log1p(T)(T x, Context inContext) {
	T term = x;
	T pwr  = x;
	T sum  = T.zero;
	T n    = T.one;
	auto context = guard(inContext);
	while (term.copyAbs >= T.epsilon(context)) {
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
	dec9 x = "0.1";
	assertEqual(log1p(x), "0.095310179804");
	assertStringEqual(log1p(x, 14), "0.095310179814325");
	writeln("passed");
}

/// Decimal version of std.math.log10.
/// Required by General Decimal Arithmetic Specification
public T log10(T)(T x, Context inContext) {
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	auto context = guard(inContext);
	int k = ilogb(x) + 1;
	x.exponent = x.exponent - k;
	T lg = add(div(log(x, context), ln10!T(context)), k);
	return roundToPrecision(lg, inContext);
}

unittest {
	write("-- log10............");
	dec9 x = dec9("2.55");
	assertEqual(log10(x), dec9("0.40654018"));
	x = 123.456;
	assertEqual(log10(x), dec9("2.09151220"));
	x = 10.0;
	assertEqual(log10(x), 1);
	writeln("passed");
}

/**
 * Decimal version of std.math.log2.
 * Required by General Decimal Arithmetic Specification
 */
public T log2(T)(T x, Context inContext) {
	auto context = guard(inContext);
	T lb = div(log(x, context), ln2!T(context), context);
	return roundToPrecision(lb, inContext);
}

unittest {
	write("-- log2.............");
	assertEqual(log2(dec9(10)), dec9("3.32192809"));
	assertEqual(log2(e!dec9), dec9("1.44269504"));
	writeln("passed");
}

/**
 * Decimal version of std.math.pow.
 * Required by General Decimal Arithmetic Specification
 */
public Decimal pow(T)(T x, Decimal y) {
	return power(x,y);
}

unittest {
	write("pow............");
	writeln("test missing");
}


/**
 * power.
 * Required by General Decimal Arithmetic Specification
 */
public Decimal power(T)(T x, T y) {
	return exp(x*ln(y));
}

unittest {
	write("power..........");
	writeln("test missing");
}

/// Returns the square root of the sum of the squares to the specified precision.
/// Decimal version of std.math function.
public T hypot(T)(T x, T y, int precision = T.precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return hypot!T(x, y, T.context);
}

/// Returns the square root of the sum of the squares in the specified context.
/// Decimal version of std.math function.
public T hypot(T)(T x, T y, Context context)
{
	// special values
	if (x.isInfinite || y.isInfinite) return T.infinity();
    if (x.isZero) return y; //.copy;
	if (y.isZero) return x; //.copy;
	if (x.isNaN || y.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
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
    b = div(b ,a, context);
    return mul(a, sqrt(add(T.one, sqr(b, context),context),context));
}

// TODO: (testing) Need to test operation near precisions where this operation is really useful.
unittest {
	write("-- hypot............");
	dec9 x = 3;
	dec9 y = 4;
	dec9 expect = 5;
	dec9 actual = hypot(x,y);
	assertTrue(actual == expect);
	assertEqual(actual, expect);
	writeln("passed");
}

//--------------------------------
// TRIGONOMETRIC FUNCTIONS
//--------------------------------

// Returns the reduced argument and the quadrant.
//o 0 <= pi/4 and sets the quadrant.
private T reducedAngle(T)(T x, out int quadrant, Context inContext) {
	auto context = guard(inContext);
	T c = mul(invPi!T(context), 2, context);
	x = mul(x, c, context);
	int k = trunc(x).toInt;
	if (k < 0) k = 1 - k;
	quadrant = k % 4;
	T red = div(sub(x, k, context), c, context);
	return red;
}

/// Decimal version of std.math function.
public T sin(T)(T x, int precision = T.precision) {
	// TODO: (language) check validity of x
	auto context = Context(precision, Rounding.HALF_EVEN);
	int quadrant;
	T red = reducedAngle(x, quadrant, dec9.context);
	switch(quadrant) {
		case 0: return( sin( red, context));
		case 1: return( cos( red, context));
		case 2: return(-sin( red, context));
		case 3: return(-cos( red, context));
		default: return T.nan;
	}
}

// Decimal version of std.math function.
private T sin(T)(T x, Context inContext) {
	auto context = guard(inContext);
	T sum = 0;
	int n = 1;
	T powx = x;
	T sqrx = sqr(x, context);
	T fact = 1;	// TODO: this should be bigint
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n += 2;
		powx = mul(-powx, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(powx, fact, context);
	}
	return roundToPrecision(sum, inContext);
}

/*/// Decimal version of std.math function.
public T calcSin(T)(T x) {
	T sum = 0;
	int n = 1;
	T powx = x;
	T sqrx = x * x;
	T fact = 1;
	T term = powx;
writefln("term = %s", term);
	while (term.copyAbs > T.epsilon) {
		sum += term;
writefln("n = %s", n);
		n += 2;
		powx = -powx * sqrx;
		fact = fact * (n*(n-1));
writefln("fact = %s", fact);
		term = powx/fact;
writefln("term = %s", term);
	}
	return sum;
}*/

unittest {
	write("sin..........");
	writeln;
	writeln("sin(1) = 0.84147098480789650665250232163029899962256306079837");
writefln("sin(1, dec9.context) = %s", sin(dec9.one, dec9.context));
//	pushContext(50);
	writefln("sin(1) = %s", sin(dec9.one));
writefln("...");
writeln;
	dec9 test = dec9(2); //10,22);
writefln("test = %s", test);
//	writefln("sin(10^^22) = %s", sin(test));
	/*dec9*/ test = dec9("22000.12345");
writefln("test = %s", test);
	/*dec9*/ test = dec9("2");
//	/*dec9*/ test = dec9("1");
//	/*dec9*/ test = dec9("0.5");
//	writeln("sin(22) = -0.008851309290403875921690256815772332463289203951");
dec9.verbose = true;
writefln("test = %s", test);
	writefln("sin(test) = %s", sin(dec9(2)));
	writefln("sin(2) = %s", sin(dec9(2), 12));
dec9.verbose = false;
/*
//	popContext();
	writefln("sin(101.23456789) = %s", sin(dec9("101.23456789"), 25));
	writefln("sin(pi + 1.0) = %s", sin(pi(25) + dec9("1.0"),25));

	writeln("..failed");*/
}

/// Decimal version of std.math function.
/*public T cos(T)(const T x) {
	int quadrant;
	T red = reducedAngle(x, quadrant);
	switch(quadrant) {
		case 0: return(calcCos(red));
		case 1: return(calcSin(-red));
		case 2: return(-calcSin(red));
		case 3: return(-calcCos(-red));
		case 4: return(-calcCos(red));
		case 5: return(-calcSin(-red));
		case 6: return(calcSin(red));
		case 7: return(calcCos(-red));
		default: return T.nan;
	}
}*/

/// Decimal version of std.math function.
public T cos(T)(T x, int precision = T.precision) {
	// TODO: (language) check validity of x
	auto context = Context(precision, Rounding.HALF_EVEN);
	int quadrant;
	T red = reducedAngle(x, quadrant, context);
	switch(quadrant) {
		case 0: return( cos(red, context));
		case 1: return(-sin(red, context));
		case 2: return(-cos(red, context));
		case 3: return( sin(red, context));
		default: return T.nan;
	}
}

/// Decimal version of std.math function.
public T cos(T)(T x, Context context) {
	T sum = 0;
	int n = 0;
	T powx = 1;
	T sqrx = sqr(x, context);
	T fact = 1;	// TODO: this should be bigint
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n += 2;
		powx = mul(-powx, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(powx, fact, context);
	}
	return sum;
}

unittest {
	write("cos..........");
	writeln;
	writeln("cos(1) = 0.54030230586813971740093660744297660373231042061792");
writefln("cos(dec9.one, dec9.context) = %s", cos(dec9.one, dec9.context));
	writefln("cos(1) = %s", cos(dec9(1)));
	writeln("..failed");
}

/**
 * Replaces std.math function expi
 *
 */
// TODO: (behavior) context, angle reduction
public void sincos(T)(T x, out T sine, out T cosine) {

	T csum, cterm, cx;
	T ssum, sterm, sx;
	T sqrx = x*x;
	long n = 2;
	T fact = 1;
	cx = 1;	cterm = cx;	csum = cterm;
	sx = x;	sterm = sx;	ssum = sterm;
	while (sterm.abs > T.epsilon/* && n < 10*/) {
		cx = -sx;
		fact = fact * n++;
		cterm = cx/fact;
		csum = csum + cterm;
		sx = -sx*sqrx;
		fact = fact  * n++;
		sterm = sx/fact;
		ssum = ssum + sterm;
	}
    sine = ssum;
	cosine = csum;
}

unittest {
	write("sincos.......");
	dec9 sine;
	dec9 cosine;
	sincos(dec9("1.0"), sine, cosine);
writeln;
writefln("sine = %s", sine);
writefln("cosine = %s", cosine);
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
 // Newton's method .. is it faster?
public T tan(T)(T x) {
	T sine;
	T cosine;
	sincos(x, sine, cosine);
	return sine/cosine;
}

unittest {
	write("tan..........");
	// tan(1.0) = 1.5574077246549022305069748074583601730872507723815
writefln("tan(1.0) = %s", tan(dec9("1.0")));

	writeln("..failed");
}

/// Calculates the value of pi in the specified context.
private T arctan(T)(T x, Context inContext = T.context) {
	auto context = guard(inContext, 3);
	int k = 1;
	sqrt1_2!T(context);
	T a1 = sqrt(T.one + sqr(x));
	T b1 = T.one;
	T s1 = T.half;
	T a2, b2, s2;
	while (!equals(a1, b1, context)) {
//		a2 = (a1 + b1) * T.half;		// arithmetic mean
//		b2 = sqrt(a1*b1);				// geometric mean
//		s2 = s1 - k*(sqr(a2)-sqr(b2));  // weighted sum of the difference of the means
//		pi = T.two * sqr(a2)/s2;
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

/**
 * Decimal version of std.math function.
 *
 */
public T asin(T)(T x) {
	T result = 2 * atan!T(x/(1+sqrt(1-sqr(x)))); //^^2)));
	return result;
}

unittest {
	write("asin.........");
	dec9 x;
	x = 0.5;
	dec9 a = asin(x);
writeln;
writefln("asin(0.5) = %s", a);
writeln("asin(0.5) = 0.523599");
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T acos(T)(T x) {
	T result = 2 * atan(sqrt(1-sqr(x))/(1 + x));
	return result;
}

unittest {
	write("acos.........");
	dec9 x;
	x = 0.5;
	dec9 a = acos(x);
writeln;
writefln("acos(0.5) = %s", a);
writeln("acos(0.5) = 1.0472");
	writeln("..failed");
}

/+
/// Decimal version of std.math function.
// TODO: (efficiency) only valid if x < 1.0; convergence very slow if x ~ 1.0;
public Decimal arctanT)(T x) {
	Decimal a = 1;
	Decimal g = sqrt(1 + sqr(x));
writefln("a = %s", a);
writefln("g = %s", g);
	for (int i = 0; i < 10; i++) {//while (abs(a-g) < Decimal.epsilon) {
writeln (" -- " );
		a = (a + g) * Decimal.HALF;
writefln("a = %s", a);
writefln("a*g = %s", a*g);
		g = sqrt(a*g);
writefln("sqrt(a*g) = %s", g);
//writefln("a-g = %s", a-g);
	}
	return x/a;
}

+/
public T atan(T)(T x, int precision = T.precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return atan!T(x, context);
}

/// Returns the arctangent of the argument in the specified context.
/// Algorithm uses Taylor's theorem for arctangent.
public T atan(T)(T x, Context inContext) {
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
	writeln("arctan.........");
	writeln ("math.arctan(1.0) = 0.7853981633974483096156608458198757210492923498438");
writefln("       atan(1.0) = %s", atan!dec9(dec9("1.0")));
writefln("     arctan(1.0) = %s", arctan!dec9(dec9("1.0")));

writeln ("math.arctan(0.1) = 0.099668652491162038065120043484057532623410224914551");
writefln("       atan(1.0) = %s", atan!dec9(dec9("0.1"), 15));
writefln("     arctan(0.1) = %s", arctan(dec9("0.1")));

writeln ("math.arctan(0.9) = 0.73281510178650655085164089541649445891380310058594");
writefln("       atan(0.9) = %s", atan!dec9(dec9("0.9")));
writefln("     arctan(0.9) = %s", arctan(dec9("0.9")));

	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public dec9 atan2(T)(T y, dec9 x) {
	dec9 result;
	return result;
}

unittest {
	write("atan2........");
	writeln("..failed");
}

//--------------------------------
// HYPERBOLIC TRIGONOMETRIC FUNCTIONS
//--------------------------------

/// Decimal version of std.math function.
public T sinh(T)(T x) {
	long n = 1;
	T sum = 0;
	T powx = x;
	T sqrx = x * x;
	T fact = n;
	T term = powx;
	while (term.abs > T.epsilon) {
		sum += term;
		n += 2;
		fact = fact * (n*(n-1));
		powx = powx * sqrx;
		term = powx/fact;
	}
	return sum;
}

unittest {
	write("sinh.........");
writeln("sinh(1.0) = 1.1752011936438014568823818505956008151557179813341");
writefln("sinh(1.0) = %s", sinh(dec9("1.0")));
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T cosh(T)(T x) {
	long n = 0;
	T sum = 0;
	T powx = 1;
	T sqrx = x * x;
	T fact = 1;
	T term = powx;
	while (term.abs > T.epsilon) {
		sum += term;
		n += 2;
		fact = fact * (n*(n-1));
		powx = powx * sqrx;
		term = powx/fact;
	}
	return sum;
}

unittest {
	write("cosh.........");
writeln("cosh(1.0) = 1.5430806348152437784779056207570616826015291123659");
writefln("cosh(1.0) = %s", cosh(dec9("1.0")));
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T tanh(T)(T x) {
	return cosh(x)/sinh(x);
}

unittest {
	write("tanh.........");
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T asinh(T)(T x) {
	// TODO: (behavior) special values
	T arg = x + sqrt(sqr(x) + T.one);
	return ln(arg);
}

unittest {
	write("asinh........");
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T acosh(T)(T x) {
	// TODO: (behavior) special values
	T arg = x + sqrt(x+T.one)* sqrt(x-T.one);
	return ln(arg);
}

unittest {
	write("acosh........");
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public T atanh(T)(T x) {
	// TODO: (behavior) special values
	T arg = (x + T.one)/(x-T.one);
	return T.HALF * ln(arg);
	// also atanh(x) = x + x^3/3 + x^5/5 + x^7/7 + ... (speed of convergence?)
}

unittest {
	write("atanh........");
	writeln("..failed");
}

//--------------------------------
//
// General Decimal Arithmetic Specification Functions
//
//--------------------------------

/**
 * part of spec
 *
 */
public T ln(T)(T x) {
	return log(x);
}

unittest {
	write("ln.............");
	writeln("test missing");
}

unittest {
	writeln("==========================");
	writeln("decimal math...........end");
	writeln("==========================");
}



