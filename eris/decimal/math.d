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

//--------------------------------
// ROUNDING
//--------------------------------

// FIXTHIS: need to be able to use guarded numbers..
/// Rounds the argument to an integer using the specified rounding mode.
/// The default rounding mode is the current context mode.
public T round(T)(const T arg, const Rounding mode = Rounding.HALF_EVEN) {
	T value = roundToIntegralExact(arg, mode);
	return value;
}

/// Rounds the argument to the nearest integer. If the argument is exactly
/// half-way between two integers the even integer is returned.
public T rint(T)(const T arg) {
	return round(arg, Rounding.HALF_EVEN);
}

/// Returns the nearest integer less than or equal to the argument.
/// Rounds toward negative infinity.
public T floor(T)(const T arg) {
	return round(arg, Rounding.FLOOR);
}

/// Returns the nearest integer greater than or equal to the argument.
/// Rounds toward positive infinity.
public T ceil(T)(const T arg) {
	return round(arg, Rounding.CEILING);
}

/// Returns the truncated argument.
/// Rounds toward zero.
public T trunc(T)(const T arg) {
	return round(arg, Rounding.DOWN);
}

/// Returns the nearest integer value. If the value is greater (less) than
/// the maximum (minimum) int value the maximum (minimum) value is returned.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public int toInt(T)(const T arg,
		const Rounding mode = Rounding.HALF_EVEN) {
	if (arg.isNaN) return 0;
	if (arg.isInfinite) return arg.isNegative ? int.min : int.max;
	return toBigInt(arg, mode).toInt;
}

/// Returns the nearest long value. If the value is greater (less) than
/// the maximum (minimum) long value the maximum (minimum) value is returned.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public long toLong(T)(const T arg,
		const Rounding mode = Rounding.HALF_EVEN) {
	if (arg.isNaN) return 0;
	if (arg.isInfinite) return arg.isNegative ? long.min : long.max;
	return toBigInt(arg, mode).toLong;
}

/// Returns the nearest extended integer value.
/// The value is rounded based on the specified rounding mode. The default
/// mode is half-even.
public xint toBigInt(T)(const T arg,
		const Rounding mode = Rounding.HALF_EVEN) {
	if (arg.isNaN) return xint(0);
	if (arg.isInfinite) {
		return arg.isNegative ? T.min.coefficient : T.max.coefficient;
	}
	if (arg.exponent != 0) {
		return round(arg, mode).coefficient;
	}
	return arg.coefficient;
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
//	CONSTANTS
//--------------------------------

template GenConstant(string name)
{
const char[] GenConstant =
	"/// Returns the value of the constant at its built-in precision.\n" ~
	"public T " ~ name ~ "(T)() {
		static T value;
		if (value.isNaN) value = " ~ name ~ "!T(T.precision);
		return value;
	}" ~

	"/// Returns the value of the constant at the specified precision.\n" ~
   	"public T " ~ name ~ "(T)(int precision) {
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

mixin (GenConstant!("pi"));
mixin (GenConstant!("invPi"));
mixin (GenConstant!("e"));
mixin (GenConstant!("ln10"));
mixin (GenConstant!("ln2"));
mixin (GenConstant!("sqrt2"));
mixin (GenConstant!("sqrt1_2"));

/// Adds guard digits to the context precision and sets rounding to HALF_EVEN.
private Context guard(Context context, int guardDigits = 2) {
	return Context(context.precision + guardDigits, context.rounding);
}

/// Calculates the value of pi in the specified context.
private T pi(T)(in Context inContext) {
	auto context = guard(inContext, 3);
	int k = 1;
	T a1 = T.one;
	T b1 = sqrt1_2!T(context);
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

unittest {
	write("-- pi...............");
	assertStringEqual(dec9("3.14159265358979"), pi!dec9(15));
	assertStringEqual(dec9("3.14159265"), pi!dec9);
	assertStringEqual(dec9("3.141592653589793238462643"), pi!dec9(25));
//writefln("pi!dec9(5) = %s", pi!dec9(5)); // TODO: (behavior) fails with small precision
	writeln("passed");
}
//	immutable decimal PI = roundString("3.141592653589793238462643 3832795028841"
//		"9716939937510582097494459230781640628620899862803482534211707");

// TODO: (efficiency) Need to ensure that previous version of pi isn't reset.
// TODO: (behavior) Reciprocal is way worse than one over division.
/// Calculates the value of pi in the specified context.
private T invPi(T)(in Context inContext) {
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
private T e(T)(in Context inContext) {
	auto context = guard(inContext);
	long n = 2;
	T term = T.one;
	T sum  = T.one;
	while (term > T.epsilon(context)) { // && n < 20) {
		sum  = add(sum, term, context); //+= term;
		term = div!T(term, T(n), context);
		n++;
	}
	return roundToPrecision(sum, inContext);
}

/// Returns e to the type precision.
//public enum E(T) = e!T(T.precision);

unittest {
	write("-- e................");
	assertStringEqual(dec9("2.71828183"), e!dec9);
	assertStringEqual(dec9("2.7182818284590452353602874713526625"), e!dec9(35));
	writeln("passed");
}

/*
// TODO: why does this compile at the same time as the enum
public T E(T)() {
	static T value;
writefln("value = %s", value);
	return value? value : e(T.precision);
}
*/

private T ln10(T)(Context context) {
	return log(T.TEN, context);
}

private T ln2(T)(Context context) {
	return log(T.TWO, context);
}

private T sqrt2(T)(Context context) {
	return sqrt(T.TWO, context);
}

private enum T sqrt1_2(T)(Context context) {
	return sqrt(T.HALF, context);
}

unittest {
	write("constants...");
writefln("dec9.sqrt1_2 = %s", sqrt1_2!dec9);
	writeln("test missing");
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

public T reciprocal(T)(in T x, int precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return reciprocal!T(x, context);
}
// NOTE: faster (is it?) but requires more precision
private T reciprocal(T)(in T x, in Context inContext) {

	// special values
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.copyAbs.isOne) return x.dup;
	if (x.isInfinite) return T.zero(x.sign);
	if (x.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}

	// extend precision
	auto context = guard(inContext);

	// initial estimate
	T x0 = x.reduce;
	T x1 = T.zero;
	T x2 = T(2, -ilogb(x0)-1);

	// Newton's method
	while (true) {
		x1 = x2;
//		x2 = x1 * (2 - x0 * x1);
		x2 = mul(x1, sub(T.two, mul(x0, x1, context), context), context);
		if (equals(x1, x2 ,context)) break;
	}
	// round to input precision
	return roundToPrecision(x2,inContext);
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
	b = reciprocal(num,10);
	assertEqual(b, a);
	writeln("passed");
}

public T invSqrt(T)(in T x) {
	return invSqrt!T(x, T.precision);
}

public T invSqrt(T)(in T x, int precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return invSqrt!T(x, context);
}

// TODO: (behavior) sometimes fails to break out of loop (e.g. reduce guard digits to +2)
public T invSqrt(T)(in T x, in Context inContext = T.context) {
	// special values
	if (x.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.isOne) return x.dup;
	if (x.isInfinite) return T.zero(x.sign);

	// extend precision slightly
	Context context = Context(inContext.precision + 3, inContext.rounding);

	// operands
	T x0 = x.dup;
	T x1, x2;
//writefln("\n--- x0 = %s", x0);

	// initial estimate
	int k = ilogb(x0);
	if (isOdd(k)) {
		x2 = T(2, -1);
	}
	else {
		x2 = T(5, -1);
		k++;
	}
	const x3 = T(3);

	// reduce the exponent
	x0.exponent = x0.exponent - k - 1;

	// Newton's method
//	auto count = 0;
//	while (count < 40) {
	while (true) {
		x1 = x2;
//		x2 = x1 * T.half * (x3 - x0 * sqr(x1));
		x2 = mul(x1, mul(T.half, sub(x3, mul(x0, sqr(x1, context), context), context), context), context);
		if (equals(x1, x2, context)) break;
//		count++;
	}
	// restore the exponent
	x2.exponent = x2.exponent - k/2 - 1;
	// round the result
	return roundToPrecision(x2, inContext);
}

unittest {
	write("-- inverse sqrt.....");
//	assertEqual(invSqrt(dec9.two, 11), dec9("0.707106781"));
	assertEqual(invSqrt(dec9(2))/*),11)*/, dec9("0.707106781"));
//writefln("invSqrt(dec9(2), 14) = %s", invSqrt(dec9(2), 14));
	assertEqual(invSqrt(dec9(2), 14)/*),11)*/, dec9("0.70710678118655"));
	assertEqual(invSqrt(dec9(20))/*),11)*/, dec9("0.223606798"));
//	assertEqual(invSqrt(dec9(20))/*,11)*/, dec9("2.23606798E-8"));
	assertEqual(invSqrt(dec9(300))/*,11)*/, dec9("0.0577350269"));
	assertEqual(invSqrt(dec9(4000),11), dec9("0.0158113883"));
//	assertEqual(invSqrt(dec9(50000),7), dec9("0.00000141421356"));
//	assertEqual(invSqrt(dec9(600000),29), dec9("4.08248290E-18"));
	assertEqual(invSqrt(dec9(98763)), dec9("0.00318201969"));
	assertEqual(invSqrt(dec9(98763098)), dec9("0.000100624248"));
	assertEqual(invSqrt(dec9(9876387982347)), dec9("3.18200552E-7"));
	writeln("passed");
}


public T sqrt(T)(in T x) {
	return sqrt!T(x, T.precision);
}

public T sqrt(T)(in T x, int precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return sqrt!T(x, context);
}

/// Returns the square root of the argument to the type precision.
/// Uses Newton's method.
public T sqrt(T)(in T x, in Context context) {
	// special values
	if (x.isNaN || x.isNegative) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	if (x.isOne) return T.one;
	if (x.isZero) return T.zero;
	if (x.isInfinite) return T.infinity;

	// guard the operands
	T arg = T(x); //T.guard!T(x);
	T old = T.zero; //T.guard!T.zero;
	T est = T.zero; //T.guard!T.zero;

	// reduce the exponent and estimate the result
	int k = ilogb(arg);
	if (isOdd(k)) {
		est = T(6, -1);
	}
	else {
		est = T(2, -1);
		k++;
	}
	arg.exponent = arg.exponent - k - 1;

	// Newton's method
	int count = 0;
	// TODO: estimated value may cycle with lots of guard digits.
	// possibly we are not using all the guard digits somewhere?
	while(count < 200) { //(true) {
		// save the previous value
		old = est;
		// calculate the new value
		est = mul(T.half, add(old, div(arg, old, context), context), context);

		// if new == old, we're done
		if (equals(est, old, context)) break;
		count++;
	}
	// restore the exponent
	est.exponent = est.exponent + (k+1)/2;
	// round the result
	return roundToPrecision(est, context);
}

unittest {
	write("-- square root......");
//writefln("sqrt(t.TWO, 25) = %s", sqrt(dec9.two, 11));
//writefln("sqrt(t.TWO, 30) = %s", sqrt(dec9.two, 30));
//writefln("sqrt(t.TWO, 25) = %s", sqrt(dec9.two, 25));

//auto r1 = sqrt(dec9(2));
//auto r2 = reciprocal(r1);
//auto r3 = sqrt(r2);
//writefln("r1 = %s", r1);
//writefln("r2 = %s", r2);
//writefln("r3 = %s", r3);

//writeln;
/*for (int i = 0; i < 10; i++) {
	dec9 val = dec9(3, i);
//	writefln("val = %s", val);
	writefln("sqrt(val) = %s", sqrt(val));
}*/
/*sqrt(val) = 1.73205081
sqrt(val) = 5.47722560
sqrt(val) = 17.3205081
sqrt(val) = 54.7722560
sqrt(val) = 173.205081
sqrt(val) = 547.722560
sqrt(val) = 1732.05081
sqrt(val) = 5477.22560
sqrt(val) = 17320.5081
sqrt(val) = 54772.2560*/

assertEqual(sqrt(dec9(2)), dec9("1.41421356"));
// 1.41421356237310
// 0.707106781186548
// 0.840896415253715
assertEqual(sqrt(dec9(dec9.one/sqrt(dec9(2)))), dec9("0.840896415"));
assertEqual(sqrt(dec9(200)), dec9("14.1421356"));
assertEqual(sqrt(dec9(25)), dec9("5.00000000"));
assertEqual(sqrt(dec9(2E-5)), dec9("0.00447213596"));
assertEqual(sqrt(dec9(1E-15)), dec9("3.16227766E-8"));
assertEqual(sqrt(dec9(1E-16)), dec9("1.00000000E-8"));
	writeln("passed");
}

/// Returns the square root of the sum of the squares to the current precision.
/// Decimal version of std.math function.
public T hypot(T)(in T x, in T y) {
	return hypot!T(x, y, T.context);
}

/// Returns the square root of the sum of the squares to the specified precision.
/// Decimal version of std.math function.
public T hypot(T)(in T x, in T y, int precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return hypot!T(x, y, T.context);
}

/// Returns the square root of the sum of the squares in the specified context.
/// Decimal version of std.math function.
public T hypot(T)(const T x, const T y, Context context)
{
	// special values
	if (x.isInfinite || y.isInfinite) return T.infinity();
    if (x.isZero) return y.copy;
	if (y.isZero) return x.copy;
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
// EXPONENTIAL AND LOGARITHMIC FUNCTIONS
//--------------------------------

/// Decimal version of a std.math function.
/// Required by General Decimal Arithmetic Specification
public T exp(T)(in T x, int precision = T.precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	return exp(x, context);
}

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
private T exp(T)(in T x, Context inContext)
{
	if (x.isNaN || x.isNegative) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	auto context = guard(inContext);
	T x0   = x.dup;
	T sqrx = sqr(x, context);
	long n = 1;
	T fact = T.one;
	T t1   = T.one;
	T t2   = x0;
	T term = add(t1, t2, context);
	T sum  = term;
	while (term > T.epsilon(context)) {
		n   += 2;
		t1   = mul(t2, mul(x0, n, context), context);
		t2   = mul(t2, sqrx, context);
		fact = mul(fact, (n * (n-1)), context);
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
public Decimal exp2(Decimal arg) {
	Decimal result;
	return result;
}

unittest {
	write("exp2...........");
	writeln("test missing");
}

/// Returns exp(x) - 1.
/// expm1(x) will be more accurate than exp(x) - 1 for x near 1.
/// Decimal version of std.math function.
/// Reference: Beebe, Nelson H. F., "Computation of expm1(x) = exp(x) - 1".
public Decimal expm1(Decimal x) {
//	if (invalidOperand!Decimal(arg, arg)) {
	if (x.isNaN) return Decimal.nan;
	if (x.isZero) return x;
	// this function only useful near zero
	const Decimal lower = Decimal("-0.7");
	const Decimal upper = Decimal("0.5");
	if (x < lower || x > upper) return exp(x) - Decimal.one;

	Decimal term = x;
	Decimal sum = Decimal.zero;
	long n = 1;
	// TODO: (efficiency) make this test more efficient
	while (term.copyAbs > Decimal.epsilon) {
		sum += term;
		term *= (x / ++n);
	}
	return sum;
}

unittest {
	write("expm1..........");
	writeln("test missing");
}
+/

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
public T log(T)(in T x, int precision = T.precision) {
	Context context = Context(precision, Rounding.HALF_EVEN);
	if (x.isNaN) return T.nan;
	int k = ilogb(x) + 1;
	T a = T(x.sign, x.coefficient, x.exponent - k);
	return log(a, context) + ln10!T(precision) * k;
}

// TODO: (language) this seems to compile but not used...?
/// Returns pi to the type precision.
//public enum PI(T) = pi!T(T.precision);

//unittest {
//	write("ln10...");
//writefln("ln10!dec9(10) = %s", PI!dec9(10));
//	writeln("test missing");
//}

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
private T log(T)(in T x, Context inContext) {
	auto context = guard(inContext);
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	T xc = x.dup;
	T y = div(sub(xc, 1, context), add(xc, 1, context), context);
	T ysqr = sqr(y, context);
	T term = y;
	T sum  = y;
	long n = 3;
	while (true) {
		term = mul(term, ysqr, context);
		T nsum = add(sum, div(term, T(n), context), context);
		if (equals(sum, nsum, context)) {
			return roundToPrecision(mul(sum, 2, context), inContext);
		}
		sum = nsum;
		n += 2;
	}
}

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
/*private T calcLog(T)(const T x) {
	T xc = x.dup;
//	xc.isGuarded = true;
	T y = (xc - 1)/(xc + 1);
	T yy = sqr(y);
	T term = y;
	T sum  = y;
	long n = 3;
	while (true) {
		term *= yy;
		auto nsum = sum + (term/n);
		if (sum == nsum) {
//			sum.isGuarded = false;
			return roundToPrecision(sum * 2);
		}
		sum = nsum;
		n += 2;
	}
}*/

unittest {
	write("log............");
writeln;
	dec9 one = dec9.one;
writefln("log(exp(one)) = %s", log(exp(one)));
writefln("log(dec9(10)) = %s", log(dec9(10)));
writefln("log(dec9(\"99.999e+8\")) = %s", log(dec9("99.999e+88")));
	writeln("test missing");
}

/**
 * log1p (== log(1 + x)).
 * Decimal version of std.math function.
 */
public T log1p(T)(const T x) {
	auto term = x.dup;
	auto pwr  = x.dup;
	auto sum  = T.zero;
	auto n    = T.one;
	while (true) {
		sum += term;
		pwr = -pwr * x;
		n++;
		term = pwr/n;
		if (term.copyAbs < T.epsilon) {
			sum += term;
			break;
		}
	}
	return sum/ln10!T;
}

unittest {
	write("log1p..........");
	dec9 x = "0.1";
writefln("log1p(x) = %s", log1p(x));
	writeln("test missing");
}

/// Decimal version of std.math.log10.
/// Required by General Decimal Arithmetic Specification
public T log10(T)(const T x) {
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	int k = ilogb(x) + 1;
	T a = T(x.sign, x.coefficient, x.exponent - k);
	return log(a)/ln10!T + k;
}

unittest {
	writeln("log10..........");
	dec9 x = dec9("2.55");
writefln("x = %s", x);
writefln("log(x) = %s", log10(x));
writeln("std.math.log10(2.55) = ", std.math.log10(2.55));
	x = 123.456;
writefln("x = %s", x);
writefln("log(x) = %s", log10(x));
writeln("std.math.log(123.456) = ", std.math.log10(123.456));
	x = 10.0;
writefln("x = %s", x);
writefln("log(x) = %s", log(x));

	writeln("test missing");
}

/+
/**
 * Decimal version of std.math.log2.
 * Required by General Decimal Arithmetic Specification
 */
public Decimal log2(Decimal arg) {
	Decimal result;
	return result;
}

unittest {
	write("log2...........");
	writeln("test missing");
}

/**
 * Decimal version of std.math.pow.
 * Required by General Decimal Arithmetic Specification
 */
public Decimal pow(Decimal x, Decimal y) {
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
public Decimal power(Decimal x, Decimal y) {
	return exp(x*ln(y));
}

unittest {
	write("power..........");
	writeln("test missing");
}
+/
//--------------------------------
// TRIGONOMETRIC FUNCTIONS
//--------------------------------

// Returns the reduced argument and the quadrant.
//o 0 <= pi/4 and sets the quadrant.
private T reducedAngle(T)(in T xIn, out int quadrant, in Context inContext) {
	auto context = guard(inContext);
	T c = mul(invPi!T(context), 2, context);
	T x = mul(xIn, c, context);
	int k = trunc(x).toInt;
	if (k < 0) k = 1 - k;
	quadrant = k % 4;
	T red = div(sub(x, k, context), c, context);
	return red;
}

/// Decimal version of std.math function.
public T sin(T)(in T x, int precision = T.precision) {
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
private T sin(T)(in T x, Context inContext) {
	auto context = guard(inContext);
	T sum = 0;
	int n = 1;
	T powx = x.dup;
	T sqrx = sqr(x, context);
	T fact = 1;
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
public T calcSin(T)(in T x) {
	T sum = 0;
	int n = 1;
	T powx = x.dup;
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
public T cos(T)(in T x, int precision = T.precision) {
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
public T cos(T)(in T x, Context context) {
	T sum = 0;
	int n = 0;
	T powx = 1;
	T sqrx = sqr(x, context);
	T fact = 1;
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
	sx = x.dup;	sterm = sx;	ssum = sterm;
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

/+
/**
 * Decimal version of std.math function.
 *
 */
public Decimal asin(Decimal arg) {
	Decimal result;
	return result;
}

unittest {
	write("asin.........");
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public Decimal acos(Decimal arg) {
	Decimal result;
	return result;
}

unittest {
	write("acos.........");
	writeln("..failed");
}


/// Decimal version of std.math function.
// TODO: (efficiency) only valid if x < 1.0; convergence very slow if x ~ 1.0;
public Decimal arctan(Decimal x) {
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

public Decimal atan(Decimal x) {
	Decimal sum = 0;
	Decimal powx = x.dup;
	Decimal sqrx = x * x;
	Decimal dvsr = 1;
	Decimal term = powx;
	while (term.abs > Decimal.epsilon && dvsr < Decimal(50)) {
		sum += term;
		powx = -powx * sqrx;
		dvsr = dvsr + 2; //dvsr * (n*(n-1));
		term = powx/dvsr;
	}
	return sum;
}

unittest {
	writeln("arctan.........");
writeln ("math.arctan(1.0) = 0.7853981633974483096156608458198757210492923498438");
writefln("       atan(1.0) = %s", atan(Decimal("1.0")));
writefln("     arctan(1.0) = %s", arctan(Decimal("1.0")));
writeln ("math.arctan(0.1) = 0.099668652491162038065120043484057532623410224914551");
writefln("       atan(0.1) = %s", atan(Decimal("0.1")));
writefln("     arctan(0.1) = %s", arctan(Decimal("0.1")));
writeln ("math.arctan(0.9) = 0.73281510178650655085164089541649445891380310058594");
writefln("       atan(0.9) = %s", atan(Decimal("0.9")));
writefln("     arctan(0.9) = %s", arctan(Decimal("0.9")));
//writefln("arctan(0.9)) = %s", arctan(Decimal("0.9")));
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public Decimal atan2(Decimal y, Decimal x) {
	Decimal result;
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
public Decimal sinh(Decimal x) {
	long n = 1;
	Decimal sum = 0;
	Decimal powx = x.dup;
	Decimal sqrx = x * x;
	Decimal fact = n;
	Decimal term = powx;
	while (term.abs > Decimal.epsilon) {
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
	// sinh(1.0) = 1.1752011936438014568823818505956008151557179813341
writefln("sinh(1.0) = %s", sinh(Decimal("1.0")));
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public Decimal cosh(Decimal x) {
	long n = 0;
	Decimal sum = 0;
	Decimal powx = 1;
	Decimal sqrx = x * x;
	Decimal fact = 1;
	Decimal term = powx;
	while (term.abs > Decimal.epsilon) {
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
	// cosh(1.0) = 1.5430806348152437784779056207570616826015291123659
writefln("cosh(1.0) = %s", cosh(Decimal("1.0")));
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
public Decimal tanh(Decimal x) {
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
public Decimal asinh(Decimal x) {
	// TODO: (behavior) special values
	Decimal arg = x + sqrt(sqr(x) + Decimal.one);
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
public Decimal acosh(Decimal x) {
	// TODO: (behavior) special values
	Decimal arg = x + sqrt(x+Decimal.one)* sqrt(x-Decimal.one);
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
public Decimal atanh(const Decimal x) {
	// TODO: (behavior) special values
	Decimal arg = (x + Decimal.one)/(x-Decimal.one);
	return Decimal.HALF * ln(arg);
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
public Decimal ln(Decimal x) {
	return log(x);
}

unittest {
	write("ln.............");
	writeln("test missing");
}
+/
unittest {
	writeln("==========================");
	writeln("decimal math...........end");
	writeln("==========================");
}



