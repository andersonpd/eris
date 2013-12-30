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

/// Rounds the argument to an integer using the specified rounding mode.
/// The default rounding mode is the current context mode.
public T round(T)(const T arg, const Rounding mode = contextRounding) {
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
	writeln("passed");
}

//--------------------------------
// CONSTANTS
//--------------------------------

/// Returns true if n is odd, false otherwise.
private bool isOdd(int n) {
	return n & 1;
}

unittest {	// isOdd
	assert(isOdd(3));
	assert(!isOdd(8));
	assert(isOdd(-1));
}

// TODO: add bitshift function

// NOTE: faster but not as accurate(?)
public T reciprocal(T)(const T a, const int precision = T.precision) {

	// special values
	if (a.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	if (a.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(a.sign);
	}
	if (a.copyAbs.isOne) return a.dup;
	if (a.isInfinite) return T.zero(a.sign);

	T.guardDigits = 2;
	T x = a.dup; //a.reduce; (?)
	T x1, x2;
	// initial estimate
	x1 = T.ZERO;
	x2 = T(2, -ilogb(x)-1);
	// Newton's method
	while (true) {
		x1 = x2;
		x2 = x1 * (T.TWO - x * x1);
		if (x2 == x1) break;
	}
	T.guardDigits = 0;
	return roundToPrecision(x2);
}

unittest {	// reciprocal
	write("-- reciprocal.......");
	dec9 one = dec9.one;
	dec9 num = dec9("1234567890123456789");
	dec9 a = one/num;
	dec9 b = reciprocal(num);
	assertEqual(b, a);
	num = dec9("12345678906789");
	a = one/num;
	b = reciprocal(num);
	assertEqual(b, a);
	writeln("passed");
}

public T invSqrt(T)(const T a, const int precision = T.precision) {
	// special values
	if (a.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	if (a.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(a.sign);
	}
	if (a.copyAbs.isOne) return a.dup;
	if (a.isInfinite) return T.zero(a.sign);

	T.guardDigits = 2;
	T x = a.dup;

	// initial estimate
	T x1, x2, x3;
	x1 = T.ZERO;
	int k = ilogb(x);
	if (isOdd(k)) {
		x2 = T(2, -1);
	}
	else {
		x2 = T(5, -1);
		k++;
	}
	// reduce the exponent
	x.exponent = x.exponent - k - 1;

	while (true) {
		x3 = x1;
		x1 = x2;
		x2 = x1 * T.HALF * (T.THREE - x * sqr(x1));
		if (x2 == x1 || x2 == x3) break;
	}
	// restore the exponent
	x2.exponent = x2.exponent - k/2 - 1;
	T.guardDigits = 0;
	return x2;
}

unittest {
	write("-- inverse sqrt.....");
	assertEqual(invSqrt(dec9.TWO), dec9("0.707106781"));
	assertEqual(invSqrt(dec9(20)), dec9("0.223606798"));
	assertEqual(invSqrt(dec9(20,14)), dec9("2.23606798E-8"));
	assertEqual(invSqrt(dec9(300,23)), dec9("1.82574186E-13"));
	assertEqual(invSqrt(dec9(4000,45)), dec9("5E-25"));
	assertEqual(invSqrt(dec9(50000,7)), dec9("0.00000141421356"));
	assertEqual(invSqrt(dec9(600000,29)), dec9("4.08248290E-18"));
/*	assertEqual(invSqrt(dec9(98763)), dec9("0.5"));
	assertEqual(invSqrt(dec9(98763098)), dec9("0.5"));
	assertEqual(invSqrt(dec9(9876387982347)), dec9("0.5"));*/
	writeln("passed");
}

/// Returns the square root of the argument to the type precision.
/// Uses Newton's method.
/// TODO: the precision can be adjusted as the computation proceeds
public T sqrt(T)(const T a) {
	// special values
	if (a.isNaN || a.isNegative) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	if (a.isInfinite) return T.infinity;

	T x = a.reduce;
	if (x.isOne) return x;

	// reduce the exponent and estimate the result
	T x1, x2;
	int k = ilogb(x);
	if (isOdd(k)) {
		x1 = T(6, -1);
	}
	else {
		x1 = T(2, -1);
		k++;
	}
	x.exponent = x.exponent - k - 1;

	// Newton's method
	x2 = x1;
	while(true) {
		x1 = x2;
		x2 = T.half * (x1 + x/x1);
		if (x2 == x1) break;
	}

	// restore the reduced exponent
	x2.exponent = x2.exponent + k/2 + 1;
	return x2;
}
unittest {
	write("-- square root......");
/*writeln;
for (int i = 0; i < 10; i++) {
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
assertEqual(sqrt(dec9(200)), dec9("14.1421356"));
assertEqual(sqrt(dec9(25)), dec9("5.00000000"));
assertEqual(sqrt(dec9(2E-5)), dec9("0.0447213596"));
assertEqual(sqrt(dec9(1E-15)), dec9("3.16227766E-7"));
assertEqual(sqrt(dec9(1E-16)), dec9("1.00000000E-7"));
	writeln("passed");
}

/// Returns the square root of the sum of the squares.
/// Decimal version of std.math function.
public T hypot(T)(const T x, const T y)
{
	// special values
	if (x.isNaN) return T.nan;
	if (x.isInfinite || y.isInfinite) return T.infinity();
    if (x.isZero) return y.copy;
	if (y.isZero) return x.copy;

	T a = x.copyAbs;
    T b = y.copyAbs;
	if (a < b) {
		//swap operands
		T t = a;
		a = b;
		b = t;
	}
    b /= a;
    return a * sqrt(T.one + sqr(b));
}

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

/// Returns the value of e at the current precision.
public T e(T)(int precision = T.precision) {
	static int lastPrecision = 0;
	static T value;
	if (precision != lastPrecision) {
//		if (precision > 99) {
			value = calcE!T();
/*		}
		else {
			value = roundToPrecision(E);
		}*/
		lastPrecision = precision;
	}
	return value;
}

/// Calculates and returns the value of e at the current precision.
private T calcE(T)() {
	long n = 2;
	T term = 1;
	T sum  = 1;
	while (term > T.epsilon) {
		sum += term;
		term /= n++;
	}
	return sum;
}

unittest {
	write("e.............");
	dec9 E = e!dec9();
writeln;
writefln("E      = %s", E);
/*writefln("e      = %s", e(35));
	pushContext(199);
	decimalContext.maxExpo = 250;
writefln("calcE()  = %s", calcE());
	popContext;
writefln("e(5)  = %s", e(5));*/
	writeln("test missing");
}

/+
unittest {	// PI
	assert(PI.digits == 99);
	assert(numDigits(PI.coefficient) == 99);
}

/// Returns the value of pi to the specified precision.
public Decimal pi(uint precision) {
	static int lastPrecision = 0;
	static Decimal value;
	if (precision != lastPrecision) {
		pushContext(precision);
		value = pi();
		popContext();
		lastPrecision = precision;
	}
	return value;
}

/// Returns the value of pi to the current precision.
public Decimal pi() {
	static int lastPrecision = 0;
	static Decimal value;
	int precision = decimalContext.precision;
	if (precision != lastPrecision) {
		if (precision > 99) {
			value = calcPi();
		}
		else {
			value = roundToPrecision(PI, decimalContext);
		}
		lastPrecision = precision;
	}
	return value;
}

/// Calculates the value of pi to the current precision.
public Decimal calcPi() {
	const Decimal one = Decimal(1);
	const Decimal two = Decimal(2L);
	const Decimal four = Decimal(4L);
	Decimal a = one.dup;
	Decimal b = a / sqrt(two); // TODO: use sqrt2 constant
	Decimal t = one/four;
	Decimal x = one.dup;
	while (a != b) {
		Decimal y = a;    // save the value of a
		a = (a + b)/two;     // arithmetic mean
		b = sqrt(b * y);       // geometric mean
		t -= x * (sqr(a) - sqr(b));  // weighted sum of the difference of the means
		// TODO: x = 2;
		x = x * 2;
	}
	Decimal result = sqr(a+b)/(four*t);
	return result;
}

unittest {
	write("pi.............");
writeln;
writefln("PI      = %s", PI);
writefln("pi      = %s", pi);
writefln("pi(25)  = %s", pi(25));
	writeln("test missing");
}
+/
//--------------------------------
//
// EXPONENTIAL AND LOGARITHMIC FUNCTIONS
//
//--------------------------------

/*public T exp(const T arg, const uint precision) {
	pushContext(precision);
	T value = exp(arg);
	popContext();
	return value;
}*/

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
public T exp(T)(const T x) {
	if (x.isNaN) return T.nan;
	if (x.isNegative) return T.nan;
//	if (x.isOne) return e();
	return exp0(x);
}

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
public T exp0(T)(const T x) {
	T sqrx = sqr(x);
	long n = 1;
	T fact = T.one;
	T t1   = T.one;
	T t2   = x.dup;
	T term = t1 + t2;
	T sum  = term;
	while (term > T.epsilon) {
		n   += 2;
		t1   = t2*x*n;
		t2   = t2*sqrx;
		fact = fact*(n*(n-1));
		term = (t1 + t2)/fact;
		sum += term;
	}
	return sum;
}

unittest {
	write("exp............");
writeln;
writefln("exp(1) = %s", exp(dec9.ONE));
	writeln("test missing");
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
	// TODO: make this test more efficient
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
public T log(T)(const T x) {
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	int k = ilogb(x) + 1;
	T a = T(x.sign, x.coefficient, x.exponent - k);
	return calcLog(a) + T.LN10 * k;
}

/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
private T calcLog(T)(const T x) {
	T y = (x - 1)/(x + 1);
	T ysq = sqr(y);
	T term = y;
	T sum  = y;
	long n = 3;
	while (true) {
		term *= ysq;
		auto nsum = sum + (term/n);
		if (sum == nsum) {
			return sum * 2;
		}
		sum = nsum;
		n += 2;
	}
}

unittest {
	write("log............");
writeln;
	dec9 one = dec9.one;
writefln("log(exp(one)) = %s", log(exp(one)));
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
	return sum/T.LN10;
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
	return calcLog(a)/T.LN10 + k;
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


// Returns the argument reduced to 0 <= pi/4 and sets the octant.
private T reducedAngle(T)(const T x, out int octant) {
	T x2 = 4*x/T.PI;
	T ki = trunc(x2);
	int k2 = trunc(x2).coefficient.toInt;
	if (k2 < 0) k2 = 1 - k2;
	T red = T.PI/4 * (x2 - k2);
	octant = k2 % 8;
	return red;
}

unittest {
	writeln("range reduction...");
	dec9 deg = dec9("180")/dec9.PI;
	int octant;
	for (int x = 0; x <= 720; x += 35) {
		dec9 xr = x/deg;
		dec9 y = reducedAngle(xr, octant);
		dec9 yd = y*deg;
		writefln("   x = %s, y = %s, octant = %s", x, rint(yd), octant);
/*		y = reducedAngleOld(xr, octant);
		yd = y*deg;*/
		writefln("-- x = %s, y = %s, octant = %s", x, rint(yd), octant);
	}
	writeln("passed");
}

/// Decimal version of std.math function.
public T sin(T)(const T x) {
	int octant;
	T rx = reducedAngle(x, octant);
	switch(octant) {
		case 0: return( calcSin( rx));
		case 1: return( calcCos(-rx));
		case 2: return( calcCos( rx));
		case 3: return( calcSin(-rx));
		case 4: return(-calcSin( rx));
		case 5: return(-calcCos(-rx));
		case 6: return(-calcCos( rx));
		case 7: return(-calcSin(-rx));
		default: return T.nan;
	}
}

/*/// Decimal version of std.math function.
public Decimal sin(const Decimal arg, uint precision) {
	pushContext(precision);
	Decimal value = sin(arg);
	popContext();
	return value;
}*/

/// Decimal version of std.math function.
public T calcSin(T)(const T x) {
	T sum = 0;
	int n = 1;
	T powx = x.dup;
	T sqrx = x * x;
	T fact = 1;
	T term = powx;
	while (term.abs > T.epsilon) {
		sum += term;
		n += 2;
		powx = -powx * sqrx;
		fact = fact * (n*(n-1));
		term = powx/fact;
	}
	return sum;
}

unittest {
	write("sin..........");
	writeln;
	writeln("sin(1) = 0.84147098480789650665250232163029899962256306079837");
//	pushContext(50);
	writefln("calcSin(1) = %s", calcSin(dec9(1)));
//	dec9 test = dec9(10,22);
//	writefln("sin(10^^22) = %s", sin(test));
	dec9 test = dec9("22000.12345");
	writeln("sin(22) = -0.008851309290403875921690256815772332463289203951");
	writefln("sin(22) = %s", sin(test));
/*
//	popContext();
	writefln("sin(101.23456789) = %s", sin(dec9("101.23456789"), 25));
	writefln("sin(pi + 1.0) = %s", sin(pi(25) + dec9("1.0"),25));

	writeln("..failed");*/
}

/// Decimal version of std.math function.
public T cos(T)(const T x) {
	int octant;
	T y = reducedAngle(x, octant);
	switch(octant) {
		case 0: return(calcCos(y));
		case 1: return(calcSin(-y));
		case 2: return(-calcSin(y));
		case 3: return(-calcCos(-y));
		case 4: return(-calcCos(y));
		case 5: return(-calcSin(-y));
		case 6: return(calcSin(y));
		case 7: return(calcCos(-y));
		default: return T.nan;
	}
}

/*/// Decimal version of std.math function.
public T cos(T)(const T x, uint precision) {
	pushContext(precision);
	T value = cos(x);
	popContext();
	return value;
}*/

/// Decimal version of std.math function.
public T calcCos(T)(const T x) {
	T sum = 0;
	int n = 0;
	T powx = 1;
	T sqrx = x * x;
	T fact = 1;
	T term = powx;
	while (term.abs > T.epsilon) {
		sum += term;
		n += 2;
		powx = -powx * sqrx;
		fact = fact * (n*(n-1));
		term = powx/fact;
	}
	return sum;
}

unittest {
	write("cos..........");
	writeln;
	writeln("cos(1) = 0.54030230586813971740093660744297660373231042061792");
//	pushContext(50);
	writefln("cos(1) = %s", calcCos(dec9(1)));
//	popContext();
	writeln("..failed");
}

/**
 * Replaces std.math function expi
 *
 */
public void sincos(T)(T x, out T sine, out T cosine) {
	T[2] result;

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
 // Newton's method .. is it faster
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
// TODO: only valid if x < 1.0; convergence very slow if x ~ 1.0;
public Decimal arctan(Decimal x) {
	Decimal a = 1;
	Decimal g = sqrt(1 + sqr(x));
writefln("a = %s", a);
writefln("g = %s", g);
	for (int i = 0; i < 10; i++) {//while (abs(a-g) < Decimal.epsilon) {
writeln (" -- " );
		a = (a + g) * Decimal.half;
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
//
// HYPERBOLIC TRIGONOMETRIC FUNCTIONS
//
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
	// TODO: special functions
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
	// TODO special values
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
	// TODO: special values
	Decimal arg = (x + Decimal.one)/(x-Decimal.one);
	return Decimal.half * ln(arg);
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
 * (M)TODO: implement
 */
public Decimal ln(Decimal x) {
	return calcLog(x);
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



