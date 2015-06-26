// Written in the D programming language

/**
 *	A D programming language implementation of the
 *	General Decima l Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 *	http://www.speleotrove.com/decimal/decarith.pdf)
 *
 *	Copyright Paul D. Anderson 2009 - 2015.
 *	Distributed under the Boost Software License, Version  1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/


/// Most of these arithmetic operations accept values for precision and
/// rounding modes, but these parameters are not generally available to
/// the ordinary user. They are included to allow algorithm designers to
/// carry out internal operations at a precision higher than the type
/// precision.

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

// TODO: (testing) opEquals unit test should include numerically equal testing.

// TODO: (testing) write some test cases for flag setting. test the add/sub/mul/div functions

module eris.decimal.arithmetic;

import std.string;
//import std.traits : isIntegral;

import eris.integer.extended;
import eris.decimal;
import eris.decimal.context;
import eris.decimal.rounding;

unittest {
	writeln("==========================");
	writeln("decimal arithmetic...begin");
	writeln("==========================");
}

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
	import eris.decimal;
	import eris.decimal.test;
}

alias xcompare = eris.integer.extended.xint.compare;

//--------------------------------
// classification functions
//--------------------------------

/// Returns a string indicating the class and sign of the argument.
/// Classes are: sNaN, NaN, Infinity, Zero, Normal, and Subnormal.
/// The sign of any NaN values is ignored in the classification.
/// The argument is not rounded and no flags are changed.
///
/// Implements the 'class' function in the specification. (p. 42)
public string classify(T)(in T x) if (isDecimal!T)
{
	if (x.isFinite)
	{
		if (x.isZero)		{ return x.sign ? "-Zero" : "+Zero"; }
		if (x.isNormal)		{ return x.sign ? "-Normal" : "+Normal"; }
		if (x.isSubnormal)	{ return x.sign ? "-Subnormal" : "+Subnormal"; }
	}
	if (x.isInfinite)  { return x.sign ? "-Infinity" : "+Infinity"; }
	if (x.isSignaling) { return "sNaN"; }
	return "NaN";
}

// needs better test
unittest {	// classify
	write("-- classify.........");
	static struct S { TD actual; string expect; }
	static S[] tests =
	[
		{ TD.nan,         "NaN" },
		{ TD.snan,        "sNaN" },
		{ TD.infinity,    "+Infinity" },
		{ TD("1E-10"),    "+Normal" },
		{ TD("-0"),       "-Zero" },
		{ TD("-0.1E-99"), "-Subnormal" },
	];
	foreach (i, s; tests)
		assertEqual(classify(s.actual), s.expect, i);
	writeln("passed");
}

/// Returns the truncated base 10 logarithm of the argument.
/// "...The integer which is the exponent of the magnitude
/// of the most significant digit of the operand.
/// (As though the operand were truncated to a single digit
/// while maintaining the value of that digit and without
/// limiting the resulting exponent)".
///
/// Implements the 'logb' function in the specification. (p. 47)
///
/// Flags: InvalidOperation, DivisionByZero.
///
public int ilogb(T)(in T x) if (isDecimal!T)
{
	if (x.isInfinite || x.isNaN) {
		invalidOperation!T;
		return int.init;
	}
	if (x.isZero)
	{
		contextFlags.set(DivisionByZero);
		return int.init;
	}
	return x.digits + x.exponent - 1;
}

unittest {	// ilogb
	ArithTestData!(TD,1)[] data =
	[
		{ 250,    2 },
		{ 2.5,    0 },
		{ 0.03,  -2 },
		{ "Inf",  0 },	// sets InvalidOperation flag
		{ 0,      0 },	// sets DivisionByZero flag
	];
	TestResults tr = testArith!(TD,1)
		("ilogb", &ilogb!TD, data);
    writeln(tr.report);
}

/// Returns the truncated base 10 logarithm of the argument.
/// "...The integer which is the exponent of the magnitude
/// of the most significant digit of the operand.
/// (As though the operand were truncated to a single digit
/// while maintaining the value of that digit and without
/// limiting the resulting exponent)".
/// May set the InvalidOperation and DivisionByZero flags.
/// Implements the 'logb' function in the specification. (p. 47)
public T logb(T)(in T x) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	if (x.isInfinite) return T.infinity;
	if (x.isZero)
	{
		contextFlags.set(DivisionByZero);
		return T.infinity(true);
	}
	int expo = x.digits + x.exponent - 1;
	return T(expo);
}

unittest {	// logb
	ArithTestData!(TD,1)[] data =
	[
		{ 250,    2 },
		{ 2.5,    0 },
		{ 0.03,  -2 },
		{ "Inf", "Inf" },
		{ 0,     "-Inf" },
		{ "NaN", "NaN" },
	];
	TestResults tr = testArith!(TD,1,true)
		("logb", &logb!TD, data);
    writeln(tr.report);
}

/// If the first operand is infinite then that operand is returned,
/// otherwise the result is the first operand modified by
/// adding the value of the second operand to its exponent.
/// The second operand must be a finite integer (<= int.max && >= int.min)
///	with an exponent of zero.
/// The result may overflow or underflow.
///
/// Implements the 'scaleb' function in the specification. (p. 48)
///
/// Flags: InvalidOperation, Underflow, Overflow.
public T scaleb(T)(in T x, in T y) if (isDecimal!T)
{
	T scaled = x.dup;

	if (x.isNaN || y.isNaN) return invalidOperand(x,y);

	if (x.isInfinite) return scaled;

	if (y.isInfinite || y.exponent != 0)
	{
		return invalidOperand(y);
	}
	if (y > T.IntMax || y < T.IntMin)
	{
		return invalidOperand(y);
	}

	int scale = cast(int)y.coefficient.toInt;

	if (y.isSigned)
	{
		scale = -scale;
	}
	// TODO: (behavior) check for overflow/underflow (GDA "scaleb").
	scaled.exponent = scaled.exponent + scale;
	return scaled;
}

unittest
{	// scaleb
	ArithTestData!(TD,2)[] data =
	[
		{ "7.50", "-2", "0.0750" },
//		{ "7.50", "-3", "0.0750" },
	];
	TestResults tr = testArith!(TD,2)
		("scaleb", &scaleb!TD, data);
    writefln(tr.report);
}

//--------------------------------
// unary functions
//--------------------------------

///
/// Returns the operand reduced to its simplest form.
///
/// <code>reduce</code> has the same semantics as the plus operation,
/// except that a finite result is
/// reduced to its simplest form, with all trailing
/// zeros removed and its sign preserved.
///
/// Implements the 'reduce' function in the specification. (p. 37)
/// "This operation was called 'normalize' prior to
/// version 1.68 of the specification." (p. 37)
///
/// Flags: InvalidOperation
///
public T reduce(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	// special cases
	if (x.isNaN) return invalidOperand(x);
	if (!x.isFinite) return x.dup;

	T reduced = plus(x, context);

	// have to check again -- rounding may have made it infinite
	if (!reduced.isFinite) return reduced;

	int digits = reduced.digits;
	auto temp = reduced.coefficient;
	int zeros = trimZeros(temp, digits);
	if (zeros) {
		reduced.coefficient = temp;
		reduced.digits = digits - zeros;
		reduced.exponent = reduced.exponent + zeros;
	}
	// remove leading zeros from the coefficient.
	// NOTE: is this the place??
	reduced.coefficient = reduced.coefficient.trim;
	return reduced;
}

// just a wrapper
public T normalize(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	return reduce(x, context);
}

unittest {	// reduce
	ArithTestData!(TD,1)[] data =
	[
		{ "1.200", "1.2" },
//		{ "1.200", "1.3" },
//		{ "1.200", "1.20" },	// should fail
		{ "1.2001", "1.2001" },
		{ "1.200000001", "1.2" },
	];
	// NOTE: need to compare strings, not values
	TestResults tr = testArith!(TD,1,true)
		("reduce", &reduce!TD, data);
    writeln(tr.report);
}

///
/// Returns the absolute value of the argument.
/// This operation rounds the result and may set flags.
/// The result is equivalent to plus(x) for positive numbers
/// and to minus(x) for negative numbers.
///
/// To return the absolute value without rounding or setting flags
/// use the 'copyAbs' function.
///
/// Implements the 'abs' function in the specification. (p. 26)
///
/// Flags: InvalidOperation
///
public T abs(T)(in T x, Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);

	return roundToPrecision(x.copyAbs, context);
}

version(unittest)
{
	T abst(T)(in T x) if (isDecimal!T)
	{
		return abs(x, TD.context);
	}
}

unittest {	// abs
	ATF1 fctn = &abst!TD;
	static ATD1[] data =
	[
		{ "-Inf", "Inf" },
		{ "101.5", "101.5" },
		{ "-101.5", "101.5" },
		{ "-1.23456789012E+23", "1.2345678900E+23" },	// rounds the argument
	];
	auto test = Test!(ATF1,ATD1)("abs", fctn);
	foreach (int i, d; data) test.run(d, i);
	writeln(test.report);
}

///
/// Returns -1, 0, or 1 if the argument is
/// negative, zero, or positive, respectively.
/// The sign of zero is ignored: returns 0 for +0 or -0.
///
public int sgn(T)(in T x) if (isDecimal!T)
{
	if (x.isZero) return 0;
	return x.isNegative ? -1 : 1;
}

unittest {	// sqn
	ATFI1 fctn = &sgn!TD;
	static ATDI1[] data =
	[
		{  "-123", -1 },
		{  "2345",  1 },
		{ "-2345", -1 },
		{     "0",  0 },
		{    "-0",  0 },
		{  "0.00",  0 },
		{  "-Inf", -1 },
	];
	auto test = Test!(ATFI1,ATDI1)("sgn", fctn);
	foreach (int i, d; data) test.run(d, i);
	writeln(test.report);
}

///
/// Returns -1, 0, or 1
/// if the argument is negative, zero, or positive, respectively.
///
public int sgn(T:xint)(T x) {
	if (x < 0) return -1;
	if (x > 0) return 1;
	return 0;
}

///
/// Returns a copy of the argument with the same sign as the argument.
/// The result is equivalent to add('0', x).
///
/// This operation rounds the result and may set flags.
/// To copy without rounding or setting flags use the 'copy' function.
///
/// Implements the 'plus' function in the specification. (p. 33)
///
/// Flags: InvalidOperation
///
public T plus(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	return roundToPrecision(x, context);
}

version(unittest)
{
	T plust(T)(in T x) if (isDecimal!T)
	{
		return plus(x, TD.context);
	}
}

unittest {	// plus
	ATF1 fctn = &plust!TD;
	static ATD1[] data =
	[
		{ "1.3", "1.3" },
		{ "-101.5", "-101.5" },
//		{ "-101.5", "101.5" },
	];
	auto test = Test!(ATF1,ATD1)("plus", fctn);
	foreach (int i, d; data) test.run(d, i);
	writeln(test.report);
}

///
/// Returns a copy of the argument with the opposite sign.
/// The result is equivalent to subtract('0', x).
///
/// This operation rounds the argument and may set flags.
/// To copy without rounding or setting flags use the 'copyNegate' function.
///
/// Implements the 'minus' function in the specification. (p. 37)
///
/// Flags: InvalidOperation
///
public T minus(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	return roundToPrecision(x.copyNegate, context);
}

unittest {	// minus
	ArithTestData!(TD,1)[] data =
	[
		{ "1.3", "-1.3" },
//		{ "-101.5", "-101.5" },
		{ "-101.5", "101.5" },
	];
	TestResults tr = testArith!(TD,1)
		("minus", &minus!TD, data);
    writeln(tr.report);
}

//-----------------------------------
// next-plus, next-minus, next-toward
//-----------------------------------

///
/// Returns the smallest representable number that is larger than
/// the argument.
///
/// Implements the 'next-plus' function in the specification. (p. 34)
///
/// Note that the overflow flag is not set by this operation.
///
/// Flags: InvalidOperation
///
public T nextPlus(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);

	if (x.isInfinite) {
		if (x.sign) {
			return T.max.copyNegate;
		}
		else {
			return x.dup;
		}
	}
	int adjustedExpo = x.exponent + x.digits - context.precision;
	if (adjustedExpo < T.tinyExpo) {
			return T(true, 0L, T.tinyExpo);
	}

	T y = T(1L, adjustedExpo);
	T z = add(x, y, context, false);
	if (z > T.max) {
		z = T.infinity;
	}

	// FIXTHIS: need to pass setFlags value
	return roundToPrecision(z);
}

unittest {
	ArithTestData!(TD,1)[] data =
	[
		{ "1", 			 "1.00000001" },
		{ "-1E-107", 	 "-0E-107" },
		{ "-1.00000003", "-1.00000002" },
		{ "-Infinity",	 "-9.99999999E+99" },
		{ "9.99999999E+99",	 "Infinity" },	// overflow flag should not be set!
		{ "1E+101",	 "Infinity" },
	];
	TestResults tr = testArith!(TD,1)
		("nextPlus", &nextPlus!TD, data);
    writeln(tr.report);
}

///
/// Returns the largest representable number that is smaller than
/// the argument.
///
/// Implements the 'next-minus' function in the specification. (p. 34)
///
/// Flags: InvalidOperation.
/// Note that the overflow flag is not set by this operation.
///
public T nextMinus(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);

	if (x.isInfinite) {
		if (!x.sign) {
			return T.max;
		}
		else {
			return x.dup;
		}
	}
	int adjustedExpo = x.exponent + x.digits - context.precision;
	if (x.coefficient == 1) adjustedExpo--;
	if (adjustedExpo < T.tinyExpo) {
		return T(0L, T.tinyExpo);
	}
	T y = T(1L, adjustedExpo);
	y = sub!T(x, y, context);
		if (y < T.max.copyNegate) {
		y = T.infinity.copyNegate;
	}
	return y;
}

unittest {	// nextMinus
	ArithTestData!(TD,1)[] data =
	[
		{ "1", 					"0.999999999" },
		{ "1E-107",				"0E-107" },
		{ "-1.00000003",		"-1.00000004" },
		{ "Infinity",			"9.99999999E+99" },
		{ "-9.99999999E+99",	"-Infinity" },
	];
	TestResults tr = testArith!(TD,1)
		("nextMinus", &nextMinus!TD, data);
    writeln(tr.report);
}

///
/// Returns the representable number that is closest to the first operand
/// in the direction of the second operand.
///
/// Implements the 'next-toward' function in the specification. (p. 34-35)
///
/// Flags: InvalidOperation
// TODO: anomalous flag settings
public T nextToward(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
   	T nan;
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);

	// compare them but don't round yet
	int comp = compare(x, y, context);
	if (comp < 0) return nextPlus(x, context);
	if (comp > 0) return nextMinus(x, context);

	return roundToPrecision(x.copySign(y), context);
}

unittest {
	ArithTestData!(TD,2)[] data =
	[
		{ " 1",          "2", " 1.00000001" },
		{ "-1.00000003", "0", "-1.00000002" },
	];
	TestResults tr = testArith!(TD,2)
		("nextToward", &nextToward!TD, data);
    writeln(tr.report);
}

//--------------------------------
// comparison functions
//--------------------------------

/// Compares two operands numerically to the current precision.
///
/// Note: The operands are rounded before they are compared.
/// This may result in unexpected results. For instance,
/// if both operands are too large (small) for the context
/// they will both be rounded to infinity (zero) and the function will
/// return 0, indicating that they are equal, even though they are
/// numerically different before rounding.
///
/// To compare numbers without rounding, use compareTotal.
///
/// Returns -1, 0, or +1 if the second operand is, respectively,
/// less than, equal to, or greater than the first operand.
/// Implements the 'compare' function in the specification. (p. 27)
/// Flags: InvalidOperation
public int compare(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// any operation with a signaling NaN is invalid.
	// if both are signaling, return as if x > y.
	if (x.isSignaling || y.isSignaling) {
		contextFlags.set(InvalidOperation);
		return x.isSignaling ? 1 : -1;
	}

	// if both are NaN, return as if x > y.
	if (x.isNaN || y.isNaN) {
		return x.isNaN ? 1 : -1;
	}

	// if either is zero...
	if (x.isZero) {
		if (y.isZero) return 0;
		return y.isNegative ? 1 : -1;
	}
	if (y.isZero) {
		return x.isNegative ? -1 : 1;
	}

	// if signs differ, just compare the signs
	if (x.sign != y.sign) {
		// check for zeros: +0 and -0 are equal
		if (x.isZero && y.isZero) return 0;
		return x.sign ? -1 : 1;
	}

	// if either is infinite...
	if (x.isInfinite || y.isInfinite) {
		if (x.isInfinite && y.isInfinite) return 0;
		return x.isInfinite ? 1 : -1;
	}

	T xx = x.dup;
	T yy = y.dup;

	// TODO: (testing) test compare at precision limits.
	// restrict operands to current precision
	if (xx.digits > context.precision) {
		xx = roundToPrecision(xx, context);
	}
	if (yy.digits > context.precision) {
		yy = roundToPrecision(yy, context);
	}

	// TODO: this will return inf == inf after rounding
	// Check again for infinities
	if (xx.isInfinite || yy.isInfinite) {
		if (xx.isInfinite && yy.isInfinite) return 0;
		return xx.isInfinite ? 1 : -1;
	}

	// compare the magnitudes of the numbers
	xx = xx.reduce;
	yy = yy.reduce;
	int diff = (xx.exponent + xx.digits) - (yy.exponent + yy.digits);
	if (diff != 0) {
		if (!xx.sign) {
			if (diff > 0) return 1;
			if (diff < 0) return -1;
		}
		else {
			if (diff > 0) return -1;
			if (diff < 0) return 1;
		}
	}
	// align the operands
	alignOps(xx, yy);
	// They have the same exponent after alignment.
	// The only difference is in the coefficients.
    int comp = xcompare(xx.coefficient, yy.coefficient);
	return xx.sign ? -comp : comp;
}

unittest {	// compare
	ArithTestData!(TD,2)[] data =
	[
		{ " 3  ", " 2.1 ", " 1" },
		{ "-3  ", " 2.1 ", "-1" },
		{ " 2.1", "-3   ", " 1" },
		{ " 2.1", " 2.1 ", " 0" },
		{ " 2.1", " 2.10", " 0" },
		{ " Inf", "-Inf ", " 1" },
		{ " Inf", " Inf ", " 0" },
		{ " Inf", " 12  ", " 1" },
		{ "-Inf", " 12  ", "-1" },
	];
	TestResults tr = testArith!(TD,2)
		("compare", &compare!TD, data);
    writeln(tr.report);
}

/// Returns true if the operands are equal to the context precision.
/// Finite numbers are equal if they are numerically equal
/// to the context precision.
/// Infinities are equal if they have the same sign.
/// Zeros are equal regardless of sign.
/// A NaN is not equal to any number, not even another NaN.
/// In particular, a decimal NaN is not equal to itself (this != this).
///
/// Note: The operands are rounded before they are compared.
/// This may result in unexpected results. For instance,
/// if both operands are too large (small) for the context
/// they will both be rounded to infinity (zero) and the function will
/// return true, indicating that they are equal, even though they are
/// numerically different before rounding.
///
/// Flags: InvalidOperation
public bool equals(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// any operation with a signaling NaN is invalid.
	if (x.isSignaling || y.isSignaling) {
		contextFlags.set(InvalidOperation);
		return false;
	}
	// if either is NaN...
	// NaN is never equal to any number, not even another NaN
	if (x.isNaN || y.isNaN) return false;

	// if they are identical...
	if (x is y) return true;

	// if either is zero...
	if (x.isZero || y.isZero) {
		// ...they are equal if both are zero (regardless of sign)
		return (x.isZero && y.isZero);
	}

	// if their signs differ they are not equal (except for zero, handled above)
	if (x.sign != y.sign) return false;

	// if either is infinite...
	if (x.isInfinite || y.isInfinite) {
		// ...they are equal only if both are infinite with the same sign
		return (x.isInfinite && y.isInfinite);
	}

	// if they have the same representation, they are equal
	if (x.exponent == y.exponent && x.coefficient == y.coefficient) {
		return true;
	}

	// restrict operands to the context precision
	T rx, ry;
	rx = roundToPrecision(x, context);
	ry = roundToPrecision(y, context);

	// if they are not of the same magnitude they are not equal
	if (rx.exponent + rx.digits != ry.exponent + ry.digits) return false;
	// align the operands
	alignOps(rx, ry);
	return rx.coefficient == ry.coefficient;
}

unittest {	// equals
	ArithTestData!(TD,2)[] data =
	[
		{ " 123.4567     ", " 123.4568   ", false},
		{ " 123.4567     ", " 123.4567   ", true },
		{ " 123.45671234 ", " 123.4567121", true }, // equals to precision
		{ " 1000000E-8   ", " 1E-2       ", true },
		{ "+100000000E-08", "+1E+00      ", true },
		{ "-1.00000000   ", "-1          ", true },
	];
	TestResults tr = testArith!(TD,2)
		("equals", &equals!TD, data);
    writeln(tr.report);
}

/// Returns true if the operands are equal to the specified precision. Special
/// values are handled as in the equals() function. This function allows
/// comparison at precision values other than the context precision.
public bool precisionEquals(T)(T x, T y, int precision) if (isDecimal!T)
{
	auto context = Context(precision, T.maxExpo, T.rounding);
	return (equals(x, y, context));
}

/// Compares the numeric values of two numbers. CompareSignal is identical to
/// compare except that quiet NaNs are treated as if they were signaling.
/// This operation may set the invalid-operation flag.
/// Implements the 'compare-signal' function in the specification. (p. 27)
/// Flags: InvalidOperation
public int compareSignal(T) (in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{

	// any operation with NaN is invalid.
	// if both are NaN, return as if x > y.
	if (x.isNaN || y.isNaN) {
		contextFlags.set(InvalidOperation);
		return x.isNaN ? 1 : -1;
	}
	return (compare!T(x, y, context));
}

unittest {	// compareSignal
	ArithTestData!(TD,2)[] data =
	[
/*		{ " 3  ", " 2.1 ", " 1" },
		{ "-3  ", " 2.1 ", "-1" },
		{ " 2.1", "-3   ", " 1" },
		{ " 2.1", " 2.1 ", " 0" },
		{ " 2.1", " 2.10", " 0" },
		{ " Inf", "-Inf ", " 1" },
		{ " Inf", " Inf ", " 0" },
		{ " Inf", " 12  ", " 1" },
		{ "-Inf", " 12  ", "-1" },*/
	];
	TestResults tr = testArith!(TD,2)
		("compSignal", &compareSignal!TD, data);
    writeln(tr.report);
}

unittest {
	write("-- compareSignal....");
	TD x, y;
	int value;
	x = 0;
	y = 5;
	assertGreaterThan(y,x);
	contextFlags.resetFlags(InvalidOperation);
	y = TD.snan;
	value = compare(x, y);
	assertTrue(contextFlags.getFlags(InvalidOperation));
	contextFlags.resetFlags(InvalidOperation);
	y = TD.nan;
	value = compare(x, y);
	assertFalse(contextFlags.getFlags(InvalidOperation));
	contextFlags.set(InvalidOperation, false);
	y = TD.nan;
	value = compareSignal(x, y);
	assertTrue(contextFlags.getFlags(InvalidOperation));
	writeln("passed");
}


/// Takes two numbers and compares the operands using their
///	abstract representation rather than their numerical value.
/// Numbers (representations which are not NaNs) are ordered such that
/// a larger numerical value is higher in the ordering.
/// If two representations have the same numerical value
/// then the exponent is taken into account;
/// larger (more positive) exponents are higher in the ordering.
/// Returns -1 If the first operand is lower in the total ordering
/// and returns 1 if the first operand is higher in the total ordering.
/// Returns 0 only if the numbers are equal and have the same representation.
/// Implements the 'compare-total' function in the specification. (p. 42-43)
/// Flags: NONE.
public int compareTotal(T)(in T x, in T y) if (isDecimal!T)
{
	if (x.isFinite && y.isFinite
		&& x.sign == y.sign
		&& x.exponent == y.exponent
		&& x.coefficient == y.coefficient)
	return 0;

	int ret1 =	1;
	int ret2 = -1;

	// if signs differ...
	if (x.sign != y.sign) {
		return x.sign ? ret2 : ret1;
	}

	// if both numbers are signed swap the return values
	if (x.sign) {
		ret1 = -1;
		ret2 =	1;
	}

	// if either is zero...
	if (x.isZero || y.isZero) {
		// if both are zero compare exponents
		if (x.isZero && y.isZero) {
			auto result = x.exponent - y.exponent;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return x.isZero ? ret1 : ret2;
	}

	// if either is infinite...
	if (x.isInfinite || y.isInfinite) {
		if (x.isInfinite && y.isInfinite) {
			return 0;
		}
		return x.isInfinite ? ret1 : ret2;
	}

	// if either is quiet...
	if (x.isQuiet || y.isQuiet) {
		// if both are quiet compare payloads.
		if (x.isQuiet && y.isQuiet) {
			auto result = x.payload - y.payload;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return x.isQuiet ? ret1 : ret2;
	}

	// if either is signaling...
	if (x.isSignaling || y.isSignaling) {
		// if both are signaling compare payloads.
		if (x.isSignaling && y.isSignaling) {
			auto result = x.payload - y.payload;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return x.isSignaling ? ret1 : ret2;
	}

	// if both exponents are equal, any difference is in the coefficient
	if (x.exponent == y.exponent) {
		auto result = x.coefficient - y.coefficient;
		if (result == 0) return 0;
		return (result > 0) ? ret1 : ret2;
	}

	// if the (finite) numbers have different magnitudes...
	int diff = (x.exponent + x.digits) - (y.exponent + y.digits);
	if (diff > 0) return ret1;
	if (diff < 0) return ret2;

	// we know the numbers have the same magnitude
	// and that the exponents are not equal -- align the operands
 	T xx = x.dup;
	T yy = y.dup;
	alignOps(xx, yy);

	// They have the same exponent after alignment.
	// The only difference is in the coefficients.
    int comp = xcompare(xx.coefficient, yy.coefficient);

	// if equal after alignment, compare the original exponents
	if (comp == 0) {
		return (x.exponent > y.exponent) ? ret1 : ret2;
	}
	// otherwise return the numerically larger
	return (comp > 0) ? ret2 : ret1;
}

/// compare-total-magnitude takes two numbers and compares them
/// using their abstract representation rather than their numerical value
/// with their sign ignored and assumed to be 0.
/// The result is identical to that obtained by using compare-total
/// on two operands which are the copy-abs copies of the operands.
/// Implements the 'compare-total-magnitude' function in the specification.
/// (p. 43)
/// Flags: NONE.
int compareTotalMagnitude(T)(T x, T y) if (isDecimal!T)
{
	return compareTotal(x.copyAbs, y.copyAbs);
}

unittest {	// compareTotal
	ArithTestData!(TD,2)[] data =
	[
		{ " 12.30", " 12.3  ", -1},
		{ " 12.30", " 12.30 ",  0},
		{ " 12.3 ", " 12.300",  1},
	];
	TestResults tr = testArith!(TD,2)
		("compTotal", &compareTotal!TD, data);
    writeln(tr.report);
}

/// Returns true if the numbers have the same exponent.
/// If either operand is NaN or Infinity, returns true if and only if
/// both operands are NaN or Infinity, respectively.
/// No context flags are set.
/// Implements the 'same-quantum' function in the specification. (p. 48)
public bool sameQuantum(T)(in T x, in T y) if (isDecimal!T)
{
	if (x.isNaN || y.isNaN) {
		return x.isNaN && y.isNaN;
	}
	if (x.isInfinite || y.isInfinite) {
		return x.isInfinite && y.isInfinite;
	}
	return x.exponent == y.exponent;
}

unittest {	// sameQuantum
	ArithTestData!(TD,2)[] data =
	[
		{ "2.17", "0.001", false },
		{ "2.17", "0.01 ", true },
		{ "2.17", "0.1  ", false },
	];
	TestResults tr = testArith!(TD,2)
		("sameQuant", &sameQuantum!TD, data);
    writefln(tr.report);
}

/// Returns the maximum of the two operands (or NaN).
/// If either is a signaling NaN, or both are quiet NaNs, a NaN is returned.
/// Otherwise, any finite or infinite number is larger than a NaN.
/// If they are not numerically equal, the larger is returned.
/// If they are numerically equal:
/// 1) If the signs differ, the one with the positive sign is returned.
/// 2) If they are positive, the one with the larger exponent is returned.
/// 3) If they are negative, the one with the smaller exponent is returned.
/// 4) Otherwise, they are indistinguishable; the first is returned.
/// The returned number will be rounded to the current context.
/// Implements the 'max' function in the specification. (p. 32)
/// Flags: InvalidOperation, Rounded.
public T max(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// if both are NaNs or either is an sNan, return NaN.
	if (x.isNaN && y.isNaN || x.isSignaling || y.isSignaling) {
		contextFlags.set(InvalidOperation);
		return T.nan;
	}
	// if both are infinite, return the one with a positive sign
	if (x.isInfinite && y.isInfinite) {
		return x.isNegative ? y.dup : x.dup;
	}

	// result will be a finite number or infinity
	// use x as default value
	T result = x.dup;

	// if one op is a quiet NaN return the other
	if (x.isQuiet || y.isQuiet) {
		if (x.isQuiet) result = y;
	}
	// if the signs differ, return the unsigned operand
	else if (x.sign != y.sign) {
		if (x.sign) result = y;
	}
	else {
		// if not numerically equal, return the larger
		int comp = compare!T(x, y, context);
		if (comp != 0) {
			if (comp < 0) result = y;
		}
		// if they have the same exponent they are identical, return either
		else if (x.exponent == y.exponent) {
			// no assignment -- use default value
		}
		// if they are non-negative, return the one with larger exponent.
		else if (x.sign == 0) {
			if (x.exponent < y.exponent) result = y;
		}
		else {
			// they are negative; return the one with smaller exponent.
			if (x.exponent > y.exponent) result = y;
		}
	}
	// result must be rounded
	return roundToPrecision(result, context);
}

unittest {	// max
	ArithTestData!(TD,2)[] data =
	[
		{   3, 2, 3 },
		{ -10, 3, 3 },
	];
	TestResults tr = testArith!(TD,2)
		("max", &max!TD, data);
    writeln(tr.report);
}

/// Returns the larger of the two operands (or NaN). Returns the same result
/// as the 'max' function if the signs of the operands are ignored.
/// Implements the 'max-magnitude' function in the specification. (p. 32)
/// Flags: NONE.
public T maxMagnitude(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// both positive
	if (x >= 0 && y >= 0) {
		return max(x, y, context);
	}
	// both negative
	if (x < 0 && y < 0) {
		return min(x, y, context);
	}
	// one of each
	if (x.copyAbs > y.copyAbs) {
		return roundToPrecision(x, context);
	}
	return roundToPrecision(y, context);
}

unittest {	// maxMagnitude
	ArithTestData!(TD,2)[] data =
	[
		{   -1, -2, -2 },
		{    1, -2, -2 },
		{    1,  2,  2 },
		{   -1,  2,  2 },
	];
	TestResults tr = testArith!(TD,2)
		("maxMag", &maxMagnitude!TD, data);
    writeln(tr.report);
}

/// Returns the minimum of the two operands (or NaN).
/// If either is a signaling NaN, or both are quiet NaNs, a NaN is returned.
/// Otherwise, Any (finite or infinite) number is smaller than a NaN.
/// If they are not numerically equal, the smaller is returned.
/// If they are numerically equal:
/// 1) If the signs differ, the one with the negative sign is returned.
/// 2) If they are negative, the one with the larger exponent is returned.
/// 3) If they are positive, the one with the smaller exponent is returned.
/// 4) Otherwise, they are indistinguishable; the first is returned.
/// Implements the 'min' function in the specification. (p. 32-33)
/// Flags: INVALID OPERATION, Rounded.
public T min(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// if both are NaNs or either is an sNan, return NaN.
	if (x.isNaN && y.isNaN || x.isSignaling || y.isSignaling) {
		contextFlags.set(InvalidOperation);
		return T.nan;
	}
	// if both are infinite, return the one with a negative sign
	if (x.isInfinite && y.isInfinite) {
		return x.isNegative ? x.dup : y.dup;
	}
	// result will be a finite number or infinity
	T min;

	// if one op is a quiet NaN return the other
	if (x.isQuiet || y.isQuiet) {
		min = x.isQuiet? y : x;
	}
    // if the signs differ, return the negative operand
	else if (x.sign != y.sign) {
		min = x.sign ? x : y;
	}
	// if not numerically equal, return the lesser
	else {
		int comp = compare(x, y, context);
		if (comp != 0) {
			min = comp > 0 ? y : x;
		}
		// if they have the same exponent they are identical, return either
		else if (x.exponent == y.exponent) {
			min = x;
		}
		// if they are non-negative, return the one with smaller exponent.
		else if (x.sign == 0) {
			min = x.exponent > y.exponent ? y : x;
		}
		// else they are negative; return the one with larger exponent.
		else {
			min = x.exponent > y.exponent ? y : x;
		}
	}
	// min must be rounded
	return roundToPrecision(min, context);
}

unittest {	// min
	ArithTestData!(TD,2)[] data =
	[
		{   3, 2,   2 },
		{ -10, 3, -10 },
	];
	TestResults tr = testArith!
		(TD,2)("min", &min!TD, data);
    writeln(tr.report);
}

/// Returns the smaller of the two operands (or NaN). Returns the same result
/// as the 'min' function if the signs of the operands are ignored.
/// Implements the 'min-magnitude' function in the specification. (p. 33)
/// Flags: INVALID OPERATION, Rounded.
public T minMagnitude(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// both positive
	if (x >= 0 && y >= 0) {
		return min(x, y, context);
	}
	// both negative
	if (x < 0 && y < 0) {
		return max(x, y, context);
	}
	// one of each
	if (x.copyAbs > y.copyAbs) {
		return roundToPrecision(y, context);
	}
	return roundToPrecision(x, context);
}

unittest {	// minMagnitude
	ArithTestData!(TD,2)[] data =
	[
		{   -1, -2, -1 },
		{    1, -2,  1 },
		{    1,  2,  1 },
		{   -1,  2, -1 },
	];
	TestResults tr = testArith!(TD,2)
		("minMag", &minMagnitude!TD, data);
    writeln(tr.report);
}

/// Returns a number with a coefficient of 1 and
/// the same exponent as the argument.
/// Flags: NONE.
public T quantum(T)(in T x)  {
	return T(1, x.exponent);
}

unittest {	// plus
	ArithTestData!(TD,1)[] data =
	[
		{ "23.14E-12", "1E-14" },
	];
	TestResults tr = testArith!(TD,1)
		("quantum", &quantum!TD, data);
    writeln(tr.report);
}

//--------------------------------
// decimal shift and rotate
//--------------------------------

/// Shifts the first operand by the specified number of DECIMAL digits.
/// (NOT BINARY digits!) Positive values of the second operand shift the
/// first operand left (multiplying by tens). Negative values shift right
/// (dividing by tens). If the number is NaN, or if the shift value is less
/// than -precision or greater than precision, an InvalidOperation is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'shift' function in the specification. (p. 49)
public T shift(T, U=T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.exponent != 0) return invalidOperand(y);
	if (y.coefficient > context.precision ||
		y.coefficient < -context.precision) return invalidOperand(y);
	int n = y.coefficient.toInt;
	if (y.sign) n = -n;
	return shift(x, n, context);
}

/// Shifts the first operand by the specified number of DECIMAL digits.
/// (NOT BINARY digits!) Positive values of the second operand shift the
/// first operand left (multiplying by tens). Negative values shift right
/// (dividing by tens). If the first operand is NaN, or if the shift value is less
/// than -precision or greater than precision, an InvalidOperation is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'shift' function in the specification. (p. 49)
public T shift(T, U:int)(in T x, U n,
		Context context = T.context) if (isDecimal!T)
{

	// check for NaN
	if (x.isNaN) return invalidOperand(x);

	// shift by zero returns the argument
	if (n == 0) return x.dup;

	// shift of an infinite number returns the argument
	if (x.isInfinite) return x.dup;

	int precision = context.precision;

	// shifting by more than precision is invalid.
	if (n < -precision || n > precision)
	{
		return invalidOperation!T;
	}

	auto xx = x.dup;

	if (n > 0)
	{
		// shift left
		xx.coefficient = xx.coefficient * pow10(n);
		xx.digits = numDigits(xx.coefficient);
		if (xx.digits > context.precision)
		{
			xx.coefficient = xx.coefficient % pow10(precision);
			xx.digits = precision;
		}
	}
	else
	{
		// shift right
		xx.coefficient = xx.coefficient / pow10(-n);
		xx.digits = numDigits(xx.coefficient);
	}
	return xx;
}

unittest {	// shift
	ArithTestData!(TD,2)[] data =
	[
		{ 34, 8, 400000000 },
		{ 12, 9, 0 },
		{ 123456789, -2, 1234567 },
		{ 123456789,  0, 123456789 },
		{ 123456789,  2, 345678900 },
	];
	TestResults tr = testArith!(TD,2)
		("shift", &shift!(TD,TD), data);
    writeln(tr.report);
}

/// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an InvalidOperation is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T, U=T)(in T x, in U y,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	if (y.isNaN) return invalidOperand(y);
	if (y.exponent != 0) return invalidOperand(y);
	if (y.coefficient > context.precision ||
		y.coefficient < -context.precision) return invalidOperand(y);
	int n = y.coefficient.toInt;
	if (y.sign) n = -n;
	return rotate(x, n, context);
}

// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an InvalidOperation is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T, U:int)(in T x, U n,
		Context context = T.context) if (isDecimal!T)
{

	// check for NaN
	if (x.isNaN) return invalidOperand(x);

	// shift by zero returns the argument
	if (n == 0) return x.dup;

	// shift of an infinite number returns the argument
	if (x.isInfinite) return x.dup;

	int precision = context.precision;

	// shifting by more than precision is invalid.
	if (n < -precision || n > precision) {
		return invalidOperation!T;
	}

	auto xx = x.dup;

	// clip leading digits
	if (xx.digits > precision){
		xx.coefficient = xx.coefficient % pow10(precision);
	}

	if (n > 0) {
		// rotate left
		xx.coefficient = xx.coefficient * pow10(n);
		xx.digits = numDigits(xx.coefficient);
		if (xx.digits > context.precision) {
			xint rem;
			xint div = xint.divmod(xx.coefficient, pow10(precision), rem);
			xx.coefficient = div + rem;
			xx.digits = numDigits(xx.coefficient);
		}
	}
	else {
		// rotate right
		n = -n;
		xint rem;
		xint div = xint.divmod(xx.coefficient, pow10(n), rem);
		xx.coefficient = rem * pow10(precision - n) + div;
		xx.digits = numDigits(xx.coefficient);
	}
	return xx;
}

unittest {	// rotate
	ArithTestData!(TD,2)[] data =
	[
		{ 34, 8, 400000003 },
		{ 12, 9, 12 },
		{ 123456789,   2, 345678912 },
		{ 123456789,   0, 123456789 },
		{ 123456789,  -2, 891234567 },
		{ 123456789,  -5, 567891234 },
		{ 1234567890, -2, 902345678 },
		{ 912345678900000, 2, 890000067 },
		{ 123000456789,  2, 45678900 },
		{ 123000456789, -2, 890004567 },
	];
	TestResults tr = testArith!(TD,2)("rotate", &rotate!TD, data);
    writeln(tr.report);
}

/*unittest {
	write("-- rotate...........");
	TD x, y, z;
	x = "34";
	y = 8;
	z = "400000003";
	assertEqual(rotate(x, y), z);
	x = "12";
	y = 9;
	z = "12";
	assertEqual(rotate(x, y), z);
	x = "123456789";
	y = -2;
	z = "891234567";
	assertEqual(rotate(x, y), z);
	x = "1234567890";
	y = -2;
	z = "902345678";
	assertEqual(rotate(x, y), z);

	x = "912345678900000";
	y = 2;
	z = "890000067";
	assertEqual(rotate(x, y), z);

	x = "123000456789";
	y = 2;
	z = "45678900";
	assertEqual(rotate(x, y), z);

	x = "123000456789";
	y = -2;
	z = "890004567";
	assertEqual(rotate(x, y), z);

	x = "123456789";
	y = -5;
	z = "567891234";
	assertEqual(rotate(x, y), z);
	y = 0;
	z = "123456789";
	assertEqual(rotate(x, y), z);
	y = 2;
	z = "345678912";
	assertEqual(rotate(x, y), z);
	writeln("passed");
}
*/
//------------------------------------------
// binary arithmetic operations
//------------------------------------------

/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: InvalidOperation, Overflow.
public T add(T)(in T x, in T y,
		Context context = T.context, bool setFlags = true) if (isDecimal!T)
{
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);

	// if both operands are infinite...
	if (x.isInfinite && y.isInfinite) {
		// if the signs differ return NaN and set invalid operation flag
		if (x.sign != y.sign) {
			return invalidOperation!T;
		}
		// both infinite with same sign, return the first
		return x.dup;
	}
	// if only the first is infinite, return it
	if (x.isInfinite) {
		return x.dup;
	}
	// if only the second is infinite, return it
	if (y.isInfinite) {
		return y.dup;
	}

	T sum = T.zero;
	// add(0, 0)
	if (x.isZero && y.isZero) {
		sum = x;
		// the exponent is the smaller of the two exponents
		sum.exponent = std.algorithm.min(x.exponent, y.exponent);
		// the sign is the logical and of the two signs
		sum.sign = x.sign && y.sign;
		return sum;
	}
	// add(0,f)
	if (x.isZero) return y.dup;
	// add(f,0)
	if (y.isZero) return x.dup;

	// sum is finite and not zero.
	auto xx = x.dup;
	auto yy = y.dup;
	// align the operands
	alignOps(xx, yy);
	// if the operands have the same sign add the aligned coefficients
	if (xx.sign == yy.sign) {
		sum.coefficient = xx.coefficient + yy.coefficient;
		sum.sign = xx.sign;
	}
	// otherwise subtract the lesser from the greater
	else {
		if (xx.coefficient >= yy.coefficient) {
			sum.coefficient = xx.coefficient - yy.coefficient;
			sum.sign = xx.sign;
		}
		else {
			sum.coefficient = yy.coefficient - xx.coefficient;
			sum.sign = yy.sign;
		}
	}
	sum.digits = numDigits(sum.coefficient);
	sum.exponent = xx.exponent;
	// round the result
	return roundToPrecision(sum, context, setFlags);
}


/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: InvalidOperation, Overflow.
public T add(T, U)(in T x, in U y,
		Context context = T.context, bool setFlags = true)
		if (isDecimal!T && isConvertible!U)
{
	return add(x, T(y), context, setFlags);
}

unittest {	// add, addLong
	ArithTestData!(TD,2)[] data =
	[
		{ "12", "7.00", "19.00" },
		{ "1E+2", "1E+4", "1.01E+4" },
		{ "12345678901", "54321098765", "66666777700" },
		{ "1.3", "-2.07", "-0.77" },
//		{ "1.3", "2.07", "-0.77" }, // uncomment to test failure
	];
	TestResults tr = testArith!(TD,2)("add", &add!TD, data);
    writeln(tr.report);
}

/// Subtracts the second operand from the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'subtract' function in the specification. (p. 26)
public T sub(T, U:T) (in T x, in U y,
		Context context = T.context, bool setFlags = true) if (isDecimal!T)
{
	return add(x, y.copyNegate, context, setFlags);
}	 // end sub(x, y)


/// Subtracts the second operand from the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'subtract' function in the specification. (p. 26)
public T sub(T, U)(in T x, U y,
		Context context = T.context, bool setFlags = true)
		if (isDecimal!T && isConvertible!U)
{
	return add(x, T(y).copyNegate, context, setFlags);
}	// end sub(x, y)

unittest
{
	ArithTestData!(TD,2)[] data =
	[
		{ "1.3", "1.07", "0.23" },
		{ "1.3", "1.30", "0.00" },
		{ "1.3", "2.07", "-0.77" },
//		{ "1.3", "2.07", "0.77" },	// uncomment to test failure
	];
	TestResults tr = testArith!(TD,2)("sub", &sub!(TD,TD), data);
    writeln(tr.report);
}

/// Multiplies the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'multiply' function in the specification. (p. 33-34)
public T mul(T)(in T x, in T y, Context context = T.context)
		if (isDecimal!T)
//public T mul(T)(in T x, in T y,
//		Context context = T.context) if (isDecimal!T)
{
	// if invalid, return NaN
	if (x.isNaN || y.isNaN) return invalidOperand(x,y);

	// infinity * zero => invalid operation
	if (x.isZero && y.isInfinite || x.isInfinite && y.isZero) {
		return invalidOperand(x,y);
	}
	// if either operand is infinite, return infinity
	if (x.isInfinite || y.isInfinite) {
		return T.infinity(x.sign ^ y.sign);
	}

	// mul(0,f) or (f,0)
	if (x.isZero || y.isZero) {
		T z = T.zero;
		// TODO: (behavior) is the exponent really the sum of the operand exponents? (how about just use the larger?
		z.exponent = std.algorithm.min(T.maxExpo, x.exponent + y.exponent);
		z.sign = x.sign ^ y.sign;
		return (z);
	}

	// at this point the product is a finite, non-zero number
	T product = T.zero.dup;
	product.coefficient = x.coefficient * y.coefficient;
	product.exponent = std.algorithm.min(T.maxExpo, x.exponent + y.exponent);
	product.sign = x.sign ^ y.sign;
	product.digits = numDigits(product.coefficient);

	return roundToPrecision(product, context);
}

/// Multiplies a decimal number by a long integer.
/// The result may be rounded and context flags may be set.
/// Not a required function, but useful because it avoids
/// an unnecessary conversion to a decimal when multiplying by an integer.
public T mul(T, U : long)(in T x, in U n, Context context = T.context)
//		if (isDecimal!T && isIntegral!U)
//public T mul(T)(in T x, long n, Context context = T.context)
		if (isDecimal!T)
{
	// if invalid, return NaN
	if (x.isNaN) return invalidOperand(x);

	// infinity * zero => invalid operation
	if (x.isInfinite && n == 0) {
		return invalidOperand(x);
	}
	// if decimal operand is infinite, return infinity
	if (x.isInfinite) {
		return T.infinity(x.sign ^ (n < 0));
	}

	// mul(0,f) or (f,0)
	if (x.isZero || n == 0) {
		T z = T.zero;
		z.exponent = x.exponent;
		z.sign = x.sign ^ (n < 0);
		return (z);
	}

	// at this point the product is a finite, non-zero number
	T product = T.zero;
	product.coefficient = x.coefficient * n;
	product.exponent = x.exponent;
	product.sign = x.sign ^ (n < 0);
	product.digits = numDigits(product.coefficient);
	return roundToPrecision(product, context);
}	// end mul(x, n)

/// Multiplies the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'multiply' function in the specification. (p. 33-34)
public T mul(T, U)(in T x, in U y, Context context = T.context)
		if (isDecimal!T && isConvertible!U)
{
	return mul(x, T(y), context);
}	// end mul(x, y)

unittest {	// mul
	ArithTestData!(TD,2)[] data =
	[
		{ "1.20", "3", "3.60" },
//		{ "1.20", "3", "3.61" },	// uncomment to test failure
		{ "7", "3", "21" },
		{ "-7000", "3", "-21000" },
		{ "Infinity", "3", "Infinity" },
	];
	TestResults tr = testArith!(TD,2)("mul", &mul!TD, data);
    writeln(tr.report);
}

/// Squares the argument and returns the xx.
/// The result may be rounded and context flags may be set.
public T sqr(T)(in T x, Context context = T.context) if (isDecimal!T)
{
	// if operand is invalid, return NaN
	if (x.isNaN) {
		return invalidOperand(x);
	}
	// if operand is infinite, return infinity
	if (x.isInfinite) {
		return T.infinity;
	}
	// if operand is zero, return zero
	if (x.isZero) {
		return T.zero;
	}

	// product is non-zero
	T copy = x.copy;
	copy.coefficient = copy.coefficient.sqr;
// TODO : why does this fail ("not an lvalue")
//	copy.exponent *= 2; //copy.exponent * 2;
	copy.exponent = 2 * copy.exponent;
	copy.sign = false;
	copy.digits = numDigits(copy.coefficient);
	return roundToPrecision(copy, context);
}

//TODO: need to test high-precision numbers. Requires
// a test with a high-precision context.
unittest {	// sqr
	ArithTestData!(TD,1)[] data =
	[
		{ "-Inf", "Inf" },
		{ "101.5", "10302.25" },
		{ "-101.5", "10302.25" },
		{ "-1.23456789012E+23", "1.52415788E+46" },
		{ "0.8535533905932737622000", "0.728553391" },
	];
	TestResults tr = testArith!(TD,1)
		("sqr", &sqr!TD, data);
    writeln(tr.report);
}


/// Multiplies the first two operands and adds the third operand to the result.
/// The result of the multiplication is not rounded prior to addition.
/// The result may be rounded and context flags may be set.
/// Implements the 'fused-multiply-add' function in the specification. (p. 30)
public T fma(T)(in T x, in T y, in T z,
		Context context = T.context) if (isDecimal!T)
{
	T xy = mul(x, y, Context(context.precision, T.maxExpo, Rounding.none));
	return add(xy, z, context);
}

unittest {	// fma
	ArithTestData!(TD,3)[] data =
	[
		{ 3,  5, 7, 22 },
		{ 3, -5, 7, -8 },
		{ "888565290", "1557.96930", "-86087.7578", "1.38435736E+12" },
		{ 888565290, 1557.96930, -86087.7578, 1.38435736E+12 },
	];
	TestResults tr = testArith!(TD,3)("fma", &fma!TD, data);
    writeln(tr.report);
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN and division by zero
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.isZero) return divisionByZero(x, y);

	// copy the arguments
	auto dividend = x.dup;
	auto divisor  = y.dup;
	auto quotient = T.zero;

	int diff = dividend.exponent - divisor.exponent;
	if (diff > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, diff);
		dividend.exponent = dividend.exponent - diff;
		dividend.digits = dividend.digits + diff;
	}
	int shift = 4 + context.precision + cast(int)divisor.digits - cast(int)dividend.digits;
	if (shift > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, shift);
		dividend.exponent = dividend.exponent - shift;
		dividend.digits = dividend.digits + shift;
	}
	// divisor may have become zero. Check again.
	if (divisor.coefficient.isZero)
		return divisionByZero(x, y);
	quotient.coefficient = dividend.coefficient / divisor.coefficient;
	quotient.exponent = dividend.exponent - divisor.exponent;
	quotient.sign = dividend.sign ^ divisor.sign;
	quotient.digits = numDigits(quotient.coefficient);
	quotient = roundToPrecision(quotient, context);
	quotient = reduceToIdeal(quotient, diff);
	return quotient;
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T, U : long)(in T x, in U n,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN and division by zero
	if (x.isNaN) return invalidOperand(x);
	if (n == 0) return divisionByZero(x, n);

	auto dividend = x.dup;
	auto q = T.zero;

	int diff = dividend.exponent;
	if (diff > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, diff);
		dividend.exponent = dividend.exponent - diff;
		dividend.digits = dividend.digits + diff;
	}
	int shift = 4 + context.precision + numDigits(cast(uint)n) - dividend.digits;
	if (shift > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, shift);
		dividend.exponent = dividend.exponent - shift;
		dividend.digits = dividend.digits + shift;
	}
//	// divisor may have become zero. Check again.
//	if (divisionIsInvalid!T(dividend, n, q)) {
//		return nan;
//	}
	q.coefficient = dividend.coefficient / n;
	q.exponent = dividend.exponent; // - n.exponent;
	q.sign = dividend.sign ^ (n < 0);
	q.digits = numDigits(q.coefficient);
	q = roundToPrecision(q, context);
	q = reduceToIdeal(q, diff);
	return q;
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T, U)(in T x, in U z, Context context = T.context)
		if (isDecimal!T && isConvertible!U)
{
	return div(x, T(z), context);
}	// end div(x, z)

/**
 * Reduces operand to simplest form. All trailing zeros are removed.
 * Reduces operand to specified exponent.
 */
 // TODO: (behavior) has non-standard flag setting
// NOTE: flags only
private T reduceToIdeal(T)(T x, int ideal) if (isDecimal!T) {

	if (!x.isFinite()) {
		return x;
	}
	int zeros = trailingZeros(x.coefficient, numDigits(x.coefficient));

	int idealshift = ideal - x.exponent;
	int	canshift = idealshift > zeros ? zeros : idealshift;
	x.coefficient = shiftRight(x.coefficient, canshift);
	x.exponent = x.exponent + canshift;

	if (x.coefficient == 0) {
		x = T.zero;
	}
	x.digits = numDigits(x.coefficient);
	return x;
}

unittest {	// div
	ArithTestData!(TD,2)[] data =
	[
		{ 1, 3, "0.333333333" },
		{ 2, 3, "0.666666667" },
		{ 5, 2, "2.5" },
		{ 1, 10, "0.1" },
		{ 12, 12, 1 },
		{ "8.00", "2", "4.00" },
		{ "2.400", "2.0", "1.20" },
		{ 1000, 10,  100 },
		{ 1000,  1, 1000 },
		{ "2.4E+6", "2.0", "1.2E+6" },
//		{ "1.3", "2.07", "0.77" },	// uncomment to test failure
	];
	TestResults tr = testArith!(TD,2,true)("div", &div!TD, data);
    writeln(tr.report);
}

/*public T integerPart(T)(T x) if (isDecimal!T)
{
	int expo = x.exponent;
	int digits = x.digits;
	if (expo >= 0) return x;
	expo = -expo;
	if (expo >= digits) return T.zero(x.sign);	// TODO: review conditions for -0


}*/

// TODO: (behavior) Does this implement the actual spec operation?
/// Divides the first operand by the second and returns the integer portion
/// of the quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide-integer' function in the specification. (p. 30)
public T divideInteger(T)(in T x, in T y)  {
	T quo;
	remquo(x,y,quo);
	return quo;
/*	// check for NaN and division by zero
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.isZero) return divisionByZero(x, y);

	auto dividend = x.dup;
	auto divisor  = y.dup;
	auto quotient = T.zero;

	// align operands
	int diff = dividend.exponent - divisor.exponent;
	if (diff < 0) {
		divisor.coefficient = shiftLeft(divisor.coefficient, -diff);
	}
	if (diff > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, diff);
	}
	quotient.sign = dividend.sign ^ divisor.sign;
	quotient.coefficient = dividend.coefficient / divisor.coefficient;
	if (quotient.coefficient == 0) return T.zero(quotient.sign);
	quotient.exponent = 0;
	// number of digits cannot exceed precision
	int digits = numDigits(quotient.coefficient);
	if (digits > T.precision) {
		return invalidOperation!T;
	}
	quotient.digits = digits;
	return quotient;*/
}

// TODO: (behavior) Does this implement the actual spec operation?
/// Divides the first operand by the second and returns the integer portion
/// of the quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide-integer' function in the specification. (p. 30)
public T remquo(T)(in T x, in T y, out T quotient) if (isDecimal!T)
{
	// check for NaN and division by zero
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.isZero) return divisionByZero(x, y);

	auto dividend = x.dup;
	auto divisor  = y.dup;
	quotient.sign = dividend.sign ^ divisor.sign;
	if (x.isZero) {
		quotient = T.zero(quotient.sign);
		return quotient.dup;
	}
//    auto remainder = T.zero;

	// align operands
	int diff = dividend.exponent - divisor.exponent;
	if (diff < 0) {
		divisor.coefficient = shiftLeft(divisor.coefficient, -diff);
	}
	if (diff > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, diff);
	}
	xint div, mod;
	div = eris.integer.extended.xint.divmod(dividend.coefficient, divisor.coefficient, mod);
	quotient = T(div);
	T remainder = T(mod);
	// number of digits cannot exceed precision
	int digits = numDigits(quotient.coefficient);
	if (digits > T.precision) {
		remainder = T.nan;
		return invalidOperation!T;
	}
	quotient.digits = digits;
	remainder.digits = numDigits(remainder.coefficient);
	remainder.sign = quotient.sign;
	return remainder;
}

unittest {	// divideInteger
	ArithTestData!(TD,2)[] data =
	[
		{  1,  3, 0 },
		{  2,  3, 0 },
		{  3,  3, 1 },
		{  3, "2.999", 1 },
		{ 10,  3, 3 },
        {  1,  0.3, 3 },
		{  1,  3, 0 },
		{  5,  2, 2 },
		{  1, 10, 0 },
		{ 12, 12, 1 },
		{  8,  2, 4 },
		{ "2.400", 2, 1 },
		{ 1000, 10,  100 },
		{ 1000,  1, 1000 },
		{ "2.4E+6", "2.0", "1.2E+6" },
	];
	TestResults tr = testArith!(TD,2)("divInt", &divideInteger!TD, data);
    writeln(tr.report);
}

/// Divides the first operand by the second and returns the
/// fractional remainder.
/// Division by zero sets a flag and returns infinity.
/// The sign of the remainder is the same as that of the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'remainder' function in the specification. (p. 37-38)
// TODO: (behavior) do we need a context version??
public T remainder(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN and division by zero
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.isZero) return divisionByZero(x, y);

	T quotient = divideInteger!T(x, y,);
	T remainder = x - mul!T(y, quotient, Context(context.precision, context.maxExpo, Rounding.none));
	return remainder;
}

unittest {	// remainder
	ArithTestData!(TD,2)[] data =
	[
		{ 2.1, 3,  2.1 },
		{  10, 3,  1 },
		{ -10, 3, -1 },
		{   3, "2.999", "0.001" },
        {10.2, 1, 0.2 },
        {  10, 0.3, 0.1 },
		{ 3.6, 1.3, 1 },
		{  5,  2, 1 },
		{  1, 10, 1 },
		{ 12, 12, 0 },
		{  8,  2, 0 },
		{ "2.400", 2, "0.400" },
		{ 1000, 10, 0 },
		{ 1000,  1, 0 },
		{ "2.4E+6", "2.0", "0.0" },
	];
	TestResults tr = testArith!(TD,2)("remainder", &remainder!TD, data);
    writeln(tr.report);
}

/// Divides the first operand by the second and returns the
/// fractional remainder.
/// Division by zero sets a flag and returns Infinity.
/// The sign of the remainder is the same as that of the first operand.
/// This function corresponds to the "remainder" function
/// in the General Decimal Arithmetic Specification.
public T remainderNear(T)(in T x, in T y) if (isDecimal!T)
{
	// check for NaN and division by zero
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.isZero) return divisionByZero(x, y);
	T quotient = x/y;
	// TODO: (behavior) roundToIntegralValue?
	T remainder = x - y * (roundToIntegralExact(quotient));
	return remainder;
}

unittest {	// remainder
	ArithTestData!(TD,2)[] data =
	[
		{ 2.1, 3, -0.9 },
		{   3, 2, -1 },
		{ -10, 3, -1 },
		{  10, 3, 1 },
		{   3, "2.999", "0.001" },
        { 10.2, 1, 0.2 },
        {  10, 0.3, 0.1 },
		{ 3.6, 1.3, -0.3 },
	];
	TestResults tr = testArith!(TD,2)
		("remNear", &remainderNear!TD, data);
    writeln(tr.report);
}


// TODO: (behavior) add 'remquo' function. (Uses remainder-near(?))

//--------------------------------
// rounding routines
//--------------------------------

/// Returns the number which is equal in value and sign
/// to the first operand with the exponent of the second operand.
/// The returned value is rounded to the current precision.
/// This operation may set the invalid-operation flag.
/// Implements the 'quantize' function in the specification. (p. 36-37)
public T quantize(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);

	// if one operand is infinite and the other is not...
	if (x.isInfinite != y.isInfinite) {
		return invalidOperation!T;
	}
	// if both arguments are infinite
	if (x.isInfinite && y.isInfinite) {
		return x.dup;
	}
	T z = x.dup;
	int diff = x.exponent - y.exponent;

	if (diff == 0) {
		return z;
	}

	// TODO: (behavior) this shift can cause integer overflow for fixed size decimals
	if (diff > 0) {
		z.coefficient = shiftLeft(z.coefficient, diff/*, precision*/);
		z.digits = z.digits + diff;
		z.exponent = y.exponent;
		if (z.digits > T.precision) {
			z = T.nan;
		}
		return z;
	}
	else {
		int precision = (-diff > x.digits) ? 0 : x.digits + diff;
		z = roundToPrecision(z, precision, context.maxExpo, context.rounding);
		z.exponent = y.exponent;
		if (z.isZero && x.isSigned) {
			z.sign = true;
		}
		return z;
	}
}

unittest {	// quantize
	ArithTestData!(TD,2)[] data =
	[
		{ "2.17", "0.001", "2.170" },
		{ "2.17", "0.01", "2.17" },
		{ "2.17", "0.1", "2.2" },
		{ "2.17", "1", "2" },
		{ "2.17", "1E+1", "0E+1" },

		{ "-Infinity", "Infinity", "-Infinity" },
		{ "2", "Infinity", "NaN" },
		{ "-0.1", "1", "-0" },
		{ "-0", "1E+5", "-0E+5" },
		{ "+35236450.6", "1E-2", "NaN" },

		{ "-35236450.6", "1E-2", "NaN" },
		{ "217", "1E-1", "217.0" },
		{ "217", "1E+0", "217" },
		{ "217", "1E+1", "2.2E+2" },
		{ "217", "1E+2", "2E+2" },
	];
	TestResults tr = testArith!(TD,2,true)
		("quantize", &quantize!TD, data);
    writeln(tr.report);
}

/// Returns the nearest integer value to the argument.
/// Context flags may be set.
/// Implements the 'round-to-integral-exact' function
/// in the specification. (p. 39)
public T roundToIntegralExact(T)(in T x,
		in Rounding rounding = Rounding.halfEven) if (isDecimal!T)
{
	T result = x.dup;
	if (result.isSignaling) return invalidOperation!T;
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	// TODO: (behavior) need to prevent precision overrides
	int precision = result.digits + result.exponent;
	result = roundToPrecision(result, precision, T.maxExpo, rounding);
	return result;
}

unittest {	// roundToIntegralExact
	ArithTestData!(TD,1)[] data =
	[
		{ 2.1, 2 },
		{ 0.7, 1 },
		{ 100, 100 },
		{ 101.5,  102 },
		{-101.5, -102 },
		{ "10E+5", "1.0E+6" },
		{ "7.89E+77", "7.89E+77" },
		{ "-Inf", "-Inf" },
	];
	TestResults tr = testArith!(TD,1,true)
		("roundIntEx", &roundToIntegralExact!TD, data);
    writeln(tr.report);
}

// TODO: (behavior) need to re-implement this so no flags are set.
/// The result may be rounded and context flags may be set.
/// Implements the 'round-to-integral-value' function
/// in the specification. (p. 39)
public T roundToIntegralValue(T)(in T arg,
		const Rounding rounding = T.rounding) if (isDecimal!T)
{
	T result = arg.dup;
	if (result.isSignaling) return invalidOperation!T;
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	int precision = result.digits + result.exponent;
	result = roundToPrecision(result, context);
	return result;
}

/// Aligns the two operands by raising the smaller exponent
/// to the value of the larger exponent, and adjusting the
/// coefficient so the value remains the same.
/// Both operands will have the same exponent on their return.
/// No flags are set and the result is not rounded.
private void alignOps(T, U)(ref T x, ref U y) if (isDecimal!T && isDecimal!U)
{
	int diff = x.exponent - y.exponent;
	if (diff > 0) {
		x.coefficient = shiftLeft(x.coefficient, diff);
		x.exponent = y.exponent;
	}
	else if (diff < 0) {
		y.coefficient = shiftLeft(y.coefficient, -diff);
		y.exponent = x.exponent;
	}
}

/// Aligns the two operands by raising the smaller exponent
/// to the value of the larger exponent, and adjusting the
/// coefficient so the value remains the same.
/// Both operands will have the same exponent on their return.
/// No flags are set and the result is not rounded.
private void alignOps(T, U:long)(ref T x, U n) if (isDecimal!T)
{
	int diff = x.exponent;
	if (x.exponent == 0) return;

	if (x.exponent > 0) {
		x.coefficient = shiftLeft(x.coefficient, x.exponent);
		x.exponent = 0;
	}
	else if (diff < 0) {
		y.coefficient = shiftLeft(y.coefficient, -diff);
		y.exponent = x.exponent;
	}
}

unittest { // alignOps
 	write("-- alignOps.........");
	TD arg1, arg2;
	arg1 = TD("1.3E35");
	arg2 = TD("-17.4E29");
	alignOps(arg1, arg2);
	assertEqual(arg1.coefficient, 13000000);
	assertEqual(arg2.exponent, 28);
	writeln("passed");
}

//--------------------------------
// validity functions
//--------------------------------

/// Sets the invalid-operation flag and returns a quiet NaN.
// TODO: combine this with invalidOperand?
package T invalidOperation(T)(ushort payload = 0)
		if (isDecimal!T)
{
	contextFlags.set(InvalidOperation);
	return T.nan(payload);
}

unittest {	// invalidOperation
/*	TD arg, expect, actual;
	// TODO: (testing) Can't actually test payloads at this point.
	arg = TD("sNaN123");
	expect = TD("NaN123");
	actual = abs!TD(arg);
	assertTrue(actual.isQuiet);
	assertTrue(contextFlags.getFlag(InvalidOperation));
	assertEqual(actual.toAbstract, expect.toAbstract);*/
}

/// Returns a quiet NaN and sets the invalid-operation flag.
/// "The result of any arithmetic operation which has an operand
/// which is a NaN (a quiet NaN or a signaling NaN) is [s,qNaN]
/// or [s,qNaN,d]. The sign and any diagnostic information is copied
/// from the first operand which is a signaling NaN, or if neither is
/// signaling then from the first operand which is a NaN."
/// -- General Decimal Arithmetic Specification, p. 24
//@safe
package T invalidOperand(T)(in T x) if (isDecimal!T)
{
	// flag the invalid operation
	contextFlags.set(InvalidOperation);
	// if the operand is a quiet NaN return it.
	if (x.isQuiet) return x.dup;
	// Otherwise change the signalling NaN to a quiet NaN.
	if (x.isSignaling) return T.nan(x.payload, x.sign);
	// if the operand is neither quiet nor signaling something else is wrong
	// so return NaN.
	return T.nan.dup;
}

/// Returns a quiet NaN and sets the invalid-operation flag.
/// "The result of any arithmetic operation which has an operand
/// which is a NaN (a quiet NaN or a signaling NaN) is [s,qNaN]
/// or [s,qNaN,d]. The sign and any diagnostic information is copied
/// from the first operand which is a signaling NaN, or if neither is
/// signaling then from the first operand which is a NaN."
/// -- General Decimal Arithmetic Specification, p. 24
//@safe
package T invalidOperand(T)(in T x, in T y) if (isDecimal!T)
{
	// flag the invalid operation
	contextFlags.set(InvalidOperation);
	// if either operand is signaling return a quiet NaN.
	// NOTE: sign is ignored.
	if (x.isSignaling) return T.nan(x.payload);
	if (y.isSignaling) return T.nan(y.payload);
	// if the operand is a quiet NaN return it.
	if (x.isQuiet) return x.dup;
	if (y.isQuiet) return y.dup;
	// if neither of the operands is quiet or signaling,
	// the operands are invalid for some reason. return a quiet NaN.
	return T.nan;
}

/// Checks for invalid operands and division by zero.
/// If found, the function returns NaN or infinity, respectively,
/// and sets the context flags.
/// This is a helper function implementing checks for division by zero
/// and invalid operation in the specification. (p. 51-52)
// precondition: divisor is zero.
private T divisionByZero(T)(in T dividend, in T divisor) if (isDecimal!T)
{
	// division of zero by zero is undefined
	if (dividend.isZero) return invalidOperation!T;
	// set flag and return signed infinity
	contextFlags.set(DivisionByZero);
	return T.infinity(dividend.sign ^ divisor.sign);
}

/// Checks for invalid operands and division by zero.
/// If found, the function returns NaN or infinity, respectively,
/// and sets the context flags.
/// This is a helper function implementing checks for division by zero
/// and invalid operation in the specification. (p. 51-52)
// precondition: divisor is zero.
private T divisionByZero(T, U:long)(in T dividend, U divisor)
		if (isDecimal!T)
{
	// division of zero by zero is undefined
	if (dividend.isZero) return invalidOperation!T;
	// set flag and return signed infinity
	contextFlags.set(DivisionByZero);
	return T.infinity(dividend.sign ^ (divisor < 0));
}

unittest {
	write("divisionByZero......");
	writeln("test missing");
}

unittest
{
	writeln("==========================");
	writeln("decimal arithmetic.....end");
	writeln("==========================");
}

