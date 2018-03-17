// Written in the D programming language

/**
 * Floating-point decimal arithmetic.
 *
 * An implementation of the
 * General Decimal Arithmetic Specification.
 *
 * Authors: Paul D. Anderson
 *
 * Copyright: Copyright 2009-2016 by Paul D. Anderson.
 *
 * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
 *
 * Standards: Conforms to the
 *	General Decimal Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 */

/*

 *  carry out internal operations at a precision different from the type
 *  precision.
 */

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

import eris.decimal;
import std.string;
import std.algorithm;
	import std.stdio;

unittest {
	writeln("     arithmetic.tests     ");
	writeln("==========================");
}

version(unittest)
{
	import std.stdio;
	import eris.decimal.test;
}


//--------------------------------
// classification functions
//--------------------------------

/**
 *  Returns a string indicating the class and sign of the argument.
 *
 *  Classes are: sNaN, NaN, Infinity, Zero, Normal, and Subnormal.
 *  The sign of any NaN values is ignored in the classification.
 *  The argument is not rounded and no flags are changed.
 *
 * Standards: Implements the 'class' function in the specification. (p. 42)
 *
 * Flags: None
 *
 */
public string classify(T)(in T x) if (isDecimal!T)
{
	if (x.isFinite)
	{
		if (x.isZero)		{ return x.sign ? "-Zero" : "+Zero"; }
		if (x.isNormal)		{ return x.sign ? "-Normal" : "+Normal"; }
		if (x.isSubnormal)	{ return x.sign ? "-SUBNORMAL" : "+SUBNORMAL"; }
	}
	if (x.isInfinite)  { return x.sign ? "-Infinity" : "+Infinity"; }
	if (x.isSignaling) { return "sNaN"; }
	return "NaN";
}

unittest
{	// classify
	static struct S { TD x; string expect; }
	S[] s =
	[
		{ TD.nan,         "NaN" },
		{ TD.snan,        "sNaN" },
		{ TD.infinity,    "+Infinity" },
		{ TD("1E-10"),    "+Normal" },
		{ TD("-0"),       "-Zero" },
		// definitions of normal and subnormal depend on context -- pass these?
		{ TD("-0.9E-368"), "-SUBNORMAL" },
	];
	auto f = FunctionTest!(S,string)("classify");
	foreach (t; s) f.test(t, classify(t.x));
    writefln(f.report);
}

/**
 *  Returns the truncated base 10 logarithm of the argument.
 *
 *  "...The integer which is the exponent of the magnitude
 *  of the most significant digit of the operand.
 *  (As though the operand were truncated to a single digit
 *  while maintaining the value of that digit and without
 *  limiting the resulting exponent)". (p. 47)
 *
 *  Standards: Implements the 'logb' function in the specification. (p. 47)
 *
 *  Flags: INVALID_OPERATION, DIVISION_BY_ZERO.
 *
 */
public int ilogb(T)(in T x) if (isDecimal!T)
{
	if (x.isInfinite || x.isNaN) {
		invalidOperation!T;
		return int.init;
	}
	if (x.isZero)
	{
		contextFlags.set(DIVISION_BY_ZERO);
		return int.init;
	}
	return x.digits + x.expo - 1;
}

unittest
{	// ilogb
	static struct S { TD x; int expect; }
	S[] s =
	[
		{ 250,    2 },
		{ 2.5,    0 },
		{ 0.03,  -2 },
		{ "Inf",  0 },	// sets INVALID_OPERATION flag
		{ 0,      0 },	// sets DIVISION_BY_ZERO flag
	];
	auto f = FunctionTest!(S,int)("ilogb");
	foreach (t; s) f.test(t, ilogb(t.x));
    writefln(f.report);
}

/**
 *  Returns the truncated base 10 logarithm of the argument.
 *
 *  "...The integer which is the exponent of the magnitude
 *  of the most significant digit of the operand.
 *  (As though the operand were truncated to a single digit
 *  while maintaining the value of that digit and without
 *  limiting the resulting exponent)". (p. 47)
 *
 * Flags: INVALID_OPERATION, DIVISION_BY_ZERO
 *
 *
 *  Standard: Implements the 'logb' function in the specification. (p. 47)
 *
 */
public T logb(T)(in T x) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	if (x.isInfinite) return T.infinity;
	if (x.isZero)
	{
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity(true);
	}
	int exp = x.digits + x.expo - 1;
	return T(exp);
}

unittest
{	// logb
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ 250,    2 },
		{ 2.5,    0 },
		{ 0.03,  -2 },
		{ "Inf", "Inf" },
		{ 0,     "-Inf" },
		{ "NaN", "NaN" }, // NOTE: this test will fail: NaN != NaN
	];
	auto f = FunctionTest!(S,TD)("logb");
	foreach (t; s) f.test(t, logb(t.x));
    writefln(f.report);
}

/**
 *  If the first operand is infinite then that operand is returned,
 *  otherwise the result is the first operand modified by
 *  adding the value of the second operand to its exponent.
 *
 *  The second operand must be a finite integer (<= int.max && >= int.min)
 * 	with an exponent of zero.
 *
 *  Remarks: The result may overflow or underflow.
 *
 *  Standards: Implements the 'scaleb' function in the specification. (p. 48)
 *
 *  Flags: INVALID_OPERATION, UNDERFLOW, OVERFLOW.
 *
 */
public T scaleb(T)(in T x, in T y) if (isDecimal!T)
{
	T scaled = x.dup;

	if (x.isNaN || y.isNaN) return invalidOperand(x,y);

	if (x.isInfinite) return scaled;

	if (y.isInfinite || y.expo != 0)
	{
		return invalidOperand(y);
	}
//	if (y > T.IntMax || y < T.IntMin)
	if (y > T(int.max) || y < T(int.min))
	{
		return invalidOperand(y);
	}

	int scale = /*cast(int)*/y.coff.toInt;

	if (y.isSigned)
	{
		scale = -scale;
	}
	// TODO: (behavior) check for overflow/underflow (GDA "scaleb").
	scaled.expo = scaled.expo + scale;
	return scaled;
}

unittest
{	// scaleb
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
		{ "7.50", "-2", "0.0750" },
//		{ "7.50", "-3", "0.0750" },
	];
	auto f = FunctionTest!(S,TD)("scaleb");
	foreach (t; s) f.test(t, scaleb(t.x, t.y));
    writefln(f.report);
}

//--------------------------------
// unary functions
//--------------------------------

/**
 *  Rounds the argument to an integer using the specified rounding mode.
 *  The default rounding mode is the current context mode. //FIXTHIS
 */
public D round(D)(D x, Round mode = D.mode)
{
	if (x.isNaN)
    {
		contextFlags.set(INVALID_OPERATION);
		return D.nan;
	}
	D value = roundToIntegralExact(x, mode);
	return value;
}


/**
 *
 *  Returns the operand reduced to its simplest form.
 *
 *  <code>reduce</code> has the same semantics as the plus operation,
 *  except that a finite result is
 *  reduced to its simplest form, with all trailing
 *  zeros removed and its sign preserved.
 *
 *  zeros removed and its sign preserved.
 *
 *  Standard: Implements the 'reduce' function in the specification. (p. 37)
 *  "This operation was called 'normalize' prior to
 *  version 1.68 of the specification." (p. 37)
 *
 *  Flags: INVALID_OPERATION
 *
 */
public T reduce(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	// special cases
	if (x.isNaN) return invalidOperand(x);
	if (!x.isFinite) return x.dup;

	// round the argument
	T reduced = plus(x, context);

	// have to check again -- rounding may have made it infinite
	if (!reduced.isFinite) return reduced;

	int digits = reduced.digits;
	auto temp = reduced.coff;
	int zeros = clipZeros(temp, digits);
	if (zeros)
	{
		reduced.coff = temp;
		reduced.digits = digits - zeros;
		reduced.expo = reduced.expo + zeros;
	}

	return reduced;
}

// just a wrapper
public T normalize(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	return reduce(x, context);
}

unittest
{	// reduce
	// test results depend on context
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.200", "1.2" },
//		{ "1.200", "1.3" },	// should fail
//	FIXTHIS: should fail but doesn't
		{ "1.200", "1.20" },	// NOTE: should fail but doesn't
		{ "1.2001", "1.2001" },
		{ "1.2000000000000001", "1.2" },
	];
	auto f = FunctionTest!(S,TD)("reduce");
	foreach (t; s) f.test(t, reduce(t.x));
    writefln(f.report);
}

/**
 *  Returns the absolute value of the argument.
 *
 *  The result is equivalent to plus(arg) for positive numbers
 *  and to minus(arg) for negative numbers.
 *
 *  Note: This operation rounds the result and may set flags.
 *  To return the absolute value without rounding or setting flags
 *  use the 'copyAbs' function.
 *
 *  Standards: Implements the 'abs' function in the specification. (p. 26)
 *
 *  Flags: INVALID_OPERATION
 */
public D abs(D)(in D arg, Context context = D.context) if (isDecimal!D)
{
	if (arg.isNaN) return invalidOperand(arg);
	return roundToPrecision(arg.copyAbs, context);
}

unittest
{	// abs
	static struct S { TD arg; TD expect; }
	S[] s =
	[
		{ "-Inf", "Inf" },
		{ "101.5", "101.5" },
		{ "-101.5", "101.5" },
		// test results that are rounded are dependent on context
		{ "-1.234567890123456749E+23", "1.2345678901234567E+23" },	// rounds the argument
	];
	auto f = FunctionTest!(S,TD)("abs");
	foreach (t; s) f.test(t, abs(t.arg));
    writefln(f.report);
}

/**
 *  Returns -1, 0, or 1 if the argument is
 *  negative, zero, or positive, respectively.
 *  The sign of zero is ignored: returns 0 for +0 or -0.
 */
public int sgn(D)(in D arg) if (isDecimal!D)
{
	if (arg.isZero) return 0;
	return arg.isNegative ? -1 : 1;
}

unittest
{	// sgn
	static struct S { TD arg; int expect; }
	S[] s =
	[
		{  "-123", -1 },
		{  "2345",  1 },
		{ "-2345", -1 },
		{     "0",  0 },
		{    "-0",  0 },
		{  "0.00",  0 },
		{  "-Inf", -1 },
	];
	auto f = FunctionTest!(S,int)("sgn");
	foreach (t; s) f.test(t, sgn(t.arg));
    writefln(f.report);
}

/**
 *  Returns -1, 0, or 1
 *  if the argument is negative, zero, or positive, respectively.
 */
/*public int sgn(T:BigInt)(T arg) {
	if (arg < 0) return -1;
	if (arg > 0) return 1;
	return 0;
}*/

/**
 *  Returns a copy of the argument with the same sign as the argument.
 *  The result is equivalent to add('0', arg).
 *
 *  Note: This operation rounds the result and may set flags.
 *  To copy without rounding or setting flags use the 'copy' function.
 *
 *  Standards: Implements the 'plus' function in the specification. (p. 33)
 *
 *  Flags: INVALID_OPERATION
 */
public D plus(D)(in D arg,
		Context context = D.context) if (isDecimal!D)
{
	if (arg.isNaN) return invalidOperand(arg);
//if (!__ctfe) writefln("arg = %s", arg);
	return roundToPrecision(arg, context);
}

unittest
{	// plus -- depends on context
	static struct S { TD arg; TD expect; }
	S[] s =
	[
		{ "1.3", "1.3" },
		{ "101.5", "101.5" },
		{ "-101.5", "-101.5" },
	];
	auto f = FunctionTest!(S,TD)("plus");
	foreach (t; s) f.test(t, plus(t.arg));
    writefln(f.report);
}

/**
 *
 *  Returns a copy of the argument with the opposite sign.
 *  The result is equivalent to subtract('0', arg).
 *
 *  This operation rounds the argument and may set flags.
 *  To copy without rounding or setting flags use the 'copyNegate' function.
 *
 *  Implements the 'minus' function in the specification. (p. 37)
 *
 *  Flags: INVALID_OPERATION
 *
 */
public D minus(D)(in D arg,
		Context context = D.context) if (isDecimal!D)
{
	if (arg.isNaN) return invalidOperand(arg);
	return roundToPrecision(arg.copyNegate, context);
}

unittest
{	// minus -- depends on context
	static struct S { TD arg; TD expect; }
	S[] s =
	[
		{ "1.3", "-1.3" },
		{ "101.5", "-101.5" },
		{ "-101.5", "101.5" },
	];
	auto f = FunctionTest!(S,TD)("minus");
	foreach (t; s) f.test(t, minus(t.arg));
    writefln(f.report);
}

//-----------------------------------
// next-plus, next-minus, next-toward
//-----------------------------------

/**
 *
 *  Returns the smallest representable number that is larger than
 *  the argument.
 *
 *  Implements the 'next-plus' function in the specification. (p. 34)
 *
 *  Note that the overflow flag is not set by this operation.
 *
 *  Flags: INVALID_OPERATION
 *
 */
public D nextPlus(D)(in D arg, Context context = D.context)
	if (isDecimal!D)
{
	if (arg.isNaN) return invalidOperand(arg);

	if (arg.isInfinite) {
		if (arg.sign) {
			return D.max.copyNegate;
		}
		else {
			return arg;
		}
	}
	int adjustedExpo = arg.expo + arg.digits - context.precision;
	if (adjustedExpo < D.tinyExpo) {
			return D(0, D.tinyExpo, true);
	}

	D increment = D(1, adjustedExpo);
	D next = add(arg, increment, context, false);
	if (next > D.max) {
		next = D.infinity;
	}

	// FIXTHIS: need to pass setFlags value
	return roundToPrecision(next);
}

unittest
{	// nextPlus -- depends on context
	static struct S { TD arg; TD expect; }
	S[] s =
	[
/*		D99 tests
		{ "1", 			 "1.00000001" },
		{ "-1E-107", 	 "-0E-107" },
		{ "-1.00000003", "-1.00000002" },
		{ "-Infinity",	 "-9.99999999E+99" },
		{ "9.99999999E+99",	 "Infinity" },	// overflow flag should not be set!
		{ "1E+101",	 "Infinity" },*/

		{ "1",			"1.000000000000001" },
		{ "-1E-384",	"-0E-384" },
		{ "-1.000000000000003", "-1.000000000000002" },
		{ "-Infinity",	"-9.999999999999999E+369" },
		{ "9.999999999999999E+369",	 "Infinity" },	// overflow flag should not be set!
		{ "1E+371",	 "Infinity" },
	];
	auto f = FunctionTest!(S,TD)("nextPlus");
	foreach (t; s) f.test(t, nextPlus(t.arg));
    writefln(f.report);
}

/**
 *  Returns the largest representable number that is smaller than
 *  the argument.
 *
 *  Standards: Implements the 'next-minus' function in the specification. (p. 34)
 *
 *  Flags: INVALID_OPERATION.
 *
 *  Note: The overflow flag is not set by this operation.
 */
public D nextMinus(D)(in D num,	Context context = D.context)
    if (isDecimal!D)
{
	if (num.isNaN) return invalidOperand(num);

	if (num.isInfinite) {
		return num.sign ? num : D.max;
	}

	int adjustedExpo = num.expo + num.digits - context.precision;
	if (num.coff == 1) adjustedExpo--;
	if (adjustedExpo < D.tinyExpo) {
		return D(0, D.tinyExpo);
	}

	D increment = D(1, adjustedExpo);
	D next = sub(num, increment, context);
	if (next < D.max.copyNegate) {
		next = D.infinity.copyNegate;
	}
	return next;
}

unittest
{	// nextMinus -- depends on context
	static struct S { TD arg; TD expect; }
	S[] s =
	[
		{ "1", 					"0.9999999999999999" },
		{ "1E-384",				"0E-384" },
		{ "-1.000000000000003",		"-1.000000000000004" },
		{ "Infinity",			"9.999999999999999E+369" },
		{ "-9.999999999999999E+368",	"-Infinity" },
	];
	auto f = FunctionTest!(S,TD)("nextMinus");
	foreach (t; s) f.test(t, nextMinus(t.arg));
    writefln(f.report);
}

/**
 *
 *  Returns the representable number that is closest to the first operand
 *  in the direction of the second operand.
 *
 *  Implements the 'next-toward' function in the specification. (p. 34-35)
 *
 *  Flags: INVALID_OPERATION
 *
 */
// TODO: anomalous flag settings
public D nextToward(D)(in D arg, in D toward,
		Context context = D.context) if (isDecimal!D)
{
//   	D nan;
	if (arg.isNaN || toward.isNaN) return invalidOperand(arg, toward);

	// compare them but don't round yet
	int comp = compare(arg, toward, context);
	if (comp < 0) return nextPlus(arg, context);
	if (comp > 0) return nextMinus(arg, context);

	return roundToPrecision(arg.copySign(toward), context);
}

unittest
{	// nextToward -- depends on context
	static struct S { TD arg; TD toward; TD expect; }
	S[] s =
	[
/*		{ " 1",          "2", " 1.00000001" },
		{ "-1.00000003", "0", "-1.00000002" },*/
	];
	auto f = FunctionTest!(S,TD)("nextToward");
	foreach (t; s) f.test(t, nextToward(t.arg, t.toward));
    writefln(f.report);
}

//--------------------------------
// comparison functions
//--------------------------------

/**
 *  Compares two operands numerically to the current precision.
 *
 *  Note: The operands are rounded before they are compared.
 *  This may result in unexpected results. For instance,
 *  if both operands are too large (small) for the context
 *  they will both be rounded to infinity (zero) and the function will
 *  return 0, indicating that they are equal, even though they are
 *  numerically different before rounding.
 *
 *  To compare numbers without rounding, use compareTotal.
 *
 *  Returns: -1, 0, or +1 if the second operand is, respectively,
 *  less than, equal to, or greater than the first operand.
 *
 *  Standards: Implements the 'compare' function in the specification. (p. 27)
 *
 *  Flags: INVALID_OPERATION
 *
 */
public int compare(T)(in T left, in T right,
		Context context = T.context) if (isDecimal!T)
{
	// any operation with a signaling NaN is invalid.
	// if both are signaling, return as if left > right.
	if (left.isSignaling || right.isSignaling) {
		contextFlags.set(INVALID_OPERATION);
		return left.isSignaling ? 1 : -1;
	}

	// if both are NaN, return as if left > right.
	if (left.isNaN || right.isNaN) {
		return left.isNaN ? 1 : -1;
	}

	// if either is zero...
	if (left.isZero) {
		if (right.isZero) return 0;
		return right.isNegative ? 1 : -1;
	}
	if (right.isZero) {
		return left.isNegative ? -1 : 1;
	}

	// if signs differ, just compare the signs
	if (left.sign != right.sign) {
		// check for zeros: +0 and -0 are equal
		if (left.isZero && right.isZero) return 0;
		return left.sign ? -1 : 1;
	}

	// if either is infinite...
	if (left.isInfinite || right.isInfinite) {
		if (left.isInfinite && right.isInfinite) return 0;
		return left.isInfinite ? 1 : -1;
	}

	T lf = left.dup;
	T rt = right.dup;

	// TODO: (testing) test compare at precision limits.
	// restrict operands to current precision
	if (lf.digits > context.precision) {
		lf = roundToPrecision(lf, context);
	}
	if (rt.digits > context.precision) {
		rt = roundToPrecision(rt, context);
	}

	// TODO: this will return inf == inf after rounding
	// Check again for infinities
	if (lf.isInfinite || rt.isInfinite) {
		if (lf.isInfinite && rt.isInfinite) return 0;
		return lf.isInfinite ? 1 : -1;
	}

	// compare the magnitudes of the numbers
	lf = lf.reduce;
	rt = rt.reduce;
	int diff = (lf.expo + lf.digits) - (rt.expo + rt.digits);
	if (diff != 0) {
		if (!lf.sign) {
			if (diff > 0) return 1;
			if (diff < 0) return -1;
		}
		else {
			if (diff > 0) return -1;
			if (diff < 0) return 1;
		}
	}
	// align the operands
	alignOps(lf, rt);	// both operands now have the same exponent

	// The only remaining difference is in the coefficients.
	if (lf.coff == rt.coff) return 0;
	return (lf.coff > rt.coff) ? 1 : -1;
}

unittest
{	// compare -- depends on context?
	static struct S { TD x; TD y; int expect; }
	S[] s =
	[
		{ " 3  ", " 2.1 ",  1 },
		{ "-3  ", " 2.1 ", -1 },
		{ " 2.1", "-3   ",  1 },
		{ " 2.1", " 2.1 ",  0 },
		{ " 2.1", " 2.10",  0 },
		{ " Inf", "-Inf ",  1 },
		{ " Inf", " Inf ",  0 },
		{ " Inf", " 12  ",  1 },
		{ "-Inf", " 12  ", -1 },
	];
	auto f = FunctionTest!(S,int)("compare");
	foreach (t; s) f.test(t, compare(t.x, t.y));
    writefln(f.report);
}

/**
 *  Returns true if the operands are equal to the context precision.
 *  Finite numbers are equal if they are numerically equal
 *  to the context precision.
 *  Infinities are equal if they have the same sign.
 *  Zeros are equal regardless of sign.
 *  A NaN is not equal to any number, not even another NaN.
 *  In particular, a decimal NaN is not equal to itself (this != this).
 *
 *  Note: The operands are rounded before they are compared.
 *  This may result in unexpected results. For instance,
 *  if both operands are too large (small) for the context
 *  they will both be rounded to infinity (zero) and the function will
 *  return true, indicating that they are equal, even though they are
 *  numerically different before rounding.
 *
 *  Flags: INVALID_OPERATION
 */
public bool equals(D)(in D left, in D right,
		Context context = D.context) if (isDecimal!D)
{
	// any operation with a signaling NaN is invalid.
	if (left.isSignaling || right.isSignaling) {
		contextFlags.set(INVALID_OPERATION);
		return false;
	}
	// if either is NaN...
	// NaN is never equal to any number, not even another NaN
	if (left.isNaN || right.isNaN) return false;

	// if they are identical...
	if (left is right) return true;

	// if either is zero...
	if (left.isZero || right.isZero) {
		// ...they are equal if both are zero (regardless of sign)
		return (left.isZero && right.isZero);
	}

	// if their signs differ they are not equal (except for zero, handled above)
	if (left.sign != right.sign) return false;

	// if either is infinite...
	if (left.isInfinite || right.isInfinite) {
		// ...they are equal only if both are infinite with the same sign
		return (left.isInfinite && right.isInfinite);
	}

	// if they have the same representation, they are equal
	if (left.expo == right.expo && left.coff == right.coff) {
		return true;
	}

	// restrict operands to the context precision
	D rx, ry;
	rx = roundToPrecision(left, context);
	ry = roundToPrecision(right, context);

	// if they are not of the same magnitude they are not equal
	if (rx.expo + rx.digits != ry.expo + ry.digits) return false;
	// align the operands
	alignOps(rx, ry);
	return rx.coff == ry.coff;
}

unittest
{	// equals -- depends on context
	static struct S { TD left; TD right; bool expect; }
	S[] s =
	[
		{ " 123.4567     ", " 123.4568   ", false},
		{ " 123.4567     ", " 123.4567   ", true },
		{ " 234123.4567121236 ", " 234123.45671212356", true }, // equals to precision
		{ " 1000000E-8   ", " 1E-2       ", true },
		{ "+100000000E-08", "+1E+00      ", true },
		{ "-1.00000000   ", "-1          ", true },
	];
	auto f = FunctionTest!(S,bool)("equals");
	foreach (t; s) f.test(t, equals(t.left, t.right));
    writefln(f.report);
}

/**
 *  Returns true if the operands are equal to the specified precision. Special
 *  values are handled as in the equals() function. This function allows
 *  comparison at precision values other than the context precision.
 */
public bool precisionEquals(D)(D left, D right, int precision) if (isDecimal!D)
{
	auto context = Context(precision, D.maxExpo, D.mode);
	return (equals(left, right, context));
}

/**
 *  Compares the numeric values of two numbers. CompareSignal is identical to
 *  compare except that quiet NaNs are treated as if they were signaling.
 *  This operation may set the invalid-operation flag.
 *  Implements the 'compare-signal' function in the specification. (p. 27)
 *  Flags: INVALID_OPERATION
 */
public int compareSignal(D) (in D left, in D right,
		Context context = D.context) if (isDecimal!D)
{

	// any operation with NaN is invalid.
	// if both are NaN, return as if left > right.
	if (left.isNaN || right.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return left.isNaN ? 1 : -1;
	}
	return (compare!D(left, right, context));
}

unittest
{	// compareSignal -- depends on context
	static struct S { TD left; TD right; int expect; }
	S[] s =
	[
/*		{ " 3  ", " 2.1 ",  1 },
		{ "-3  ", " 2.1 ", -1 },
		{ " 2.1", "-3   ",  1 },
		{ " 2.1", " 2.1 ",  0 },
		{ " 2.1", " 2.10",  0 },
		{ " Inf", "-Inf ",  1 },
		{ " Inf", " Inf ",  0 },
		{ " Inf", " 12  ",  1 },
		{ "-Inf", " 12  ", -1 },*/
	];
	auto f = FunctionTest!(S,int)("compSignal");
	foreach (t; s) f.test(t, compareSignal(t.left, t.right));
    writefln(f.report);
}

unittest
{
	write("-- compareSignal....");
	TD left, right;
	int value;
	left = 0;
	right = 5;
	assertGreaterThan(right,left);
	contextFlags.resetFlags(INVALID_OPERATION);
	right = TD.snan;
	value = compare(left, right);
	assertTrue(contextFlags.getFlags(INVALID_OPERATION));
	contextFlags.resetFlags(INVALID_OPERATION);
	right = TD.nan;
	value = compare(left, right);
	assertFalse(contextFlags.getFlags(INVALID_OPERATION));
	contextFlags.set(INVALID_OPERATION, false);
	right = TD.nan;
	value = compareSignal(left, right);
	assertTrue(contextFlags.getFlags(INVALID_OPERATION));
	writeln("passed");
}


/**
 *  Takes two numbers and compares the operands using their
 * 	abstract representation rather than their numerical value.
 *  Numbers (representations which are not NaNs) are ordered such that
 *  a larger numerical value is higher in the ordering.
 *  If two representations have the same numerical value
 *  then the exponent is taken into account;
 *  larger (more positive) exponents are higher in the ordering.
 *  Returns -1 If the first operand is lower in the total ordering
 *  and returns 1 if the first operand is higher in the total ordering.
 *  Returns 0 only if the numbers are equal and have the same representation.
 *  Implements the 'compare-total' function in the specification. (p. 42-43)
 *  Flags: NONE.
 */
public int compareTotal(D)(in D left, in D right) if (isDecimal!D)
{
	if (left.isFinite && right.isFinite
		&& left.sign == right.sign
		&& left.expo == right.expo
		&& left.coff == right.coff)
	return 0;

	int ret1 =	1;
	int ret2 = -1;

	// if signs differ...
	if (left.sign != right.sign) {
		return left.sign ? ret2 : ret1;
	}

	// if both numbers are signed swap the return values
	if (left.sign) {
		ret1 = -1;
		ret2 =	1;
	}

	// if either is zero...
	if (left.isZero || right.isZero) {
		// if both are zero compare exponents
		if (left.isZero && right.isZero) {
			auto result = left.expo - right.expo;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return left.isZero ? ret1 : ret2;
	}

	// if either is infinite...
	if (left.isInfinite || right.isInfinite) {
		if (left.isInfinite && right.isInfinite) {
			return 0;
		}
		return left.isInfinite ? ret1 : ret2;
	}

	// if either is quiet...
	if (left.isQuiet || right.isQuiet) {
		// if both are quiet compare payloads.
		if (left.isQuiet && right.isQuiet) {
			auto result = left.coff - right.coff;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return left.isQuiet ? ret1 : ret2;
	}

	// if either is signaling...
	if (left.isSignaling || right.isSignaling) {
		// if both are signaling compare payloads.
		if (left.isSignaling && right.isSignaling) {
			auto result = left.coff - right.coff;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return left.isSignaling ? ret1 : ret2;
	}

	// if both exponents are equal, any difference is in the coefficient
	if (left.expo == right.expo) {
		auto result = left.coff - right.coff;
		if (left.coff == right.coff) return 0;
		return (left.coff > right.coff) ? ret1 : ret2;
	}

	// if the (finite) numbers have different magnitudes...
	int diff = (left.expo + left.digits) - (right.expo + right.digits);
	if (diff > 0) return ret1;
	if (diff < 0) return ret2;

	// we know the numbers have the same magnitude
	// and that the exponents are not equal -- align the operands
 	D lf = left.dup;
	D rt = right.dup;
	alignOps(lf, rt);	// the operands now have the same exponent.

	// if equal after alignment, compare the original exponents
	if (lf.coff == rt.coff) {
		return (left.expo > right.expo) ? ret1 : ret2;
	}
	// otherwise return the numerically larger
	return (lf.coff > rt.coff) ? ret2 : ret1;
}

unittest
{	// compareTotal
	static struct S { TD left; TD right; int expect; }
	S[] s =
	[
		{ " 12.30", " 12.3  ", -1},
		{ " 12.30", " 12.30 ",  0},
		{ " 12.3 ", " 12.300",  1},
	];
	auto f = FunctionTest!(S,int)("compTotal");
	foreach (t; s) f.test(t, compareTotal(t.left, t.right));
    writefln(f.report);
}

/**
 *  compare-total-magnitude takes two numbers and compares them
 *  using their abstract representation rather than their numerical value
 *  with their sign ignored and assumed to be 0.
 *  The result is identical to that obtained by using compare-total
 *  on two operands which are the copy-abs copies of the operands.
 *  Implements the 'compare-total-magnitude' function in the specification.
 *  (p. 43)
 *  Flags: NONE.
 */
int compareTotalMagnitude(D)(D left, D right) if (isDecimal!D)
{
	return compareTotal(left.copyAbs, right.copyAbs);
}

/**
 *  Returns true if the numbers have the same exponent.
 *  If either operand is NaN or Infinity, returns true if and only if
 *  both operands are NaN or Infinity, respectively.
 *  Flags: NONE
 *
 * Standards: Implements the 'same-quantum' function in the specification. (p. 48)
 */
public bool sameQuantum(D)(in D left, in D right) if (isDecimal!D)
{
	if (left.isNaN || right.isNaN) {
		return left.isNaN && right.isNaN;
	}
	if (left.isInfinite || right.isInfinite) {
		return left.isInfinite && right.isInfinite;
	}
	return left.expo == right.expo;
}

unittest
{	// sameQuantum
	static struct S { TD left; TD right; bool expect; }
	S[] s =
	[
		{ "2.17", "0.001", false },
		{ "2.17", "0.01 ", true },
		{ "2.17", "0.1  ", false },
	];
	auto f = FunctionTest!(S,bool)("sameQuant");
	foreach (t; s) f.test(t, sameQuantum(t.left, t.right));
    writefln(f.report);
}

/**
 *  Returns the maximum of the two operands (or NaN).
 *
 *  If either is a signaling NaN, or both are quiet NaNs, a NaN is returned.
 *  Otherwise, any finite or infinite number is larger than a NaN.
 *
 *  If they are not numerically equal, the larger is returned.
 *  If they are numerically equal:
 *  1. If the signs differ, the one with the positive sign is returned.
 *  2. If they are positive, the one with the larger exponent is returned.
 *  3. If they are negative, the one with the smaller exponent is returned.
 *  4. Otherwise, they are indistinguishable; the first is returned.
 *  The returned number will be rounded to the current context.
 *  Implements the 'max' function in the specification. (p. 32)
 *  Flags: INVALID_OPERATION, ROUNDED.
 *
 */
public D max(D)(in D left, in D right,	Context context = D.context)
	if (isDecimal!D)
{
	// if both are NaNs or either is an sNan, return NaN.
	if (left.isNaN && right.isNaN || left.isSignaling || right.isSignaling) {
		contextFlags.set(INVALID_OPERATION);
		return D.nan;
	}
	// if both are infinite, return the one with a positive sign
	if (left.isInfinite && right.isInfinite) {
		return left.isNegative ? right.dup : left.dup;
	}

	// result will be a finite number or infinity
	// use left as default value
	D result = left.dup;

	// if one op is a quiet NaN return the other
	if (left.isQuiet || right.isQuiet) {
		if (left.isQuiet) result = right;
	}
	// if the signs differ, return the unsigned operand
	else if (left.sign != right.sign) {
		if (left.sign) result = right;
	}
	else {
		// if not numerically equal, return the larger
		int comp = compare!D(left, right, context);
		if (comp != 0) {
			if (comp < 0) result = right;
		}
		// if they have the same exponent they are identical, return either
		else if (left.expo == right.expo) {
			// no assignment -- use default value
		}
		// if they are non-negative, return the one with larger exponent.
		else if (left.sign == 0) {
			if (left.expo < right.expo) result = right;
		}
		else {
			// they are negative; return the one with smaller exponent.
			if (left.expo > right.expo) result = right;
		}
	}
	// result must be rounded
	return roundToPrecision(result, context);
}

unittest
{	// max -- depends on context
	static struct S { TD left; TD right; TD expect; }
	S[] s =
	[
		{   3, 2, 3 },
		{ -10, 3, 3 },
	];
	auto f = FunctionTest!(S,TD)("max");
	foreach (t; s) f.test(t, max(t.left, t.right));
    writefln(f.report);
}

/// Returns the larger of the two operands (or NaN). Returns the same result
/// as the 'max' function if the signs of the operands are ignored.
/// Implements the 'max-magnitude' function in the specification. (p. 32)
/// Flags: NONE.
public D maxMagnitude(D)(in D left, in D right,
		Context context = D.context) if (isDecimal!D)
{
	if (left.copyAbs > right.copyAbs)
	{
		return roundToPrecision(left, context);
	}
	else
	{
		return roundToPrecision(right, context);
	}
}

unittest
{	// maxMagnitude -- depends on context
	static struct S { TD left; TD right; TD expect; }
	S[] s =
	[
		{   -1, -2, -2 },
		{    1, -2, -2 },
		{    1,  2,  2 },
		{   -1,  2,  2 },
	];
	auto f = FunctionTest!(S,TD)("maxMag");
	foreach (t; s) f.test(t, maxMagnitude(t.left, t.right));
    writefln(f.report);
}

/// Returns the minimum of the two operands (or NaN).
/// If either is a signaling NaN, or both are quiet NaNs, a NaN is returned.
/// Otherwise, Any (finite or infinite) number is smaller than a NaN.
/// If they are not numerically equal, the smaller is returned.
/// If they are numerically equal:
/// 1. If the signs differ, the one with the negative sign is returned.
/// 2. If they are negative, the one with the larger exponent is returned.
/// 3. If they are positive, the one with the smaller exponent is returned.
/// 4. Otherwise, they are indistinguishable; the first is returned.
/// Implements the 'min' function in the specification. (p. 32-33)
/// Flags: INVALID_OPERATION, ROUNDED.
public D min(D)(in D left, in D right,
		Context context = D.context) if (isDecimal!D)
{
	// if both are NaNs or either is an sNan, return NaN.
	if (left.isNaN && right.isNaN || left.isSignaling || right.isSignaling) {
		contextFlags.set(INVALID_OPERATION);
		return D.nan;
	}
	// if both are infinite, return the one with a negative sign
	if (left.isInfinite && right.isInfinite) {
		return left.isNegative ? left.dup : right.dup;
	}
	// result will be a finite number or infinity
	D min;

	// if one op is a quiet NaN return the other
	if (left.isQuiet || right.isQuiet) {
		min = left.isQuiet? right : left;
	}
    // if the signs differ, return the negative operand
	else if (left.sign != right.sign) {
		min = left.sign ? left : right;
	}
	// if not numerically equal, return the lesser
	else {
		int comp = compare(left, right, context);
		if (comp != 0) {
			min = comp > 0 ? right : left;
		}
		// if they have the same exponent they are identical, return either
		else if (left.expo == right.expo) {
			min = left;
		}
		// if they are non-negative, return the one with smaller exponent.
		else if (left.sign == 0) {
			min = left.expo > right.expo ? right : left;
		}
		// else they are negative; return the one with larger exponent.
		else {
			min = left.expo > right.expo ? right : left;
		}
	}
	// min must be rounded
	return roundToPrecision(min, context);
}

unittest
{	// min -- depends on context
	static struct S { TD left; TD right; TD expect; }
	S[] s =
	[
		{   3, 2,   2 },
		{ -10, 3, -10 },
	];
	auto f = FunctionTest!(S,TD)("min");
	foreach (t; s) f.test(t, min(t.left, t.right));
    writefln(f.report);
}

/// Returns the smaller of the two operands (or NaN). Returns the same result
/// as the 'min' function if the signs of the operands are ignored.
/// Implements the 'min-magnitude' function in the specification. (p. 33)
/// Flags: INVALID_OPERATION, ROUNDED.
public D minMagnitude(D)(in D left, in D right,
		Context context = D.context) if (isDecimal!D)
{
	if (left.copyAbs < right.copyAbs)
	{
		return roundToPrecision(left, context);
	}
	else
	{
		return roundToPrecision(right, context);
	}
/*	// one of each
	if (left.copyAbs > right.copyAbs) {
		return roundToPrecision(right, context);
	}
	return roundToPrecision(left, context);*/
}

unittest
{	// minMagnitude -- depends on context
	static struct S { TD left; TD right; TD expect; }
	S[] s =
	[
		{   -1, -2, -1 },
		{    1, -2,  1 },
		{    1,  2,  1 },
		{   -1,  2, -1 },
	];
	auto f = FunctionTest!(S,TD)("minMag");
	foreach (t; s) f.test(t, minMagnitude(t.left, t.right));
    writefln(f.report);
}

/// Returns a number with a coefficient of 1 and
/// the same exponent as the argument.
/// Flags: NONE.
public T quantum(T)(in T x)  {
	return T(1, x.expo);
}

unittest
{	// quantum
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "23.14E-12", "1E-14" },
	];
	auto f = FunctionTest!(S,TD)("quantum");
	foreach (t; s) f.test(t, quantum(t.x));
    writefln(f.report);
}

//--------------------------------
// decimal shift and rotate
//--------------------------------

/// Shifts the first operand by the specified number of DECIMAL digits.
/// (NOT BINARY digits!) Positive values of the second operand shift the
/// first operand left (multiplying by tens). Negative values shift right
/// (dividing by tens). If the number is NaN, or if the shift value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'shift' function in the specification. (p. 49)
public T shift(T, U=T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.expo != 0) return invalidOperand(y);
	if (y.coff > context.precision ||
		y.coff < -context.precision) return invalidOperand(y);
	int n = y.coff.toInt;
	if (y.sign) n = -n;
	return shift(x, n, context);
}

/// Shifts the first operand by the specified number of DECIMAL digits.
/// (NOT BINARY digits!) Positive values of the second operand shift the
/// first operand left (multiplying by tens). Negative values shift right
/// (dividing by tens). If the first operand is NaN, or if the shift value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'shift' function in the specification. (p. 49)
public T shift(T, U:int)(in T arg, U n,
		Context context = T.context) if (isDecimal!T)
{

	// check for NaN
	if (arg.isNaN) return invalidOperand(arg);

	// shift by zero returns the argument
	if (n == 0) return arg.dup;

	// shift of an infinite number returns the argument
	if (arg.isInfinite) return arg.dup;

	int precision = context.precision;

	// shifting by more than precision is invalid.
	if (n < -precision || n > precision)
	{
		return invalidOperation!T;
	}

	auto copy = arg.dup;

	if (n > 0)
	{
		// shift left
		copy.coff = copy.coff * pow10b(n);
		copy.digits = countDigits(copy.coff);
		if (copy.digits > context.precision)
		{
			copy.coff = copy.coff % pow10b(precision);
			copy.digits = precision;
		}
	}
	else
	{
		// shift right
		copy.coff = copy.coff / pow10b(-n);
		copy.digits = countDigits(copy.coff);
	}
	return copy;
}

unittest
{	// shift  -- depends on context
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
// 9 digit data
/*		{ 34, 8, 400000000 },
		{ 12, 9, 0 },
		{ 123456789, -2, 1234567 },
		{ 123456789,  0, 123456789 },
		{ 123456789,  2, 345678900 },*/
		{ 34, 8, 3400000000 },
		{ 12, 9, 12000000000 },
		{ 123456789, -2, 1234567 },
		{ 123456789,  0, 123456789 },
		{ 123456789,  2, 12345678900 },
	];
	auto f = FunctionTest!(S,TD)("shift");
	foreach (t; s) f.test(t, shift(t.x, t.y));
    writefln(f.report);
}

/// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T, U=T)(in T x, in U y,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	if (y.isNaN) return invalidOperand(y);
	if (y.expo != 0) return invalidOperand(y);
	if (y.coff > context.precision ||
		y.coff < -context.precision) return invalidOperand(y);
	int n = y.coff.toInt;
	if (y.sign) n = -n;
	return rotate(x, n, context);
}

// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T, U:int)(in T arg, U n,
		Context context = T.context) if (isDecimal!T)
{

	// check for NaN
	if (arg.isNaN) return invalidOperand(arg);

	// shift by zero returns the argument
	if (n == 0) return arg.dup;

	// shift of an infinite number returns the argument
	if (arg.isInfinite) return arg.dup;

	int precision = context.precision;

	// shifting by more than precision is invalid.
	if (n < -precision || n > precision) {
		return invalidOperation!T;
	}

	auto copy = arg.dup;

	// TODO: test this
	// if coefficient is longer than the precision truncate leading digits
	if (copy.digits > precision)
	{
		copy.coff = copy.coff % pow10b(precision);
	}

	if (n > 0) {
		// rotate left
		copy.coff = copy.coff * pow10b(n);
		copy.digits = countDigits(copy.coff);
		if (copy.digits > context.precision) {
			BigInt divisor = pow10b(precision);
			BigInt mod, div;
			divMod(copy.coff, divisor, div, mod);
			copy.coff = div + mod;
			copy.digits = countDigits(copy.coff);
		}
	}
	else {
		// rotate right
		n = -n;
		BigInt divisor = pow10b(n);
		BigInt mod, div;
		divMod(copy.coff, divisor, div, mod);
		copy.coff = mod * pow10b(precision - n) + div;
		copy.digits = countDigits(copy.coff);
	}
	return copy;
}

unittest
{	// rotate -- depends on context
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
/*		{ 34, 8, 400000003 },
		{ 12, 9, 12 },
		{ 123456789,   2, 345678912 },
		{ 123456789,   0, 123456789 },
		{ 123456789,  -2, 891234567 },
		{ 123456789,  -5, 567891234 },
		{ 1234567890, -2, 902345678 },
		{ 912345678900000, 2, 890000067 },
		{ 123000456789,  2, 45678900 },
		{ 123000456789, -2, 890004567 },*/
		{ 34, 15, 4000000000000003 },
		{ 12, 16, 12 },
		{ 1234567890123456,   2, 3456789012345612 },
		{ 1234567890123456,   0, 1234567890123456 },
		{ 1234567890123456,  -2, 5612345678901234 },
		{ 1234567890123456,  -5, 2345612345678901 },
		{ 1234567890,        -2, 9000000012345678 },
		{ 8912345678900000,   2, 1234567890000089 },
		{ 1230004567891234,   2, 3000456789123412 },
		{ 123000456789,      -2, 8900001230004567 },
	];
	auto f = FunctionTest!(S,TD)("rotate");
	foreach (t; s) f.test(t, rotate(t.x, t.y));
    writefln(f.report);
}

//------------------------------------------
// binary arithmetic operations
//------------------------------------------

/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION, OVERFLOW.
public T add(T)(in T left, in T right,
		Context context = T.context, bool setFlags = true) if (isDecimal!T)
{
	if (left.isNaN || right.isNaN) return invalidOperand(left, right);

	// if both operands are infinite...
	if (left.isInfinite && right.isInfinite) {
		// if the signs differ return NaN and set invalid operation flag
		if (left.sign != right.sign) {
			return invalidOperation!T;
		}
		// both infinite with same sign, return the first
		return left.dup;
	}
	// if only the first is infinite, return it
	if (left.isInfinite) {
		return left.dup;
	}
	// if only the second is infinite, return it
	if (right.isInfinite) {
		return right.dup;
	}

	T sum = T.zero;
	// add(0, 0)
	if (left.isZero && right.isZero) {
		sum = left;
		// the exponent is the smaller of the two exponents
		sum.expo = std.algorithm.min(left.expo, right.expo);
		// the sign is the logical AND of the two signs
		sum.sign = left.sign && right.sign;
		return sum;
	}
	// add(0,f)
	if (left.isZero) return right.dup;
	// add(f,0)
	if (right.isZero) return left.dup;

	// sum is finite and not zero.
	auto lf = left.dup;
	auto rt = right.dup;
	// align the operands
	alignOps(lf, rt);
	// if the operands have the same sign add the aligned coefficients
	if (lf.sign == rt.sign) {
		sum.coff = lf.coff + rt.coff;
		sum.sign = lf.sign;
	}
	// otherwise subtract the lesser from the greater
	else {
		if (lf.coff >= rt.coff) {
			sum.coff = lf.coff - rt.coff;
			sum.sign = lf.sign;
		}
		else {
			sum.coff = rt.coff - lf.coff;
			sum.sign = rt.sign;
		}
	}
	sum.digits = countDigits(sum.coff);
	sum.expo = lf.expo;
	// round the result
	return roundToPrecision(sum, context, setFlags);
}


/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION, OVERFLOW.
public T add(T, U)(in T left, in U right,
		Context context = T.context, bool setFlags = true)
		if (isDecimal!T && isConvertible!U)
{
	return add(left, T(right), context, setFlags);
}

unittest
{	// add -- depends on context
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
		{ "12", "7.00", "19.00" },
		{ "1E+2", "1E+4", "1.01E+4" },
		{ "1234567890123456789", "5432109876543210987", "6.666677766666668E+18" },
		{ "1.3", "-2.07", "-0.77" },
//		{ "1.3", "2.07", "-0.77" }, // uncomment to test failure
	];
	auto f = FunctionTest!(S,TD)("add");
	foreach (t; s) f.test(t, add(t.x, t.y));
    writefln(f.report);
}

/// Subtracts the second operand from the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'subtract' function in the specification. (p. 26)
public D sub(D, U:D) (in D left, in U right,
		Context context = D.context, bool setFlags = true)
	if (isDecimal!D)
{
	return add(left, right.copyNegate, context, setFlags);
}	 // end sub(left, right)


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
{	// sub -- depends on context
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
		{ "1.3", "1.07", "0.23" },
		{ "1.3", "1.30", "0.00" },
		{ "1.3", "2.07", "-0.77" },
//		{ "1.3", "2.07", "0.77" },	// uncomment to test failure
	];
	auto f = FunctionTest!(S,TD)("sub");
	foreach (t; s) f.test(t, sub(t.x, t.y));
    writefln(f.report);
}

///
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
		z.expo = std.algorithm.min(T.maxExpo, x.expo + y.expo);
		z.sign = x.sign ^ y.sign;
		return (z);
	}

	// at this point the product is a finite, non-zero number
	T product = T.zero.dup;
	product.coff = x.coff * y.coff;
	product.expo = std.algorithm.min(T.maxExpo, x.expo + y.expo);
	product.sign = x.sign ^ y.sign;
	product.digits = countDigits(product.coff);

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
		z.expo = x.expo;
		z.sign = x.sign ^ (n < 0);
		return (z);
	}

	// at this point the product is a finite, non-zero number
	T product = T.zero;
	product.coff = x.coff * n;
	product.expo = x.expo;
	product.sign = x.sign ^ (n < 0);
	product.digits = countDigits(product.coff);
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

unittest
{	// mul -- depends on context
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
		{ "1.20", "3", "3.60" },
//		{ "1.20", "3", "3.61" },	// uncomment to test failure
		{ "7", "3", "21" },
		{ "-7000", "3", "-21000" },
		{ "Infinity", "3", "Infinity" },
	];
	auto f = FunctionTest!(S,TD)("mul");
	foreach (t; s) f.test(t, mul(t.x, t.y));
    writefln(f.report);
}

/// Squares the argument and returns the result.
/// The result may be rounded and context flags may be set.
public T sqr(T)(in T arg, Context context = T.context) if (isDecimal!T)
{
	// if operand is invalid, return NaN
	if (arg.isNaN) {
		return invalidOperand(arg);
	}
	// if operand is infinite, return infinity
	if (arg.isInfinite) {
		return T.infinity;
	}
	// if operand is zero, return zero
	if (arg.isZero) {
		return T.zero;
	}

	// product is non-zero
	T copy = arg.copy;
	copy.coff = copy.coff * copy.coff;
// TODO : why does this fail ("not an lvalue")
//	copy.expo *= 2; //copy.expo * 2;
	copy.expo = 2 * copy.expo;
	copy.sign = false;
	copy.digits = countDigits(copy.coff);
	return roundToPrecision(copy, context);
}

unittest
{	// sqr -- depends on context
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "-Inf", "Inf" },
		{ "101.5", "10302.25" },
		{ "-101.5", "10302.25" },
/*		{ "-1.23456789012E+23", "1.52415788E+46" },
		{ "0.8535533905932737622000", "0.728553391" },*/
		{ "-1.23456789012E+23", "1.524157875315348E+46" },
		{ "0.8535533905932737622000", "0.7285533905932738" },
	];
	auto f = FunctionTest!(S,TD)("sqr");
	foreach (t; s) f.test(t, sqr(t.x));
    writefln(f.report);
}

//TODO: need to test high-precision numbers. Requires
// a test with a high-precision context.
unittest
{	// sqr -- depends on context
}


/// Multiplies the first two operands and adds the third operand to the result.
/// The result of the multiplication is not rounded prior to addition.
/// The result may be rounded and context flags may be set.
/// Implements the 'fused-multiply-add' function in the specification. (p. 30)
public T fma(T)(in T x, in T y, in T z,
		Context context = T.context) if (isDecimal!T)
{
	T xy = mul(x, y, Context(context.precision, T.maxExpo, ROUND_NONE));
	return add(xy, z, context);
}

unittest
{	// fma -- depends on context
	static struct S { TD x; TD y; TD z; TD expect; }
	S[] s =
	[
		{ 3,  5, 7, 22 },
		{ 3, -5, 7, -8 },
/*		{ "888565290", "1557.96930", "-86087.7578", "1.38435736E+12" },
		{ 888565290, 1557.96930, -86087.7578, 1.38435736E+12 },*/
		{ "888565290", "1557.96930", "-86087.7578", "1384357356777.839" },
		{ 888565290, 1557.96930, -86087.7578, "1384357356777.839" },
	];
	auto f = FunctionTest!(S,TD)("fma");
	foreach (t; s) f.test(t, fma(t.x, t.y, t.z));
    writefln(f.report);
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
	auto dvnd = x.dup;
	auto dvsr = y.dup;
	auto quo = T.zero;

	int diff = dvnd.expo - dvsr.expo;
	if (diff > 0) {
		dvnd.coff = shiftBig(dvnd.coff, diff);
		dvnd.expo = dvnd.expo - diff;
		dvnd.digits = dvnd.digits + diff;
	}
	int shift = 4 + context.precision + cast(int)dvsr.digits - cast(int)dvnd.digits;
	if (shift > 0) {
		dvnd.coff = shiftBig(dvnd.coff, shift);
		dvnd.expo = dvnd.expo - shift;
		dvnd.digits = dvnd.digits + shift;
	}
	// the divisor may have become zero. Check again.
	if (dvsr.isZero)
		return divisionByZero(x, y);
	quo.coff = dvnd.coff / dvsr.coff;
	quo.expo = dvnd.expo - dvsr.expo;
	quo.sign = dvnd.sign ^ dvsr.sign;
	quo.digits = countDigits(quo.coff);
	quo = roundToPrecision(quo, context);
	// TODO: what's up with this? revisit
	quo = reduceToIdeal(quo, diff);
	return quo;
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

	auto dvnd = x.dup;
	auto quo = T.zero;

	int diff = dvnd.expo;
	if (diff > 0) {
		dvnd.coff = shiftBig(dvnd.coff, diff);
		dvnd.expo = dvnd.expo - diff;
		dvnd.digits = dvnd.digits + diff;
	}
	int shift = 4 + context.precision + countDigits(cast(uint)n) - dvnd.digits;
	if (shift > 0) {
		dvnd.coff = shiftBig(dvnd.coff, shift);
		dvnd.expo = dvnd.expo - shift;
		dvnd.digits = dvnd.digits + shift;
	}

	quo.coff = dvnd.coff / n;
	quo.expo = dvnd.expo; // - n.expo;
	quo.sign = dvnd.sign ^ (n < 0);
	quo.digits = countDigits(quo.coff);
	quo = roundToPrecision(quo, context);
	quo = reduceToIdeal(quo, diff);
	return quo;
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
	int zeros = trailingZeros(x.coff, x.digits);

	int idealshift = ideal - x.expo;
	int	canshift = idealshift > zeros ? zeros : idealshift;
	x.coff = shiftBig(x.coff, -canshift);
	x.expo = x.expo + canshift;

	if (x.coff == 0) {
		x = T.zero;
	}
	x.digits = countDigits(x.coff);
	return x;
}
unittest
{	// div  -- depends on context
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
/*		{ 1, 3, "0.333333333" },
		{ 2, 3, "0.666666667" },*/
		{ 1, 3, "0.3333333333333333" },
		{ 2, 3, "0.6666666666666667" },
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
	auto f = FunctionTest!(S,TD)("div");
	foreach (t; s) f.test(t, div(t.x, t.y));
    writefln(f.report);
}

/*public T integerPart(T)(T x) if (isDecimal!T)
{
	int exp = x.expo;
	int digits = x.digits;
	if (exp >= 0) return x;
	exp = -exp;
	if (exp >= digits) return T.zero(x.sign);	// TODO: review conditions for -0


}*/

// TODO: (behavior) Does this implement the actual spec operation?
/// Divides the first operand by the second and returns the integer portion
/// of the quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide-integer' function in the specification. (p. 30)
public T divideInteger(T)(in T x, in T y)  {
	T quo;
	remQuo(x,y,quo);
	return quo;
}

// TODO: (behavior) Does this implement the actual spec operation?
/// Divides the first operand by the second and returns the integer portion
/// of the quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide-integer' function in the specification. (p. 30)
public T remQuo(T)(in T x, in T y, out T quo) if (isDecimal!T)
{
	// check for NaN and division by zero
	if (x.isNaN || y.isNaN) return invalidOperand(x, y);
	if (y.isZero) return divisionByZero(x, y);

	auto dividend = x.dup;
	auto dvsr  = y.dup;
	quo.sign = dividend.sign ^ dvsr.sign;
	if (x.isZero) {
		quo = T.zero(quo.sign);
		return quo.dup;
	}
//    auto rem = T.zero;

	// align the operands
	alignOps(dividend, dvsr);

	BigInt div, mod;
	divMod(dividend.coff, dvsr.coff, div, mod);
	quo = T(div);
	T rem = T(mod);
	// number of digits cannot exceed precision
	int digits = countDigits(quo.coff);
	if (digits > T.precision) {
		rem = T.nan;
		return invalidOperation!T;
	}
	quo.digits = digits;
	rem.digits = countDigits(rem.coff);
	rem.sign = quo.sign;
	return rem;
}

unittest
{	// divideInteger
	static struct S { TD x; TD y; TD expect; }
	S[] s =
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
	auto f = FunctionTest!(S,TD)("divInt");
	foreach (t; s) f.test(t, divideInteger(t.x, t.y));
    writefln(f.report);
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

	T quo = divideInteger!T(x, y,);
	T rem = x - (y * quo);
	return rem;
}

unittest
{	// remainder
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
		{ 2.1, 3,  2.1 },
		{  10, 3,  1 },
//		{ -10, 3, -1 },	// fails
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
	auto f = FunctionTest!(S,TD)("remainder");
	foreach (t; s) f.test(t, remainder(t.x, t.y));
    writefln(f.report);
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
	T quo = x/y;
	// TODO: (behavior) roundToIntegralValue?
	T rem = x - y * roundToIntegralExact(quo);
	return rem;
}

unittest
{	// remainderNear
	static struct S { TD x; TD y; TD expect; }
	S[] s =
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
	auto f = FunctionTest!(S,TD)("remNear");
	foreach (t; s) f.test(t, remainderNear(t.x, t.y));
    writefln(f.report);
}

//--------------------------------
// rounding routines
//--------------------------------

/// Returns the number which is equal in value and sign
/// to the first operand with the exponent of the second operand.
/// The returned value is rounded to the current precision.
/// This operation may set the invalid-operation flag.
/// Implements the 'quantize' function in the specification. (p. 36-37)
public D quantize(D)(in D left, in D right,
		Context context = D.context) if (isDecimal!D)
{
	if (left.isNaN || right.isNaN) return invalidOperand(left, right);

	// if one operand is infinite and the other is not...
	if (left.isInfinite != right.isInfinite) {
		return invalidOperation!D;
	}
	// if both arguments are infinite
	if (left.isInfinite && right.isInfinite) {
		return left.dup;
	}
	D result = left.dup;
	int diff = left.expo - right.expo;

	if (diff == 0) {
		return result;
	}

	// TODO: (behavior) this shift can cause integer overflow for fixed size decimals
	if (diff > 0) {
		result.coff = shiftBig(result.coff, diff/*, precision*/);
		result.digits = result.digits + diff;
		result.expo = right.expo;
		if (result.digits > D.precision) {
			result = D.nan;
		}
		return result;
	}
	else {
		int precision = (-diff > left.digits) ? 0 : left.digits + diff;
		result = roundToPrecision(result, precision, context.mode);
		result.expo = right.expo;
		if (result.isZero && left.isSigned) {
			result.sign = true;
		}
		return result;
	}
}

// TODO: need to handle NaNs
unittest
{	// quantize
	static struct S { TD x; TD y; TD expect; }
	S[] s =
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
//		{ "+35236450.6", "1E-2", "NaN" },
		{ "+35236450.6", "1E-2", "35236450.60" },

//		{ "-35236450.6", "1E-2", "NaN" },
		{ "-35236450.6", "1E-2", "-35236450.60" },
		{ "217", "1E-1", "217.0" },
		{ "217", "1E+0", "217" },
		{ "217", "1E+1", "2.2E+2" },
		{ "217", "1E+2", "2E+2" },
	];
	auto f = FunctionTest!(S,TD)("quantize");
	foreach (t; s) f.test(t, quantize(t.x, t.y));
    writefln(f.report);
}

/// Returns the nearest integer value to the argument.
/// Context flags may be set.
/// Implements the 'round-to-integral-exact' function
/// in the specification. (p. 39)
public D roundToIntegralExact(D)(in D num,
		in Round mode = HALF_EVEN) if (isDecimal!D)
{
	if (num.isSignaling) return invalidOperation!D;
	if (num.isSpecial) return num;
	if (num.expo >= 0) return num;

	// TODO: (behavior) need to prevent precision overrides
	int precision = num.digits + num.expo;
	return roundToPrecision(num, precision, mode); //, D.maxExpo);
}

unittest
{	// roundToIntegralExact
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ 2.1, 2 },
		{ 0.7, 1 },
		{ 100, 100 },
		{ "101.5",  102 },
		{ "-101.5", -102 },
		{ "10E+5", "1.0E+6" },
		{ "7.89E+77", "7.89E+77" },
		{ "-Inf", "-Inf" },
	];
	auto f = FunctionTest!(S,TD)("roundIntEx");
	foreach (t; s) f.test(t, roundToIntegralExact(t.x));
    writefln(f.report);
}

// TODO: (behavior) need to re-implement this so no flags are set.
/// The result may be rounded and context flags may be set.
/// Implements the 'round-to-integral-value' function
/// in the specification. (p. 39)
public T roundToIntegralValue(T)(in T arg) if (isDecimal!T)
{
	T result = arg.dup;
	if (result.isSignaling) return invalidOperation!T;
	if (result.isSpecial) return result;
	if (result.expo >= 0) return result;

	int precision = result.digits + result.expo;
	result = roundToPrecision(result, context);
	return result;
}

// TODO: implement
/*unittest
{	// roundToIntegralValue
	static struct S { TD x; TD expect; }
	S[] s =
	[
	];
	auto f = FunctionTest!(S,TD)("roundIntVal");
	foreach (t; s) f.test(t, roundToIntegralValue(t.x));
    writefln(f.report);
}*/

/// Aligns the two operands by raising the smaller exponent
/// to the value of the larger exponent, and adjusting the
/// coefficient so the value remains the same.
/// Both operands will have the same exponent on their return.
/// No flags are set and the result is not rounded.
private void alignOps(T, U)(ref T x, ref U y) if (isDecimal!T && isDecimal!U)
{
	int diff = x.expo - y.expo;
	if (diff > 0) {
		x.coff = shiftBig(x.coff, diff);
		x.expo = y.expo;
	}
	else if (diff < 0) {
		y.coff = shiftBig(y.coff, -diff);
		y.expo = x.expo;
	}
}

/// Aligns the two operands by raising the smaller exponent
/// to the value of the larger exponent, and adjusting the
/// coefficient so the value remains the same.
/// Both operands will have the same exponent on their return.
/// No flags are set and the result is not rounded.
private void alignOps(T, U:long)(ref T x, U n) if (isDecimal!T)
{
	int diff = x.expo;
	if (x.expo == 0) return;

	if (x.expo > 0) {
		x.coff = shiftBig(x.coff, x.expo);
		x.expo = 0;
	}
	else if (diff < 0) {
		y.coff = shiftBig(y.coff, -diff);
		y.expo = x.expo;
	}
}

unittest { // alignOps
 	write("-- alignOps.........");
	TD arg1, arg2;
	arg1 = TD("1.3E35");
	arg2 = TD("-17.4E29");
	alignOps(arg1, arg2);
	assertEqual(arg1.coff, 13000000);
	assertEqual(arg2.expo, 28);
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
	contextFlags.set(INVALID_OPERATION);
	return T.nan(payload);
}

unittest {	// invalidOperation
	TD arg, expect, actual;
	// TODO: (testing) Can't actually test payloads at this point.
	arg = TD("sNaN123");
	expect = TD("NaN123");
	actual = abs!TD(arg);
	assertTrue(actual.isQuiet);
	assertTrue(contextFlags.getFlags(INVALID_OPERATION));
	assertEqual(actual.toAbstract, expect.toAbstract);
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
	contextFlags.set(INVALID_OPERATION);
	// if the operand is a quiet NaN return it.
	if (x.isQuiet) return x.dup;
	// Otherwise change the signalling NaN to a quiet NaN.
	if (x.isSignaling) return T.nan(cast(ushort)x.coff);
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
	contextFlags.set(INVALID_OPERATION);
	// if either operand is signaling return a quiet NaN.
	// NOTE: sign is ignored.
	if (x.isSignaling) return T.nan(cast(ushort)x.coff);
	if (y.isSignaling) return T.nan(cast(ushort)y.coff);
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
	contextFlags.set(DIVISION_BY_ZERO);
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
	contextFlags.set(DIVISION_BY_ZERO);
	return T.infinity(dividend.sign ^ (divisor < 0));
}

unittest {
	write("divisionByZero......");
	writeln("test missing");
}

unittest
{
	writeln("==========================");
}

