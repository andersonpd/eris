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


/// Most of the arithmetic operations accept values for precision and
/// rounding modes, but these parameters are not generally available to
/// the ordinary user. They are included to allow algorithm designers to
/// carry out internal operations at a precision higher than the type
/// precision.

// TODO: (testing) ensure context flags are being set and cleared properly.

// TODO: (testing) opEquals unit test should include numerically equal testing.

// TODO: (testing) write some test cases for flag setting. test the add/sub/mul/div functions

// TODO: (behavior?) to/from real or double (float) values needs definition and implementation.

module eris.decimal.arithmetic;

import eris.integer.extended;
import std.string;
import std.traits : isIntegral;

import eris.decimal;
import eris.decimal.context;
import eris.decimal.rounding;

unittest {
	writeln("==========================");
	writeln("decimal arithmetic...begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertion;
}

alias xcompare = eris.integer.extended.xint.compare;

//--------------------------------
// classification functions
//--------------------------------

	/// Returns a string indicating the class and sign of the argument.
	/// Classes are: sNaN, NaN, Infinity, Zero, Normal, and Subnormal.
	/// The sign of any NaN values is ignored in the classification.
	/// The argument is not rounded and no flags are changed.
	/// Implements the 'class' function in the specification. (p. 42)
	public string classify(T)(T x) if (isDecimal!T)
	{
		if (x.isFinite) {
			if (x.isZero) 	 { return x.sign ? "-Zero" : "+Zero"; }
			if (x.isNormal)	 { return x.sign ? "-Normal" : "+Normal"; }
			if (x.isSubnormal) { return x.sign ? "-Subnormal" : "+Subnormal"; }
		}
		if (x.isInfinite)  { return x.sign ? "-Infinity" : "+Infinity"; }
		if (x.isSignaling) { return "sNaN"; }
		return "NaN";
	}

	unittest {	// classify
		write("-- classify.........");
		dec9 x;
		x = dec9("Inf");
		assertEqual(classify(x), "+Infinity");
		x = dec9("1E-10");
		assertEqual(classify(x), "+Normal");
		x = dec9("-0");
		assertEqual(classify(x), "-Zero");
		x = dec9("-0.1E-99");
		assertEqual(classify(x), "-Subnormal");
		x = dec9("NaN");
		assertEqual(classify(x), "NaN");
		x = dec9("sNaN");
		assertEqual(classify(x), "sNaN");
		writeln("passed");
	}

/+
//--------------------------------
// copy functions
//--------------------------------

/// Returns a copy of the operand.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy' function in the specification. (p. 43)
//@safe
public T copy(T)(in T x)  {
	return x.dup;
}

/// Returns a copy of the operand with a positive sign.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy-abs' function in the specification. (p. 44)
//@safe
public T copyAbs(T)(in T x)  {
	T copy = x.dup;
	copy.sign = false;
	return copy;
}

/// Returns a copy of the operand with the sign inverted.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy-negate' function in the specification. (p. 44)
//@safe
public T copyNegate(T)(in T x)  {
	T copy = x.dup;
	copy.sign = !x.sign;
	return copy;
}

/// Returns a copy of the first operand with the sign of the second operand.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy-sign' function in the specification. (p. 44)
//@safe
public T copySign(T)(in T x1, in T x2)  {
	T copy = x1.dup;
	copy.sign = x2.sign;
	return copy;
}

unittest {	// copy
	write("-- copy.............");
	dec9 arg, expect;
	arg = dec9("2.1");
	expect = dec9("2.1");
	assertZero(compareTotal(copy(arg),expect));
	arg = dec9("-1.00");
	expect = dec9("-1.00");
	assertZero(compareTotal(copy(arg),expect));
	// copyAbs
	arg = 2.1;
	expect = 2.1;
	assertZero(compareTotal(copyAbs(arg),expect));
	arg = dec9("-1.00");
	expect = dec9("1.00");
	assertZero(compareTotal(copyAbs(arg),expect));
	// copyNegate
	arg	= dec9("101.5");
	expect = dec9("-101.5");
	assertZero(compareTotal(copyNegate(arg),expect));
	// copySign
	dec9 arg1, arg2;
	arg1 = 1.50; arg2 = 7.33; expect = 1.50;
	assertZero(compareTotal(copySign(arg1, arg2),expect));
	arg2 = -7.33;
	expect = -1.50;
	assertZero(compareTotal(copySign(arg1, arg2),expect));
	writeln("passed");
}
+/

/// Returns the truncated base 10 logarithm of the argument.
/// "...The integer which is the exponent of the magnitude
/// of the most significant digit of the operand.
/// (As though the operand were truncated to a single digit
/// while maintaining the value of that digit and without
/// limiting the resulting exponent)".
/// May set the INVALID_OPERATION and DIVISION_BY_ZERO flags.
/// Implements the 'logb' function in the specification. (p. 47)
public int ilogb(T)(T x) if (isDecimal!T)
{
	if (operandIsInvalid!T(x)) {
		return 0;
	}
	if (x.isInfinite) {
		return int.max;
	}
	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return int.min;
	}
	return x.digits + x.exponent - 1;
}

/// Returns the truncated base 10 logarithm of the argument.
/// "...The integer which is the exponent of the magnitude
/// of the most significant digit of the operand.
/// (As though the operand were truncated to a single digit
/// while maintaining the value of that digit and without
/// limiting the resulting exponent)".
/// May set the INVALID_OPERATION and DIVISION_BY_ZERO flags.
/// Implements the 'logb' function in the specification. (p. 47)
public T logb(T)(T x) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);

	if (x.isInfinite) return T.infinity;

	if (x.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(true);
	}
	int expo = x.digits + x.exponent - 1;
	return T(expo);
}

unittest {
	write("-- logb.............");
	dec9 a;
	a = dec9("250");
	assertEqual(logb(a), 2L);
	a = "2.50";
	assertEqual(logb(a), 0);
	a = "0.03";
	assertEqual(logb(a), -2);
	a = 0;
	assertEqual(logb(a), dec9.infinity(true));
	writeln("passed");
}


/// If the first operand is infinite then that operand is returned,
/// otherwise the result is the first operand modified by
/// adding the value of the second operand to its exponent.
/// The second operand must be a finite integer with an exponent of zero.
/// The result may overflow or underflow.
/// Flags: INVALID_OPERATION, UNDERFLOW, OVERFLOW.
/// Implements the 'scaleb' function in the specification. (p. 48)
public T scaleb(T)(T x, T y) if (isDecimal!T)
{
	if (x.isNaN || y.isNaN) return invalidOperands(x,y);

	if (x.isInfinite) return x;

	if (y.isInfinite || y.exponent != 0) {
		return invalidOperand(y);
	}
	int scale = cast(int)y.coefficient.toInt;
	if (y.isSigned) {
		scale = -scale;
	}
	// TODO: (behavior) check for overflow/underflow (GDA "scaleb").
	x.exponent = x.exponent + scale;
	return x;
}

unittest {	// scaleb
	write("-- scaleb...........");
	dec9 expect, actual;
	auto x = dec9("7.50");
	auto y = dec9("-2");
	expect = dec9("0.0750");
	actual = scaleb(x, y);
	assertEqual(actual, expect);
	writeln("passed");
}

//--------------------------------
// reduce, absolute value, unary plus and minus functions
//--------------------------------

/// Returns the operand reduced to its simplest form.
/// It has the same semantics as the plus operation,
/// except that if the final result is finite it is
/// reduced to its simplest form, with all trailing
/// zeros removed and its sign preserved.
/// Implements the 'reduce' function in the specification. (p. 37)
/// "This operation was called 'normalize' prior to
/// version 1.68 of the specification." (p. 37)
/// Flags: INVALID_OPERATION
public T reduce(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	// TODO: (language) remove constness from x.
	// special cases
	if (operandIsInvalid(x)) return x.dup;

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
	return reduced;
}

// just a wrapper
public T normalize(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	return reduce(x, context);
}

unittest {	// reduce
	write("-- reduce...........");
	dec9 x;
	dec9 expect, actual;
	x = dec9("1.200");
	expect = dec9("1.2");
	actual = reduce(x);
	assertNotEqual(x.toString, expect.toString);
	assertStringEqual(actual, expect);
	x = dec9("12.34");
	expect = dec9("1.234");
	actual = reduce(x);
	assertStringEqual(actual, x);
	assertStringNotEqual(x, expect);
	assertStringNotEqual(actual, expect);
	writeln("passed");
}

/// Returns the absolute value of the argument.
/// This operation rounds the result and may set flags.
/// The result is equivalent to plus(x) for positive numbers
/// and to minus(x) for negative numbers.
/// To return the absolute value without rounding or setting flags
/// use the 'copyAbs' function.
/// Implements the 'abs' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION
public T abs(T)(T x,
		Context context = T.context) if (isDecimal!T)
{
	if (x.isNaN) return invalidOperand(x);
	return roundToPrecision(x.copyAbs, context);
}

unittest {	// abs
	write("-- abs..............");
	dec9 x;
	dec9 expect;
	x = dec9("-Inf");
	expect = dec9("Inf");
	assertEqual(abs(x), expect);
	x = 101.5;
	expect = 101.5;
	assertEqual(abs(x), expect);
	x = -101.5;
	assertEqual(abs(x), expect);
	writeln("passed");
}

/// Returns -1, 0, or 1
/// if the argument is negative, zero, or positive, respectively.
/// The sign of zero is ignored. Returns 0 for +0 or -0.
public int sgn(T)(T x) if (isDecimal!T)
{
	if (x.isZero) return 0;
	return x.isNegative ? -1 : 1;
}

unittest {	// sgn
	write("-- sgn..............");
	dec9 x;
	x = -123;
	assertEqual(sgn(x), -1);
	x = 2345;
	assertEqual(sgn(x), 1);
	x = dec9("0.0000");
	assertEqual(sgn(x), 0);
	x = dec9.infinity(true);
	assertEqual(sgn(x), -1);
	xint n = -5;
	assertEqual(sgn(n), -1);
	writeln("passed");
}

/// Returns -1, 0, or 1
/// if the argument is negative, zero, or positive, respectively.
public int sgn(T:xint)(T x) {
	if (x < 0) return -1;
	if (x > 0) return 1;
	return 0;
}

/// Returns a copy of the argument with same sign as the argument.
/// This operation rounds the result and may set flags.
/// The result is equivalent to add('0', arg).
/// To copy without rounding or setting flags use the 'copy' function.
/// Implements the 'plus' function in the specification. (p. 33)
/// Flags: INVALID_OPERATION
public T plus(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	if (operandIsInvalid!T(x)) return x.dup;
	return roundToPrecision(x, context);
}

/// Returns a copy of the argument with the opposite sign.
/// This operation rounds the argument and may set flags.
/// Result is equivalent to subtract('0', x).
/// To copy without rounding or setting flags use the 'copyNegate' function.
/// Implements the 'minus' function in the specification. (p. 37)
/// Flags: INVALID_OPERATION
public T minus(T)(in T x,
		Context context = T.context) if (isDecimal!T)
{
	if (operandIsInvalid!T(x)) return x.dup;
	return roundToPrecision(x.copyNegate, context);
}

unittest {	// plus
	write("-- plus, minus......");
	dec9 zero = dec9.zero;
	dec9 x, expect, actual;
	x = 1.3;
	expect = add(zero, x);
	actual = plus(x);
	assertEqual(actual, expect);
	x = -1.3;
	expect = add(zero, x);
	actual = plus(x);
	assertEqual(actual, expect);
	// minus
	x = 1.3;
	expect = sub(zero, x);
	actual = minus(x);
	assertEqual(actual, expect);
	x = -1.3;
	expect = sub(zero, x);
	actual = minus(x);
	assertEqual(actual, expect);
	writeln("passed");
}

//-----------------------------------
// next-plus, next-minus, next-toward
//-----------------------------------

/// Returns the smallest representable number that is larger than
/// the argument.
/// Implements the 'next-plus' function in the specification. (p. 34)
/// Flags: INVALID_OPERATION
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
			return T(0L, T.tinyExpo);
	}
	// (A)TODO: (behavior) must add the increment w/o setting flags
	T y = T(1L, adjustedExpo);
	T z = add(x, y, context);
	if (z > T.max) {
		z = T.infinity;
	}
	return z;
}

/// Returns the largest representable number that is smaller than
/// the argument.
/// Implements the 'next-minus' function in the specification. (p. 34)
/// Flags: INVALID_OPERATION
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
	y = sub!T(x, y, context);	// TODO: (behavior) are the flags set/not set correctly?
		if (y < T.max.copyNegate) {
		y = T.infinity.copyNegate;
	}
	return y;
}

/// Returns the representable number that is closest to the first operand
/// in the direction of the second operand.
/// Implements the 'next-toward' function in the specification. (p. 34-35)
/// Flags: INVALID_OPERATION
public T nextToward(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{

	T nan;
	if (operationIsInvalid(x, y, nan)) return nan;

	// compare them but don't round
	int comp = compare(x, y, context);
	if (comp < 0) return nextPlus(x, context);
	if (comp > 0) return nextMinus(x, context);
	return roundToPrecision(x.copySign(y), context);
}

unittest {
	write("-- next.............");
	dec9 arg, expect, actual;
	arg = 1;
	expect = dec9("1.00000001");
	actual = nextPlus(arg);
	assertEqual(actual, expect);
	verbose = false;
	dec9.verbose = false;
	arg = 10;
	expect = dec9("10.0000001");
	actual = nextPlus(arg);
	assertEqual(actual, expect);
	// nextMinus
	arg = 1;
	expect = 0.999999999;
	actual = nextMinus(arg);
	assertEqual(actual, expect);
	arg = -1.00000003;
	expect = -1.00000004;
	actual = nextMinus(arg);
	assertEqual(actual, expect);
	// nextToward
	dec9 arg1, arg2;
	arg1 = 1;
	arg2 = 2;
	expect = 1.00000001;
	actual = nextToward(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = -1.00000003;
	arg2 = 0;
	expect = -1.00000002;
	actual = nextToward(arg1, arg2);
	assertEqual(actual, expect);
	writeln("passed");
}

//--------------------------------
// comparison functions
//--------------------------------

// TODO: (behavior) compares to full precision, not context precision.
/// Compares two operands numerically to the current precision.
/// Returns -1, 0, or +1 if the second operand is, respectively,
/// less than, equal to, or greater than the first operand.
/// Implements the 'compare' function in the specification. (p. 27)
/// Flags: INVALID_OPERATION
public int compare(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{

	// any operation with a signaling NaN is invalid.
	// if both are signaling, return as if x > y.
	if (x.isSignaling || y.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
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

	// TODO: (testing) test compare at precision limits.
	// restrict operands to current precision
	if (x.digits > context.precision) {
		roundToPrecision(x, context);
	}
	if (y.digits > context.precision) {
		roundToPrecision(y, context);
	}

	// compare the magnitudes of the numbers
	T xr = x.reduce;
	T yr = y.reduce;
	int diff = (xr.exponent + xr.digits) - (yr.exponent + yr.digits);
	if (diff != 0) {
		if (!x.sign) {
			if (diff > 0) return 1;
			if (diff < 0) return -1;
		}
		else {
			if (diff > 0) return -1;
			if (diff < 0) return 1;
		}
	}
	// align the operands
 	T xx = x.dup;
	T yy = y.dup;
	alignOps!T(xx, yy);
	// They have the same exponent after alignment.
	// The only difference is in the coefficients.
    int comp = xcompare(xx.coefficient, yy.coefficient);
	return x.sign ? -comp : comp;
}

unittest {	// compare
	write("-- compare..........");
	dec9 x, y;
	x = dec9(2.1);
	y = dec9(3);
	assertEqual(compare(y, x), 1);
	y = -y;
	assertEqual(compare(x, y), 1);
	assertEqual(compare(y, x), -1);
	x = dec9(2.1);
	y = dec9(2.1);
	assertEqual(compare(x, y), 0);
	y = dec9(2.10);
	assertEqual(compare(x, y), 0);
	x = dec9.infinity;
	y = dec9.infinity(true);
	assertEqual(compare(x,y), 1);
	y = -y;
	assertEqual(compare(x,y), 0);
	y = 12;
	assertEqual(compare(x,y), 1);
	x = -x;
	assertEqual(compare(x,y), -1);
	// TODO: (testing) test rounded to precision comparison.
	writeln("passed");
}

/// Returns true if the operands are equal to the context precision.
/// Finite numbers are equal if they are numerically equal
/// to the context precision.
/// A NaN is not equal to any number, not even another NaN or itself.
/// Infinities are equal if they have the same sign.
/// Zeros are equal regardless of sign.
/// A decimal NaN is not equal to itself (this != this).
/// Flags: INVALID_OPERATION
public bool equals(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// any operation with a signaling NaN is invalid.
	if (x.isSignaling || y.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
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
	alignOps!T(rx, ry);
	return rx.coefficient == ry.coefficient;
}

unittest {	// equals
	write("-- equals...........");
	dec9 x, y;
	x = 123.4567;
	y = 123.4568;
	assertFalse(equals(x, y));
	y = 123.4567;
	assertTrue(equals(x, y));
	// test for equality to precision
	x = 123.45671234;
	y = 123.4567121;
	assertTrue(equals(x, y));
	x = "1000000E-8";
	y = "1E-2";
	assertTrue(equals(x, y));
	x = "+100000000E-08";
	y = "+1E+00";
	assertTrue(equals(x, y));
	x = "1.00000000";
	y = "1";
	assertTrue(equals(x, y));
	writeln("passed");
}

/// Returns true if the operands are equal to the specified precision. Special
/// values are handled as in the equals() function. This function allows
/// comparison at precision values other than the context precision.
public bool precisionEquals(T)(T x, T y, int precision) if (isDecimal!T)
{
	auto context = Context(precision);
	return (equals(x, y, context));
}

/// Returns true if the actual value equals the expected value to the specified precision.
/// Otherwise prints an error message and returns false.
public bool assertPrecisionEqual(T)(T actual, T expected, int precision,
		string file = __FILE__, int line = __LINE__ ) {
//	auto context = Context(precision);
	if (precisionEquals!T(expected, actual, precision))	{
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
	        	" expected \"", expected, "\"",
	        	" but found \"", actual, "\".");
		return false;
	}
}

/// Returns true if the actual value equals the expected value to the specified precision.
/// Otherwise prints an error message and returns false.
public bool assertPrecisionEqual(T)(T actual, string expected, int precision,
		string file = __FILE__, int line = __LINE__ ) {
	auto context = Context(precision, T.maxExpo, T.rounding);
	if (equals(T(expected), actual, context))	{
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
	        	" expected \"", expected, "\"",
	        	" but found \"", actual, "\".");
		return false;
	}
}


/// Compares the numeric values of two numbers. CompareSignal is identical to
/// compare except that quiet NaNs are treated as if they were signaling.
/// This operation may set the invalid-operation flag.
/// Implements the 'compare-signal' function in the specification. (p. 27)
/// Flags: INVALID_OPERATION
public int compareSignal(T) (T x, T y,
		Context context = T.context) if (isDecimal!T)
{

	// any operation with NaN is invalid.
	// if both are NaN, return as if x > y.
	if (x.isNaN || y.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return x.isNaN ? 1 : -1;
	}
	return (compare!T(x, y, context));
}

unittest {
	write("-- compareSignal....");
	dec9 x, y;
	int value;
	x = 0;
	y = 5;
	assertGreaterThan(y,x);
	contextFlags.resetFlags(INVALID_OPERATION);
	y = dec9.snan;
	value = compare(x, y);
	assertTrue(contextFlags.getFlags(INVALID_OPERATION));
	contextFlags.resetFlags(INVALID_OPERATION);
	y = dec9.nan;
	value = compare(x, y);
	assertFalse(contextFlags.getFlags(INVALID_OPERATION));
	contextFlags.setFlags(INVALID_OPERATION, false);
	y = dec9.nan;
	value = compareSignal(x, y);
	assertTrue(contextFlags.getFlags(INVALID_OPERATION));
	writeln("passed");
}


/// Numbers (representations which are not NaNs) are ordered such that
/// a larger numerical value is higher in the ordering.
/// If two representations have the same numerical value
/// then the exponent is taken into account;
/// larger (more positive) exponents are higher in the ordering.
/// Compares the operands using their abstract representation rather than
/// their numerical value.
/// Returns 0 if the numbers are equal and have the same representation.
/// Implements the 'compare-total' function in the specification. (p. 42-43)
/// Flags: NONE.
public int compareTotal(T)(T x, T y) if (isDecimal!T)
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
	alignOps!T(xx, yy);

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
/// and with their sign ignored and assumed to be 0.
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
	write("-- compareTotal.....");
	dec9 x, y;
	int result;
	x = dec9("12.30");
	y = dec9("12.3");
	result = compareTotal(x, y);
	assertEqual(result, -1);
	x = dec9("12.30");
	y = dec9("12.30");
	result = compareTotal(x, y);
	assertEqual(result, 0);
	x = dec9("12.3");
	y = dec9("12.300");
	result = compareTotal(x, y);
	assertEqual(result, 1);
	writeln("passed");
}

/// Returns true if the numbers have the same exponent.
/// If either operand is NaN or Infinity, returns true if and only if
/// both operands are NaN or Infinity, respectively.
/// No context flags are set.
/// Implements the 'same-quantum' function in the specification. (p. 48)
public bool sameQuantum(T)(T x, T y) if (isDecimal!T)
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
	write("-- sameQuantum......");
	dec9 x, y;
	x = 2.17;
	y = 0.001;
	assertFalse(sameQuantum(x, y));
	y = 0.01;
	assertTrue(sameQuantum(x, y));
	y = 0.1;
	assertFalse(sameQuantum(x, y));
	writeln("passed");
}

/// Returns the maximum of the two operands (or NaN).
/// If either is a signaling NaN, or both are quiet NaNs, a NaN is returned.
/// Otherwise, Any finite or infinite number is larger than a NaN.
/// If they are not numerically equal, the larger is returned.
/// If they are numerically equal:
/// 1) If the signs differ, the one with the positive sign is returned.
/// 2) If they are positive, the one with the larger exponent is returned.
/// 3) If they are negative, the one with the smaller exponent is returned.
/// 4) Otherwise, they are indistinguishable; the first is returned.
/// The returned number will be rounded to the current context.
/// Implements the 'max' function in the specification. (p. 32)
/// Flags: INVALID_OPERATION, ROUNDED.
public T max(T)(T x, T y,
		Context context = T.context) if (isDecimal!T)
{
	// if both are NaNs or either is an sNan, return NaN.
	if (x.isNaN && y.isNaN || x.isSignaling || y.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
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
	write("-- max..............");
	dec9 x, y;
	x = 3; y = 2;
	assertEqual(max(x, y), x);
	x = -10; y = 3;
	assertEqual(max(x, y), y);
	writeln("passed");
}

/// Returns the larger of the two operands (or NaN). Returns the same result
/// as the 'max' function if the signs of the operands are ignored.
/// Implements the 'max-magnitude' function in the specification. (p. 32)
/// Flags: NONE.
public T maxMagnitude(T)(T x, T y,
		Context context = T.context) if (isDecimal!T)
{
// FIXTHIS: special values...
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

unittest {	// max
	write("-- maxMagnitude.....");
	dec9 x, y;
	x = -1; y = -2;
	assertEqual(maxMagnitude(x, y), y);
	x =  1; y = -2;
	assertEqual(maxMagnitude(x, y), y);
	x =  1; y =  2;
	assertEqual(maxMagnitude(x, y), y);
	x = -1; y =  2;
	assertEqual(maxMagnitude(x, y), y);
	writeln("passed");
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
/// Flags: INVALID OPERATION, ROUNDED.
public T min(T)(T x, T y,
		Context context = T.context) if (isDecimal!T)
{
	// if both are NaNs or either is an sNan, return NaN.
	if (x.isNaN && y.isNaN || x.isSignaling || y.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
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

unittest {
	write("-- min..............");
	dec9 x, y;
	x = 3; y = 2;
	assertEqual(min(x,y), y);
	x = -3; y = -2;
	assertEqual(min(x,y), x);
	x = -10; y = 3;
	assertEqual(min(x,y), x);
	writeln("passed");
}

/// Returns the smaller of the two operands (or NaN). Returns the same result
/// as the 'max' function if the signs of the operands are ignored.
/// Implements the 'min-magnitude' function in the specification. (p. 33)
/// Flags: INVALID OPERATION, ROUNDED.
public T minMagnitude(T)(T x, T y,
		Context context = T.context) if (isDecimal!T)
{
	return min(copyAbs!T(x), copyAbs!T(y), context);
}

unittest {
	write("minMagnitude...");
	writeln("test missing");
}

/// Returns a number with a coefficient of 1 and
/// the same exponent as the argument.
/// Flags: NONE.
public T quantum(T)(T x)  {
	return T(1, x.exponent);
}

unittest {	// quantum
	dec9 f, expect, actual;
	f = 23.14E-12;
	expect = 1E-14;
	actual = quantum(f);
	assertEqual(actual, expect);
}

unittest {
	write("quantum...");
	writeln("test missing");
}

//--------------------------------
// binary shift
//--------------------------------

// TODO: (behavior, language) these don't work because we don't want to truncate the coefficient.
// shl is okay, but shr isn't.
public T shl(T)(in T arg, const int n,
		const Rounding rounding = T.rounding) if (isDecimal!T)
{
	T result = T.nan;
	if (operandIsInvalid(arg, result)) {
		return result;
	}
	result = arg;
	with (result) {
		coefficient = coefficient << n;
		digits = numDigits(coefficient);
	}
	return roundToPrecision(result, T.precision, rounding);
}

unittest {	// shl
/*	dec9 big, expect, actual;
	big = dec9(4);
	expect = dec9(16);
	actual = shl(big, 2);
	assertEqual(actual, expect);
	big = dec9(437);
	expect = dec9(1748);
	actual = shl(big, 2);
	assertEqual(actual, expect);
	big = dec9(-0.07);
	expect = dec9(-0.56);
	actual = shl(big, 3);
	assertEqual(actual, expect);*/
}

/*
public Decimal shr(const Decimal arg,
		const DecimalContext context = Decimal.context) {

	Decimal result = Decimal.nan;
	if (operandIsInvalid!Decimal(arg, result)) {
		return result;
	}
	result = arg;
	with (result) {
	if (isOdd(coefficient)) {
		coefficient = coefficient >> 1;
		coefficient += 5;
		exponent = exponent - 1;
	}
	else {
		coefficient = coefficient >> 1;
	}
		digits = numDigits(coefficient);
	}
	return roundToPrecision(result, context);
}

public bool isOdd(const ExtendedInt big) {
	ExtendedInt test = mutable(big);
	test >>= 1;
	test <<= 1;
	return (test != big);
}
*/

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
public T shift(T)(T x, T y,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN
	if (x.isNaN) return invalidOperand(x);
	if (y.isNaN) return invalidOperand(y);
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
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'shift' function in the specification. (p. 49)
public T shift(T)(T x, int n,
		Context context = T.context) if (isDecimal!T)
{

	// check for NaN
	if (x.isNaN) return invalidOperand(x);

	// shift by zero returns the argument
	if (n == 0) return x;

	// shift of an infinite number returns the argument
	if (x.isInfinite) return x;

	int precision = context.precision;

	// shifting by more than precision is invalid.
	if (n < -precision || n > precision) {
		return setInvalidFlag!T;
	}

	if (n > 0) {
		// shift left
		x.coefficient = x.coefficient * pow10(n);
		x.digits = numDigits(x.coefficient);
		if (x.digits > context.precision) {
			x.coefficient = x.coefficient % pow10(precision);
			x.digits = precision;
		}
	}
	else {
		// shift right
		x.coefficient = x.coefficient / pow10(-n);
		x.digits = numDigits(x.coefficient);
	}
	return x;
}

unittest {
	write("-- shift............");
	dec9 x, y, z;
	x = "34";
	y = 8;
	z = "400000000";
	assertEqual(shift(x, y), z);
	x = "12";
	y = 9;
	z = "0";
	assertEqual(shift(x, y), z);
	x = "123456789";
	y = -2;
	z = "1234567";
	assertEqual(shift(x, y), z);
	y = 0;
	z = "123456789";
	assertEqual(shift(x, y), z);
	y = 2;
	z = "345678900";
	assertEqual(shift(x, y), z);
	writeln("passed");
}

/// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T)(T x, T y,
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
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T)(T x, int n,
		Context context = T.context) if (isDecimal!T)
{

	// check for NaN
	if (x.isNaN) return invalidOperand(x);

	// shift by zero returns the argument
	if (n == 0) return x;

	// shift of an infinite number returns the argument
	if (x.isInfinite) return x;

	int precision = context.precision;

	// shifting by more than precision is invalid.
	if (n < -precision || n > precision) {
		return setInvalidFlag!T;
	}
    // clip leading digits
	if (x.digits > precision){
		x.coefficient = x.coefficient % pow10(precision);
	}

	if (n > 0) {
		// rotate left
		x.coefficient = x.coefficient * pow10(n);
		x.digits = numDigits(x.coefficient);
		if (x.digits > context.precision) {
			xint rem;
			xint div = xint.divmod(x.coefficient, pow10(precision), rem);
			x.coefficient = div + rem;
			x.digits = numDigits(x.coefficient);
		}
	}
	else {
		// rotate right
		n = -n;
		xint rem;
		xint div = xint.divmod(x.coefficient, pow10(n), rem);
		x.coefficient = rem * pow10(precision - n) + div;
		x.digits = numDigits(x.coefficient);
	}
	return x;
}

unittest {
	write("-- rotate...........");
	dec9 x, y, z;
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

//------------------------------------------
// binary arithmetic operations
//------------------------------------------

/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION, OVERFLOW.
public T add(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	T nan;
	if (operationIsInvalid(x, y, nan)) {
		return nan;
	}

	// if both operands are infinite...
	if (x.isInfinite && y.isInfinite) {
		// if the signs differ return NaN and set invalid operation flag
		if (x.sign != y.sign) {
			return setInvalidFlag!T;
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

	// TODO: (language) this code is the same as add(decimal,long) from here
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
	return roundToPrecision(sum, context);
}


/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION, OVERFLOW.
public T add(T, U)(in T x, U n,
		Context context = T.context) if (isDecimal!T && !isDecimal!U)
{
	return add(x, T(n), context);
}

// TODO: (testing) change inputs to real numbers
unittest {	// add, addLOng
	write("-- add..............");
	dec9 arg1, arg2, sum;
	arg1 = dec9("12");
	arg2 = dec9("7.00");
	sum = add(arg1, arg2);
	assertStringEqual(sum, "19.00");
	arg1 = dec9("1E+2");
	arg2 = dec9("1E+4");
	sum = add(arg1, arg2);
	assertStringEqual(sum, "1.01E+4");
	dec9 arg12, arg22, sum2;
	arg12 = dec9("12345678901");
	arg22 = dec9("54321098765");
	sum2 = add(arg12, arg22);
	assertEqual(sum2, dec9("66666777700"));
	long arg3;
	arg3 = 12;
	arg1 = dec9("7.00");
	sum = add(arg1, arg3);
	assertStringEqual(sum, "19.00");
	arg1 = dec9("1E+2");
	arg3 = 10000;
	sum = add(arg1, arg3);
	assertStringEqual(sum, "10100");
	writeln("passed");
}

/// Subtracts the second operand from the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'subtract' function in the specification. (p. 26)
public T sub(T) (in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	return add(x, y.copyNegate, context);
}	 // end sub(x, y)


/// Subtracts the second operand from the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'subtract' function in the specification. (p. 26)
public T sub(T, U)(in T x, U z,
		Context context = T.context) if (isDecimal!T && !isDecimal!U)
{
	return add(x, T(z).copyNegate, context);
}	// end sub(x, z)

unittest {
	write("-- subtract.........");
	dec9 arg1, arg2, diff;
	arg1 = dec9("1.3");
	arg2 = dec9("1.07");
	diff = sub(arg1, arg2);
	assertStringEqual(diff, "0.23");
	arg2 = dec9("1.30");
	diff = sub(arg1, arg2);
	assertStringEqual(diff, "0.00");
	arg2 = dec9("2.07");
	diff = sub(arg1, arg2);
	assertStringEqual(diff, "-0.77");
	writeln("passed");
}

/// Multiplies the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'multiply' function in the specification. (p. 33-34)
public T mul(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// if invalid, return NaN
	if (x.isNaN || y.isNaN) return invalidOperands(x,y);

	// infinity * zero => invalid operation
	if (x.isZero && y.isInfinite || x.isInfinite && y.isZero) {
		return invalidOperands(x,y);
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
	T product = T.zero;
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
public T mul(T, U)(in T x, U n, Context context = T.context)
		if (isDecimal!T && isIntegral!U)
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
public T mul(T, U)(in T x, U z, Context context = T.context)
		if (isDecimal!T && !isIntegral!U && !isDecimal!U)
{
	return mul(x, T(z), context);
}	// end mul(x, z)

unittest {	// mul
	write("-- multiply.........");
	dec9 arg1, arg2, result;
	arg1 = dec9("1.20");
	arg2 = 3;
	result = mul(arg1, arg2);
	assertStringEqual(result, "3.60");
	arg1 = 7;
	result = mul(arg1, arg2);
	assertStringEqual(result, "21");
	long arg3;
	arg1 = dec9("1.20");
	arg3 = 3;
	result = mul(arg1, arg3);
	assertStringEqual(result, "3.60");
	arg1 = -7000;
	result = mul(arg1, arg3);
	assertStringEqual(result, "-21000");
	result = mul(dec9.infinity, arg2);
// TODO: test this result
	writeln("passed");
}

/// Squares the argument and returns the xx.
/// The result may be rounded and context flags may be set.
public T sqr(T)(T x,
		Context context = T.context) if (isDecimal!T)
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
	x.coefficient = x.coefficient^^2;
	x.exponent = x.exponent * 2;
	x.sign = false;
	x.digits = numDigits(x.coefficient);
	return roundToPrecision(x, context);
}

/// Multiplies the first two operands and adds the third operand to the result.
/// The result of the multiplication is not rounded prior to addition.
/// The result may be rounded and context flags may be set.
/// Implements the 'fused-multiply-add' function in the specification. (p. 30)
public T fma(T)(in T x, in T y, in T z,
		Context context = T.context) if (isDecimal!T)
{
	T xy = mul(x, y, Context(context.precision, T.maxExpo, Rounding.NONE));
	return add(xy, z, context);
}

unittest {	// fma
	write("-- fma..............");
	dec9 x, y, z, expect;
	x = 3; y = 5; z = 7;
	expect = 22;
	assertEqual(fma(x, y, z), expect);
	x = 3; y = -5; z = 7;
	expect = -8;
	assertEqual(fma(x, y, z), expect);
	x = 888565290;
	y = 1557.96930;
	z = -86087.7578;
	expect = dec9(1.38435736E+12);
	assertEqual(fma(x, y, z), expect);
	writeln("passed");
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T)(in T x, in T y,
		Context context = T.context) if (isDecimal!T)
{
	// check for NaN and division by zero
	T nan;
	if (divisionIsInvalid(x, y, nan)) {
		return nan;
	}
	// copy the arguments
	auto xx = x.dup;
	auto yy  = y.dup;
	auto q = T.zero;

	int diff = xx.exponent - yy.exponent;
	if (diff > 0) {
		xx.coefficient = shiftLeft(xx.coefficient, diff);
		xx.exponent = xx.exponent - diff;
		xx.digits = xx.digits + diff;
	}
	int shift = 4 + context.precision + yy.digits - xx.digits;
	if (shift > 0) {
		xx.coefficient = shiftLeft(xx.coefficient, shift);
		xx.exponent = xx.exponent - shift;
		xx.digits = xx.digits + shift;
	}
	// TODO: (behavior) is this check necessary? Why does it never exit?
//	// divisor may have become zero. Check again.
//	if (divisionIsInvalid!T(xx, yy, q)) {
//		return nan;
//	}
	q.coefficient = xx.coefficient / yy.coefficient;
	q.exponent = xx.exponent - yy.exponent;
	q.sign = xx.sign ^ yy.sign;
	q.digits = numDigits(q.coefficient);
	q = roundToPrecision(q, context);
	q = reduceToIdeal(q, diff);
	return q;
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T,U)(T x, U n,
		Context context = T.context) if (isDecimal!T && isIntegral!U)
{
	// check for NaN and division by zero
	T nan;
	if (divisionIsInvalid(x, T(n), nan)) {
		return nan;
	}

	auto q = T.zero;

	int diff = x.exponent;
	if (diff > 0) {
		x.coefficient = shiftLeft(x.coefficient, diff);
		x.exponent = x.exponent - diff;
		x.digits = x.digits + diff;
	}
	int shift = 4 + context.precision + numDigits(n)- x.digits;
	if (shift > 0) {
		x.coefficient = shiftLeft(x.coefficient, shift);
		x.exponent = x.exponent - shift;
		x.digits = x.digits + shift;
	}
//	// divisor may have become zero. Check again.
//	if (divisionIsInvalid!T(x, n, q)) {
//		return nan;
//	}
	q.coefficient = x.coefficient / n;
	q.exponent = x.exponent; // - n.exponent;
	q.sign = x.sign ^ (n < 0);
	q.digits = numDigits(q.coefficient);
	q = roundToPrecision(q, context);
	q = reduceToIdeal(q, diff);
	return q;
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T, U)(in T x, U z, Context context = T.context)
		if (isDecimal!T && !isIntegral!U && !isDecimal!U)
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
		x = T.ZERO;
	}
	x.digits = numDigits(x.coefficient);
	return x;
}

unittest {	// div
	write("-- divide...........");
	dec9 arg1, arg2, actual, expect;
	arg1 = dec9("1");
	arg2 = dec9("3");
	expect = dec9("0.333333333");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("2");
	arg2 = dec9("3");
	expect = dec9("0.666666667");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("5");
	arg2 = dec9("2");
	expect = dec9("2.5");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("1");
	arg2 = dec9("10");
	expect = dec9("0.1");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("12");
	arg2 = dec9("12");
	expect = dec9("1");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("8.00");
	arg2 = dec9("2");
	expect = dec9("4.00");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("2.400");
	arg2 = dec9("2.0");
	expect = dec9("1.20");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("1000");
	arg2 = dec9("100");
	expect = dec9("10");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("1000");
	arg2 = dec9("1");
	expect = dec9("1000");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	arg1 = dec9("2.40E+6");
	arg2 = dec9("2");
	expect = dec9("1.20E+6");
	actual = div(arg1, arg2);
	assertEqual(actual, expect);
	assertStringEqual(actual, expect);
	writeln("passed");
}

// TODO: (behavior) Does this implement the actual spec operation?
/// Divides the first operand by the second and returns the integer portion
/// of the quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide-integer' function in the specification. (p. 30)
public T divideInteger(T)(in T arg1, in T arg2)  {
	// check for NaN or divide by zero
	T result = T.nan;
	if (divisionIsInvalid!T(arg1, arg2, result)) {
		return result;
	}

	auto dividend = arg1.dup;
	auto divisor  = arg2.dup;
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
		return setInvalidFlag!T;
	}
	quotient.digits = digits;
	return T(quotient);
}

unittest {	// divideInteger
	write("-- div..............");
	dec9 arg1, arg2, actual, expect;
	arg1 = 2;
	arg2 = 3;
	actual = divideInteger(arg1, arg2);
	expect = 0;
	assertEqual(actual, expect);
	arg1 = 10;
	actual = divideInteger(arg1, arg2);
	expect = 3;
	assertEqual(actual, expect);
	arg1 = 1;
	arg2 = 0.3;
	actual = divideInteger(arg1, arg2);
	expect = 3;
	assertEqual(actual, expect);
	writeln("passed");
}

/// Divides the first operand by the second and returns the
/// fractional remainder.
/// Division by zero sets a flag and returns infinity.
/// The sign of the remainder is the same as that of the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'remainder' function in the specification. (p. 37-38)
public T remainder(T)(in T arg1, in T arg2,
		in int precision = T.precision)  {
	T quotient;
	if (divisionIsInvalid!T(arg1, arg2, quotient)) {
		return quotient;
	}
	quotient = divideInteger!T(arg1, arg2,);
	T remainder = arg1 - mul!T(arg2, quotient, Context(precision, T.maxExpo, Rounding.NONE));
	return remainder;
}

unittest {	// remainder
	write("-- remainder........");
	dec9 arg1, arg2, actual, expect;
	arg1 = dec9("2.1");
	arg2 = dec9("3");
	actual = remainder(arg1, arg2);
	expect = dec9("2.1");
	assertEqual(actual, expect);
	arg1 = dec9("10");
	arg2 = dec9("3");
	actual = remainder(arg1, arg2);
	expect = dec9("1");
	assertEqual(actual, expect);
	arg1 = dec9("-10");
	arg2 = dec9("3");
	actual = remainder(arg1, arg2);
	expect = dec9("-1");
	assertEqual(actual, expect);
	arg1 = dec9("10.2");
	arg2 = dec9("1");
	actual = remainder(arg1, arg2);
	expect = dec9("0.2");
	assertEqual(actual, expect);
	arg1 = dec9("10");
	arg2 = dec9("0.3");
	actual = remainder(arg1, arg2);
	expect = dec9("0.1");
	assertEqual(actual, expect);
	arg1 = dec9("3.6");
	arg2 = dec9("1.3");
	actual = remainder(arg1, arg2);
	expect = dec9("1.0");
	assertEqual(actual, expect);
	writeln("passed");
}

/// Divides the first operand by the second and returns the
/// fractional remainder.
/// Division by zero sets a flag and returns Infinity.
/// The sign of the remainder is the same as that of the first operand.
/// This function corresponds to the "remainder" function
/// in the General Decimal Arithmetic Specification.
public T remainderNear(T)(in T x, in T y) if (isDecimal!T)
{
	T quotient;
	if (divisionIsInvalid!T(x, y, quotient)) {
		return quotient;
	}
	quotient = x/y;
	// TODO: (behavior) roundToIntegralValue?
	T remainder = x - y * (roundToIntegralExact(quotient));
	return remainder;
}

unittest {
	write("-- remainderNear....");
	dec9 arg1, arg2, actual, expect;
	arg1 = dec9("2.1");
	arg2 = dec9("3");
	actual = remainderNear(arg1, arg2);
	expect = dec9("-0.9");
	assertEqual(actual, expect);
	arg1 = dec9("3");
	arg2 = dec9("2");
	actual = remainderNear(arg1, arg2);
	expect = dec9("-1");
	assertEqual(actual, expect);
	arg1 = dec9("10");
	arg2 = dec9("3");
	actual = remainderNear(arg1, arg2);
	expect = dec9("1");
	assertEqual(actual, expect);
	arg1 = dec9("-10");
	arg2 = dec9("3");
	actual = remainderNear(arg1, arg2);
	expect = dec9("-1");
	assertEqual(actual, expect);
	arg1 = dec9("10.2");
	arg2 = dec9("1");
	actual = remainderNear(arg1, arg2);
	expect = dec9("0.2");
	assertEqual(actual, expect);
	arg1 = dec9("10");
	arg2 = dec9("0.3");
	actual = remainderNear(arg1, arg2);
	expect = dec9("0.1");
	assertEqual(actual, expect);
	arg1 = dec9("3.6");
	arg2 = dec9("1.3");
	actual = remainderNear(arg1, arg2);
	expect = dec9("-0.3");
	assertEqual(actual, expect);
	writeln("passed");
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
	T nan;
	if (operationIsInvalid(x, y, nan)) return nan;

	// if one operand is infinite and the other is not...
	if (x.isInfinite != y.isInfinite()) {
		return setInvalidFlag!T;
	}
	// if both arguments are infinite
	if (x.isInfinite() && y.isInfinite()) {
		return x.dup;
	}
	T result = x.dup;
	int diff = x.exponent - y.exponent;

	if (diff == 0) {
		return result;
	}

	// TODO: (behavior) this shift can cause integer overflow for fixed size decimals
	if (diff > 0) {
		result.coefficient = shiftLeft(result.coefficient, diff/*, precision*/);
		result.digits = result.digits + diff;
		result.exponent = y.exponent;
		if (result.digits > T.precision) {
			result = T.nan;
		}
		return result;
	}
	else {
		int precision = (-diff > x.digits) ? 0 : x.digits + diff;
		result = roundToPrecision(result, precision, context.rounding);
		result.exponent = y.exponent;
		if (result.isZero && x.isSigned) {
			result.sign = true;
		}
		return result;
	}
}

unittest {	// quantize
	write("-- quantize.........");
	dec9 arg1, arg2, actual, expect;
	string str;
	arg1 = dec9("2.17");
	arg2 = dec9("0.001");
	expect = dec9("2.170");
	actual = quantize!dec9(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = dec9("2.17");
	arg2 = dec9("0.01");
	expect = dec9("2.17");
	actual = quantize(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = dec9("2.17");
	arg2 = dec9("0.1");
	expect = dec9("2.2");
	actual = quantize(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = dec9("2.17");
	arg2 = dec9("1e+0");
	expect = dec9("2");
	actual = quantize(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = dec9("2.17");
	arg2 = dec9("1e+1");
	expect = dec9("0E+1");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("-Inf");
	arg2 = dec9("Infinity");
	expect = dec9("-Infinity");
	actual = quantize(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = dec9("2");
	arg2 = dec9("Infinity");
	expect = dec9("NaN");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("-0.1");
	arg2 = dec9("1");
	expect = dec9("-0");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("-0");
	arg2 = dec9("1e+5");
	expect = dec9("-0E+5");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("+35236450.6");
	arg2 = dec9("1e-2");
	expect = dec9("NaN");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("-35236450.6");
	arg2 = dec9("1e-2");
	expect = dec9("NaN");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("217");
	arg2 = dec9("1e-1");
	expect = dec9( "217.0");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("217");
	arg2 = dec9("1e+0");
	expect = dec9("217");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("217");
	arg2 = dec9("1e+1");
	expect = dec9("2.2E+2");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	arg1 = dec9("217");
	arg2 = dec9("1e+2");
	expect = dec9("2E+2");
	actual = quantize(arg1, arg2);
	assertStringEqual(actual, expect);
	assertEqual(actual, expect);
	writeln("passed");
}

/// Returns the nearest integer value to the argument.
/// Context flags may be set.
/// Implements the 'round-to-integral-exact' function
/// in the specification. (p. 39)
public T roundToIntegralExact(T)(in T x,
		in Rounding rounding = Rounding.HALF_EVEN) if (isDecimal!T)
{
	T result = x.dup;
	if (result.isSignaling) return setInvalidFlag!T;
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	// TODO: (behavior) need to prevent precision overrides
	int precision = result.digits + result.exponent;
	result = roundToPrecision(result, precision, rounding);
	return result;
}

unittest { // roundToIntegralExact
 	write("-- roundToIntExact..");
	dec9 arg, expect, actual;
	arg = 2.1;
	expect = 2;
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = 0.7;
	expect = 1;
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = 100;
	expect = 100;
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = 101.5;
	expect = 102;
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = -101.5;
	expect = -102;
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = dec9("10E+5");
	expect = dec9("1.0E+6");
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = dec9("7.89E+77");
	expect = dec9("7.89E+77");
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	arg = dec9("-Inf");
	expect = dec9("-Infinity");
	actual = roundToIntegralExact(arg);
	assertEqual(actual, expect);
	writeln("passed");
}

// TODO: (behavior) need to re-implement this so no flags are set.
/// The result may be rounded and context flags may be set.
/// Implements the 'round-to-integral-value' function
/// in the specification. (p. 39)
public T roundToIntegralValue(T)(in T arg,
		const Rounding rounding = T.rounding) if (isDecimal!T)
{
	T result = arg.dup;
	if (result.isSignaling) return setInvalidFlag!T;
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
private void alignOps(T)(ref T x, ref T y) if (isDecimal!T)
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
private void alignOps(T)(ref T x, int n) if (isDecimal!T)
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
	dec9 arg1, arg2;
	arg1 = dec9("1.3E35");
	arg2 = dec9("-17.4E29");
	alignOps(arg1, arg2);
	assertEqual(arg1.coefficient, 13000000);
	assertEqual(arg2.exponent, 28);
	writeln("passed");
}

//--------------------------------
// logical operations
//--------------------------------

/// Returns true if the argument is a valid logical string.
/// All characters in a valid logical string must be either '1' or '0'.
private bool isLogicalString(const string str) {
	foreach(char ch; str) {
		if (ch != '0' && ch != '1') return false;
	}
	return true;
}

/// Returns true if the argument is a valid logical decimal number.
/// The sign and exponent must both be zero, and all decimal digits
/// in the coefficient must be either '1' or '0'.
public bool isLogical(T)(in T arg) if (isDecimal!T)
{
	if (arg.sign != 0 || arg.exponent != 0) return false;
	string str = arg.coefficient.toString;
	return isLogicalString(str);
}

/// Returns true and outputs a valid logical string if the argument is
/// a valid logical decimal number.
/// The sign and exponent must both be zero, and all decimal digits
/// in the coefficient must be either '1' or '0'.
private bool isLogicalOperand(T)(in T arg, out string str)
		if (isDecimal!T)
{
	if (arg.sign != 0 || arg.exponent != 0) return false;
	str = arg.coefficient.toString;
	return isLogicalString(str);
}

unittest {	// logical string/number tests
//	import decimal.dec32;
 	write("-- logical tests....");
	assertTrue(isLogicalString("010101010101"));
	assertTrue(isLogical(dec9("1010101")));
//	string str;
//	assert(isLogicalOperand(Dec32("1010101"), str));
//	assert(str == "1010101");
	writeln("passed");
}

//--------------------------------
// unary logical operations
//--------------------------------

/// Inverts and returns a decimal logical number.
/// Implements the 'invert' function in the specification. (p. 44)
public T invert(T)(T arg) if (isDecimal!T)
{
	string str;
	if (!isLogicalOperand(arg, str)) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	return T(invert(str));
}

/// Inverts and returns a logical string.
/// Each '1' is changed to a '0', and vice versa.
private T invert(T: string)(T arg)
{
	char[] result = new char[arg.length];
	for(int i = 0; i < arg.length; i++) {
		result[i] = arg[i] == '0' ? '1' : '0';
	}
	return result.idup;
}

unittest {	// inverse
 	write("-- logical inverse..");
	// TODO: (behavior, language) why can't we compare ints and decimals?
	dec9 num;
	num = invert(dec9(101001));
	assertStringEqual(num, 10110);
	num = invert(dec9(1));
	assertEqual(num, dec9(0));
//	assertStringEqual(num, 0);
	num = invert(dec9(0));
	assertStringEqual(num, 1);
	writeln("passed");
}

//--------------------------------
// binary logical operations
//--------------------------------

/// called by opBinary.
private T opLogical(string op, T)(in T x, in T y)
		if (isDecimal!T)
{
	int precision = T.precision;
	string str1;
	if (!isLogicalOperand(x, str1)) {
		return setInvalidFlag!T;
	}
	string str2;
	if (!isLogicalOperand(y, str2)) {
		return setInvalidFlag!T;
	}
	static if (op == "and") {
		string str = and(str1, str2);
	}
	static if (op == "or") {
		string str = or(str1, str2);
	}
	static if (op == "xor") {
		string str = xor(str1, str2);
	}
	return T(str);
}

/// Performs a logical 'and' of the arguments and returns the result
/// Implements the 'and' function in the specification. (p. 41)
public T and(T)(in T x, in T y) if(isDecimal!T)
{
	return opLogical!("and", T)(x, y);
}

/// Performs a logical 'and' of the (string) arguments and returns the result
T and(T: string)(in T x, in T y)
{
	string str1, str2;
	int length, diff;
	if (x.length > y.length) {
		length = y.length;
		diff = x.length - y.length;
		str2 = x;
		str1 = rightJustify(y, x.length, '0');
	}
	else if (x.length < y.length) {
		length = x.length;
		diff = y.length - x.length;
		str1 = rightJustify(x, y.length, '0');
		str2 = y;
	} else {
		length = x.length;
		diff = 0;
		str1 = x;
		str2 = y;
	}
	char[] result = new char[length];
	for(int i = 0; i < length; i++) {
		if (str1[i + diff] == '1' && str2[i + diff] == '1') {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return result.idup;
}

/// Performs a logical 'or' of the arguments and returns the result
/// Implements the 'or' function in the specification. (p. 47)
public T or(T)(in T x, in T y) if (isDecimal!T)
{
	return opLogical!("or", T)(x, y/*, context*/);
}

/// Performs a logical 'or' of the (string) arguments and returns the result
T or(T: string)(in T x, in T y)
{
	string str1, str2;
	int length;
	if (x.length > y.length) {
		length = x.length;
		str1 = x;
		str2 = rightJustify(y, x.length, '0');
	}
	if (x.length < y.length) {
		length = y.length;
		str1 = rightJustify(x, y.length, '0');
		str2 = y;
	} else {
		length = x.length;
		str1 = x;
		str2 = y;
	}
	char[] result = new char[length];
	for(int i = 0; i < length; i++) {
		if (str1[i] == '1' || str2[i] == '1') {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return result.idup;
}

/// Performs a logical 'xor' of the arguments and returns the result
/// Implements the 'xor' function in the specification. (p. 49)
public T xor(T)(in T x, in T y) if (isDecimal!T)
{
	return opLogical!("xor", T)(x, y);
}

/// Performs a logical 'xor' of the (string) arguments
/// and returns the result.
T xor(T: string)(in T x, in T y)
{
	string str1, str2;
	int length;
	if (x.length > y.length) {
		length = x.length;
		str1 = x;
		str2 = rightJustify(y, x.length, '0');
	}
	if (x.length < y.length) {
		length = y.length;
		str1 = rightJustify(x, y.length, '0');
		str2 = y;
	} else {
		length = x.length;
		str1 = x;
		str2 = y;
	}
	char[] result = new char[length];
	for(int i = 0; i < length; i++) {
		if (str1[i] != str2[i]) {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return result.idup;
}

unittest { // binary logical ops
	dec9 op1, op2;
	op1 = 10010101;
	op2 = 11100100;
//	assertEqual((op1 & op2), dec9(10000100));
//	assert((op1 | op2) == dec9(11110101));
//	assert((op1 ^ op2) == dec9( 1110001));
	op1 =   100101;
	op2 = 11100100;
//	assert((op1 & op2) == dec9(  100100));
//	assert((op1 | op2) == dec9(11100101));
//	assert((op1 ^ op2) == dec9(11000001));
}

//--------------------------------
// validity functions
//--------------------------------

/// Sets the invalid-operation flag and returns a quiet NaN.
// TODO: combine this with invalidOperand?
private T setInvalidFlag(T)(bool sign = false, ushort payload = 0)
		if (isDecimal!T)
{
	contextFlags.setFlags(INVALID_OPERATION);
//	T result = T.nan(payload, sign);
//	if (payload != 0) {
//		result.payload = payload;
//	}
	return T.nan(payload, sign);
}

unittest {	// setInvalidFlag
/*	dec9 arg, expect, actual;
	// TODO: (testing) Can't actually test payloads at this point.
	arg = dec9("sNaN123");
	expect = dec9("NaN123");
	actual = abs!dec9(arg);
	assertTrue(actual.isQuiet);
	assertTrue(contextFlags.getFlag(INVALID_OPERATION));
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
	contextFlags.setFlags(INVALID_OPERATION);
	// if the operand is a quiet NaN return it.
	if (x.isQuiet) return x.dup;
	// Otherwise change the signalling NaN to a quiet NaN.
	if (x.isSignaling) return T.nan(x.payload, x.sign);
	// if the operand is neither quiet nor signaling something else is wrong
	// so return NaN.
	return T.nan;
}

/// Returns a quiet NaN and sets the invalid-operation flag.
/// "The result of any arithmetic operation which has an operand
/// which is a NaN (a quiet NaN or a signaling NaN) is [s,qNaN]
/// or [s,qNaN,d]. The sign and any diagnostic information is copied
/// from the first operand which is a signaling NaN, or if neither is
/// signaling then from the first operand which is a NaN."
/// -- General Decimal Arithmetic Specification, p. 24
//@safe
package T invalidOperands(T)(in T x, in T y) if (isDecimal!T)
{
	// flag the invalid operation
	contextFlags.setFlags(INVALID_OPERATION);
	// if either operand is signaling return a quiet NaN.
	// NOTE: sign is ignored.
	if (x.isSignaling) return T.nan(x.payload, x.sign);
	if (y.isSignaling) return T.nan(y.payload, y.sign);
	// if the operand is a quiet NaN return it.
	if (x.isQuiet) return x.dup;
	if (y.isQuiet) return y.dup;
	// if neither of the operands is quiet or signaling,
	// the operands are invalid for some reason. return a quiet NaN.
	return T.nan;
}

/// Returns true and sets the invalid-operation flag if the operand
/// is a NaN.
/// "The result of any arithmetic operation which has an operand
/// which is a NaN (a quiet NaN or a signaling NaN) is [s,qNaN]
/// or [s,qNaN,d]. The sign and any diagnostic information is copied
/// from the first operand which is a signaling NaN, or if neither is
/// signaling then from the first operand which is a NaN."
/// -- General Decimal Arithmetic Specification, p. 24
//@safe
package bool operandIsInvalid(T)(in T x) if (isDecimal!T)
{
	// if the operand is a signaling NaN...
	if (x.isNaN) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// TODO: should retain payload if signalling, but change to quiet.
		// retain payload; convert to qNaN
//		result = T.nan(x.payload);
		return true;
	}
/*	// ...else if the operand is a quiet NaN...
	if (x.isQuiet) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the result to the qNaN operand
//		result = x;
		return true;
	}*/
	// ...otherwise, no flags are set and result is unchanged
	return false;
}

unittest {
	write("operandIsInvalid...");
	writeln("test missing");
}

/// Returns true and sets the invalid-operation flag if either operand
/// is a NaN.
/// "The result of any arithmetic operation which has an operand
/// which is a NaN (a quiet NaN or a signaling NaN) is [s,qNaN]
/// or [s,qNaN,d]. The sign and any diagnostic information is copied
/// from the first operand which is a signaling NaN, or if neither is
/// signaling then from the first operand which is a NaN."
/// -- General Decimal Arithmetic Specification, p. 24
private bool operationIsInvalid(T)(in T x, in T y, out T nan)
		if (isDecimal!T)
{
	// if either operand is a quiet NaN...
	if (x.isQuiet || y.isQuiet) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the nan to the first qNaN operand
		nan = x.isQuiet ? x : y;
		return true;
	}
	// ...if either operand is a signaling NaN...
	if (x.isSignaling || y.isSignaling) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the nan to the first sNaN operand
		nan = x.isSignaling ? T.nan(x.payload) : T.nan(y.payload);
		return true;
	}
	// ...otherwise, no flags are set and nan is unchanged
	return false;
}

private bool operationIsInvalid(T)(T x, long y, out T nan)
		if (isDecimal!T)
{
	// if either operand is a quiet NaN...
	if (x.isQuiet) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the nan to the first qNaN operand
		nan = x;
		return true;
	}
	// ...if either operand is a signaling NaN...
	if (x.isSignaling) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the nan to the first sNaN operand
		nan = T.nan(x.payload);
		return true;
	}
	// ...otherwise, no flags are set and nan is unchanged
	return false;
}

unittest {
	write("operationIsInvalid...");
	writeln("test missing");
}

/// Checks for invalid operands and division by zero.
/// If found, the function sets the quotient to NaN or infinity, respectively,
/// and returns true after setting the context flags.
/// Also checks for zero dividend and calculates the result as needed.
/// This is a helper function implementing checks for division by zero
/// and invalid operation in the specification. (p. 51-52)
private bool divisionIsInvalid(T)(in T dividend, in T divisor,
		ref T quotient) if (isDecimal!T)
{

	if (operationIsInvalid(dividend, divisor, quotient)) {
		return true;
	}
	if (divisor.isZero()) {
		if (dividend.isZero()) {
			quotient = setInvalidFlag!T;
		}
		else {
			contextFlags.setFlags(DIVISION_BY_ZERO);
			quotient = T.infinity;
			quotient.sign = dividend.sign ^ divisor.sign;
		}
		return true;
	}
	// TODO: (behavior) what purpose does this check serve?
	// The dividend can be zero without any difficulties, right?
	// NOTE: this is just a short circuit to avoid dividing zero.
/*	if (dividend.isZero()) {
		quotient = T.zero;
		return true;
	}*/
	return false;
}

/// Checks for invalid operands and division by zero.
/// If found, the function sets the quotient to NaN or infinity, respectively,
/// and returns true after setting the context flags.
/// Also checks for zero dividend and calculates the result as needed.
/// This is a helper function implementing checks for division by zero
/// and invalid operation in the specification. (p. 51-52)
private bool divisionIsInvalid(T)(T dividend, long divisor,
		ref T quotient) if (isDecimal!T)
{

	if (operationIsInvalid(dividend, divisor, quotient)) {
		return true;
	}
	if (divisor == 0) {
		if (dividend.isZero()) {
			quotient = setInvalidFlag!T;
		}
		else {
			contextFlags.setFlags(DIVISION_BY_ZERO);
			quotient = T.infinity;
			quotient.sign = dividend.sign ^ (divisor < 0);
		}
		return true;
	}
	// TODO: (behavior) what purpose does this check serve?
	// The dividend can be zero without any difficulties, right?
/*	if (dividend.isZero()) {
		quotient = T.zero;
		return true;
	}*/
	return false;
}

unittest {
	write("divisionIsInvalid...");
	writeln("test missing");
}

unittest {
	writeln("==========================");
	writeln("decimal arithmetic.....end");
	writeln("==========================");
}


