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

// TODO: ensure context flags are being set and cleared properly.

// TODO: opEquals unit test should include numerically equal testing.

// TODO: write some test cases for flag setting. test the add/sub/mul/div functions

// TODO: to/from real or double (float) values needs definition and implementation.

module eris.decimal.arithmetic;

import eris.integer.extended;
import eris.assertions;
import std.string;

import eris.decimal.context;
import eris.decimal.decimal;
import eris.decimal.rounding;

unittest {
	writeln("==========================");
	writeln("decimal arithmetic...begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
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
	public string classify(T)(in T x)  {
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
public T copySign(T)(const T arg1, const T arg2)  {
	T copy = arg1.dup;
	copy.sign = arg2.sign;
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

/// Returns the truncated base 10 logarithm of the argument.
/// "...The integer which is the exponent of the magnitude
/// of the most significant digit of the operand.
/// (As though the operand were truncated to a single digit
/// while maintaining the value of that digit and without
/// limiting the resulting exponent)".
/// May set the INVALID_OPERATION and DIVISION_BY_ZERO flags.
/// Implements the 'logb' function in the specification. (p. 47)
public int ilogb(T)(in T x) {

	T nan;
	if (operandIsInvalid!T(x, nan)) {
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
public T logb(T)(in T x) {

	T nan;
	if (operandIsInvalid!T(x, nan)) {
		return nan;
	}
	if (x.isInfinite) {
		return T.infinity;
	}
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
public T scaleb(T)(in T x, in T y)  {

	T nan;
	if (operationIsInvalid(x, y, nan)) return nan;

	if (x.isInfinite) return x.dup;

	T result;
	int expo = y.exponent;
	if (expo != 0 /* && not within range */) {
		result = setInvalidFlag!T;
		return result;
	}
	result = x;
	int scale = cast(int)y.coefficient.toInt;
	if (y.isSigned) {
		scale = -scale;
	}
	// (A)TODO: check for overflow/underflow -- should this be part of setting
	// the exponent? Don't want that in construction but maybe do here.
	result.exponent = result.exponent + scale;
	return result;
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
/// NOTE: if the operand is guarded, the result will be guarded.
public T reduce(T)(in T x,
		Context context = T.context)  {
	// special cases
	T nan;
	if (operandIsInvalid(x, nan)) return nan;

	if (!x.isFinite) return x.dup;

	T reduced = plus(x.dup, context);

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
		Context context = T.context)  {
	return reduce(x, context.precision, context.rounding);
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
public T abs(T)(in T x,
		Context context = T.context)  {
  	T nan;
	if (operandIsInvalid!T(x, nan)) return nan;
	return roundToPrecision(x.copyAbs, context.precision, context.rounding);
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

/// Returns the sign of the argument: -1, 0, -1.
/// If the argument is (signed or unsigned) zero, 0 is returned.
/// If the argument is negative, -1 is returned.
/// Otherwise +1 is returned.
/// This function is not required by the specification.
public int sgn(T)(in T x)  {
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
public int sgn(T:xint)(const T num) {
	if (num < 0) return -1;
	if (num > 0) return 1;
	return 0;
}

/// Returns a copy of the argument with same sign as the argument.
/// This operation rounds the result and may set flags.
/// The result is equivalent to add('0', arg).
/// To copy without rounding or setting flags use the 'copy' function.
/// Implements the 'plus' function in the specification. (p. 33)
/// Flags: INVALID_OPERATION
public T plus(T)(in T x, in Context context = T.context)  {
	T nan;
	if (operandIsInvalid!T(x, nan)) return nan;
	return roundToPrecision(x, context.precision, context.rounding);
}

/// Returns a copy of the argument with the opposite sign.
/// This operation rounds the argument and may set flags.
/// Result is equivalent to subtract('0', x).
/// To copy without rounding or setting flags use the 'copyNegate' function.
/// Implements the 'minus' function in the specification. (p. 37)
/// Flags: INVALID_OPERATION
public T minus(T)(in T x, in Context context = T.context) {
	T nan;
	if (operandIsInvalid!T(x, nan)) return nan;
	return roundToPrecision(x.copyNegate, context.precision, context.rounding);
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
public T nextPlus(T)(in T x, in Context context = T.context) {
	T nan;
	if (operandIsInvalid!T(x, nan)) return nan;

	if (x.isInfinite) {
		if (x.sign) {
			return T.max.copyNegate;
		}
		else {
			return x.dup;
		}
	}
	// TODO: use of precision
	int adjustedExpo = x.exponent + x.digits - context.precision;
	if (adjustedExpo < T.tinyExpo) {
			return T(0L, T.tinyExpo);
	}
	// (A)TODO: must add the increment w/o setting flags
	T y = T(1L, adjustedExpo);
	T next = add(x, y, context);
	if (next > T.max) {
		next = T.infinity;
	}
	return next;
}

/// Returns the largest representable number that is smaller than
/// the argument.
/// Implements the 'next-minus' function in the specification. (p. 34)
/// Flags: INVALID_OPERATION
public T nextMinus(T)(in T x, in Context context = T.context) {

	T nan;
	if (operandIsInvalid!T(x, nan)) {
		return nan;
	}
	if (x.isInfinite) {
		if (!x.sign) {
			return T.max.dup;
		}
		else {
			return x.dup;
		}
	}
	// This is necessary to catch the special case where the coefficient == 1
	T reduced = reduce!T(x, context);
	int adjustedExpo = reduced.exponent + reduced.digits - T.precision;
	if (x.coefficient == 1) adjustedExpo--;
	if (adjustedExpo < T.tinyExpo) {
		return T(0L, T.tinyExpo);
	}
	T addend = T(1, adjustedExpo);
	reduced = sub!T(x, addend, context);	//(A)TODO: are the flags set/not set correctly?
		if (reduced < copyNegate(T.max)) {
		reduced = copyNegate(T.infinity);
	}
	return reduced;
}

/// Returns the representable number that is closest to the first operand
/// in the direction of the second operand.
/// Implements the 'next-toward' function in the specification. (p. 34-35)
/// Flags: INVALID_OPERATION
public T nextToward(T)(in T x, in T y, in Context context = T.context) {

	T nan;
	if (operationIsInvalid(x, y, nan)) return nan;

	// compare them but don't round
	int comp = compare(x, y, context);
	if (comp < 0) return nextPlus(x, context);
	if (comp > 0) return nextMinus(x, context);
	return roundToPrecision(copySign(x,y), context.precision, context.rounding);
}

unittest {	// nextPlus
	write("-- next.............");
	dec9 arg, expect, actual;
	arg = 1;
	expect = dec9("1.00000001");
	actual = nextPlus(arg);
	assertEqual(actual, expect);
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

// TODO: compares to full precision, not context precision.
/// Compares two operands numerically to the current precision.
/// Returns -1, 0, or +1 if the second operand is, respectively,
/// less than, equal to, or greater than the first operand.
/// Implements the 'compare' function in the specification. (p. 27)
/// Flags: INVALID_OPERATION
public int compare(T)(in T x, in T y, in Context context = T.context) {

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

	// if signs differ, just compare the signs
	if (x.sign != y.sign) {
		// check for zeros: +0 and -0 are equal
		if (x.isZero && y.isZero) return 0;
		return x.sign ? -1 : 1;
	}

	// if either is infinite...
	if (x.isInfinite || y.isInfinite) {
		if (x.isInfinite && y.isInfinite) return 0;
		// TODO: still need to test the signs
		return x.isInfinite ? -1 : 1;
	}

/*	// TODO: here is where they should be rounded
	// restrict operands to current precision
	if (x.digits > precision) {
		roundToPrecision(x, context.precision, context.rounding);
	}
	if (y.digits > precision) {
		roundToPrecision(y, context.precision, context.rounding);
	}
*/
	// compare the magnitudes of the numbers
	int diff = (x.exponent + x.digits) - (y.exponent + y.digits);
	if (diff != 0)
		if (!x.sign) {
			if (diff > 0) return 1;
			if (diff < 0) return -1;
		}
		else {
			if (diff > 0) return -1;
			if (diff < 0) return 1;
		}

	// numbers have the same magnitude -- compare coefficients
    int comp = xcompare(x.reduce.coefficient, y.reduce.coefficient);
	return x.sign ? -comp : comp;
}

unittest {	// compare
	write("-- compare..........");
	dec9 x, y;
	x = dec9(2.1);
	y = dec9(3);
	assertEqual(compare(x, y), 1);
	x = dec9(2.1);
	y = dec9(2.1);
	assertEqual(compare(x, y), 0);
	writeln("passed");
}

/// Returns true if the operands are equal to the current precision.
/// Finite numbers are equal if they are numerically equal
/// to the current precision.
/// A NaN is not equal to any number, not even another NaN or itself.
/// Infinities are equal if they have the same sign.
/// Zeros are equal regardless of sign.
/// A decimal NaN is not equal to itself (this != this).
/// This function is not included in the specification.
/// Flags: INVALID_OPERATION
public bool equals(T)(in T x, in T y, in Context context = T.context) {

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

	// if either is infinite...
	if (x.isInfinite || y.isInfinite) {
		return (x.isInfinite && y.isInfinite && x.isSigned == y.isSigned);
	}

	// if either is zero...
	if (x.isZero || y.isZero) {
		return (x.isZero && y.isZero);
	}

	// if their signs differ...
	if (x.sign != y.sign) return false;

	// if they have the same representation, they are equal
	if (x.exponent == y.exponent && x.coefficient == y.coefficient) {
		return true;
	}

	// restrict operands to current precision
	// NOTE: they will be rounded to the guarded precision
	T rx, ry;
	if (x.isGuarded || y.isGuarded) {
		rx.isGuarded = true;
		ry.isGuarded = true;
	}
	rx = roundToPrecision(x, context.precision, context.rounding);
	ry = roundToPrecision(y, context.precision, context.rounding);

	// if they have different magnitudes, they are not equal
	int diff = (rx.exponent + rx.digits) - (ry.exponent + ry.digits);
	if (diff != 0) {
		return false;
	}

	// otherwise they are equal if they represent the same value
	return rx.reduce.coefficient == ry.reduce.coefficient;
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
	writeln("passed");
}

/// Compares the numeric values of two numbers. CompareSignal is identical to
/// compare except that quiet NaNs are treated as if they were signaling.
/// This operation may set the invalid-operation flag.
/// Implements the 'compare-signal' function in the specification. (p. 27)
/// Flags: INVALID_OPERATION
public int compareSignal(T) (in T x, in T y,
		in Context context = T.context) {

	// any operation with NaN is invalid.
	// if both are NaN, return as if x > y.
	if (x.isNaN || y.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return x.isNaN ? 1 : -1;
	}
	return (compare!T(x, y, context.precision, context.rounding));
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
public int compareTotal(T)(in T x, in T y)  {

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

	// we know the numbers have the same magnitude --
	// compare coefficients
	auto result = xcompare(x.reduce.coefficient, y.reduce.coefficient);

	// if equal after alignment, compare the original exponents
	if (result == 0) {
		return (x.exponent > y.exponent) ? ret1 : ret2;
	}
	// otherwise return the numerically larger
	return (result > 0) ? ret2 : ret1;
}

/// compare-total-magnitude takes two numbers and compares them
/// using their abstract representation rather than their numerical value
/// and with their sign ignored and assumed to be 0.
/// The result is identical to that obtained by using compare-total
/// on two operands which are the copy-abs copies of the operands.
/// Implements the 'compare-total-magnitude' function in the specification.
/// (p. 43)
/// Flags: NONE.
int compareTotalMagnitude(T)(in T x, in T y)  {
	return compareTotal(x.copyAbs, y.copyAbs);
}

unittest {
	write("-- compareSignal....");
	writeln("test missing");
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
public bool sameQuantum(T)(in T x, in T y)  {
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
public T max(T)(in T x, in T y,
		in Context context = T.context)  {

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
	return roundToPrecision(result, context.precision, context.rounding);
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
public T maxMagnitude(T)(in T x, in T y, in Context context = T.context) {
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
		return roundToPrecision(x, context.precision, context.rounding);
	}
	return roundToPrecision(y, context.precision, context.rounding);
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
public T min(T)(in T x, in T y,
		Context context = T.context)  {

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
	return roundToPrecision(min, context.precision, context.rounding);
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
public T minMagnitude(T)(in T x, in T y,
		Context context = T.context)  {
	return min(copyAbs!T(x), copyAbs!T(y), context.precision, context.rounding);
}

unittest {
	write("minMagnitude...");
	writeln("test missing");
}

// TODO: need to check for overflow?
/// Returns a number with a coefficient of 1 and
/// the same exponent as the argument.
/// Not required by the specification.
/// Flags: NONE.
public T quantum(T)(const T arg)  {
	return T(1, arg.exponent);
}

unittest {	// quantum
/*	dec9 arg, expect, actual;
	arg = 23.14E-12;
	expect = 1E-14;
	actual = quantum(arg);
	assertEqual(actual, expect);*/
}

unittest {
	write("quantum...");
	writeln("test missing");
}

//--------------------------------
// binary shift
//--------------------------------

// TODO: these don't work because we don't want to truncate the coefficient.
// shl is okay, but shr isn't.
public T shl(T)(const T arg, const int n,
		const Rounding rounding = T.rounding)  {

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
public T shift(T)(const T x, const int n //,
		/*const DecimalContext context = T.context*/)  {

	// check for NaN operand
	if (operandIsInvalid(x, x)) {
		return T.nan;
	}
	// can't shift more than precision
	if (n < -precision || n > precision) {
		return setInvalidFlag!T;
	}
	// shift by zero returns the argument
	if (n == 0) {
		return x;
	}
	// shift of an infinite number returns the argument
	if (x.isInfinite) {
		return x.dup;
	}

	Decimal result = x.dup; //toBigDecimal!T(x);
	if (n > 0) {
		shiftLeft(result.coefficient, n, precision);
	}
	else {
		shiftRight(result.coefficient, n, precision);
	}
	return T(result);
}

unittest {
	write("shift...");
	writeln("test missing");
}

/// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T)(const T arg, const int n/*,
		const DecimalContext context = T.context*/)  {

	// check for NaN operand
	if (operandIsInvalid!T(arg, result)) {
		return T.nan;
	}
	if (n < -precision || n > precision) {
		return setInvalidFlag();
	}
	if (arg.isInfinite) {
		return arg;
	}
	if (n == 0) {
		return arg;
	}

//	result = arg.dup;
	Decimal result = arg.dup;//toBigDecimal!T(arg);
	if (n > 0) {
		shiftLeft(result);
	}
	else {
		shiftRight(result);
	}
	return T(result);

//	return n < 0 ? decRotR!T(// (L)TODO: And then a miracle happens....

	return result;
}

unittest {
	write("shift, rotate...");
	writeln("test missing");
}

//------------------------------------------
// binary arithmetic operations
//------------------------------------------

/// Adds the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'add' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION, OVERFLOW.
/// If either operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T add(T)(in T x, in T y,
		in Context context = T.context)  {

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
	sum.isGuarded = (x.isGuarded || y.isGuarded);

	// round the result
	return roundToPrecision(sum, context.precision, context.rounding);
}	 // end add(x, y)


/// Adds a long value to a decimal number. The result is identical to that of
/// the 'add' function as if the long value were converted to a decimal number.
/// The result may be rounded and context flags may be set.
/// This function is not included in the specification.
/// Flags: INVALID_OPERATION, OVERFLOW.
/// If the decimal operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T add(T)(in T x, in long n,
		in Context context = T.context)  {

	// check for invalid operand(s)
	T nan;
	if (operandIsInvalid!T(x, nan)) return nan;

	// if decimal is infinite,
	if (x.isInfinite) return x.dup;

	T sum;
	// add(0, 0)
	if (x.isZero && n == 0) {
		sum = x;
		sum.exponent = std.algorithm.min(x.exponent, 0);
		sum.sign = x.sign && (n < 0);
		return sum;
	}
	// add(0,f)
	if (x.isZero) {
		sum = T(n);
		sum.exponent = std.algorithm.min(x.exponent, 0);
		// TODO: should round?
		return sum;
	}
	// add(f,0)
	if (n == 0) {
		sum = x;
		sum.exponent = std.algorithm.min(x.exponent, 0);
		// TODO: should round?
		return sum;
	}

	// at this point, the result will be finite and not zero.
	sum = T.zero;
	auto augend = x.dup;
	// TODO: this can be done without alignment if exponent of int is zero
	auto addend = T(n);
	// align the operands
	alignOps(augend, addend);
	// if operands have the same sign...
	if (augend.sign == addend.sign) {
		sum.coefficient = augend.coefficient + addend.coefficient;
		sum.sign = augend.sign;
	}
	// ...else operands have different signs
	else {
		if (augend.coefficient >= addend.coefficient) {
			sum.coefficient = augend.coefficient - addend.coefficient;
			sum.sign = augend.sign;
		}
		else {
			sum.coefficient = addend.coefficient - augend.coefficient;
			sum.sign = addend.sign;
		}
	}
	// set the number of digits and the exponent
	sum.digits = numDigits(sum.coefficient);
	sum.exponent = augend.exponent;
	sum.isGuarded = x.isGuarded;

	return roundToPrecision(sum, context.precision, context.rounding);
}	 // end add(x, n)

	// (A)TODO: change inputs to real numbers
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
	arg12.isGuarded = true;
writefln("arg12 = %s", arg12);
	arg22 = dec9("54321098765");
	arg22.isGuarded = true;
writefln("arg22 = %s", arg22);
	sum2 = add(arg12, arg22);
writefln("sum2 = %s", sum2);
	arg12.isGuarded(false);
	arg22.isGuarded(false);
	sum2 = add(arg12, arg22);
writefln("sum2 = %s", sum2);
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
/// If either operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T sub(T) (in T x, in T y,
		in Context context = T.context) {
	return add(x, copyNegate(y), context);
}	 // end sub(x, y)


/// Subtracts a long value from a decimal number.
/// The result is identical to that of the 'subtract' function
/// as if the long value were converted to a decimal number.
/// This function is not included in the specification.
/// If the decimal operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T sub(T) (in T x, in long n,	in Context context = T.context)  {
	return add(x, -n, context);
}	 // end sub(x, n)


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
/// If either operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T mul(T)(in T x, in T y, in Context context = T.context)  {

	T product;	// NaN
	// if invalid, return NaN
	if (operationIsInvalid!T(x, y, product)) {
		return product;
	}
	// infinity * zero => invalid operation
	if (x.isZero && y.isInfinite || x.isInfinite && y.isZero) {
		return product;
	}
	// if either operand is infinite, return infinity
	if (x.isInfinite || y.isInfinite) {
		product = T.INFINITY;
		product.sign = x.sign ^ y.sign;
		return product;
	}

	// product is a finite number, not NaN
	product = T.ZERO;
	// mul(0,f) or (f,0)
	if (x.isZero || y.isZero) {
		product.exponent = x.exponent + y.exponent;
		product.sign = x.sign ^ y.sign;
	}
	else {
		product.coefficient = x.coefficient * y.coefficient;
		product.exponent = x.exponent + y.exponent;
		product.sign = x.sign ^ y.sign;
		product.digits = numDigits(product.coefficient);
	}

	product.isGuarded = (x.isGuarded || y.isGuarded);
	return roundToPrecision(product, context.precision, context.rounding);
}

/// Multiplies a decimal number by a long integer.
/// The result may be rounded and context flags may be set.
/// Not a required function, but useful because it avoids
/// an unnecessary conversion to a decimal when multiplying.
/// If the decimal operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T mul(T)(in T x, long n, in Context context = T.context) {

	T nan;
	// if invalid, return NaN
	if (operandIsInvalid(x, nan)) {
		return nan;
	}
	// infinity * zero => invalid operation
	if (x.isInfinite && n == 0) {
		return nan;
	}
	T product = T.zero;
	// if decimal operand is infinite, return infinity
	if (x.isInfinite) {
		product = T.infinity;
		product.sign = x.sign ^ (n < 0);
		return product;
	}

	// mul(0,f) or (f,0)
	if (x.isZero || n == 0) {
		product = T.zero;
		product.exponent = x.exponent;
		product.sign = x.sign ^ (n < 0);
	}
	else {
		product.coefficient = x.coefficient * n;
		product.exponent = x.exponent;
		product.sign = x.sign ^ (n < 0);
		product.digits = numDigits(product.coefficient);
	}
	product.isGuarded = x.isGuarded;
	return roundToPrecision(product, context.precision, context.rounding);
}

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
writefln("result = %s", result);
	writeln("passed");
}

/// Squares the argument and returns the xx.
/// The result may be rounded and context flags may be set.
/// If the operand is guarded the result is guarded and is rounded
/// to the guarded precision.
public T sqr(T)(in T x,
		Context context = T.context)  {

	// if operand is invalid, return NaN
	T nan;
	if (operationIsInvalid(x, x, nan)) {
		return nan;
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
	T xx = T.zero;
	xx.coefficient = x.coefficient^^2;
	xx.exponent = 2 * x.exponent;
	xx.sign = false;
	xx.digits = numDigits(xx.coefficient);
	xx.isGuarded = (x.isGuarded);
	return roundToPrecision(xx, context.precision, context.rounding);
}

/// Multiplies the first two operands and adds the third operand to the result.
/// The result of the multiplication is not rounded prior to the addition.
/// The result may be rounded and context flags may be set.
/// Implements the 'fused-multiply-add' function in the specification. (p. 30)
public T fma(T)(in T x, in T y, in T z,
		Context context = T.context)  {
	T xy = mul(x, y, Context(context.precision, Rounding.NONE));
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
public T div(T)(in T x, in T y, in Context context = T.context)  {

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
//	// divisor may have become zero. Check again.
//	if (divisionIsInvalid!T(xx, yy, q)) {
//		return nan;
//	}
//writefln("xx.coefficient = %s", xx.coefficient);
//writefln("yy.coefficient = %s", yy.coefficient);
	q.coefficient = xx.coefficient / yy.coefficient;
	q.exponent = xx.exponent - yy.exponent;
	q.sign = xx.sign ^ yy.sign;
	q.digits = numDigits(q.coefficient);
	q.isGuarded = x.isGuarded || y.isGuarded;	// TODO: how does this affect reduction?
	q = roundToPrecision(q, context.precision, context.rounding);
	q = reduceToIdeal(q, diff);
	return q;
}

/**
 * Reduces operand to simplest form. All trailing zeros are removed.
 * Reduces operand to specified exponent.
 */
 // TODO: has non-standard flag setting
// NOTE: flags only
private T reduceToIdeal(T)(const T num, int ideal)  {

	T result = num.dup;
	if (!result.isFinite()) {
		return result;
	}
	int zeros = trailingZeros(result.coefficient, numDigits(result.coefficient));

	int idealshift = ideal - result.exponent;
	int	canshift = idealshift > zeros ? zeros : idealshift;
	result.coefficient = shiftRight(result.coefficient, canshift/*, T.precision*/);
	result.exponent = result.exponent + canshift;

	if (result.coefficient == 0) {
		result = T.ZERO;
	}
	result.digits = numDigits(result.coefficient);
	return result;
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

// TODO: Does this implement the actual spec operation?
/// Divides the first operand by the second and returns the integer portion
/// of the quotient.
/// Division by zero sets a flag and returns infinity.
/// The result may be rounded and context flags may be set.
/// Implements the 'divide-integer' function in the specification. (p. 30)
// TODO: guard?
public T divideInteger(T)(const T arg1, const T arg2)  {
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
// TODO: guard?
public T remainder(T)(const T arg1, const T arg2,
		in int precision = T.precision)  {
	T quotient;
	if (divisionIsInvalid!T(arg1, arg2, quotient)) {
		return quotient;
	}
	quotient = divideInteger!T(arg1, arg2,);
	T remainder = arg1 - mul!T(arg2, quotient, Context(precision, Rounding.NONE));
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
// TODO: guard?
public T remainderNear(T)(const T x, const T y)  {
	T quotient;
	if (divisionIsInvalid!T(x, y, quotient)) {
		return quotient;
	}
	quotient = x/y;
	// TODO: roundToIntegralValue?
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

// TODO: add 'remquo' function. (Uses remainder-near(?))

//--------------------------------
// rounding routines
//--------------------------------

/// Returns the number which is equal in value and sign
/// to the first operand with the exponent of the second operand.
/// The returned value is rounded to the current precision.
/// This operation may set the invalid-operation flag.
/// Implements the 'quantize' function in the specification. (p. 36-37)
public T quantize(T)(const T arg1, const T arg2, in Context context = T.context) {

	T nan;
	if (operationIsInvalid(arg1, arg2, nan)) return nan;

	// if one operand is infinite and the other is not...
	if (arg1.isInfinite != arg2.isInfinite()) {
		return setInvalidFlag!T;
	}
	// if both arguments are infinite
	if (arg1.isInfinite() && arg2.isInfinite()) {
		return arg1.dup;
	}
	T result = arg1.dup;
	int diff = arg1.exponent - arg2.exponent;

	if (diff == 0) {
		return result;
	}

	// TODO: this shift can cause integer overflow for fixed size decimals
	if (diff > 0) {
		result.coefficient = shiftLeft(result.coefficient, diff/*, precision*/);
		result.digits = result.digits + diff;
		result.exponent = arg2.exponent;
		if (result.digits > T.precision) {
			result = T.nan;
		}
		return result;
	}
	else {
		int precision = (-diff > arg1.digits) ? 0 : arg1.digits + diff;
		result = roundToPrecision(result, precision, context.rounding);
		result.exponent = arg2.exponent;
		if (result.isZero && arg1.isSigned) {
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
public T roundToIntegralExact(T)(in T x, in Rounding rounding = Rounding.HALF_EVEN) {
	T result = x.dup;
	if (result.isSignaling) return setInvalidFlag!T;
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	// TODO: need to prevent precision overrides
	int precision = result.digits + result.exponent;
if (T.verbose) writefln("result = %s", result.toExact);
if (T.verbose) writefln("precision = %s", precision);
	result = roundToPrecision(result, precision, rounding);
if (T.verbose) writefln("result = %s", result);
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

// TODO: need to re-implement this so no flags are set.
/// The result may be rounded and context flags may be set.
/// Implements the 'round-to-integral-value' function
/// in the specification. (p. 39)
public T roundToIntegralValue(T)(const T arg,
		const Rounding rounding = T.rounding)  {
	T result = arg.dup;
	if (result.isSignaling) return setInvalidFlag!T;
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	int precision = result.digits + result.exponent;
	result = roundToPrecision(result, context.precision, context.rounding);
	return result;
}

/// Aligns the two operands by raising the smaller exponent
/// to the value of the larger exponent, and adjusting the
/// coefficient so the value remains the same.
/// Both operands will have the same exponent on their return.
/// No flags are set and the result is not rounded.
private void alignOps(T)(ref T x, ref T y) {
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
public bool isLogical(T)(const T arg)  {
	if (arg.sign != 0 || arg.exponent != 0) return false;
	string str = arg.coefficient.toString;
	return isLogicalString(str);
}

/// Returns true and outputs a valid logical string if the argument is
/// a valid logical decimal number.
/// The sign and exponent must both be zero, and all decimal digits
/// in the coefficient must be either '1' or '0'.
private bool isLogicalOperand(T)(const T arg, out string str)  {
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
public T invert(T)(T arg)  {
	string str;
	if (!isLogicalOperand(arg, str)) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	return T(invert(str));
}

/// Inverts and returns a logical string.
/// Each '1' is changed to a '0', and vice versa.
private T invert(T: string)(T arg) {
	char[] result = new char[arg.length];
	for(int i = 0; i < arg.length; i++) {
		result[i] = arg[i] == '0' ? '1' : '0';
	}
	return result.idup;
}

unittest {	// inverse
 	write("-- logical inverse..");
	// TODO: why can't we compare ints and decimals?
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
private T opLogical(string op, T)(const T arg1, const T arg2/*,
		const DecimalContext context = T.context*/) /*if(isDecimal!T)*/ {
	int precision = T.precision;
	string str1;
	if (!isLogicalOperand(arg1, str1)) {
		return setInvalidFlag!T;
	}
	string str2;
	if (!isLogicalOperand(arg2, str2)) {
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
public T and(T)(const T arg1, const T arg2/*,
		const DecimalContext context = T.context*/)  {
	return opLogical!("and", T)(arg1, arg2/*, context*/);
}

/// Performs a logical 'and' of the (string) arguments and returns the result
T and(T: string)(const T arg1, const T arg2) {
	string str1, str2;
	int length, diff;
	if (arg1.length > arg2.length) {
		length = arg2.length;
		diff = arg1.length - arg2.length;
		str2 = arg1;
		str1 = rightJustify(arg2, arg1.length, '0');
	}
	else if (arg1.length < arg2.length) {
		length = arg1.length;
		diff = arg2.length - arg1.length;
		str1 = rightJustify(arg1, arg2.length, '0');
		str2 = arg2;
	} else {
		length = arg1.length;
		diff = 0;
		str1 = arg1;
		str2 = arg2;
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
public T or(T)(const T arg1, const T arg2/*,
		const DecimalContext context = T.context*/)  {
	return opLogical!("or", T)(arg1, arg2/*, context*/);
}

/// Performs a logical 'or' of the (string) arguments and returns the result
T or(T: string)(const T arg1, const T arg2) {
	string str1, str2;
	int length;
	if (arg1.length > arg2.length) {
		length = arg1.length;
		str1 = arg1;
		str2 = rightJustify(arg2, arg1.length, '0');
	}
	if (arg1.length < arg2.length) {
		length = arg2.length;
		str1 = rightJustify(arg1, arg2.length, '0');
		str2 = arg2;
	} else {
		length = arg1.length;
		str1 = arg1;
		str2 = arg2;
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
public T xor(T)(const T arg1, const T arg2/*,
		const DecimalContext context = T.context*/)  {
	return opLogical!("xor", T)(arg1, arg2/*, context*/);
}

/// Performs a logical 'xor' of the (string) arguments
/// and returns the result.
T xor(T: string)(const T arg1, const T arg2) {
	string str1, str2;
	int length;
	if (arg1.length > arg2.length) {
		length = arg1.length;
		str1 = arg1;
		str2 = rightJustify(arg2, arg1.length, '0');
	}
	if (arg1.length < arg2.length) {
		length = arg2.length;
		str1 = rightJustify(arg1, arg2.length, '0');
		str2 = arg2;
	} else {
		length = arg1.length;
		str1 = arg1;
		str2 = arg2;
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
	assert((op1 & op2) == dec9(10000100));
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
private T setInvalidFlag(T)(ushort payload = 0)  {
	contextFlags.setFlags(INVALID_OPERATION);
	T result = T.nan;
	if (payload != 0) {
		result.payload = payload;
	}
	return result;
}

unittest {	// setInvalidFlag
/*	dec9 arg, expect, actual;
	// TODO: Can't actually test payloads at this point.
	arg = dec9("sNaN123");
	expect = dec9("NaN123");
	actual = abs!dec9(arg);
	assertTrue(actual.isQuiet);
	assertTrue(contextFlags.getFlag(INVALID_OPERATION));
	assertEqual(actual.toAbstract, expect.toAbstract);*/
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
private bool operandIsInvalid(T)(const T arg, ref T result)  {
	// if the operand is a signaling NaN...
	if (arg.isSignaling) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// retain payload; convert to qNaN
		result = T.nan(arg.payload);
		return true;
	}
	// ...else if the operand is a quiet NaN...
	if (arg.isQuiet) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the result to the qNaN operand
		result = arg;
		return true;
	}
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
private bool divisionIsInvalid(T)(const T dividend, const T divisor,
		ref T quotient)  {

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
	// TODO: what purpose does this check serve?
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


