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

// TODO: move this to conv? distribute among types?
//--------------------------------
// classification functions
//--------------------------------

	/// Returns a string indicating the class and sign of the argument.
	/// Classes are: sNaN, NaN, Infinity, Zero, Normal, and Subnormal.
	/// The sign of any NaN values is ignored in the classification.
	/// The argument is not rounded and no flags are changed.
	/// Implements the 'class' function in the specification. (p. 42)
	public string classify(T)(const T arg)  {
		if (arg.isFinite) {
			if (arg.isZero) 	 { return arg.sign ? "-Zero" : "+Zero"; }
			if (arg.isNormal)	 { return arg.sign ? "-Normal" : "+Normal"; }
			if (arg.isSubnormal) { return arg.sign ? "-Subnormal" : "+Subnormal"; }
		}
		if (arg.isInfinite)  { return arg.sign ? "-Infinity" : "+Infinity"; }
		if (arg.isSignaling) { return "sNaN"; }
		return "NaN";
	}

	unittest {	// classify
		write("-- classify.........");
		dec9 arg;
		arg = dec9("Inf");
		assertEqual(classify(arg), "+Infinity");
		arg = dec9("1E-10");
		assertEqual(classify(arg), "+Normal");
		arg = dec9("-0");
		assertEqual(classify(arg), "-Zero");
		arg = dec9("-0.1E-99");
		assertEqual(classify(arg), "-Subnormal");
		arg = dec9("NaN");
		assertEqual(classify(arg), "NaN");
		arg = dec9("sNaN");
		assertEqual(classify(arg), "sNaN");
		writeln("passed");
	}

//--------------------------------
// copy functions
//--------------------------------

/// Returns a copy of the operand.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy' function in the specification. (p. 43)
//@safe
public T copy(T)(const T arg)  {
	return arg.dup;
}

/// Returns a copy of the operand with a positive sign.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy-abs' function in the specification. (p. 44)
//@safe
public T copyAbs(T)(const T arg)  {
	T copy = arg.dup;
	copy.sign = false;
	return copy;
}

/// Returns a copy of the operand with the sign inverted.
/// The copy is unaffected by context and is quiet -- no flags are changed.
/// Implements the 'copy-negate' function in the specification. (p. 44)
//@safe
public T copyNegate(T)(const T arg)  {
	T copy = arg.dup;
	copy.sign = !arg.sign;
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
public int ilogb(T)(const T arg) {

	T result = T.nan;

	if (invalidOperand!T(arg, result)) {
		return 0;
	}
	if (arg.isInfinite) {
		return int.max;
	}
	if (arg.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return int.min;
	}
	return arg.digits + arg.exponent - 1;
}

/// Returns the truncated base 10 logarithm of the argument.
/// "...The integer which is the exponent of the magnitude
/// of the most significant digit of the operand.
/// (As though the operand were truncated to a single digit
/// while maintaining the value of that digit and without
/// limiting the resulting exponent)".
/// May set the INVALID_OPERATION and DIVISION_BY_ZERO flags.
/// Implements the 'logb' function in the specification. (p. 47)
public T logb(T)(const T arg) {

	T result = T.nan;

	if (invalidOperand!T(arg, result)) {
		return result;
	}
	if (arg.isInfinite) {
		return T.infinity;
	}
	if (arg.isZero) {
		contextFlags.setFlags(DIVISION_BY_ZERO);
		return T.infinity(true);
	}
	int expo = arg.digits + arg.exponent - 1;
	return T(expo);
}

unittest {	// logb
	write("-- logb.............");
//	assert(logb(Decimal(250))    == Decimal(2));
//	assert(logb(Decimal("2.5"))  == Decimal(0));
//	assert(logb(Decimal("0.03")) == Decimal(-2));
	// TODO: this shouldn't be this complicated.
//	assert(logb(Decimal(0))   == Decimal(Decimal.infinity(true)));
	writeln("test missing");
}

/// If the first operand is infinite then that operand is returned,
/// otherwise the result is the first operand modified by
/// adding the value of the second operand to its exponent.
/// The second operand must be a finite integer with an exponent of zero.
/// The result may overflow or underflow.
/// Flags: INVALID_OPERATION, UNDERFLOW, OVERFLOW.
/// Implements the 'scaleb' function in the specification. (p. 48)
public T scaleb(T)(const T arg1, const T arg2)  {
	T result = T.nan;
	if (invalidBinaryOp!T(arg1, arg2, result)) {
		return result;
	}
	if (arg1.isInfinite) {
		return arg1.dup;
	}
	int expo = arg2.exponent;
	if (expo != 0 /* && not within range */) {
		result = setInvalidFlag!T();
		return result;
	}
	result = arg1;
	int scale = cast(int)arg2.coefficient.toInt;
	if (arg2.isSigned) {
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
	auto arg1 = dec9("7.50");
	auto arg2 = dec9("-2");
	expect = dec9("0.0750");
	actual = scaleb(arg1, arg2);
	assertEqual(actual, expect);
	writeln("passed");
}

//--------------------------------
// absolute value, unary plus and minus functions
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
public T reduce(T)(const T arg, int precision = T.precision,
		Rounding mode = contextMode)  {
	T reduced = plus(arg.dup, precision, mode);

	if (!reduced.isFinite()) {
		return reduced;
	}

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

// just a wrapper TODO: can we alias this? does that work?
public T normalize(T)(const T arg, int precision = T.precision,
		Rounding mode = contextMode)  {
	return reduce!T(arg, precision, mode);
}

unittest {	// reduce
	write("-- reduce...........");
	dec9 arg;
	dec9 expect, actual;
	arg = dec9("1.200");
	expect = dec9("1.2");
	actual = reduce(arg);
	assertNotEqual(arg.toString, expect.toString);
	assertStringEqual(actual, expect);
	arg = dec9("12.34");
	expect = dec9("1.234");
	actual = reduce(arg);
	assertStringEqual(actual, arg);
	assertStringNotEqual(arg, expect);
	assertStringNotEqual(actual, expect);
	writeln("passed");
}

/// Returns the absolute value of the argument.
/// This operation rounds the result and may set flags.
/// The result is equivalent to plus(arg) for positive numbers
/// and to minus(arg) for negative numbers.
/// To return the absolute value without rounding or setting flags
/// use the 'copyAbs' function.
/// Implements the 'abs' function in the specification. (p. 26)
/// Flags: INVALID_OPERATION
public T abs(T)(const T arg, int precision = T.precision,
		const Rounding mode = contextMode)  {
	T result = T.nan;
	if (invalidOperand!T(arg, result)) {
		return result;
	}
	result = copyAbs!T(arg);
	return roundToPrecision!T(result, precision, mode);
}

unittest {	// abs
	write("-- abs..............");
	dec9 arg;
	dec9 expect, actual;
	arg = dec9("-Inf");
	expect = dec9("Inf");
	actual = abs(arg);
	assertEqual(actual, expect);
	arg = 101.5;
	expect = 101.5;
	actual = abs(arg);
	assertEqual(actual, expect);
	arg = -101.5;
	actual = abs(arg);
	assertEqual(actual, expect);
	writeln("passed");
}

/// Returns the sign of the argument: -1, 0, -1.
/// If the argument is (signed or unsigned) zero, 0 is returned.
/// If the argument is negative, -1 is returned.
/// Otherwise +1 is returned.
/// This function is not required by the specification.
public int sgn(T)(const T arg)  {
	if (arg.isZero) return 0;
	return arg.isNegative ? -1 : 1;
}

unittest {	// sgn
	write("-- sgn..............");
	dec9 arg;
	arg = -123;
	assertEqual(sgn(arg), -1);
	arg = 2345;
	assertEqual(sgn(arg), 1);
	arg = dec9("0.0000");
	assertEqual(sgn(arg), 0);
	arg = dec9.infinity(true);
	assertEqual(sgn(arg), -1);
	xint big = -5;
	assertEqual(sgn(big), -1);
	writeln("passed");
}

/// Returns -1, 0, or 1
/// if the argument is negative, zero, or positive, respectively.
public int sgn(T:ExtendedInt)(const T num) {
//	if (num < 0) return -1;
//	if (num > 0) return 1;
	ExtendedInt big = num.dup;
	if (big < 0) return -1;
	if (big > 0) return 1;
	return 0;
}

/// Returns a copy of the argument with same sign as the argument.
/// This operation rounds the result and may set flags.
/// The result is equivalent to add('0', arg).
/// To copy without rounding or setting flags use the 'copy' function.
/// Implements the 'plus' function in the specification. (p. 33)
/// Flags: INVALID_OPERATION
public T plus(T)(const T arg, int precision = T.precision,
		Rounding mode = contextMode)  {
	T result = T.nan;
	if (invalidOperand!T(arg, result)) {
		return result;
	}
	result = arg;
/*writeln("plus +++++++++++++++");
writefln("arg = %s", arg);
writefln("precision = %s", precision);
writefln("mode = %s", mode);*/
	return roundToPrecision(result, precision, mode);
}

/// Returns a copy of the argument with the opposite sign.
/// This operation rounds the argument and may set flags.
/// Result is equivalent to subtract('0', arg).
/// To copy without rounding or setting flags use the 'copyNegate' function.
/// Implements the 'minus' function in the specification. (p. 37)
/// Flags: INVALID_OPERATION
public T minus(T)(const T arg, int toPrecision = T.precision,
		const Rounding mode = contextMode)  {
	T result = T.nan;
	if (invalidOperand!T(arg, result)) {
		return result;
	}
	result = copyNegate!T(arg);
	return roundToPrecision(result, toPrecision, mode);
}

unittest {	// plus
	write("-- plus, minus......");
	dec9 zero = dec9.zero;
	dec9 arg, expect, actual;
	arg = 1.3;
	expect = add(zero, arg);
	actual = plus(arg);
	assertEqual(actual, expect);
	arg = -1.3;
	expect = add(zero, arg);
	actual = plus(arg);
	assertEqual(actual, expect);
	// minus
	arg = 1.3;
	expect = sub(zero, arg);
	actual = minus(arg);
	assertEqual(actual, expect);
	arg = -1.3;
	expect = sub(zero, arg);
	actual = minus(arg);
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
public T nextPlus(T)(const T arg1,
		const Rounding mode = contextMode)  {
	T result = T.nan;
	if (invalidOperand!T(arg1, result)) {
		return result;
	}
	if (arg1.isInfinite) {
		if (arg1.sign) {
			return copyNegate!T(T.max);
		}
		else {
			return arg1.dup;
		}
	}
	int adjustedExpo = arg1.exponent + arg1.digits - T.precision;
	if (adjustedExpo < T.tinyExpo) {
			return T(0L, T.tinyExpo);
	}
	// (A)TODO: must add the increment w/o setting flags
	T arg2 = T(1L, adjustedExpo);
	result = add!T(arg1, arg2, mode);
	if (result > T.max) {
		result = T.infinity;
	}
	return result;
}

/// Returns the largest representable number that is smaller than
/// the argument.
/// Implements the 'next-minus' function in the specification. (p. 34)
/// Flags: INVALID_OPERATION
public T nextMinus(T)(const T arg,
		const Rounding mode = contextMode)  {

	T result = T.nan;
	if (invalidOperand!T(arg, result)) {
		return result;
	}
	if (arg.isInfinite) {
		if (!arg.sign) {
			return T.max.dup;
		}
		else {
			return arg.dup;
		}
	}
	// This is necessary to catch the special case where the coefficient == 1
	T reduced = reduce!T(arg, T.precision, mode);
	int adjustedExpo = reduced.exponent + reduced.digits - T.precision;
	if (arg.coefficient == 1) adjustedExpo--;
	if (adjustedExpo < T.tinyExpo) {
		return T(0L, T.tinyExpo);
	}
	T addend = T(1, adjustedExpo);
	result = sub!T(arg, addend, mode);	//(A)TODO: are the flags set/not set correctly?
		if (result < copyNegate(T.max)) {
		result = copyNegate(T.infinity);
	}
	return result;
}

/// Returns the representable number that is closest to the
/// first operand (but not the first operand) in the
/// direction toward the second operand.
/// Implements the 'next-toward' function in the specification. (p. 34-35)
/// Flags: INVALID_OPERATION
public T nextToward(T)(const T arg1, const T arg2,
		const Rounding mode = contextMode)  {
	T result = T.nan;
	if (invalidBinaryOp!T(arg1, arg2, result)) {
		return result;
	}
	// compare them but don't round
	int comp = compare!T(arg1, arg2, mode);
	if (comp < 0) return nextPlus!T(arg1, mode);
	if (comp > 0) return nextMinus!T(arg1, mode);
	result = copySign!T(arg1, arg2);
	return roundToPrecision(result, T.precision, mode);
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

/// Compares two operands numerically to the current precision.
/// Returns -1, 0, or +1 if the second operand is, respectively,
/// less than, equal to, or greater than the first operand.
/// Implements the 'compare' function in the specification. (p. 27)
/// Flags: INVALID_OPERATION
public int compare(T)(const T x, const T y, Rounding mode = contextMode)  {
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

	// if either is infinite...
	if (x.isInfinite || y.isInfinite) {
		return (x.isInfinite && y.isInfinite
			&& x.isSigned == y.isSigned);
	}

	// if signs differ, just compare the signs
	if (x.sign != y.sign) {
		// check for zeros: +0 and -0 are equal
		if (x.isZero && y.isZero) return 0;
		return x.sign ? -1 : 1;
	}

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
	dec9 arg1, arg2;
	arg1 = dec9(2.1);
	arg2 = dec9(3);
	assertEqual(compare(arg1, arg2), 1);
	arg1 = 2.1;
	arg2 = dec9(2.1);
	assertEqual(compare(arg1, arg2), 0);
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
public bool equals(T)(const T arg1, const T arg2, Rounding mode = contextMode)  {

	// any operation with a signaling NaN is invalid.
	if (arg1.isSignaling || arg2.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
		return false;
	}
	// if either is NaN...
	// NaN is never equal to any number, not even another NaN
	if (arg1.isNaN || arg2.isNaN) return false;

	// if either is infinite...
	if (arg1.isInfinite || arg2.isInfinite) {
		return (arg1.isInfinite && arg2.isInfinite && arg1.isSigned == arg2.isSigned);
	}

	// if either is zero...
	if (arg1.isZero || arg2.isZero) {
		return (arg1.isZero && arg2.isZero);
	}

	// if their signs differ...
	if (arg1.sign != arg2.sign) {
		return false;
	}

	// if they have the same representation, they are equal
	if (arg1.exponent == arg2.exponent && arg1.coefficient == arg2.coefficient) {
		return true;
	}

	// if they have different magnitudes, they are not equal
	int diff = (arg1.exponent + arg1.digits) - (arg2.exponent + arg2.digits);
	if (diff != 0) {
		return false;
	}

	// otherwise they are equal if they represent the same value
	return arg1.reduce.coefficient == arg2.reduce.coefficient;
}

unittest {	// equals
	write("-- equals...........");
	dec9 arg1, arg2;
	arg1 = 123.4567;
	arg2 = 123.4568;
	assertFalse(equals(arg1, arg2));
	arg2 = 123.4567;
	assertTrue(equals(arg1, arg2));
	writeln("passed");
}

/// Compares the numeric values of two numbers. CompareSignal is identical to
/// compare except that quiet NaNs are treated as if they were signaling.
/// This operation may set the invalid-operation flag.
/// Implements the 'compare-signal' function in the specification. (p. 27)
/// Flags: INVALID_OPERATION
public int compareSignal(T) (const T arg1, const T arg2,
		const Rounding mode = contextMode)  {

	// any operation with NaN is invalid.
	// if both are NaN, return as if arg1 > arg2.
	if (arg1.isNaN || arg2.isNaN) {
		contextFlags.setFlags(INVALID_OPERATION);
		return arg1.isNaN ? 1 : -1;
	}
	return (compare!T(arg1, arg2, context, mode));
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
public int compareTotal(T)(const T arg1, const T arg2)  {

	if (arg1.isFinite && arg2.isFinite
		&& arg1.sign == arg2.sign
		&& arg1.exponent == arg2.exponent
		&& arg1.coefficient == arg2.coefficient)
	return 0;

	int ret1 =	1;
	int ret2 = -1;

	// if signs differ...
	if (arg1.sign != arg2.sign) {
		return !arg1.sign ? ret1 : ret2;
	}

	// if both numbers are signed swap the return values
	if (arg1.sign) {
		ret1 = -1;
		ret2 =	1;
	}

	// if either is zero...
	if (arg1.isZero || arg2.isZero) {
		// if both are zero compare exponents
		if (arg1.isZero && arg2.isZero) {
			auto result = arg1.exponent - arg2.exponent;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return arg1.isZero ? ret1 : ret2;
	}

	// if either is infinite...
	if (arg1.isInfinite || arg2.isInfinite) {
		if (arg1.isInfinite && arg2.isInfinite) {
			return 0;
		}
		return arg1.isInfinite ? ret1 : ret2;
	}

	// if either is quiet...
	if (arg1.isQuiet || arg2.isQuiet) {
		// if both are quiet compare payloads.
		if (arg1.isQuiet && arg2.isQuiet) {
			auto result = arg1.payload - arg2.payload;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return arg1.isQuiet ? ret1 : ret2;
	}

	// if either is signaling...
	if (arg1.isSignaling || arg2.isSignaling) {
		// if both are signaling compare payloads.
		if (arg1.isSignaling && arg2.isSignaling) {
			auto result = arg1.payload - arg2.payload;
			if (result == 0) return 0;
			return (result > 0) ? ret1 : ret2;
		}
		return arg1.isSignaling ? ret1 : ret2;
	}

	// if both exponents are equal, any difference is in the coefficient
	if (arg1.exponent == arg2.exponent) {
		auto result = arg1.coefficient - arg2.coefficient;
		if (result == 0) return 0;
		return (result > 0) ? ret1 : ret2;
	}

	// if the (finite) numbers have different magnitudes...
	int diff = (arg1.exponent + arg1.digits) - (arg2.exponent + arg2.digits);
	if (diff > 0) return ret1;
	if (diff < 0) return ret2;

	// we know the numbers have the same magnitude --
	// compare coefficients
	auto result = xcompare(arg1.reduce.coefficient, arg2.reduce.coefficient);

	// if equal after alignment, compare the original exponents
	if (result == 0) {
		return (arg1.exponent > arg2.exponent) ? ret1 : ret2;
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
int compareTotalMagnitude(T)(const T arg1, const T arg2)  {
	return compareTotal(copyAbs(arg1), copyAbs(arg2));
}

unittest {
	write("-- compareSignal....");
	writeln("test missing");
}

unittest {	// compareTotal
	write("-- compareTotal.....");
	dec9 arg1, arg2;
	int result;
	arg1 = dec9("12.30");
	arg2 = dec9("12.3");
	result = compareTotal(arg1, arg2);
	assertEqual(result, -1);
	arg1 = dec9("12.30");
	arg2 = dec9("12.30");
	result = compareTotal(arg1, arg2);
	assertEqual(result, 0);
	arg1 = dec9("12.3");
	arg2 = dec9("12.300");
	result = compareTotal(arg1, arg2);
	assertEqual(result, 1);
	writeln("passed");
}

/// Returns true if the numbers have the same exponent.
/// If either operand is NaN or Infinity, returns true if and only if
/// both operands are NaN or Infinity, respectively.
/// No context flags are set.
/// Implements the 'same-quantum' function in the specification. (p. 48)
public bool sameQuantum(T)(const T arg1, const T arg2)  {
	if (arg1.isNaN || arg2.isNaN) {
		return arg1.isNaN && arg2.isNaN;
	}
	if (arg1.isInfinite || arg2.isInfinite) {
		return arg1.isInfinite && arg2.isInfinite;
	}
	return arg1.exponent == arg2.exponent;
}

unittest {	// sameQuantum
	write("-- sameQuantum......");
	dec9 arg1, arg2;
	arg1 = 2.17;
	arg2 = 0.001;
	assertFalse(sameQuantum(arg1, arg2));
	arg2 = 0.01;
	assertTrue(sameQuantum(arg1, arg2));
	arg2 = 0.1;
	assertFalse(sameQuantum(arg1, arg2));
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
const(T) max(T)(const T arg1, const T arg2, int precision = T.precision,
		const Rounding mode = contextMode)  {

	// if both are NaNs or either is an sNan, return NaN.
	if (arg1.isNaN && arg2.isNaN || arg1.isSignaling || arg2.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	// if both are infinite, return the one with a positive sign
	if (arg1.isInfinite && arg2.isInfinite) {
		return !arg1.isNegative ? arg1 : arg2;
	}

	// result will be a finite number or infinity
	// use arg1 as default value
	T result = arg1.dup;

	// if one op is a quiet NaN return the other
	if (arg1.isQuiet || arg2.isQuiet) {
		if (arg1.isQuiet) result = arg2;
	}
	// if the signs differ, return the unsigned operand
	else if (arg1.sign != arg2.sign) {
		if (arg1.sign) result = arg2;
	}
	else {
		// if not numerically equal, return the larger
		int comp = compare!T(arg1, arg2, mode);
		if (comp != 0) {
			if (comp < 0) result = arg2;
		}
		// if they have the same exponent they are identical, return either
		else if (arg1.exponent == arg2.exponent) {
			// no assignment -- use default value
		}
		// if they are non-negative, return the one with larger exponent.
		else if (arg1.sign == 0) {
			if (arg1.exponent < arg2.exponent) result = arg2;
		}
		else {
			// they are negative; return the one with smaller exponent.
			if (arg1.exponent > arg2.exponent) result = arg2;
		}
	}
	// result must be rounded
	return roundToPrecision(result, precision, mode);
}

unittest {	// max
	write("-- max..............");
	dec9 arg1, arg2;
	arg1 = 3; arg2 = 2;
	assertEqual(max(arg1, arg2), arg1);
	arg1 = -10; arg2 = 3;
	assertEqual(max(arg1, arg2), arg2);
	writeln("passed");
}

/// Returns the larger of the two operands (or NaN). Returns the same result
/// as the 'max' function if the signs of the operands are ignored.
/// Implements the 'max-magnitude' function in the specification. (p. 32)
/// Flags: NONE.
const(T) maxMagnitude(T)(const T arg1, const T arg2,
		int precision = T.precision, const Rounding mode = contextMode)  {
// FIXTHIS: special values...

	// both positive
	if (arg1 >= 0 && arg2 >= 0) {
		return max(arg1, arg2, precision, mode);
	}
	// both negative
	if (arg1 < 0 && arg2 < 0) {
		return min(arg1, arg2, precision, mode);
	}
	// one of each
	if (arg1.copyAbs > arg2.copyAbs) {
		return roundToPrecision(arg1, precision, mode);
	}
	return roundToPrecision(arg2, precision, mode);
}

unittest {	// max
	write("-- maxMagnitude.....");
	dec9 arg1, arg2;
	arg1 = -1; arg2 = -2;
	assertEqual(maxMagnitude(arg1, arg2), arg2);
	arg1 =  1; arg2 = -2;
	assertEqual(maxMagnitude(arg1, arg2), arg2);
	arg1 =  1; arg2 =  2;
	assertEqual(maxMagnitude(arg1, arg2), arg2);
	arg1 = -1; arg2 =  2;
	assertEqual(maxMagnitude(arg1, arg2), arg2);
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
const(T) min(T)(const T arg1, const T arg2, int precision = T.precision,
		const Rounding mode = contextMode)  {

	// if both are NaNs or either is an sNan, return NaN.
	if (arg1.isNaN && arg2.isNaN || arg1.isSignaling || arg2.isSignaling) {
		contextFlags.setFlags(INVALID_OPERATION);
		return T.nan;
	}
	// if both are infinite, return the one with a negative sign
	if (arg1.isInfinite && arg2.isInfinite) {
		return arg1.isNegative ? arg1 : arg2;
	}
	// result will be a finite number or infinity
	// use arg1 as default value
	T result = arg1.dup;

	// if one op is a quiet NaN return the other
	if (arg1.isQuiet || arg2.isQuiet) {
		if (arg1.isQuiet) result = arg2;
	}
    // if the signs differ, return the signed operand
	else if (arg1.sign != arg2.sign) {
		if (!arg1.sign) result = arg2;
	}
	// if not numerically equal, return the lesser
	else {
		int comp = compare(arg1, arg2, mode);
		if (comp != 0) {
			if (comp > 0) result = arg2;
		}
		// if they have the same exponent they are identical, return either
		else if (arg1.exponent == arg2.exponent) {
			// no assignment -- use default value
		}
		// if they are non-negative, return the one with smaller exponent.
		else if (arg1.sign == 0) {
			if (arg1.exponent > arg2.exponent) result = arg2;
		}
		else {
			// else they are negative; return the one with larger exponent.
			if (arg1.exponent > arg2.exponent) result = arg2;
		}
	}
	// result must be rounded
	return roundToPrecision(result, precision, mode);
}

unittest {
	write("-- min..............");
	dec9 arg1, arg2, expect, actual;
	arg1 = 3; arg2 = 2; expect = 2;
	actual = min(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = -3; arg2 = -2; expect = -3;
	actual = min(arg1, arg2);
	assertEqual(actual, expect);
	arg1 = -10; arg2 = 3; expect = -10;
	actual = min(arg1, arg2);
	assertEqual(actual, expect);
	writeln("passed");
}

/// Returns the smaller of the two operands (or NaN). Returns the same result
/// as the 'max' function if the signs of the operands are ignored.
/// Implements the 'min-magnitude' function in the specification. (p. 33)
/// Flags: INVALID OPERATION, ROUNDED.
const(T) minMagnitude(T)(const T arg1, const T arg2,
		int precision = T.precision, const Rounding mode = contextMode)  {
	return min(copyAbs!T(arg1), copyAbs!T(arg2), precision, mode);
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
public const (T) quantum(T)(const T arg)  {
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
		const Rounding mode = contextMode)  {

	T result = T.nan;
	if (invalidOperand(arg, result)) {
		return result;
	}
	result = arg;
	with (result) {
		coefficient = coefficient << n;
		digits = numDigits(coefficient);
	}
	return roundToPrecision(result, T.precision, mode);
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
	if (invalidOperand!Decimal(arg, result)) {
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

/// Shifts the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand shift the
/// first operand left (multiplying by tens). Negative values shift right
/// (dividing by tens). If the number is NaN, or if the shift value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'shift' function in the specification. (p. 49)
public T shift(T)(const T arg, const int n,
		const DecimalContext context = T.context)  {

	// check for NaN operand
	if (invalidOperand!T(arg, arg)) {
		return T.nan;
	}
	// can't shift more than precision
	if (n < -context.precision || n > context.precision) {
		return setInvalidFlag!T();
	}
	// shift by zero returns the argument
	if (n == 0) {
		return arg;
	}
	// shift of an infinite number returns the argument
	if (arg.isInfinite) {
		return arg.dup;
	}

	Decimal result = arg.dup; //toBigDecimal!T(arg);
	if (n > 0) {
		shiftLeft(result.coefficient, n, context.precision);
	}
	else {
		shiftRight(result.coefficient, n, context.precision);
	}
	return T(result);
}

unittest {
	write("shift...");
//	import decimal.dec32;
//    Dec32 num;
//	shift!Dec32(num, 4, num.context);
	writeln("test missing");
}

/// Rotates the first operand by the specified number of decimal digits.
/// (Not binary digits!) Positive values of the second operand rotate the
/// first operand left (multiplying by tens). Negative values rotate right
/// (divide by 10s). If the number is NaN, or if the rotate value is less
/// than -precision or greater than precision, an INVALID_OPERATION is signaled.
/// An infinite number is returned unchanged.
/// Implements the 'rotate' function in the specification. (p. 47-48)
public T rotate(T)(const T arg, const int n,
		const DecimalContext context = T.context)  {

	// check for NaN operand
	if (invalidOperand!T(arg, result)) {
		return T.nan;
	}
	if (n < -context.precision || n > context.precision) {
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
public T add(T)(const T arg1, const T arg2,
		Rounding mode = contextMode)  {

	T result;	 // sum is initialized to quiet NaN

	// check for NaN operand(s)
	if (invalidBinaryOp!T(arg1, arg2, result)) {
		return result;
	}
	// if both operands are infinite...
	if (arg1.isInfinite && arg2.isInfinite) {
		// if the signs differ return NaN and set invalid operation flag
		if (arg1.sign != arg2.sign) {
			return setInvalidFlag!T();
		}
		// both infinite with same sign, return the first
		return arg1.dup;
	}
	// if only the first is infinite, return it
	if (arg1.isInfinite) {
		return arg1.dup;
	}
	// if only the second is infinite, return it
	if (arg2.isInfinite) {
		return arg2.dup;
	}
	// add(0, 0)
	if (arg1.isZero && arg2.isZero) {
		result = arg1;
		// exponent is the smaller of the two exponents
		result.exponent = std.algorithm.min(arg1.exponent, arg2.exponent);
		// sign is logical and of the two signs
		result.sign = arg1.sign && arg2.sign;
		return result;
	}
	// add(0,f)
	if (arg1.isZero) {
		return arg2.dup;
	}
	// add(f,0)
	if (arg2.isZero) {
		return arg1.dup;
	}

	// at this point, the result will be finite and not zero.
	auto sum = T.zero;
	auto augend = arg1.dup;
	auto addend = arg2.dup;
	// align the operands
	alignOps(augend, addend);
	// if the operands have the same sign add the aligned coefficients
	if (augend.sign == addend.sign) {
		sum.coefficient = augend.coefficient + addend.coefficient;
		sum.sign = augend.sign;
	}
	// otherwise subtract the lesser from the greater
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

	// round the result
	return roundToPrecision(sum, T.precision, mode);
}	 // end add(arg1, arg2)


/// Adds a long value to a decimal number. The result is identical to that of
/// the 'add' function as if the long value were converted to a decimal number.
/// The result may be rounded and context flags may be set.
/// This function is not included in the specification.
/// Flags: INVALID_OPERATION, OVERFLOW.
public T addLong(T)(const T arg1, const long arg2,
		const Rounding mode = contextMode)  {
	T result = T.nan;	 // sum is initialized to quiet NaN

	// check for NaN operand(s)
	if (invalidOperand!T(arg1, result)) {
		return result;
	}
	// if both operands are infinite
	if (arg1.isInfinite) {
		// (+inf) + (-inf) => invalid operation
		if (arg1.sign != (arg2 < 0)) {
			return setInvalidFlag!T();
		}
		// both infinite with same sign
		return arg1.dup;
	}
	// only augend is infinite,
	if (arg1.isInfinite) {
		return arg1.dup;
	}
	// add(0, 0)
	if (arg1.isZero && arg2 == 0) {
		result = arg1;
		result.exponent = std.algorithm.min(arg1.exponent, 0);
		result.sign = arg1.sign && (arg2 < 0);
		return result;
	}
	// add(0,f)
	if (arg1.isZero) {
		result = T(arg2);
		result.exponent = std.algorithm.min(arg1.exponent, 0);
		return result;
	}
	// add(f,0)
	if (arg2 == 0) {
		result = arg1;
		result.exponent = std.algorithm.min(arg1.exponent, 0);
		return result;
	}

	// at this point, the result will be finite and not zero.
	auto sum = T.zero;
	auto augend = arg1.dup; //toBigDecimal!T(arg1);
	auto addend = T(arg2);
	// align the operands
	alignOps(augend, addend); //, context);
	// if operands have the same sign...
	if (augend.sign == addend.sign) {
		sum.coefficient = augend.coefficient + addend.coefficient;
		sum.sign = augend.sign;
	}
	// ...else operands have different signs
	else {
		if (augend.coefficient >= addend.coefficient)
		{
			sum.coefficient = augend.coefficient - addend.coefficient;
			sum.sign = augend.sign;
		}
		else
		{
			sum.coefficient = addend.coefficient - augend.coefficient;
			sum.sign = addend.sign;
		}
	}
	// set the number of digits and the exponent
	sum.digits = numDigits(sum.coefficient);
	sum.exponent = augend.exponent;

	result = T(sum);
	// round the result
	if (mode != Rounding.NONE) {
		return roundToPrecision(result, T.precision, mode);
	}
	return result;
}	 // end add(arg1, arg2)

	// (A)TODO: change inputs to real numbers
unittest {	// add, addLOng
	write("-- add..............");
	dec9 arg1, arg2, sum;
	arg1 = dec9("12");
	arg2 = dec9("7.00");
	sum = add(arg1, arg2, contextMode);
	assertStringEqual(sum, "19.00");
	arg1 = dec9("1E+2");
	arg2 = dec9("1E+4");
	sum = add(arg1, arg2, contextMode);
	assertStringEqual(sum, "1.01E+4");
	long arg3;
	arg3 = 12;
	arg1 = dec9("7.00");
	sum = addLong(arg1, arg3, contextMode);
	assertStringEqual(sum, "19.00");
	arg1 = dec9("1E+2");
	arg3 = 10000;
	sum = addLong(arg1, arg3, contextMode);
	assertStringEqual(sum, "10100");
	writeln("passed");
}

/// Subtracts the second operand from the first operand.
/// The result may be rounded and context flags may be set.
/// Implements the 'subtract' function in the specification. (p. 26)
public T sub(T) (const T arg1, const T arg2,
		Rounding mode = contextMode) {
	return add!T(arg1, copyNegate!T(arg2), mode);
}	 // end sub(arg1, arg2)


/// Subtracts a long value from a decimal number.
/// The result is identical to that of the 'subtract' function
/// as if the long value were converted to a decimal number.
/// This function is not included in the specification.
public T subLong(T) (const T arg1, const long arg2,
		const Rounding mode = contextMode)  {
	return addLong!T(arg1, -arg2, mode);
}	 // end sub(arg1, arg2)


unittest {
	write("-- subtract.........");
	dec9 arg1, arg2, diff;
	arg1 = dec9("1.3");
	arg2 = dec9("1.07");
	diff = sub(arg1, arg2, contextMode);
	assertStringEqual(diff, "0.23");
	arg2 = dec9("1.30");
	diff = sub(arg1, arg2, contextMode);
	assertStringEqual(diff, "0.00");
	arg2 = dec9("2.07");
	diff = sub(arg1, arg2, contextMode);
	assertStringEqual(diff, "-0.77");
	writeln("passed");
}

/// Multiplies the two operands.
/// The result may be rounded and context flags may be set.
/// Implements the 'multiply' function in the specification. (p. 33-34)
public T mul(T)(const T arg1, const T arg2,
		const Rounding mode = contextMode)  {

	T result = T.nan;
	// if invalid, return NaN
	if (invalidBinaryOp!T(arg1, arg2, result)) {
		return result;
	}
	// infinity * zero => invalid operation
	if (arg1.isZero && arg2.isInfinite || arg1.isInfinite && arg2.isZero) {
		return result;
	}
	// if either operand is infinite, return infinity
	if (arg1.isInfinite || arg2.isInfinite) {
		result = T.infinity;
		result.sign = arg1.sign ^ arg2.sign;
		return result;
	}

	// product is finite
	// mul(0,f) or (f,0)
	if (arg1.isZero || arg2.isZero) {
		result = T.zero;
		result.exponent = arg1.exponent + arg2.exponent;
		result.sign = arg1.sign ^ arg2.sign;
	}
	// product is non-zero
	else {
		auto product = T.zero;
		static if (is(T)) {
			product.coefficient = arg1.coefficient * arg2.coefficient;
		}
		else {
			product.coefficient = T.bigmul(arg1, arg2);
		}
		with (product) {
			exponent = arg1.exponent + arg2.exponent;
			sign = arg1.sign ^ arg2.sign;
			digits = numDigits(coefficient);
		}
		result = T(product);
	}

	// round if rounding flag is set
	if (mode != Rounding.NONE) {
		return roundToPrecision(result, T.precision, mode);
	}
	return result;
}

/// Multiplies a decimal number by a long integer.
/// The result may be rounded and context flags may be set.
/// Not a required function, but useful because it avoids
/// an unnecessary conversion to a decimal when multiplying.
public T mulLong(T)(const T arg1, long arg2,
		const Rounding mode = contextMode)  {

	T result = T.nan;
	// if invalid, return NaN
	if (invalidOperand!T(arg1, result)) {
		return result;
	}
	// infinity * zero => invalid operation
	if (arg1.isInfinite && arg2 == 0) {
		return result;
	}
	// if either operand is infinite, return infinity
	if (arg1.isInfinite) {
		result = T.infinity;
		result.sign = arg1.sign ^ (arg2 < 0);
		return result;
	}

	// product is finite
	// mul(0,f) or (f,0)
	if (arg1.isZero || arg2 == 0) {
		result = T.zero;
		result.exponent = arg1.exponent;
		result.sign = arg1.sign ^ (arg2 < 0);
	}
	// product is non-zero
	else {
		auto product = T.zero;
		with (product) {
			coefficient = arg1.coefficient * arg2;
			exponent = arg1.exponent;
			sign = arg1.sign ^ (arg2 < 0);
			digits = numDigits(coefficient);
		}
		result = T(product);
	}
	// only needs rounding if
	if (mode != Rounding.NONE) {
		return roundToPrecision(result, T.precision, mode);
	}
	return result;
}

unittest {	// mul
	write("-- multiply.........");
	dec9 arg1, arg2, result;
	arg1 = dec9("1.20");
	arg2 = 3;
	result = mul(arg1, arg2, contextMode);
	assertStringEqual(result, "3.60");
	arg1 = 7;
	result = mul(arg1, arg2, contextMode);
	assertStringEqual(result, "21");
	long arg3;
	arg1 = dec9("1.20");
	arg3 = 3;
	result = mulLong(arg1, arg3, contextMode);
	assertStringEqual(result, "3.60");
	arg1 = -7000;
	result = mulLong(arg1, arg3, contextMode);
	assertStringEqual(result, "-21000");
	writeln("passed");
}

/// Squares the argument and returns the result.
/// The result may be rounded and context flags may be set.
public T sqr(T)(const T arg,
		const Rounding mode = contextMode)  {

	T result = T.nan;
	// if invalid, return NaN
	if (invalidBinaryOp!T(arg, arg, result)) {
		return result;
	}
	// if operand is infinite, return infinity
	if (arg.isInfinite) {
		result = T.infinity;
		return result;
	}

	// if operand is zero, return zero
	if (arg.isZero) {
		result = T.zero;
		return result;
	}

	// product is non-zero
	else {
		T product = T.zero;
		product.coefficient = arg.coefficient^^2;
		product.exponent = 2 * arg.exponent;
		product.sign = false;
		product.digits = numDigits(product.coefficient);
		result = T(product);
	}

	// round if rounding flag is set
	if (mode != Rounding.NONE) {
		return roundToPrecision(result, T.precision, mode);
	}
	return result;
}

/// Multiplies the first two operands and adds the third operand to the result.
/// The result of the multiplication is not rounded prior to the addition.
/// The result may be rounded and context flags may be set.
/// Implements the 'fused-multiply-add' function in the specification. (p. 30)
public T fma(T)(const T arg1, const T arg2, const T arg3,
		const Rounding mode = contextMode)  {

	// (A)TODO: should these both be Decimal?
	T product = mul!T(arg1, arg2, Rounding.NONE);
	return add!T(product, arg3, mode);
}

unittest {	// fma
	write("-- fma..............");
	dec9 arg1, arg2, arg3, expect, actual;
	arg1 = 3; arg2 = 5; arg3 = 7;
	expect = 22;
	actual = (fma(arg1, arg2, arg3, contextMode));
	assertEqual(actual, expect);
	arg1 = 3; arg2 = -5; arg3 = 7;
	expect = -8;
	actual = (fma(arg1, arg2, arg3, contextMode));
	assertEqual(actual, expect);
	arg1 = 888565290;
	arg2 = 1557.96930;
	arg3 = -86087.7578;
	expect = dec9(1.38435736E+12);
	actual = (fma(arg1, arg2, arg3, contextMode));
	assertEqual(actual, expect);
	writeln("passed");
}

/// Divides the first operand by the second operand and returns their quotient.
/// Division by zero sets a flag and returns infinity.
/// Result may be rounded and context flags may be set.
/// Implements the 'divide' function in the specification. (p. 27-29)
public T div(T)(const T arg1, const T arg2, int precision = T.precision,
		const Rounding mode = contextMode)  {

	// check for NaN and division by zero
	T result;
	if (invalidDivision!T(arg1, arg2, result)) {
		return result;
	}
	// convert arguments to Decimal
	auto dividend = arg1.dup; //toBigDecimal!T(arg1);
	auto divisor	= arg2.dup; // toBigDecimal!T(arg2);
	auto quotient = T.zero;

	int diff = dividend.exponent - divisor.exponent;
	if (diff > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, diff);
		dividend.exponent = dividend.exponent - diff;
		dividend.digits = dividend.digits + diff;
	}
	int shift = 4 + T.precision + divisor.digits - dividend.digits;
	if (shift > 0) {
		dividend.coefficient = shiftLeft(dividend.coefficient, shift);
		dividend.exponent = dividend.exponent - shift;
		dividend.digits = dividend.digits + shift;
	}
/*	// divisor may have become zero. Check again.
	if (invalidDivision!T(dividend, divisor, quotient)) {
		return result;
	}*/
	quotient.coefficient = dividend.coefficient / divisor.coefficient;
	quotient.exponent = dividend.exponent - divisor.exponent;
	quotient.sign = dividend.sign ^ divisor.sign;
	quotient.digits = numDigits(quotient.coefficient);
	if (mode != Rounding.NONE) {
		quotient = roundToPrecision(quotient, precision, mode);
		quotient = reduceToIdeal(quotient, diff);
	}
	return T(quotient);
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
	result.coefficient = shiftRight(result.coefficient, canshift, T.precision);
	result.exponent = result.exponent + canshift;

	if (result.coefficient == 0) {
		result = T.zero;
		// TODO: needed?
		result.exponent = 0;
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
public T divideInteger(T)(const T arg1, const T arg2)  {
	// check for NaN or divide by zero
	T result = T.nan;
	if (invalidDivision!T(arg1, arg2, result)) {
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
		return setInvalidFlag!T();
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
public T remainder(T)(const T arg1, const T arg2)  {
	T quotient;
	if (invalidDivision!T(arg1, arg2, quotient)) {
		return quotient;
	}
	quotient = divideInteger!T(arg1, arg2,);
	T remainder = arg1 - mul!T(arg2, quotient, Rounding.NONE);
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
public T remainderNear(T)(const T dividend, const T divisor)  {
	T quotient;
	if (invalidDivision!T(dividend, divisor, quotient)) {
		return quotient;
	}
	quotient = dividend/divisor;
	// TODO: roundToIntegralValue?
	T remainder = dividend - divisor * (roundToIntegralExact(quotient));
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
public T quantize(T)(const T arg1, const T arg2, int precision = T.precision,
		const Rounding mode = contextMode)  {

	T result = T.nan;
	if (invalidBinaryOp!T(arg1, arg2, result)) {
		return result;
	}
	// if one operand is infinite and the other is not...
	if (arg1.isInfinite != arg2.isInfinite()) {
		return setInvalidFlag!T();
	}
	// if both arguments are infinite
	if (arg1.isInfinite() && arg2.isInfinite()) {
		return arg1.dup;
	}
	result = arg1;
	int diff = arg1.exponent - arg2.exponent;

	if (diff == 0) {
		return result;
	}

	// TODO: this shift can cause integer overflow for fixed size decimals
	if (diff > 0) {
		result.coefficient = shiftLeft(result.coefficient, diff, precision);
		result.digits = result.digits + diff;
		result.exponent = arg2.exponent;
		if (result.digits > T.precision) {
			result = T.nan;
		}
		return result;
	}
	else {
		precision = (-diff > arg1.digits) ? 0 : arg1.digits + diff;
		result = roundToPrecision!T(result, precision, mode);
		result.exponent = arg2.exponent;
		if (result.isZero && arg1.isSigned) {
			result.sign = true;
		}
		return result;
	}
}

unittest {	// quantize
	write("-- quantize.........");
	auto context = contextMode;
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
public T roundToIntegralExact(T)(const T arg,
		const Rounding mode = contextMode)  {
	T result = arg.dup;
	if (result.isSignaling) return setInvalidFlag!T();
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	int precision = result.digits + result.exponent;
	result = roundToPrecision(result, precision, mode);
	return result;
}

unittest { // roundToIntegralExact
 	write("-- roundToIntExact..");
	dec9 arg, expect, actual;
	arg = 2.1;
	expect = 2;
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = 0.7;
	expect = 1;
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = 100;
	expect = 100;
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = 101.5;
	expect = 102;
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = -101.5;
	expect = -102;
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = dec9("10E+5");
	expect = dec9("1.0E+6");
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = dec9("7.89E+77");
	expect = dec9("7.89E+77");
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	arg = dec9("-Inf");
	expect = dec9("-Infinity");
	actual = roundToIntegralExact(arg, contextMode);
	assertEqual(actual, expect);
	writeln("passed");
}

// TODO: need to re-implement this so no flags are set.
/// The result may be rounded and context flags may be set.
/// Implements the 'round-to-integral-value' function
/// in the specification. (p. 39)
public T roundToIntegralValue(T)(const T arg,
		const Rounding mode = contextMode)  {
	T result = arg.dup;
	if (result.isSignaling) return setInvalidFlag!T();
	if (result.isSpecial) return result;
	if (result.exponent >= 0) return result;

	int precision = result.digits + result.exponent;
	result = roundToPrecision(result, precision, mode);
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
private T opLogical(string op, T)(const T arg1, const T arg2,
		const DecimalContext context = T.context) if(isDecimal!T) {
	int precision = T.context.precision;
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
public T and(T)(const T arg1, const T arg2,
		const DecimalContext context = T.context)  {
	return opLogical!("and", T)(arg1, arg2, context);
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
public T or(T)(const T arg1, const T arg2,
		const DecimalContext context = T.context)  {
	return opLogical!("or", T)(arg1, arg2, context);
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
public T xor(T)(const T arg1, const T arg2,
		const DecimalContext context = T.context)  {
	return opLogical!("xor", T)(arg1, arg2, context);
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
/*	dec9 op1, op2;
	op1 = 10010101;
	op2 = 11100100;
//	assert((op1 & op2) == dec9(10000100));
//	assert((op1 | op2) == dec9(11110101));
//	assert((op1 ^ op2) == dec9( 1110001));
	op1 =   100101;
	op2 = 11100100;
//	assert((op1 & op2) == dec9(  100100));
//	assert((op1 | op2) == dec9(11100101));
//	assert((op1 ^ op2) == dec9(11000001));*/
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
	actual = abs!dec9(arg, contextMode);
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
private bool invalidOperand(T)(const T arg, ref T result)  {
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
	write("invalidBinaryOp...");
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
private bool invalidBinaryOp(T)(const T arg1, const T arg2, T result)
		 {
	// if either operand is a quiet NaN...
	if (arg1.isQuiet || arg2.isQuiet) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the result to the first qNaN operand
		result = arg1.isQuiet ? arg1 : arg2;
		return true;
	}
	// ...if either operand is a signaling NaN...
	if (arg1.isSignaling || arg2.isSignaling) {
		// flag the invalid operation
		contextFlags.setFlags(INVALID_OPERATION);
		// set the result to the first sNaN operand
		result = arg1.isSignaling ? T.nan(arg1.payload) : T.nan(arg2.payload);
		return true;
	}
	// ...otherwise, no flags are set and result is unchanged
	return false;
}

unittest {
	write("invalidOperand...");
	writeln("test missing");
}

/// Checks for invalid operands and division by zero.
/// If found, the function sets the quotient to NaN or infinity, respectively,
/// and returns true after setting the context flags.
/// Also checks for zero dividend and calculates the result as needed.
/// This is a helper function implementing checks for division by zero
/// and invalid operation in the specification. (p. 51-52)
private bool invalidDivision(T)(const T dividend, const T divisor,
		ref T quotient)  {

	if (invalidBinaryOp!T(dividend, divisor, quotient)) {
		return true;
	}
	if (divisor.isZero()) {
		if (dividend.isZero()) {
			quotient = setInvalidFlag!T();
		}
		else {
			contextFlags.setFlags(DIVISION_BY_ZERO);
			quotient = T.infinity;
			quotient.sign = dividend.sign ^ divisor.sign;
		}
		return true;
	}
	if (dividend.isZero()) {
		quotient = T.zero;
		return true;
	}
	return false;
}

unittest {
	write("invalidDivision...");
	writeln("test missing");
}

unittest {
	writeln("==========================");
	writeln("decimal arithmetic.....end");
	writeln("==========================");
}


