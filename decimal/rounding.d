<<<<<<< HEAD
// Written in the D programming language

/**
 * Rounding methods for floating-point decimal arithmetic.
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

module eris.decimal.rounding;

import eris.decimal;
import std.conv;
import std.string;
import std.traits;

///=============================
/// Summary of 484...
///
/// Description of 484...
///
/// Returns: result
/// Params:
///		arg = 	 T argument
///=============================


unittest {
	writeln("==========================");
	writeln("decimal rounding.....begin");
	writeln("==========================");
}

//import std.stdio;

version(unittest)
{
	import std.stdio;
//	import eris.decimal.asserts;
	import eris.decimal.test;
}

/**
 *  Rounds the number to its context precision.
 *  Returns the rounded number.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
public T roundToPrecision(T)(in T num) if (isDecimal!T)
{
	return roundToPrecision(num, T.context);
}

/**
 *  Rounds the number to the precision of the context parameter.
 *  if setFlags is false no context flags will be set by this operation.
 *  Returns the rounded number.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
package T roundToPrecision(T)(
		in T num, Context context = T.context, bool setFlags = true)
	if (isDecimal!T)
{
	return roundToPrecision(num,
		context.precision, context.maxExpo, context.mode, setFlags);
}

/**
 *  Rounds the number to the specified precision using the specified rounding mode.
 *  if setFlags is false none of the context flags will be set by this operation.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
//@safe
public T roundToPrecision(T)(in T num, int precision,
		int maxExpo = T.maxExpo, Rounding mode = T.mode, bool setFlags = true)
	if (isDecimal!T)
{
	// check for overflow before rounding and copy the input
	T copy = checkOverflow(num, mode, maxExpo, setFlags);

	// check rounding mode
	if (mode == ROUND_NONE) return copy;

	// special values aren't rounded
	if (!copy.isFinite) return copy;

	// smallest normalized exponent
	int minExpo = 1 - maxExpo;
	// smallest non-normalized exponent
	int tinyExpo = 1 - maxExpo - precision;

	// zero values aren't rounded, but they are checked for
	// subnormal and out of range exponents.
	if (num.isZero) {
		if (num.expo < minExpo) {
			if (setFlags) contextFlags.set(SUBNORMAL);
			if (num.expo < tinyExpo) {
				int temp = tinyExpo;
				copy.expo = tinyExpo;
			}
		}
		return copy;
	}

	// handle subnormal numbers
	if (num.isSubnormal()) {
		if (setFlags) contextFlags.set(SUBNORMAL);
		int diff = minExpo - cast(int)copy.adjustedExponent;
		// use the subnormal precision and round
		int subprecision = precision - diff;
		if (copy.digits > subprecision) {
			copy = roundByMode(copy, subprecision, mode, maxExpo, setFlags);
		}
		// if the result of rounding a subnormal is zero
		// the clamped flag is set. (Spec. p. 51)
		if (copy.isZero) {
			copy.expo = tinyExpo;
			if (setFlags) contextFlags.set(CLAMPED);
		}
		return copy;
	}

	// round the number
	return roundByMode(copy, precision, mode, maxExpo, setFlags);

} // end roundToPrecision()

unittest
{	// roundToPrecision
	static struct S { D64 x; int n; D64 expect; }
	S[] s =
	[
		{ "9999", 3, "1.00E+4" },
		{ "1234567890", 3, "1.23E+9" },
		{ "1234567890", 4, "1.235E+9" },
		{ "1234567890", 5, "1.2346E+9" },
		{ "1234567890", 6, "1.23457E+9" },
		{ "1234567890", 7, "1.234568E+9" },
		{ "1234567890", 8, "1.2345679E+9" },
		{ "1235",  3, "1.24E+3" },
		{ "12359", 3, "1.24E+4" },
		{ "1245",  3, "1.24E+3" },
		{ "12459", 3, "1.25E+4" },
	];
	auto f = FunctionTest!(S,D64)("roundPrec");
	foreach (t; s) f.test(t, roundToPrecision!D64(t.x,t.n));
    writefln(f.report);
}

//unittest {	// roundToPrecision
//	write("-- roundToPrecision.");
//	BigInt test = "18690473486004564289165545643685440097";
//	long  count = reduceDigits(test);
//	roundToPrecision(test);
		// TODO: (testing) test for subnormal as below...
/*	Dec32 a = Dec32(0.1);
	Dec32 b = Dec32.min * Dec32(8888888);
	assert("[0,8888888,-101]" == b.abstractForm);
	Dec32 c = a * b;
	assert("[0,888889,-101]" == c.abstractForm);
	Dec32 d = a * c;
	assert("[0,88889,-101]" == d.abstractForm);
	Dec32 e = a * d;
	assert("[0,8889,-101]" == e.abstractForm);
	Dec32 f = a * e;
	assert("[0,889,-101]" == f.abstractForm);
	Dec32 g = a * f;
	assert("[0,89,-101]" == g.abstractForm);
	Dec32 h = a * g;
	assert("[0,9,-101]" == h.abstractForm);
	Dec32 i = a * h;
	assert("[0,1,-101]" == i.abstractForm);*/
//	writeln("test missing");
//}

//--------------------------------
// private methods
//--------------------------------

// TODO: needs unittests
/**
 *  Returns true if the number is too large to be represented
 *  and adjusts the number according to the rounding mode.
 *  Implements the 'overflow' processing in the specification. (p. 53)
 *  Flags: OVERFLOW, ROUNDED, INEXACT.
 *  Precondition: number must be finite.
 */
//@safe
private T checkOverflow(T)(in T num, Rounding mode = T.mode,
		int maxExpo = T.maxExpo, bool setFlags = true) if (isDecimal!T)
{
	if (num.adjustedExponent <= maxExpo) return num;

	// TODO: if the number has not been normalized will this work?
	T copy = num.copy;
	switch (mode)
	{
		case ROUND_NONE: 	// can this branch be reached? should it be?
		case HALF_UP:
		case HALF_EVEN:
		case HALF_DOWN:
		case ROUND_UP:
			copy = T.infinity(num.sign);
			break;
		case ROUND_DOWN:
			copy = num.sign ? T.max.copyNegate : T.max;
			break;
		case ROUND_CEILING:
			copy = num.sign ? T.max.copyNegate : T.infinity;
			break;
		case ROUND_FLOOR:
			copy = num.sign ? T.infinity(true) : T.max;
			break;
		default:
			break;
	}
	if (setFlags) contextFlags.set(OVERFLOW | INEXACT | ROUNDED);
	return copy;
}


// TODO: needs unittests
// Returns true if the rounding mode is half-even, half-up, or half-down.
private bool halfRounding(Rounding mode) {
	return (mode == HALF_EVEN ||
	 		mode == HALF_UP ||
	 		mode == HALF_DOWN);
}

unittest
{
	assert(halfRounding(HALF_UP));
	assert(!halfRounding(ROUND_UP));
}

/**
 *  Rounds the number to the context precision
 *  using the specified rounding mode.
 */
private T roundByMode(T)(T num, int precision,
		Rounding mode = T.mode, int maxExpo = T.maxExpo, bool setFlags = true)
	if (isDecimal!T)
{
	T copy = checkOverflow(num, mode, maxExpo, setFlags);

	// did it overflow to infinity?
	if (copy.isSpecial) return copy;

	if (mode == ROUND_NONE) return num;

	// calculate the remainder
	T remainder = getRemainder(num, precision);
	// if the number wasn't rounded, return
	if (remainder.isZero) return num;

	// check for deleted leading zeros in the remainder.
	// makes a difference only in round-half modes.
	if ((mode == HALF_EVEN || mode == HALF_UP || mode == HALF_DOWN) &&
		countDigits(remainder.coff) != remainder.digits) {
		return num.copy;
	}

	switch (mode) {
		case ROUND_UP:
			incrementAndRound(num);
			break;
		case ROUND_DOWN:
			break;
		case ROUND_CEILING:
			if (!num.sign) incrementAndRound(num);
			break;
		case ROUND_FLOOR:
			if (num.sign) incrementAndRound(num);
			break;
		case HALF_UP:
			if (firstDigit(remainder.coff) >= 5)
				incrementAndRound(num);
			break;
		case HALF_DOWN:
			if (testFive(remainder.coff) > 0)
				incrementAndRound(num);
			break;
		case HALF_EVEN:
			switch (testFive(remainder.coff)) {
				case -1:
					break;
				case 1:
					incrementAndRound(num);
					break;
				default:
					if (lastDigit(num.coff) & 1) {
						incrementAndRound(num);
					}
					break;
				}
			break;
		default:
			break;
	}	// end switch (mode)
	return checkOverflow(num, mode, maxExpo, setFlags);
}	// end roundByMode()

unittest
{	// roundByMode
	static struct S { D64 x; int p; Rounding r; D64 expect; }
	S[] s =
	[
		{ "1000",    5, HALF_EVEN,  "1000" },
		{ "1000000", 5, HALF_EVEN,  "10000E+2" },
		{ "99999",   5, HALF_EVEN,  "99999" },
		{ "1234550", 5, HALF_EVEN,  "1.2346E+6" },
		{ "1234550", 5, ROUND_DOWN, "1.2345E+6" },
		{ "1234550", 5, ROUND_UP,   "1.2346E+6" },
	];
	auto f = FunctionTest!(S,D64)("roundByMode");
	foreach (t; s) f.test(t, roundByMode!D64(t.x,t.p,t.r));
    writefln(f.report);
}

/**
 *  Shortens the coefficient of the number to the specified precision,
 *  adjusts the exponent, and returns the (unsigned) remainder.
 *  If the number is already less than or equal to the precision, the
 *  number is unchanged and the remainder is zero.
 *  Otherwise the rounded flag is set, and if the remainder is not zero
 *  the inexact flag is also set.
 *  Flags: ROUNDED, INEXACT.
 */
private T getRemainder(T) (ref T x, int precision) if (isDecimal!T)
{
	T remainder = T.zero;
	int diff = x.digits - precision;
	if (diff <= 0) {
		return remainder;
	}
	contextFlags.set(ROUNDED);
	BigInt divisor = pow10b(diff);
	BigInt dividend = x.coff;
	BigInt quotient = dividend/divisor;
	auto cf = dividend - quotient*divisor;
	if (cf != 0) {
		remainder = T.zero;
		remainder.digits = diff;
		remainder.expo = x.expo;
		remainder.coff = cf;
		contextFlags.set(INEXACT);
	}
	x.coff = quotient;
	x.digits = countDigits(quotient); //precision;
	x.expo = x.expo + diff;
	return remainder;
}

unittest
{	// getRemainder
	static struct S { D64 x; int p; D64 expect; }
	S[] s =
	[
		{ 1234567890123456L, 5, "67890123456" },
	];
	auto f = FunctionTest!(S,D64)("getRemainder");
	foreach (t; s) f.test(t, getRemainder!D64(t.x,t.p));
    writefln(f.report);
}

/**
 *  Increments the coefficient by one.
 *  If this causes an overflow the coefficient is adjusted by clipping
 *  the last digit (it will be zero) and incrementing the exponent.
 */
private void incrementAndRound(T)(ref T num) if (isDecimal!T)
{
	// if num is zero
	if (num.digits == 0) {
		num.coff = 1;
		num.digits = 1;
		return;
	}
	num.coff = num.coff + 1;
	// TODO: (efficiency) is there a less expensive test?
	if (lastDigit(num.coff) == 0) {
		if (num.coff / pow10b(num.digits) > 0) {
			num.coff = num.coff / 10;
			num.expo = num.expo + 1;
		}
	}
}

/*unittest
{	// incrementAndRound
	static struct S { D64 x; int p; D64 expect; }
	S[] s =
	[
		{ 1234567890123456L, 5, "67890123456" },
	];
	auto f = FunctionTest!(S,D64)("incrementAndRound");
	foreach (t; s) f.test(t, incrementAndRound!D64(t.x,t.p));
    writefln(f.report);
}*/

unittest {	// increment
	write("-- increment&round..");
	D64 actual, expect;
	actual = 10;
	expect = 11;
	incrementAndRound(actual);
	assertEqual(actual, expect);
	actual = 19;
	expect = 20;
	incrementAndRound(actual);
	assertEqual(actual, expect);
	actual = 999;
	expect = "1.00E+3";
	incrementAndRound(actual);
	assertEqual(actual, expect);
	writeln("passed");
}

/**
 *  Returns -1, 1, or 0 if the remainder is less than, more than,
 *  or exactly half the least significant digit of the shortened coefficient.
 *  Exactly half is a five followed by zero or more zero digits.
 */
private int testFive(in BigInt x)
{
	int num = countDigits(x);
	BigInt btens = pow10b(num - 1);
	int first = cast(int)(x / btens);
	if (first < 5) return -1;
	if (first > 5) return +1;
	BigInt zeros = x % btens;
	return zeros ? 1 : 0;
}

unittest
{	// testFive
	static struct S { BigInt x; int expect; }
	S[] s =
	[
		{ "5000000000000000000000",  0 },
		{ "4999999999999999999999", -1 },
		{ "50000000000000000000000000000000000000000000000001", 1 },
	];
	auto f = FunctionTest!(S,int)("testFive");
	foreach (t; s) f.test(t, testFive(t.x));
    writefln(f.report);
}

//-----------------------------
// useful constants
//-----------------------------

/*private enum BigInt BIG_ZERO = BigInt(0);
private enum BigInt BIG_ONE  = BigInt(1);
private enum BigInt BIG_FIVE = BigInt(5);
private enum BigInt BIG_TEN  = BigInt(10);
private enum BigInt BILLION  = BigInt(1_000_000_000);
private enum BigInt QUINTILLION  = BigInt(1_000_000_000_000_000_000);*/


///  The maximum number of decimal digits that fit in an int value.
//public enum int MAX_INT_DIGITS = 9;
///  The maximum decimal value that fits in an int.
//public enum uint MAX_DECIMAL_INT = 999999999U;
///  The maximum number of decimal digits that fit in a long value.
public enum int MAX_LONG_DIGITS = 18;
///  The maximum decimal value that fits in a long value.
public enum ulong MAX_DECIMAL_LONG = 10UL^^MAX_LONG_DIGITS - 1;

//-----------------------------
// BigInt digit functions
//-----------------------------

/**
 *  An array of unsigned long integers with values of
 *  powers of ten from 10^^0 to 10^^19
 */
private enum ulong[20] pow10UL = [10UL^^0,
		10UL^^1,  10UL^^2,  10UL^^3,  10UL^^4,  10UL^^5,  10UL^^6,
		10UL^^7,  10UL^^8,  10UL^^9,  10UL^^10, 10UL^^11, 10UL^^12,
		10UL^^13, 10UL^^14, 10UL^^15, 10UL^^16, 10UL^^17, 10UL^^18, 10UL^^19];

/**
 * Returns the number of decimal digits in a non-negative big integer
 */
public int countDigits(T)(in T m)
	if (is(T == BigInt) || isUnsigned!T)
{
	if (m == 0)
	{
		return 0;
	}

	static if (is(T == BigInt))
	{
		if (m.ulongLength == 1)
		{
			return countDigits(m.getDigit(0));
		}

		int count;
		ulong n = clipDigits(m, count);
		return count + countDigits(n);
	}
	else
	{
		auto n = cast(ulong)m;
		// special cases:
		if (n == 0) return 0;
		if (n < 10) return 1;
		if (n >= pow10UL[19]) return 20;
		// use a binary search to count the digits
		int min = 2;
		int max = 19;
		while (min <= max)
		{
			int mid = (min + max)/2;
			if (n < pow10UL[mid])
			{
				max = mid - 1;
			}
			else
			{
				min = mid + 1;
			}
		}
		return min;
	}
}

///
unittest // countDigits
{
	auto big = BigInt(
		"1234567890_1234567890_1234567890_1234567890_1234567890" ~
		"1234567890_1234567890_1234567890_1234567890_1234567890_1");
 	assert(countDigits(big) == 101);
}


unittest
{	// countDigits
	static struct S { BigInt n; int expect; }
	S[] s =
	[
		{ "123456", 6 },
		{ "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901", 101 },
		{ "1234567890123456789012345678901234567890123456789012", 52 },
	];
	auto f = FunctionTest!(S,int)("countDigits");
	foreach (t; s) f.test(t, countDigits(t.n));
    writefln(f.report);
}

unittest
{	// countDigits
	static struct S { ulong n; int expect; }
	S[] s =
	[
		{				  7,  1 },
		{				 13,  2 },
		{				999,  3 },
		{			   9999,  4 },
		{			  25978,  5 },
		{			2008617,  7 },
		{		 1234567890, 10 },
		{		10000000000, 11 },
		{	123456789012345, 15 },
		{  1234567890123456, 16 },
		{123456789012345678, 18 },
		{		   long.max, 19 },
		{		  ulong.max, 20 },
		{		  ulong.min,  0 },
	];
	auto f = FunctionTest!(S,int)("countDigits");
	foreach (t; s) f.test(t, countDigits(t.n));
    writefln(f.report);
}


/// Clips decimal digits until a single ulong digit is left.
private ulong clipDigits(in BigInt big, out int count)
{
	if (big <= ulong.max) return cast(ulong)big;

	enum BigInt tenQuint = 10UL^^19;
	count = 0;
	BigInt quotient = big;
	while (quotient > tenQuint) {
		quotient /= tenQuint;
		count += 19;
	}
	return cast(ulong)quotient;
}

unittest // clipDigits
{
	int count;
	BigInt big;
	big = BigInt("123456");
	assert(clipDigits(big, count) == 123456);
	assert(clipDigits(BigInt(ulong.max), count) == ulong.max);
	big = BigInt("1234567890_1234567890_1234567890_1234567890_1234567890");
	assert(clipDigits(big, count) == 123456789012);
}

/// returns the first decimal digit of the number
public uint firstDigit(T)(T n)
	if (is(T == BigInt) || isUnsigned!T)
{
	static if (is(T == BigInt))
	{
		int count;
		return firstDigit(clipDigits(n,count));
	}
	else
	{
		if (n == 0) return 0;
		if (n < 10) return cast(int) n;
		int digits = countDigits(n);
		return cast(uint)(n / pow10UL[digits-1]);
	}
}

unittest
{
	assert(firstDigit(BigInt("8234567890123456789012345678901234567890123")) == 8);
	assert(firstDigit(0U) == 0);
	assert(firstDigit(7U) == 7);
	assert(firstDigit(9999UL) == 9);
	assert(firstDigit(uint.max) == 4);
	assert(firstDigit(ulong.max) == 1);
}

unittest	// move to regression testing
{	// firstDigit(BigInt)
	static struct S { BigInt n; uint expect; }
	S[] s =
	[
		{ "5000000000000000000000", 5 },
		{ "45500000001209854300023400000000000000", 4 },
		{ "82345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678905", 8 },
	];
	auto f = FunctionTest!(S,uint)("1stDigit(b)");
	foreach (t; s) f.test(t, firstDigit(t.n));
    writefln(f.report);
}

unittest
{	// firstDigit(ulong)
	static struct S { ulong n; uint expect; }
	S[] s =
	[
		{ 0, 0 },
		{ 7, 7 },
		{ 13, 1 },
		{ 999, 9 },
		{ 9999, 9 },
		{ 25987, 2 },
		{ 5008617, 5 },
		{ 3234567890, 3 },
		{ 10000000000, 1 },
		{ 823456789012345, 8 },
		{ 4234567890123456, 4 },
		{ 623456789012345678, 6 },
		{long.max, 9 }
	];
	auto f = FunctionTest!(S,uint)("1stDigit(u)");
	foreach (t; s) f.test(t, firstDigit(t.n));
    writefln(f.report);
}

// TODO: move to arithmetic, templatize
/**
 *  Decimal shift left.
 *  Shifts the number left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is shifted right.
 */
public BigInt shiftLeft(BigInt num, int n)
{
	// NOTE: simply multiplies by 10^^n.
	if (n > 0) {
		num *= pow10b(n);
	}
	if (n < 0) {
		num = shiftRight(num, -n);
	}
	return num;
}


/+unittest {	// shiftLeft(BigInt)
	BigInt m;
	int n;
	m = 12345;
	n = 2;
	assert(shiftLeft(m, n/*, 100*/) == 1234500);
	m = 1234567890;
	n = 7;
	assert(shiftLeft(m, n/*, 100*/) == BigInt(12345678900000000));
	m = 12;
	n = 2;
	assert(shiftLeft(m, n/*, 100*/) == 1200);
	m = 12;
	n = 4;
	assert(shiftLeft(m, n/*, 100*/) == 120000);
	uint k;
/*	k = 12345;
	n = 2;
	assert(1234500 == cast(uint)shiftLeft(k, n, 9));
	k = 1234567890;
	n = 7;
	assert(900000000 == cast(uint)shiftLeft(k, n, 9));
	k = 12;
	n = 2;
	assert(1200 == cast(uint)shiftLeft(k, n, 9));
	k = 12;
	n = 4;
	assert(120000 == cast(uint)shiftLeft(k, n, 9));*/
}+/

/**
 *  Shifts the number to the left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is shifted to the right.
 */
/*public ulong shiftLeft(ulong num, const int n,
		const int precision = MAX_LONG_DIGITS) {
	if (n > precision) return 0;
	if (n > 0) {
		// may need to clipDigits before shifting
		int m = countDigits(num);
		int diff = precision - m - n;
		if (diff < 0 ) {
			num %= cast(ulong)ltens[m + diff];
		}
		ulong scale = cast(ulong)ltens[n];
		num *= scale;
	}
	if (n < 0) {
		num = shiftRight(num, -n, precision);
	}
	return num;
}*/

/**
 *  Decimal shift right.
 *  Shifts the number right the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is shifted left.
 */
public BigInt shiftRight(BigInt num, int n)
{
	// NOTE: simply divides by 10^^n.
	if (n > 0)
	{
		num /= pow10b(n);
	}
	if (n < 0) {
		num = shiftLeft(num, -n);
	}
	return num;
}

/*unittest {
	write("shiftRight...");
	BigInt num;
	BigInt res;
	num = BigInt("9223372036854775807");
	res = shiftRight(num, 10);
	assertEqual!BigInt(res, BigInt("922337203"));

	num = BigInt("9223372036854775808");
	res = shiftRight(num, 10);
	assertEqual!BigInt(res, BigInt("922337203"));
	writeln("passed");
}*/


/+
 *  Rotates the number to the left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is rotated to the right.
public ulong rotateLeft(ulong num, const int n, const int precision) {
	if (n > precision) return 0;
	if (n > 0) {
		int m = precision - n;
		ulong rem = num / ltens[m];
		num %= ltens[m];
		num *= ltens[n];
		num += rem;
	}
	if (n < 0) {
		num = rotateRight(num, precision, -n);
	}
	return num;
}

unittest {
	writeln("rotateLeft...");
	ulong num = 1234567;
writeln("num = ", num);
	ulong rot = rotateLeft(num, 7, 2);
writeln("rot = ", rot);
	writeln("test missing");
}

 *  Rotates the number to the right by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is rotated to the left. // TODO: (behavior) should throw.
public ulong rotateRight(ulong num, const int n, const int precision) {
	if (n > precision) return 0;
	if (n == precision) return num;
	if (n > 0) {
		int m = precision - n;
		ulong rem = num / ltens[n];
		num %= ltens[n];
		num *= ltens[m];
		num += rem;
	}
	if (n < 0) {
		num = rotateLeft(num, precision, -n);
	}
	return num;
}

unittest {
	writeln("rotateRight...");
	ulong num = 1234567;
writeln("num = ", num);
	ulong rot = rotateRight(num, 7, 2);
writeln("rot = ", rot);
	 rot = rotateRight(num, 9, 2);
writeln("rot = ", rot);
	 rot = rotateRight(num, 7, -2);
writeln("rot = ", rot);
	 rot = rotateRight(num, 7, 7);
writeln("rot = ", rot);
	writeln("test missing");
}
+/

// TODO: split into BigInt, ulong?
///  Returns the last digit of the argument.
public int lastDigit(in BigInt big)
{
	auto digit = big % BigInt(10);
	if (digit < 0) digit = -digit;
	return digit.toInt;
}

unittest
{	// lastDigit
	static struct S { BigInt n; int expect; }
	S[] s =
	[
		{  7, 7 },
		{ -13, 3 },
		{  999, 9 },
		{ -9999, 9 },
		{  25987, 7 },
		{ -5008615, 5 },
		{  3234567893, 3 },
		{ -10000000000, 0 },
		{  823456789012348, 8 },
		{  4234567890123456, 6 },
		{  623456789012345674, 4 },
		{  long.max, 7 },
	];
	auto f = FunctionTest!(S,int)("lastDigit");
	foreach (t; s) f.test(t, lastDigit(t.n));
    writefln(f.report);
}

///  Returns the number of trailing zeros in the argument.
public int trailingZeros(BigInt n, int digits)
{
	// shortcuts for frequent values
	if (n ==  0) return 0;
	if (n %  10) return 0;
	if (n % 100) return 1;
	// find by binary search
	int min = 3;
	int max =  digits - 1;
	while (min <= max) {
		int mid = (min + max)/2;
		if (n % pow10b(mid) != 0)
		{
			max = mid - 1;
		}
		else
		{
			min = mid + 1;
		}
	}
	return max;
}

///  Trims any trailing zeros and returns the number of zeros trimmed.
public int trimZeros(ref BigInt n, int digits)
{
	int zeros = trailingZeros(n, digits);
	if (zeros == 0) return 0;
	n /= pow10b(zeros);
	return zeros;
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
 *  Standard: Implements the 'reduce' function in the specification. (p. 37)
 *  "This operation was called 'normalize' prior to
 *  version 1.68 of the specification." (p. 37)
 *
 *  Flags: INVALID_OPERATION
 *
 */
public T reduce(T)(in T num,
		Context context = T.context) if (isDecimal!T)
{
	// special cases
	if (num.isNaN) return T.nan; //invalidOperand(num);
	if (!num.isFinite) return num.dup;

	// round the argument
	T reduced = roundToPrecision(num, context);

	// have to check again -- rounding may have made it infinite
	if (!reduced.isFinite) return reduced;

	int digits = reduced.digits;
	auto temp = reduced.coff;
	int zeros = trimZeros(temp, digits);
	if (zeros)
	{
		reduced.coff = temp;
		reduced.digits = digits - zeros;
		reduced.expo = reduced.expo + zeros;
	}

	return reduced;
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


unittest {
	writeln("==========================");
	writeln("decimal rounding.......end");
	writeln("==========================");
}

=======
// Written in the D programming language

/**
 * Rounding methods for floating-point decimal arithmetic.
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

module eris.decimal.rounding;

import eris.decimal;
import std.conv;
import std.string;
import std.traits;

///=============================
/// Summary of 484...
///
/// Description of 484...
///
/// Returns: result
/// Params:
///		arg = 	 T argument
///=============================


unittest {
	writeln("==========================");
	writeln("decimal rounding.....begin");
	writeln("==========================");
}

//import std.stdio;

version(unittest)
{
	import std.stdio;
//	import eris.decimal.asserts;
	import eris.decimal.test;
}

/**
 *  Rounds the number to its context precision.
 *  Returns the rounded number.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
public T roundToPrecision(T)(in T num) if (isDecimal!T)
{
	return roundToPrecision(num, T.context);
}

/**
 *  Rounds the number to the precision of the context parameter.
 *  if setFlags is false no context flags will be set by this operation.
 *  Returns the rounded number.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
package T roundToPrecision(T)(
		in T num, Context context = T.context, bool setFlags = true)
	if (isDecimal!T)
{
	return roundToPrecision(num,
		context.precision, context.maxExpo, context.mode, setFlags);
}

/**
 *  Rounds the number to the specified precision using the specified rounding mode.
 *  if setFlags is false none of the context flags will be set by this operation.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
//@safe
public T roundToPrecision(T)(in T num, int precision,
		int maxExpo = T.maxExpo, Rounding mode = T.mode, bool setFlags = true)
	if (isDecimal!T)
{
	// check for overflow before rounding and copy the input
	T copy = checkOverflow(num, mode, maxExpo, setFlags);

	// check rounding mode
	if (mode == ROUND_NONE) return copy;

	// special values aren't rounded
	if (!copy.isFinite) return copy;

	// smallest normalized exponent
	int minExpo = 1 - maxExpo;
	// smallest non-normalized exponent
	int tinyExpo = 1 - maxExpo - precision;

	// zero values aren't rounded, but they are checked for
	// subnormal and out of range exponents.
	if (num.isZero) {
		if (num.expo < minExpo) {
			if (setFlags) contextFlags.set(SUBNORMAL);
			if (num.expo < tinyExpo) {
				int temp = tinyExpo;
				copy.expo = tinyExpo;
			}
		}
		return copy;
	}

	// handle subnormal numbers
	if (num.isSubnormal()) {
		if (setFlags) contextFlags.set(SUBNORMAL);
		int diff = minExpo - cast(int)copy.adjustedExponent;
		// use the subnormal precision and round
		int subprecision = precision - diff;
		if (copy.digits > subprecision) {
			copy = roundByMode(copy, subprecision, mode, maxExpo, setFlags);
		}
		// if the result of rounding a subnormal is zero
		// the clamped flag is set. (Spec. p. 51)
		if (copy.isZero) {
			copy.expo = tinyExpo;
			if (setFlags) contextFlags.set(CLAMPED);
		}
		return copy;
	}

	// round the number
	return roundByMode(copy, precision, mode, maxExpo, setFlags);

} // end roundToPrecision()

unittest
{	// roundToPrecision
	static struct S { D64 x; int n; D64 expect; }
	S[] s =
	[
		{ "9999", 3, "1.00E+4" },
		{ "1234567890", 3, "1.23E+9" },
		{ "1234567890", 4, "1.235E+9" },
		{ "1234567890", 5, "1.2346E+9" },
		{ "1234567890", 6, "1.23457E+9" },
		{ "1234567890", 7, "1.234568E+9" },
		{ "1234567890", 8, "1.2345679E+9" },
		{ "1235",  3, "1.24E+3" },
		{ "12359", 3, "1.24E+4" },
		{ "1245",  3, "1.24E+3" },
		{ "12459", 3, "1.25E+4" },
	];
	auto f = FunctionTest!(S,D64)("roundPrec");
	foreach (t; s) f.test(t, roundToPrecision!D64(t.x,t.n));
    writefln(f.report);
}

//unittest {	// roundToPrecision
//	write("-- roundToPrecision.");
//	BigInt test = "18690473486004564289165545643685440097";
//	long  count = reduceDigits(test);
//	roundToPrecision(test);
		// TODO: (testing) test for subnormal as below...
/*	Dec32 a = Dec32(0.1);
	Dec32 b = Dec32.min * Dec32(8888888);
	assert("[0,8888888,-101]" == b.abstractForm);
	Dec32 c = a * b;
	assert("[0,888889,-101]" == c.abstractForm);
	Dec32 d = a * c;
	assert("[0,88889,-101]" == d.abstractForm);
	Dec32 e = a * d;
	assert("[0,8889,-101]" == e.abstractForm);
	Dec32 f = a * e;
	assert("[0,889,-101]" == f.abstractForm);
	Dec32 g = a * f;
	assert("[0,89,-101]" == g.abstractForm);
	Dec32 h = a * g;
	assert("[0,9,-101]" == h.abstractForm);
	Dec32 i = a * h;
	assert("[0,1,-101]" == i.abstractForm);*/
//	writeln("test missing");
//}

//--------------------------------
// private methods
//--------------------------------

// TODO: needs unittests
/**
 *  Returns true if the number is too large to be represented
 *  and adjusts the number according to the rounding mode.
 *  Implements the 'overflow' processing in the specification. (p. 53)
 *  Flags: OVERFLOW, ROUNDED, INEXACT.
 *  Precondition: number must be finite.
 */
//@safe
private T checkOverflow(T)(in T num, Rounding mode = T.mode,
		int maxExpo = T.maxExpo, bool setFlags = true) if (isDecimal!T)
{
	if (num.adjustedExponent <= maxExpo) return num;

	// TODO: if the number has not been normalized will this work?
	T copy = num.copy;
	switch (mode)
	{
		case ROUND_NONE: 	// can this branch be reached? should it be?
		case HALF_UP:
		case HALF_EVEN:
		case HALF_DOWN:
		case ROUND_UP:
			copy = T.infinity(num.sign);
			break;
		case ROUND_DOWN:
			copy = num.sign ? T.max.copyNegate : T.max;
			break;
		case ROUND_CEILING:
			copy = num.sign ? T.max.copyNegate : T.infinity;
			break;
		case ROUND_FLOOR:
			copy = num.sign ? T.infinity(true) : T.max;
			break;
		default:
			break;
	}
	if (setFlags) contextFlags.set(OVERFLOW | INEXACT | ROUNDED);
	return copy;
}


// TODO: needs unittests
// Returns true if the rounding mode is half-even, half-up, or half-down.
private bool halfRounding(Rounding mode) {
	return (mode == HALF_EVEN ||
	 		mode == HALF_UP ||
	 		mode == HALF_DOWN);
}

unittest
{
	assert(halfRounding(HALF_UP));
	assert(!halfRounding(ROUND_UP));
}

/**
 *  Rounds the number to the context precision
 *  using the specified rounding mode.
 */
private T roundByMode(T)(T num, int precision,
		Rounding mode = T.mode, int maxExpo = T.maxExpo, bool setFlags = true)
	if (isDecimal!T)
{
	T copy = checkOverflow(num, mode, maxExpo, setFlags);

	// did it overflow to infinity?
	if (copy.isSpecial) return copy;

	if (mode == ROUND_NONE) return num;

	// calculate the remainder
	T remainder = getRemainder(num, precision);
	// if the number wasn't rounded, return
	if (remainder.isZero) return num;

	// check for deleted leading zeros in the remainder.
	// makes a difference only in round-half modes.
	if ((mode == HALF_EVEN || mode == HALF_UP || mode == HALF_DOWN) &&
		countDigits(remainder.coff) != remainder.digits) {
		return num.copy;
	}

	switch (mode) {
		case ROUND_UP:
			incrementAndRound(num);
			break;
		case ROUND_DOWN:
			break;
		case ROUND_CEILING:
			if (!num.sign) incrementAndRound(num);
			break;
		case ROUND_FLOOR:
			if (num.sign) incrementAndRound(num);
			break;
		case HALF_UP:
			if (firstDigit(remainder.coff) >= 5)
				incrementAndRound(num);
			break;
		case HALF_DOWN:
			if (testFive(remainder.coff) > 0)
				incrementAndRound(num);
			break;
		case HALF_EVEN:
			switch (testFive(remainder.coff)) {
				case -1:
					break;
				case 1:
					incrementAndRound(num);
					break;
				default:
					if (lastDigit(num.coff) & 1) {
						incrementAndRound(num);
					}
					break;
				}
			break;
		default:
			break;
	}	// end switch (mode)
	return checkOverflow(num, mode, maxExpo, setFlags);
}	// end roundByMode()

unittest
{	// roundByMode
	static struct S { D64 x; int p; Rounding r; D64 expect; }
	S[] s =
	[
		{ "1000",    5, HALF_EVEN,  "1000" },
		{ "1000000", 5, HALF_EVEN,  "10000E+2" },
		{ "99999",   5, HALF_EVEN,  "99999" },
		{ "1234550", 5, HALF_EVEN,  "1.2346E+6" },
		{ "1234550", 5, ROUND_DOWN, "1.2345E+6" },
		{ "1234550", 5, ROUND_UP,   "1.2346E+6" },
	];
	auto f = FunctionTest!(S,D64)("roundByMode");
	foreach (t; s) f.test(t, roundByMode!D64(t.x,t.p,t.r));
    writefln(f.report);
}

/**
 *  Shortens the coefficient of the number to the specified precision,
 *  adjusts the exponent, and returns the (unsigned) remainder.
 *  If the number is already less than or equal to the precision, the
 *  number is unchanged and the remainder is zero.
 *  Otherwise the rounded flag is set, and if the remainder is not zero
 *  the inexact flag is also set.
 *  Flags: ROUNDED, INEXACT.
 */
private T getRemainder(T) (ref T x, int precision) if (isDecimal!T)
{
	T remainder = T.zero;
	int diff = x.digits - precision;
	if (diff <= 0) {
		return remainder;
	}
	contextFlags.set(ROUNDED);
	BigInt divisor = pow10b(diff);
	BigInt dividend = x.coff;
	BigInt quotient = dividend/divisor;
	auto cf = dividend - quotient*divisor;
	if (cf != 0) {
		remainder = T.zero;
		remainder.digits = diff;
		remainder.expo = x.expo;
		remainder.coff = cf;
		contextFlags.set(INEXACT);
	}
	x.coff = quotient;
	x.digits = countDigits(quotient); //precision;
	x.expo = x.expo + diff;
	return remainder;
}

unittest
{	// getRemainder
	static struct S { D64 x; int p; D64 expect; }
	S[] s =
	[
		{ 1234567890123456L, 5, "67890123456" },
	];
	auto f = FunctionTest!(S,D64)("getRemainder");
	foreach (t; s) f.test(t, getRemainder!D64(t.x,t.p));
    writefln(f.report);
}

/**
 *  Increments the coefficient by one.
 *  If this causes an overflow the coefficient is adjusted by clipping
 *  the last digit (it will be zero) and incrementing the exponent.
 */
private void incrementAndRound(T)(ref T num) if (isDecimal!T)
{
	// if num is zero
	if (num.digits == 0) {
		num.coff = 1;
		num.digits = 1;
		return;
	}
	num.coff = num.coff + 1;
	// TODO: (efficiency) is there a less expensive test?
	if (lastDigit(num.coff) == 0) {
		if (num.coff / pow10b(num.digits) > 0) {
			num.coff = num.coff / 10;
			num.expo = num.expo + 1;
		}
	}
}

/*unittest
{	// incrementAndRound
	static struct S { D64 x; int p; D64 expect; }
	S[] s =
	[
		{ 1234567890123456L, 5, "67890123456" },
	];
	auto f = FunctionTest!(S,D64)("incrementAndRound");
	foreach (t; s) f.test(t, incrementAndRound!D64(t.x,t.p));
    writefln(f.report);
}*/

unittest {	// increment
	write("-- increment&round..");
	D64 actual, expect;
	actual = 10;
	expect = 11;
	incrementAndRound(actual);
	assertEqual(actual, expect);
	actual = 19;
	expect = 20;
	incrementAndRound(actual);
	assertEqual(actual, expect);
	actual = 999;
	expect = "1.00E+3";
	incrementAndRound(actual);
	assertEqual(actual, expect);
	writeln("passed");
}

/**
 *  Returns -1, 1, or 0 if the remainder is less than, more than,
 *  or exactly half the least significant digit of the shortened coefficient.
 *  Exactly half is a five followed by zero or more zero digits.
 */
private int testFive(in BigInt x)
{
	int num = countDigits(x);
	BigInt btens = pow10b(num - 1);
	int first = cast(int)(x / btens);
	if (first < 5) return -1;
	if (first > 5) return +1;
	BigInt zeros = x % btens;
	return zeros ? 1 : 0;
}

unittest
{	// testFive
	static struct S { BigInt x; int expect; }
	S[] s =
	[
		{ "5000000000000000000000",  0 },
		{ "4999999999999999999999", -1 },
		{ "50000000000000000000000000000000000000000000000001", 1 },
	];
	auto f = FunctionTest!(S,int)("testFive");
	foreach (t; s) f.test(t, testFive(t.x));
    writefln(f.report);
}

//-----------------------------
// useful constants
//-----------------------------

/*private enum BigInt BIG_ZERO = BigInt(0);
private enum BigInt BIG_ONE  = BigInt(1);
private enum BigInt BIG_FIVE = BigInt(5);
private enum BigInt BIG_TEN  = BigInt(10);
private enum BigInt BILLION  = BigInt(1_000_000_000);
private enum BigInt QUINTILLION  = BigInt(1_000_000_000_000_000_000);*/


///  The maximum number of decimal digits that fit in an int value.
//public enum int MAX_INT_DIGITS = 9;
///  The maximum decimal value that fits in an int.
//public enum uint MAX_DECIMAL_INT = 999999999U;
///  The maximum number of decimal digits that fit in a long value.
public enum int MAX_LONG_DIGITS = 18;
///  The maximum decimal value that fits in a long value.
public enum ulong MAX_DECIMAL_LONG = 10UL^^MAX_LONG_DIGITS - 1;

//-----------------------------
// BigInt digit functions
//-----------------------------

/**
 *  An array of unsigned long integers with values of
 *  powers of ten from 10^^0 to 10^^19
 */
private enum ulong[20] pow10UL = [10UL^^0,
		10UL^^1,  10UL^^2,  10UL^^3,  10UL^^4,  10UL^^5,  10UL^^6,
		10UL^^7,  10UL^^8,  10UL^^9,  10UL^^10, 10UL^^11, 10UL^^12,
		10UL^^13, 10UL^^14, 10UL^^15, 10UL^^16, 10UL^^17, 10UL^^18, 10UL^^19];

/**
 * Returns the number of decimal digits in a non-negative big integer
 */
public int countDigits(T)(in T m)
	if (is(T == BigInt) || isUnsigned!T)
{
	if (m == 0)
	{
		return 0;
	}

	static if (is(T == BigInt))
	{
		if (m.ulongLength == 1)
		{
			return countDigits(m.getDigit(0));
		}

		int count;
		ulong n = clipDigits(m, count);
		return count + countDigits(n);
	}
	else
	{
		auto n = cast(ulong)m;
		// special cases:
		if (n == 0) return 0;
		if (n < 10) return 1;
		if (n >= pow10UL[19]) return 20;
		// use a binary search to count the digits
		int min = 2;
		int max = 19;
		while (min <= max)
		{
			int mid = (min + max)/2;
			if (n < pow10UL[mid])
			{
				max = mid - 1;
			}
			else
			{
				min = mid + 1;
			}
		}
		return min;
	}
}

///
unittest // countDigits
{
	auto big = BigInt(
		"1234567890_1234567890_1234567890_1234567890_1234567890" ~
		"1234567890_1234567890_1234567890_1234567890_1234567890_1");
 	assert(countDigits(big) == 101);
}


unittest
{	// countDigits
	static struct S { BigInt n; int expect; }
	S[] s =
	[
		{ "123456", 6 },
		{ "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901", 101 },
		{ "1234567890123456789012345678901234567890123456789012", 52 },
	];
	auto f = FunctionTest!(S,int)("countDigits");
	foreach (t; s) f.test(t, countDigits(t.n));
    writefln(f.report);
}

unittest
{	// countDigits
	static struct S { ulong n; int expect; }
	S[] s =
	[
		{				  7,  1 },
		{				 13,  2 },
		{				999,  3 },
		{			   9999,  4 },
		{			  25978,  5 },
		{			2008617,  7 },
		{		 1234567890, 10 },
		{		10000000000, 11 },
		{	123456789012345, 15 },
		{  1234567890123456, 16 },
		{123456789012345678, 18 },
		{		   long.max, 19 },
		{		  ulong.max, 20 },
		{		  ulong.min,  0 },
	];
	auto f = FunctionTest!(S,int)("countDigits");
	foreach (t; s) f.test(t, countDigits(t.n));
    writefln(f.report);
}


/// Clips decimal digits until a single ulong digit is left.
private ulong clipDigits(in BigInt big, out int count)
{
	if (big <= ulong.max) return cast(ulong)big;

	enum BigInt tenQuint = 10UL^^19;
	count = 0;
	BigInt quotient = big;
	while (quotient > tenQuint) {
		quotient /= tenQuint;
		count += 19;
	}
	return cast(ulong)quotient;
}

unittest // clipDigits
{
	int count;
	BigInt big;
	big = BigInt("123456");
	assert(clipDigits(big, count) == 123456);
	assert(clipDigits(BigInt(ulong.max), count) == ulong.max);
	big = BigInt("1234567890_1234567890_1234567890_1234567890_1234567890");
	assert(clipDigits(big, count) == 123456789012);
}

/// returns the first decimal digit of the number
public uint firstDigit(T)(T n)
	if (is(T == BigInt) || isUnsigned!T)
{
	static if (is(T == BigInt))
	{
		int count;
		return firstDigit(clipDigits(n,count));
	}
	else
	{
		if (n == 0) return 0;
		if (n < 10) return cast(int) n;
		int digits = countDigits(n);
		return cast(uint)(n / pow10UL[digits-1]);
	}
}

unittest
{
	assert(firstDigit(BigInt("8234567890123456789012345678901234567890123")) == 8);
	assert(firstDigit(0U) == 0);
	assert(firstDigit(7U) == 7);
	assert(firstDigit(9999UL) == 9);
	assert(firstDigit(uint.max) == 4);
	assert(firstDigit(ulong.max) == 1);
}

unittest	// move to regression testing
{	// firstDigit(BigInt)
	static struct S { BigInt n; uint expect; }
	S[] s =
	[
		{ "5000000000000000000000", 5 },
		{ "45500000001209854300023400000000000000", 4 },
		{ "82345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678905", 8 },
	];
	auto f = FunctionTest!(S,uint)("1stDigit(b)");
	foreach (t; s) f.test(t, firstDigit(t.n));
    writefln(f.report);
}

unittest
{	// firstDigit(ulong)
	static struct S { ulong n; uint expect; }
	S[] s =
	[
		{ 0, 0 },
		{ 7, 7 },
		{ 13, 1 },
		{ 999, 9 },
		{ 9999, 9 },
		{ 25987, 2 },
		{ 5008617, 5 },
		{ 3234567890, 3 },
		{ 10000000000, 1 },
		{ 823456789012345, 8 },
		{ 4234567890123456, 4 },
		{ 623456789012345678, 6 },
		{long.max, 9 }
	];
	auto f = FunctionTest!(S,uint)("1stDigit(u)");
	foreach (t; s) f.test(t, firstDigit(t.n));
    writefln(f.report);
}

// TODO: move to arithmetic, templatize
/**
 *  Decimal shift left.
 *  Shifts the number left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is shifted right.
 */
public BigInt shiftLeft(BigInt num, int n)
{
	// NOTE: simply multiplies by 10^^n.
	if (n > 0) {
		num *= pow10b(n);
	}
	if (n < 0) {
		num = shiftRight(num, -n);
	}
	return num;
}


/+unittest {	// shiftLeft(BigInt)
	BigInt m;
	int n;
	m = 12345;
	n = 2;
	assert(shiftLeft(m, n/*, 100*/) == 1234500);
	m = 1234567890;
	n = 7;
	assert(shiftLeft(m, n/*, 100*/) == BigInt(12345678900000000));
	m = 12;
	n = 2;
	assert(shiftLeft(m, n/*, 100*/) == 1200);
	m = 12;
	n = 4;
	assert(shiftLeft(m, n/*, 100*/) == 120000);
	uint k;
/*	k = 12345;
	n = 2;
	assert(1234500 == cast(uint)shiftLeft(k, n, 9));
	k = 1234567890;
	n = 7;
	assert(900000000 == cast(uint)shiftLeft(k, n, 9));
	k = 12;
	n = 2;
	assert(1200 == cast(uint)shiftLeft(k, n, 9));
	k = 12;
	n = 4;
	assert(120000 == cast(uint)shiftLeft(k, n, 9));*/
}+/

/**
 *  Shifts the number to the left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is shifted to the right.
 */
/*public ulong shiftLeft(ulong num, const int n,
		const int precision = MAX_LONG_DIGITS) {
	if (n > precision) return 0;
	if (n > 0) {
		// may need to clipDigits before shifting
		int m = countDigits(num);
		int diff = precision - m - n;
		if (diff < 0 ) {
			num %= cast(ulong)ltens[m + diff];
		}
		ulong scale = cast(ulong)ltens[n];
		num *= scale;
	}
	if (n < 0) {
		num = shiftRight(num, -n, precision);
	}
	return num;
}*/

/**
 *  Decimal shift right.
 *  Shifts the number right the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is shifted left.
 */
public BigInt shiftRight(BigInt num, int n)
{
	// NOTE: simply divides by 10^^n.
	if (n > 0)
	{
		num /= pow10b(n);
	}
	if (n < 0) {
		num = shiftLeft(num, -n);
	}
	return num;
}

/*unittest {
	write("shiftRight...");
	BigInt num;
	BigInt res;
	num = BigInt("9223372036854775807");
	res = shiftRight(num, 10);
	assertEqual!BigInt(res, BigInt("922337203"));

	num = BigInt("9223372036854775808");
	res = shiftRight(num, 10);
	assertEqual!BigInt(res, BigInt("922337203"));
	writeln("passed");
}*/


/+
 *  Rotates the number to the left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is rotated to the right.
public ulong rotateLeft(ulong num, const int n, const int precision) {
	if (n > precision) return 0;
	if (n > 0) {
		int m = precision - n;
		ulong rem = num / ltens[m];
		num %= ltens[m];
		num *= ltens[n];
		num += rem;
	}
	if (n < 0) {
		num = rotateRight(num, precision, -n);
	}
	return num;
}

unittest {
	writeln("rotateLeft...");
	ulong num = 1234567;
writeln("num = ", num);
	ulong rot = rotateLeft(num, 7, 2);
writeln("rot = ", rot);
	writeln("test missing");
}

 *  Rotates the number to the right by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0 the number is rotated to the left. // TODO: (behavior) should throw.
public ulong rotateRight(ulong num, const int n, const int precision) {
	if (n > precision) return 0;
	if (n == precision) return num;
	if (n > 0) {
		int m = precision - n;
		ulong rem = num / ltens[n];
		num %= ltens[n];
		num *= ltens[m];
		num += rem;
	}
	if (n < 0) {
		num = rotateLeft(num, precision, -n);
	}
	return num;
}

unittest {
	writeln("rotateRight...");
	ulong num = 1234567;
writeln("num = ", num);
	ulong rot = rotateRight(num, 7, 2);
writeln("rot = ", rot);
	 rot = rotateRight(num, 9, 2);
writeln("rot = ", rot);
	 rot = rotateRight(num, 7, -2);
writeln("rot = ", rot);
	 rot = rotateRight(num, 7, 7);
writeln("rot = ", rot);
	writeln("test missing");
}
+/

// TODO: split into BigInt, ulong?
///  Returns the last digit of the argument.
public int lastDigit(in BigInt big)
{
	auto digit = big % BigInt(10);
	if (digit < 0) digit = -digit;
	return digit.toInt;
}

unittest
{	// lastDigit
	static struct S { BigInt n; int expect; }
	S[] s =
	[
		{  7, 7 },
		{ -13, 3 },
		{  999, 9 },
		{ -9999, 9 },
		{  25987, 7 },
		{ -5008615, 5 },
		{  3234567893, 3 },
		{ -10000000000, 0 },
		{  823456789012348, 8 },
		{  4234567890123456, 6 },
		{  623456789012345674, 4 },
		{  long.max, 7 },
	];
	auto f = FunctionTest!(S,int)("lastDigit");
	foreach (t; s) f.test(t, lastDigit(t.n));
    writefln(f.report);
}

///  Returns the number of trailing zeros in the argument.
public int trailingZeros(BigInt n, int digits)
{
	// shortcuts for frequent values
	if (n ==  0) return 0;
	if (n %  10) return 0;
	if (n % 100) return 1;
	// find by binary search
	int min = 3;
	int max =  digits - 1;
	while (min <= max) {
		int mid = (min + max)/2;
		if (n % pow10b(mid) != 0)
		{
			max = mid - 1;
		}
		else
		{
			min = mid + 1;
		}
	}
	return max;
}

///  Trims any trailing zeros and returns the number of zeros trimmed.
public int trimZeros(ref BigInt n, int digits)
{
	int zeros = trailingZeros(n, digits);
	if (zeros == 0) return 0;
	n /= pow10b(zeros);
	return zeros;
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
 *  Standard: Implements the 'reduce' function in the specification. (p. 37)
 *  "This operation was called 'normalize' prior to
 *  version 1.68 of the specification." (p. 37)
 *
 *  Flags: INVALID_OPERATION
 *
 */
public T reduce(T)(in T num,
		Context context = T.context) if (isDecimal!T)
{
	// special cases
	if (num.isNaN) return T.nan; //invalidOperand(num);
	if (!num.isFinite) return num.dup;

	// round the argument
	T reduced = roundToPrecision(num, context);

	// have to check again -- rounding may have made it infinite
	if (!reduced.isFinite) return reduced;

	int digits = reduced.digits;
	auto temp = reduced.coff;
	int zeros = trimZeros(temp, digits);
	if (zeros)
	{
		reduced.coff = temp;
		reduced.digits = digits - zeros;
		reduced.expo = reduced.expo + zeros;
	}

	return reduced;
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


unittest {
	writeln("==========================");
	writeln("decimal rounding.......end");
	writeln("==========================");
}

>>>>>>> 01585e6cd3426df69c6d62bd0e743ef09ea710e3
