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

module eris.decimal.rounding;

import eris.decimal;
import eris.decimal.context;
import eris.decimal.arithmetic: copyNegate;
import eris.integer.extended;

unittest {
	writeln("==========================");
	writeln("decimal rounding.....begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
//	alias dectest = Decimal!(9,99);
}

public T roundToPrecision(T)(in T num, Context context) {
	return roundToPrecision(num, context.precision, context.rounding);
}

public T roundToPrecision(T)(in T num, Rounding rounding) {
	return roundToPrecision(num, T.precision, rounding);
}

/// Rounds the referenced number using the precision and rounding mode of
/// the context parameter.
/// Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
//@safe
public T roundToPrecision(T)(in T num,
	in int precision = T.precision,
	in Rounding rounding = T.rounding) {

	T result = num.dup;	// copy the input
	if (rounding == Rounding.NONE) return result;

	// special values aren't rounded
	if (!num.isFinite) return result;

	// zero values aren't rounded, but they are checked for
	// subnormal and out of range exponents.
	if (num.isZero) {
		if (num.exponent < T.minExpo) {
			contextFlags.setFlags(SUBNORMAL);
			if (num.exponent < T.tinyExpo) {
				int temp = T.tinyExpo;
				result.exponent = T.tinyExpo;
			}
		}
		return result;
	}

	// handle subnormal numbers
	if (num.isSubnormal()) {
		contextFlags.setFlags(SUBNORMAL);
		int diff = T.minExpo - result.adjustedExponent;
		// use the subnormal precision and round
		int subprecision = precision - diff;
		if (result.digits > subprecision) {
			roundByMode(result, subprecision, rounding);
		}
		// if the result of rounding a subnormal is zero
		// the clamped flag is set. (Spec. p. 51)
		if (result.isZero) {
			result.exponent = T.tinyExpo;
			contextFlags.setFlags(CLAMPED);
		}
		return result;
	}

	// Don't round the number if it is too large to represent
	if (overflow(result, rounding)) {
		return result;
    }
	// round the number
	roundByMode(result, precision, rounding);
	// check again for an overflow
	overflow(result, rounding);
	return result;
} // end roundToPrecision()

unittest {	// roundToPrecision
	write("-- roundToPrecision.");
	dec9 before = 9999;
	dec9 after;
	after = roundToPrecision(before, 3);
	assertStringEqual(after, "1.00E+4");
	before = 1234567890;
	after = roundToPrecision(before, 3);
	assertStringEqual(after, "1.23E+9");
	after = roundToPrecision(before, 4);
	assertStringEqual(after, "1.235E+9");
	after = roundToPrecision(before, 5);
	assertStringEqual(after,  "1.2346E+9");
	after = roundToPrecision(before, 6);
	assertStringEqual(after,  "1.23457E+9");
	after = roundToPrecision(before, 7);
	assertStringEqual(after,  "1.234568E+9");
	after = roundToPrecision(before, 8);
	assertStringEqual(after,  "1.2345679E+9");
	before = 1235;
	after = roundToPrecision(before, 3);
	assertStringEqual(after.toAbstract(), "[0,124,1]");
	before = 12359;
	after = roundToPrecision(before, 3);
	assertStringEqual(after.toAbstract(), "[0,124,2]");
	before = 1245;
	after = roundToPrecision(before, 3);
	assertStringEqual(after.toAbstract(), "[0,124,1]");
	before = 12459;
	after = roundToPrecision(before, 3);
	assertStringEqual(after.toAbstract(), "[0,125,2]");
//	xint test = "18690473486004564289165545643685440097";
//	long  count = countDigits(test);
//	roundToPrecision(test);
		// TODO: (testing) test for subnormal as below...
/*	Dec32 a = Dec32(0.1);
	Dec32 b = Dec32.min * Dec32(8888888);
	assert("[0,8888888,-101]" == b.toAbstract);
	Dec32 c = a * b;
	assert("[0,888889,-101]" == c.toAbstract);
	Dec32 d = a * c;
	assert("[0,88889,-101]" == d.toAbstract);
	Dec32 e = a * d;
	assert("[0,8889,-101]" == e.toAbstract);
	Dec32 f = a * e;
	assert("[0,889,-101]" == f.toAbstract);
	Dec32 g = a * f;
	assert("[0,89,-101]" == g.toAbstract);
	Dec32 h = a * g;
	assert("[0,9,-101]" == h.toAbstract);
	Dec32 i = a * h;
	assert("[0,1,-101]" == i.toAbstract);*/
	writeln("passed");
}

//--------------------------------
// private methods
//--------------------------------

/// Returns true if the number is too large to be represented
/// and adjusts the number according to the rounding mode.
/// Implements the 'overflow' processing in the specification. (p. 53)
/// Flags: OVERFLOW, ROUNDED, INEXACT.
/// Precondition: number must be finite.
//@safe
private bool overflow(T)(ref T num,	Rounding mode = T.rounding)  {
	if (num.adjustedExponent <= T.maxExpo) return false;
	switch (mode) {
		case Rounding.NONE:
		case Rounding.HALF_UP:
		case Rounding.HALF_EVEN:
		case Rounding.HALF_DOWN:
		case Rounding.UP:
			num = T.infinity(num.sign);
			break;
		case Rounding.DOWN:
			num = num.sign ? T.max.copyNegate : T.max;
			break;
		case Rounding.CEILING:
			num = num.sign ? T.max.copyNegate : T.infinity;
			break;
		case Rounding.FLOOR:
			num = num.sign ? T.infinity(true) : T.max;
			break;
		default:
			break;
	}
	// TODO: (behavior) don't set flags if not rounded??
	contextFlags.setFlags(OVERFLOW | INEXACT | ROUNDED);
	return true;
}

// Returns true if the rounding mode is half-even, half-up, or half-down.
private bool halfRounding(Rounding rounding) {
	return (rounding == Rounding.HALF_EVEN ||
	 		rounding == Rounding.HALF_UP ||
	 		rounding == Rounding.HALF_DOWN);
}

/// Rounds the number to the context precision
/// using the specified rounding mode.
private void roundByMode(T)(ref T num, int precision, Rounding mode) {

	if (mode == Rounding.NONE) return;

	// calculate the remainder
	T remainder = getRemainder(num, precision);
	// if the number wasn't rounded, return
	if (remainder.isZero) return;

	// check for deleted leading zeros in the remainder.
	// makes a difference only in round-half modes.
	if (halfRounding(mode) &&
		numDigits(remainder.coefficient) != remainder.digits) {
		return;
	}

	switch (mode) {
		case Rounding.UP:
			incrementAndRound(num);
			return;
		case Rounding.DOWN:
			return;
		case Rounding.CEILING:
			if (!num.sign) incrementAndRound(num);
			return;
		case Rounding.FLOOR:
			if (num.sign) incrementAndRound(num);
			return;
		case Rounding.HALF_UP:
			if (firstDigit(remainder.coefficient) >= 5)
				incrementAndRound(num);
			return;
		case Rounding.HALF_DOWN:
			if (testFive(remainder.coefficient) > 0)
				incrementAndRound(num);
			return;
		case Rounding.HALF_EVEN:
			switch (testFive(remainder.coefficient)) {
				case -1:
					break;
				case 1:
					incrementAndRound(num);
					break;
				default:
					if (lastDigit(num.coefficient) & 1) {
						incrementAndRound(num);
					}
					break;
				}
			return;
		default:
			return;
	}	// end switch(mode)
}	// end roundByMode()

unittest {	// roundByMode
	write("-- roundByMode......");
	dec9 num;
	num = 1000;
	roundByMode(num, 5, Rounding.HALF_EVEN);
	assertEqual(num.coefficient, 1000);
	assertEqual(num.exponent, 0);
	assertEqual(num.digits, 4);
	num = 1000000;
	roundByMode(num, 5, Rounding.HALF_EVEN);
	assertEqual(num.coefficient, 10000);
	assertEqual(num.exponent, 2);
	assertEqual(num.digits, 5);
	num = 99999;
	roundByMode(num, 5, Rounding.HALF_EVEN);
	assertEqual(num.coefficient, 99999);
	assertEqual(num.exponent, 0);
	assertEqual(num.digits, 5);
	num = 1234550;
	roundByMode(num, 5, Rounding.HALF_EVEN);
	assertEqual(num.coefficient, 12346);
	assertEqual(num.exponent, 2);
	assertEqual(num.digits, 5);
	num = 1234550;
	roundByMode(num, 5, Rounding.DOWN);
	assertEqual(num.coefficient, 12345);
	assertEqual(num.exponent, 2);
	assertEqual(num.digits, 5);
	num = 1234550;
	roundByMode(num, 5, Rounding.UP);
	assertEqual(num.coefficient, 12346);
	assertEqual(num.exponent, 2);
	assertEqual(num.digits, 5);
	writeln("passed");
}

/// Shortens the coefficient of the number to the specified precision,
/// adjusts the exponent, and returns the (unsigned) remainder.
/// If the number is already less than or equal to the precision, the
/// number is unchanged and the remainder is zero.
/// Otherwise the rounded flag is set, and if the remainder is not zero
/// the inexact flag is also set.
/// Flags: ROUNDED, INEXACT.
private T getRemainder(T) (ref T x, int precision)  {

	T remainder = T.zero;
	int diff = x.digits - precision;
	if (diff <= 0) {
		return remainder;
	}
	contextFlags.setFlags(ROUNDED);
	xint divisor = pow10(diff);
	xint dividend = x.coefficient;
	xint quotient = dividend/divisor;
	auto mant = dividend - quotient*divisor;
	if (mant != 0) {
		remainder.zero;
		remainder.digits = diff;
		remainder.exponent = x.exponent;
		remainder.coefficient = mant;
		contextFlags.setFlags(INEXACT);
	}
	x.coefficient = quotient;
	x.digits = numDigits(quotient); //precision;
	x.exponent = x.exponent + diff;
	return remainder;
}

unittest {	// getRemainder
	write("-- getRemainder.....");
	dec9 num, acrem, exnum, exrem;
	num = 1234567890123456L;
	acrem = getRemainder(num, 5);
	exnum = dec9("1.2345E+15");
	assertEqual(num, exnum);
	exrem = 67890123456;
	assertEqual(acrem, exrem);
	writeln("passed");
}

/// Increments the coefficient by one.
/// If this causes an overflow the coefficient is adjusted by clipping
/// the last digit (it will be zero) and incrementing the exponent.
private void incrementAndRound(T)(ref T x)  {

	x.coefficient = x.coefficient + 1;
	int digits = x.digits;
	// if x was zero
	if (digits == 0) {
		x.digits = 1;
		return;
	}
	if (lastDigit(x.coefficient) == 0) {
		if (x.coefficient / pow10(digits) > 0) {
			x.coefficient = x.coefficient / 10;
			x.exponent = x.exponent + 1;
		}
	}
}

unittest {	// increment
	write("-- increment&round..");
	dec9 actual, expect;
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

/// Returns -1, 1, or 0 if the remainder is less than, more than,
/// or exactly half the least significant digit of the shortened coefficient.
/// Exactly half is a five followed by zero or more zero digits.
// TODO: (efficiency) calls firstDigit and then numDigits: combine these calls.
public int testFive(in xint x) {
	int first = firstDigit(x);
	if (first < 5) return -1;
	if (first > 5) return +1;
	xint tens = BIG_TEN^^(numDigits(x)-1);
	xint zeros = x % BIG_TEN^^(numDigits(x)-1);
	return (zeros != 0) ? 1 : 0;
}

unittest {	// testFive
	write("-- testFive.........");
	xint x;
	x = "5000000000000000000000";
	assertEqual(testFive(x),  0);
	x = "4999999999999999999999";
	assertEqual(testFive(x), -1);
	x = "50000000000000000000000000000000000000000000000001";
	assertEqual(testFive(x),  1);
	writeln("passed");
}

/// Increments the number by 1.
/// Re-calculates the number of digits -- the increment may have caused
/// an increase in the number of digits, i.e., input number was all 9s.
private void increment(T)(ref T x, ref uint digits) {
	x++;
	// TODO: (efficiency) there should be a smarter way to do this: x > pow10(digits)?
	digits = numDigits(x);
}

//-----------------------------
// useful constants
//-----------------------------

private enum xint BIG_ZERO = xint(0);
private enum xint BIG_ONE  = xint(1);
private enum xint BIG_FIVE = xint(5);
private enum xint BIG_TEN  = xint(10);
private enum xint BILLION  = xint(1_000_000_000);
private enum xint QUINTILLION  = xint(1_000_000_000_000_000_000);

/// An array of unsigned long integers with values of
/// powers of ten from 10^^0 to 10^^18
public enum ulong[19] TENS = [10L^^0,
		10L^^1,  10L^^2,  10L^^3,  10L^^4,  10L^^5,  10L^^6,
		10L^^7,  10L^^8,  10L^^9,  10L^^10, 10L^^11, 10L^^12,
		10L^^13, 10L^^14, 10L^^15, 10L^^16, 10L^^17, 10L^^18];

/// An array of unsigned long integers with values of
/// powers of five from 5^^0 to 5^^26
public enum ulong[27] FIVES = [5L^^0,
		5L^^1,  5L^^2,  5L^^3,  5L^^4,  5L^^5,  5L^^6,
		5L^^7,  5L^^8,  5L^^9,  5L^^10, 5L^^11, 5L^^12,
		5L^^13, 5L^^14, 5L^^15, 5L^^16, 5L^^17, 5L^^18,
		5L^^19, 5L^^20, 5L^^21, 5L^^22, 5L^^23, 5L^^24,
		5L^^25, 5L^^26];

/// The maximum number of decimal digits that fit in an int value.
public enum int MAX_INT_DIGITS = 9;
/// The maximum decimal value that fits in an int.
public enum uint MAX_DECIMAL_INT = 10U^^MAX_INT_DIGITS - 1;
/// The maximum number of decimal digits that fit in a long value.
public enum int MAX_LONG_DIGITS = 18;
/// The maximum decimal value that fits in a long.
public enum ulong MAX_DECIMAL_LONG = 10UL^^MAX_LONG_DIGITS - 1;

//-----------------------------
// decimal digit functions
//-----------------------------

public int numDigits(in xint x) {
    // special cases
	if (x == 0) return 0;
	int count = 0;
	long n = countDigits(x, count);
	return count + numDigits(n);
}

unittest {	// numDigits(xint)
	write("-- numDigits(xint)..");
	xint big = xint("12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678905");
	assertEqual(numDigits(big), 101);
	writeln("passed");
}

/// Returns the number of digits in the argument,
/// where the argument is an unsigned long integer.
public int numDigits(long n) {
    // special cases:
	if (n == 0) return 0;
	if (n < 10) return 1;
	if (n >= TENS[18]) return 19;
    // use a binary search to count the digits
	int min = 2;
	int max = 18;
	while (min <= max) {
		int mid = (min + max)/2;
		if (n < TENS[mid]) {
			max = mid - 1;
		}
		else {
			min = mid + 1;
		}
	}
	return min;
}

unittest {	// numDigits(ulong)
	write("-- numDigits(ulong).");
	ulong num, expect;
	uint digits;
	num = 10;
	expect = 11;
	digits = numDigits(num);
	increment(num, digits);
	assertEqual(num, expect);
	assertEqual(digits, 2);
	num = 19;
	expect = 20;
	digits = numDigits(num);
	increment(num, digits);
	assertEqual(num, expect);
	assertEqual(digits, 2);
	num = 999;
	expect = 1000;
	digits = numDigits(num);
	increment(num, digits);
	assertEqual(num, expect);
	assertEqual(digits, 4);
	writeln("passed");
}

unittest {	// numDigits
	long n;
	n = 7;
	int expect = 1;
	int actual = numDigits(n);
	assertEqual(actual, expect);
	n = 13;
	expect = 2;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 999;
	expect = 3;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 9999;
	expect = 4;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 25987;
	expect = 5;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 2008617;
	expect = 7;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 1234567890;
	expect = 10;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 10000000000;
	expect = 11;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 123456789012345;
	expect = 15;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 1234567890123456;
	expect = 16;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = 123456789012345678;
	expect = 18;
	actual = numDigits(n);
	assertEqual(actual, expect);
	n = long.max;
	expect = 19;
	actual = numDigits(n);
	assertEqual(actual, expect);
}

@safe
public long countDigits(in xint x) {
	xint big = x.dup;
	while (big > QUINTILLION) {
		big /= QUINTILLION;
	}
	return big.toLong;
}

public long countDigits2(in xint arg) {
	xint big = arg.dup;
writefln("big = %s", big);
writefln("QUINTILLION = %s", QUINTILLION);
	int count = 0;
	while (big > QUINTILLION) {
writefln("count++ = %s", count++);
		big /= QUINTILLION;
	}
	return big.toLong;
}

unittest {
	write("countDigits...");
	xint x = "18690473486004564289165545643685440097";
//	countDigits2(x);
	writeln("test missing");
}

// TODO: (language) These functions need better names
@safe
public long countDigits(in xint x, out int count) {
	count = 0;
	xint big = x.dup;
	while (big > QUINTILLION) {
		big /= QUINTILLION;
		count += 18;
	}
	return big.toLong;
}

/// Returns the first digit of the argument.
public int firstDigit(in xint x) {
	return firstDigit(countDigits(x));
}

unittest {	// firstDigit(xint)
	write("-- 1stDigit(xint)...");
	xint x;
	x = "5000000000000000000000";
	assertEqual(firstDigit(x), 5);
	x = "82345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678905";
	assertEqual(firstDigit(x), 8);
	writeln("passed");
}


/// Returns the first digit of the argument.
public int firstDigit(long n) { //, int maxValue = 19) {
	if (n == 0) return 0;
	if (n < 10) return cast(int) n;
	int d = numDigits(n); //, maxValue);
	return cast(int)(n/TENS[d-1]);
}

unittest {	// firstDigit
	write("-- 1stDigit(long)..");
	long n;
	n = 7;
	int expect, actual;
	expect = 7;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 13;
	expect = 1;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 999;
	expect = 9;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 9999;
	expect = 9;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 25987;
	expect = 2;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 5008617;
	expect = 5;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 3234567890;
	expect = 3;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 10000000000;
	expect = 1;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 823456789012345;
	expect = 8;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 4234567890123456;
	expect = 4;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = 623456789012345678;
	expect = 6;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	n = long.max;
	expect = 9;
	actual = firstDigit(n);
	assertEqual(actual, expect);
	writeln("passed");
}

/// Shifts the number left by the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is shifted right.
public xint shiftLeft(xint num, int n) {
	if (n > 0) {
		xint fives = n < 27 ? xint(FIVES[n]) : BIG_FIVE^^n;
		num = num << n;
		num *= fives;
	}
	if (n < 0) {
		num = shiftRight(num, -n);
	}
	return num;
}

/*/// Shifts the number left by the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is shifted right.
public xint shiftLeft(xint num, const int n) {
	return shiftLeft(num, n); //, int.max);
// const int precision = int.max
	if (n > 0) {
		xint fives = n < 27 ? xint(FIVES[n]) : BIG_FIVE^^n;
		num = num << n;
		num *= fives;
	}
	if (n < 0) {
		num = shiftRight(num, -n, precision);
	}
	return num;
}*/

unittest {	// shiftLeft(xint)
	xint m;
	int n;
	m = 12345;
	n = 2;
	assert(shiftLeft(m, n/*, 100*/) == 1234500);
	m = 1234567890;
	n = 7;
	assert(shiftLeft(m, n/*, 100*/) == xint(12345678900000000));
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
}

/// Shifts the number to the left by the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is shifted to the right.
/*public ulong shiftLeft(ulong num, const int n,
		const int precision = MAX_LONG_DIGITS) {
	if (n > precision) return 0;
	if (n > 0) {
		// may need to clip before shifting
		int m = numDigits(num);
		int diff = precision - m - n;
		if (diff < 0 ) {
			num %= cast(ulong)TENS[m + diff];
		}
		ulong scale = cast(ulong)TENS[n];
		num *= scale;
	}
	if (n < 0) {
		num = shiftRight(num, -n, precision);
	}
	return num;
}*/

bool verbose = false;
/// Shifts the number right the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is shifted left.
public xint shiftRight(xint num, int n) {

	if (n > 0) {
		xint fives = n < 27 ? xint(FIVES[n]) : BIG_FIVE^^n;
		num = num >> n;
		num /= fives;
	}
	if (n < 0) {
		num = shiftLeft(num, -n/*, precision*/);
	}
	return num;
}

unittest {
	write("shiftRight...");
	xint num = "1000000000000000";
//	num = 10;
//	writefln("num = %s", num);
//	res = shiftRight(num, 1);
//	writefln("res = %s", res);
	writeln("test missing");
}

/*/// Shifts the number right the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is shifted left.
public ulong shiftRight(ulong num, int n,
		const int precision = MAX_LONG_DIGITS) {
	if (n > 0) {
		num /= TENS[n];
	}
	if (n < 0) {
		num = shiftLeft(num, -n, precision);
	}
	return num;
}*/

/+
/// Rotates the number to the left by the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is rotated to the right.
public ulong rotateLeft(ulong num, const int n, const int precision) {
	if (n > precision) return 0;
	if (n > 0) {
		int m = precision - n;
		ulong rem = num / TENS[m];
		num %= TENS[m];
		num *= TENS[n];
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

/// Rotates the number to the right by the specified number of decimal digits.
/// If n == 0 the number is returned unchanged.
/// If n < 0 the number is rotated to the left. // TODO: (behavior) should throw.
public ulong rotateRight(ulong num, const int n, const int precision) {
	if (n > precision) return 0;
	if (n == precision) return num;
	if (n > 0) {
		int m = precision - n;
		ulong rem = num / TENS[n];
		num %= TENS[n];
		num *= TENS[m];
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

/// Returns the last digit of the argument.
@safe
public int lastDigit(in xint arg) {
	xint big = arg.dup;
	xint digit = big % xint(10);
	if (digit < 0) digit = -digit;
	return cast(int)digit.toInt;
}

unittest {	// lastDigit(xint)
	write("lastDigit.....");
	xint n;
	n = 7;
	assertEqual(lastDigit(n), 7);
	n = -13;
	assertEqual(lastDigit(n), 3);
	n = 999;
	assertEqual(lastDigit(n), 9);
	n = -9999;
	assertEqual(lastDigit(n), 9);
	n = 25987;
	assertEqual(lastDigit(n), 7);
	n = -5008615;
	assertEqual(lastDigit(n), 5);
	n = 3234567893;
	assertEqual(lastDigit(n), 3);
	n = -10000000000;
	assertEqual(lastDigit(n), 0);
	n = 823456789012348;
	assertEqual(lastDigit(n), 8);
	n = 4234567890123456;
	assertEqual(lastDigit(n), 6);
	n = 623456789012345674;
	assertEqual(lastDigit(n), 4);
	n = long.max;
	assertEqual(lastDigit(n), 7);
	writeln("passed");
}

/// Returns the number of trailing zeros in the argument.
// TODO: (language) move to arithmetic
public int trailingZeros(xint n, int digits) {
	// shortcuts for frequent values
	if (n ==  0) return 0;
	if (n %  10) return 0;
	if (n % 100) return 1;
	// find by binary search
	int min = 3;
	int max =  digits - 1;
	while (min <= max) {
		int mid = (min + max)/2;
		if (n % tens(mid) != 0) {
			max = mid - 1;
		}
		else {
			min = mid + 1;
		}
	}
	return max;
}

/// Trims any trailing zeros and returns the number of zeros trimmed.
// TODO: (language) move this to arithmetic?
public int trimZeros(ref xint n, int digits) {
	int zeros = trailingZeros(n, digits);
	if (zeros == 0) return 0;
	n /= tens(zeros);
	return zeros;
}

/// Returns a xint value of ten raised to the specified power.
public xint tens(int n) {
	if (n < 19) return xint(TENS[n]);
	xint num = 1;
	return shiftLeft(num, n);
}

unittest {
	writeln("==========================");
	writeln("decimal rounding.......end");
	writeln("==========================");
}

