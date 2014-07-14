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

module decimal.dec64;

import std.bitmanip;
import std.conv;
import std.string;

import eris.decimal;
import eris.decimal.arithmetic;
import eris.decimal.context;
import eris.decimal.conv;
import eris.decimal.rounding;
import eris.integer.extended;


unittest {
	writeln("==========================");
	writeln("decimal64............begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
}

// BigDecimal with the same context as Dec32
private alias big64 = BigDecimal!(16, 369, Rounding.HALF_UP);

struct Dec64 {

public enum Context context = Context(17, Rounding.HALF_UP);

    /// Returns an equivalent BigDecimal number
    @property
	public big64 toBigDecimal() const {
		if (isFinite) {
			return big64(sign, coefficient, exponent);
		}
		if (isInfinite) {
			return big64.infinity(sign);
		}
		// number is a NaN
		big64 dc;
		dc = isQuiet ? big64.nan(payload) : big64.snan(payload);
		dc.sign = sign;
		return dc;
	}
    //Provide implicit conversion of Dec64 to BigDecimal
	private alias toBigDecimal this;

	unittest {
		write("-- toBigDecimal.....");
		big64 x;
	//	writefln("x = %s", x);
		Dec64 a = PI;
	//writefln("a = %s", a);
		x = 123.45;
	//	writefln("x = %s", x);
		x = a.toBigDecimal;
	//	writefln("x = %s", x);
		big64 d = a; //writefln("a = %s", a);
	//writefln("d = %s", d);
		writeln("passed");
	}

private:
	// The total number of bits in the decimal number.
	// This is equal to the number of bits in the underlying integer;
	// (must be 32, 64, or 128).
	enum uint bitLength = 64;

	// the number of bits in the sign bit (1, obviously)
	enum uint signBit = 1;

	// The number of bits in the unsigned value of the decimal number.
	enum uint unsignedBits = 63; // = bitLength - signBit;

	// The number of bits in the (biased) exponent.
	enum uint expoBits = 10;

	// The number of bits in the coefficient when the value is
	// explicitly represented.
	enum uint explicitBits = 53;

	// The number of bits used to indicate special values and implicit
	// representation
	enum uint testBits = 2;

	// The number of bits in the coefficient when the value is implicitly
	// represented. The three missing bits (the most significant bits)
	// are always '100'.
	enum uint implicitBits = 51; // = explicitBits - testBits;

	// The number of special bits, including the two test bits.
	// These bits are used to denote infinities and NaNs.
	enum uint specialBits = 4;

	// The number of bits that follow the special bits.
	// Their number is the number of bits in a special value
	// when the others (sign and special) are accounted for.
	enum uint spclPadBits = 59;
	// = bitLength - specialBits - signBit;

	// The number of infinity bits, including the special bits.
	// These bits are used to denote infinity.
	enum uint infinityBits = 5;

	// The number of bits that follow the special bits in infinities.
	// These bits are always set to zero in canonical representations.
	// Their number is the remaining number of bits in an infinity
	// when all others (sign and infinity) are accounted for.
	enum uint infPadBits = 58;
	// = bitLength - infinityBits - signBit;

	// The number of nan bits, including the special bits.
	// These bits are used to denote NaN.
	enum uint nanBits = 6;

	// The number of bits in the payload of a NaN.
	enum uint payloadBits = 16;

	// The number of bits that follow the nan bits in NaNs.
	// These bits are always set to zero in canonical representations.
	// Their number is the remaining number of bits in a NaN
	// when all others (sign, nan and payload) are accounted for.
	enum uint nanPadBits = 41;
	// = bitLength - payloadBits - specialBits - signBit;

	// length of the coefficient in decimal digits.
	enum int PRECISION = 16;
	// The maximum coefficient that fits in an explicit number.
	enum ulong C_MAX_EXPLICIT = 0x1FFFFFFFFFFFFF;  // = 36028797018963967
	// The maximum coefficient allowed in an implicit number.
	enum ulong C_MAX_IMPLICIT = 9999999999999999;  // = 0x2386F26FC0FFFF
	// masks for coefficients
	enum ulong C_IMPLICIT_MASK = 0x1FFFFFFFFFFFFF;
	enum ulong C_EXPLICIT_MASK = 0x7FFFFFFFFFFFFF;

	// The maximum biased exponent. The largest binary number that can fit
	// in the width of the exponent field without setting
	// either of the first two bits to 1.
	enum uint MAX_BIASED = 0x2FF;	// = 767
	// The exponent bias. The exponent is stored as an unsigned number and
	// the bias is subtracted from the unsigned value to give the true
	// (signed) exponent.
	enum int BIAS = 398;			// = 0x18E
	// The maximum representable exponent.
	enum int MAX_EXPO = 369;		// MAX_BIASED - BIAS

private:
	// union providing different views of the number representation.
	union {

		// entire 64-bit unsigned integer
		ulong intBits = 0x7C00000000000000;	// set to the initial value: NaN

		// unsigned value and sign bit
		mixin (bitfields!(
			ulong, "uBits", unsignedBits,
			bool, "signed", signBit)
		);
		// Ex = explicit finite number:
		//	   full coefficient, exponent and sign
		mixin (bitfields!(
			ulong, "mantEx", explicitBits,
			uint, "expoEx", expoBits,
			bool, "signEx", signBit)
		);
		// Im = implicit finite number:
		//		partial coefficient, exponent, test bits and sign bit.
		mixin (bitfields!(
			ulong, "mantIm", implicitBits,
			uint, "expoIm", expoBits,
			uint, "testIm", testBits,
			bool, "signIm", signBit)
		);
		// Spcl = special values: non-finite numbers
		//		unused bits, special bits and sign bit.
		mixin (bitfields!(
			ulong, "padSpcl",  spclPadBits,
			uint, "testSpcl", specialBits,
			bool, "signSpcl", signBit)
		);
		// Inf = infinities:
		//		payload, unused bits, infinitu bits and sign bit.
		mixin (bitfields!(
			uint, "padInf",  infPadBits,
			ulong, "testInf", infinityBits,
			bool, "signInf", signBit)
		);
		// Nan = not-a-number: qNaN and sNan
		//		payload, unused bits, nan bits and sign bit.
		mixin (bitfields!(
			ushort, "pyldNaN", payloadBits,
			ulong, "padNaN",  nanPadBits,
			uint, "testNaN", nanBits,
			bool, "signNaN", signBit)
		);
	}

//--------------------------------
//	special bit patterns
//--------------------------------

private:
	// The value of the (6) special bits when the number is a signaling NaN.
	enum uint SIG_BITS = 0x3F;
	// The value of the (6) special bits when the number is a quiet NaN.
	enum uint NAN_BITS = 0x3E;
	// The value of the (5) special bits when the number is infinity.
	enum uint INF_BITS = 0x1E;

//--------------------------------
//	special values and constants
//--------------------------------

// Integer values passed to the constructors are not copied but are modified
// and inserted into the sign, coefficient and exponent fields.
// This enum is used to force the constructor to copy the bit pattern,
// rather than treating it as a integer.
private:
	static enum Bits : ulong
	{
		POS_SIG = 0x7E00000000000000,
		NEG_SIG = 0xFE00000000000000,
		POS_NAN = 0x7C00000000000000,
		NEG_NAN = 0xFC00000000000000,
		POS_INF = 0x7800000000000000,
		NEG_INF = 0xF800000000000000,
		POS_ZRO = 0x31C0000000000000,
		NEG_ZRO = 0xB1C0000000000000,
		POS_MAX = 0x77FB86F26FC0FFFF,
		NEG_MAX = 0xF7FB86F26FC0FFFF,
		POS_ONE = 0x31C0000000000001,
		NEG_ONE = 0xB1C0000000000001,
		POS_TWO = 0x31C0000000000002,
		NEG_TWO = 0xB1C0000000000002,
		POS_FIV = 0x31C0000000000005,
		NEG_FIV = 0xB1C0000000000005,
		POS_TEN = 0x31C000000000000A,
		NEG_TEN = 0xB1C000000000000A,

		// pi and related values
		PI		 = 0x2FEB29430A256D21,
		TAU 	 = 0x2FF65286144ADA42,
		PI_2	 = 0x2FE594A18512B691,
		PI_SQR	 = 0x6BFB105A58668B4F,
		SQRT_PI  = 0x2FE64C099229FBAC,
		SQRT_2PI = 0x2FE8E7C3DFE4B159,
		INV_PI   = 0x2FCB4F02F4F1DE53,
		// 1/SQRT_PI
		// 1/SQRT_2PI

		// logarithms
		E		= 0x2FE9A8434EC8E225,
		LOG2_E	= 0x2FE5201F9D6E7083,
		LOG10_E = 0x2FCF6DE2A337B1C6,
		LN2 	= 0x2FD8A0230ABE4EDD,
		LOG10_2 = 0x2FCAB1DA13953844,
		LN10	= 0x2FE82E305E8873FE,
		LOG2_10 = 0x2FEBCD46A810ADC2,
/*	Dec32 diff
		PHI 	= 0x2F98B072,
		GAMMA	= 0x2F58137D,
*/
		// roots and squares of common values
		SQRT2	= 0x2FE50638410593E7,
		SQRT1_2 = 0x2FD91F19451BE383
	}


public:
	enum Dec64 NAN 	 = Dec64(Bits.POS_NAN);
	enum Dec64 SNAN	 = Dec64(Bits.POS_SIG);
	enum Dec64 INFINITY = Dec64(Bits.POS_INF);
	enum Dec64 NEG_INF  = Dec64(Bits.NEG_INF);
	enum Dec64 ZERO	 = Dec64(Bits.POS_ZRO);
	enum Dec64 NEG_ZERO = Dec64(Bits.NEG_ZRO);
	enum Dec64 MAX 	 = Dec64(Bits.POS_MAX);
	enum Dec64 NEG_MAX  = Dec64(Bits.NEG_MAX);

	// small integers
	enum Dec64 ONE 	 = Dec64(Bits.POS_ONE);
	enum Dec64 NEG_ONE  = Dec64(Bits.NEG_ONE);
	enum Dec64 TWO 	 = Dec64(Bits.POS_TWO);
	enum Dec64 NEG_TWO  = Dec64(Bits.NEG_TWO);
	enum Dec64 FIVE	 = Dec64(Bits.POS_FIV);
	enum Dec64 NEG_FIVE = Dec64(Bits.NEG_FIV);
	enum Dec64 TEN 	 = Dec64(Bits.POS_TEN);
	enum Dec64 NEG_TEN  = Dec64(Bits.NEG_TEN);

	// mathamatical constants
	enum Dec64 TAU 	 = Dec64(Bits.TAU);
	enum Dec64 PI		 = Dec64(Bits.PI);
	enum Dec64 PI_2	 = Dec64(Bits.PI_2);
	enum Dec64 PI_SQR	 = Dec64(Bits.PI_SQR);
	enum Dec64 SQRT_PI  = Dec64(Bits.SQRT_PI);
	enum Dec64 SQRT_2PI = Dec64(Bits.SQRT_2PI);
	enum Dec64 INV_PI   = Dec64(Bits.INV_PI);

	enum Dec64 E		 = Dec64(Bits.E);
	enum Dec64 LOG2_E	 = Dec64(Bits.LOG2_E);
	enum Dec64 LOG10_E  = Dec64(Bits.LOG10_E);
	enum Dec64 LN2 	 = Dec64(Bits.LN2);
	enum Dec64 LOG10_2  = Dec64(Bits.LOG10_2);
	enum Dec64 LN10	 = Dec64(Bits.LN10);
	enum Dec64 LOG2_10  = Dec64(Bits.LOG2_10);
	enum Dec64 SQRT2	 = Dec64(Bits.SQRT2);
	enum Dec64 SQRT1_2	 = Dec64(Bits.SQRT1_2);
/*	Dec32 diff
	enum Dec64 PHI 	 = Dec64(Bits.PHI);
	enum Dec64 GAMMA	 = Dec64(Bits.GAMMA);
*/
	// boolean constants
	enum Dec64 TRUE	 = ONE;
	enum Dec64 FALSE	 = ZERO;



//--------------------------------
//	constructors
//--------------------------------

	/// Creates a Dec64 from a special value.
	private this(const Bits bits) {
		intBits = bits;
	}

	/// Creates a Dec64 from a long integer.
	public this(long n) {
		this = zero;
		signed = n < 0;
		coefficient = signed ? -n : n;
	}

	unittest {	// this(long)
		write("-- this(long).......");
		Dec64 num;
		num = Dec64(123456789012345678L);
		assertEqual(num,"1.234567890123457E+17");
		num = Dec64(0);
		assertStringEqual(num,"0");
		num = Dec64(1);
		assertStringEqual(num,"1");
		num = Dec64(-1);
		assertStringEqual(num,"-1");
		num = Dec64(5);
		assertStringEqual(num,"5");
		writeln("passed");
	}

	/// Creates a Dec64 from a boolean value.
	public this(bool value) {
		this = value ? ONE : ZERO;
	}

	unittest {
		write("-- this(bool).......");
		Dec64 dc;
		dc = Dec64(false);
	//	assertEqual(dc, 1);	// TODO: doesn't work
		assertEqual(dc, ZERO);
	//	assertFalse(dc);	// TODO: doesn't work
	//	writefln("dc = %s", dc);
		dc = Dec64(true);
		assertEqual(dc, ONE);
	//writefln("dc = %s", dc);
		writeln("passed");
	}

	/// Creates a Dec64 from a long integer coefficient and an int exponent.
	public this(long mant, int expo) {
		this(mant);
		exponent = exponent + expo;
	}

	unittest {	// this(long,int)
		write("-- this(long,int)...");
		Dec64 num;
		num = Dec64(1234567890L, 5);
		assertEqual(num,"1.234567890E+14");
		num = Dec64(0, 2);
		assertStringEqual(num,"0E+2");
		num = Dec64(1, 75);
		assertStringEqual(num,"1E+75");
		num = Dec64(-1, -75);
		assertStringEqual(num,"-1E-75");
		num = Dec64(5, -3);
		assertStringEqual(num,"0.005");
		num = Dec64(true, 1234567890L, 5);
		assertEqual(num,"-1.234567890E+14");
		num = Dec64(0, 0, 2);
		assertStringEqual(num,"0E+2");
		writeln("passed");
	}

	///Creates a Dec64 from a boolean sign, an unsigned long
	/// coefficient, and an integer exponent.
	public this(bool sign, ulong mant, int expo) {
		this(mant, expo);
		signed = sign;
	}

	unittest {	// this(bool, ulong, int)
		write("-- this(sn,co,ex)...");
		Dec64 num;
		num = Dec64(1234567890L, 5);
		assertEqual(num,"1.234567890E+14");
		num = Dec64(0, 2);
		assertEqual(num,"0E+2");
		num = Dec64(1, 75);
		assertEqual(num,"1E+75");
		num = Dec64(-1, -75);
		assertEqual(num,"-1E-75");
		num = Dec64(5, -3);
		assertEqual(num,"0.005");
		num = Dec64(true, 1234567890L, 5);
		assertEqual(num,"-1.234567890E+14");
		num = Dec64(0, 0, 2);
		assertEqual(num,"0E+2");
		writeln("passed");
	}

	/// Creates a Dec64 from a BigDecimal
	public this(in big64 arg) {

		big64 dc = plus(arg);

		// if finite, copy and return the copy
		if (dc.isFinite) {
			this = zero;
			this.coefficient = cast(ulong)dc.coefficient.toLong;
			this.exponent = dc.exponent;
			this.sign = dc.sign;
			return;
		}
		// check for special values
		if (dc.isInfinite) {
			this = Dec64.infinity(dc.sign);
			return;
		}
		if (dc.isQuiet) {
			this = nan(dc.payload);
			this.sign = dc.sign;
			return;
		}
		if (dc.isSignaling) {
			this = snan(dc.payload);
			this.sign = dc.sign;
			return;
		}
		// shouldn't reach this point; return NaN.
		this = nan;
	}

	unittest {	// this(big64)
		write("-- this(BigDecimal).");
		big64 dec = 0;
		Dec64 num = dec;
		assertStringEqual(dec,num.toString);
		dec = 1;
		num = Dec64(dec);
		assertStringEqual(dec,num.toString);
		dec = -1;
		num = Dec64(dec);
		assertStringEqual(dec,num.toString);
		dec = -16000;
		num = Dec64(dec);
		assertStringEqual(dec,num.toString);
		dec = uint.max;
		num = Dec64(dec);
		assertEqual(num,"4294967295");
		assertEqual(dec,"4294967295");
		dec = 9999999E+12;
		num = Dec64(dec);
		assertStringEqual(dec,num.toString);
		writeln("passed");
	}

	/// Creates a Dec64 from a string.
	public this(string str) {
		big64 dc = big64(str);
		this(dc);
	}

	unittest {	// this(string)
		write("-- this(string).....");
		Dec64 num;
		num = Dec64("1.234568E+9");
		assertStringEqual(num,"1.234568E+9");
		num = Dec64("NaN");
		assertTrue(num.isQuiet && num.isSpecial && num.isNaN);
		num = Dec64("-inf");
		assertTrue(num.isInfinite && num.isSpecial && num.isNegative);
		writeln("passed");
	}

	///	Constructs a number from a real value.
	public this(in real r) {
		string str;
		if (!std.math.isFinite(r)) {
			str = std.math.isInfinity(r) ? "Infinity" : "NaN";
			this(str);
			this.sign = cast(bool)std.math.signbit(r);
			return;
		}
		str = format("%.*G", cast(int)context.precision + 2, r);
		this(str);
	}

	unittest {	// this(real)
		write("-- this(real).......");
		float f = 1.2345E+16f;
		Dec64 actual = Dec64(f);
		Dec64 expect = Dec64("1.234499980283085E+16");
		assertEqual(actual, expect);
		real r = 1.2345E+16;
		actual = Dec64(r);
		expect = Dec64("1.2345E+16");
		assertEqual(actual, expect);
		writeln("passed");
	}

	/// Copy constructor.
	public this(in Dec64 that) {
		this.bits = that.bits;
	}

	/// Returns a mutable copy
	public Dec64 dup() const {
		return Dec64(this);
	}

	/// Returns a mutable copy
	public Dec64 copy() const {
		return dup;
	}

	/// Returns a mutable copy
	public Dec64 copyNegate() const {
		Dec64 copy = dup;
		copy.sign = !sign;
		return copy;
	}

	public Dec64 copyAbs() const  {
		Dec64 copy = dup;
		copy.sign = false;
		return copy;
	}

//--------------------------------
//	properties
//--------------------------------

public:

	/// Returns the raw bits of the number.
	@property
	ulong bits() const {
		return intBits;
	}

	/// Sets the raw bits of the number.
	@property
	ulong bits(const ulong raw) {
		intBits = raw;
		return intBits;
	}

	/// Returns the sign of the number.
	@property
	bool sign() const {
		return signed;
	}

	/// Sets the sign of the number and returns the sign.
	@property
	bool sign(bool value) {
		signed = value;
		return signed;
	}

	/// Returns the exponent of the number.
	/// The exponent is undefined for infinities and NaNs: zero is returned.
	@property
	int exponent() const {
		if (this.isExplicit) {
			return expoEx - BIAS;
		}
		if (this.isImplicit) {
			return expoIm - BIAS;
		}
		// infinity or NaN.
		return 0;
	}

	/// Sets the exponent of the number.
	/// If the number is infinity or NaN, the number is converted to
	/// a quiet NaN and the invalid operation flag is set.
	/// Otherwise, if the input value exceeds the maximum allowed exponent,
	/// the number is converted to infinity and the overflow flag is set.
	/// If the input value is less than the minimum allowed exponent,
	/// the number is converted to zero, the exponent is set to tinyExpo
	/// and the underflow flag is set.
	@property
	int exponent(int expo) {
		// check for overflow
		if (expo > maxExpo) {
			this = signed ? NEG_INF : INFINITY;
			contextFlags.setFlags(OVERFLOW);
			return 0;
		}
		// check for underflow
		if (expo < minExpo) {
			// if the exponent is too small even for a subnormal number,
			// the number is set to zero.
			if (expo < tinyExpo) {
				this = signed ? NEG_ZERO : ZERO;
				expoEx = tinyExpo + BIAS;
				contextFlags.setFlags(SUBNORMAL);
				contextFlags.setFlags(UNDERFLOW);
				return tinyExpo;
			}
			// at this point the exponent is between minExpo and tinyExpo.
			// NOTE: I don't think this needs special handling
		}
		// if explicit...
		if (this.isExplicit) {
			expoEx = expo + BIAS;
			return expoEx;
		}
		// if implicit...
		if (this.isFinite) {
			expoIm = expo + BIAS;
			return expoIm;
		}
		// if this point is reached the number is either infinity or NaN;
		// these have undefined exponent values.
		contextFlags.setFlags(INVALID_OPERATION);
		this = nan;
		return 0;
	}

	unittest {	// exponent
		write("-- exponent.........");
		Dec64 num;
		// reals
		num = std.math.PI;
		assertEqual(num.exponent, -15);
		num = 9.75E89;
		assertEqual(num.exponent, 74);
		// explicit
		num = 8388607;
		assertEqual(num.exponent, 0);
		// implicit
		num = 8388610;
		assertEqual(num.exponent, 0);
		num = 9.999998E23;
		assertEqual(num.exponent, 8);
		num = 9.999999E23;
		assertEqual(num.exponent, 8);
		// setter
		num = Dec64(-12000,5);
		num.exponent = 10;
		assertEqual(num.exponent, 10);
		num = Dec64(-9000053,-14);
		num.exponent = -27;
		assertEqual(num.exponent, -27);
		num = Dec64.infinity;
		assertEqual(num.exponent, 0);
		writeln("passed");
	}

	/// Returns the coefficient of the number.
	/// The exponent is undefined for infinities and NaNs: zero is returned.
	@property
	ulong coefficient() const {
		if (this.isExplicit) {
			return mantEx;
		}
		if (this.isFinite) {
			return mantIm | (4UL << implicitBits);
		}
		// Infinity or NaN.
		return 0;
	}


	// Sets the coefficient of this number. This may cause an
	// explicit number to become an implicit number, and vice versa.
	// Sets the coefficient of this number. This may cause an
	// explicit number to become an implicit number, and vice versa.
	@property
	ulong coefficient(ulong mant) {
		// if not finite return 0.
		if (!this.isFinite) {
			return 0;
		}
		// if too large for explicit representation, round
		if (mant > C_MAX_IMPLICIT) {
			int expo = 0;
			uint digits = numDigits(mant);
			expo = setExponent(sign, mant, digits, precision);
			if (this.isExplicit) {
				expoEx = expoEx + expo;
			}
			else {
				expoIm = expoIm + expo;
			}
		}
		// at this point, the number <=
		if (mant <= C_MAX_EXPLICIT) {
			// if implicit, convert to explicit
			if (this.isImplicit) {
				expoEx = expoIm;
			}
			mantEx = /*cast(ulong)*/mant;
			return mantEx;
		}
		else {	// mant <= C_MAX_IMPLICIT;
			// if explicit, convert to implicit
			if (this.isExplicit) {
				expoIm = expoEx;
				testIm = 0x3;
			}
			mantIm = cast(ulong)mant & C_IMPLICIT_MASK;
			return mantIm | (0b100UL << implicitBits);
		}
	}

	unittest {	// coefficient
		write("-- coefficient......");
		Dec64 num;
		big64 dec;
		assertEqual(num.coefficient, 0);
		num = 9.998743;
		assertEqual(num.coefficient, 9998742999999999);
		num = Dec64(9999213,-6);
		assertEqual(num.coefficient, 9999213);
		num = Dec64(-125);
		assertEqual(num.coefficient, 125);
		dec = big64(-29999999);
		num = Dec64(-299999999999999999L);
		assertEqual(num.coefficient, 3000000000000000);
		num = Dec64(-999999999999999999);
		assertEqual(num.coefficient, 1000000000000000);
		num = Dec64(-999999999999999909);
		assertEqual(num.coefficient, 9999999999999999);
		num = Dec64(999999999999999999L);
		assertEqual(num.coefficient, 1000000000000000);
		writeln("passed");
	}

	private static int setExponent(
			bool sign, ref ulong mant, ref uint digits, int precision) {

		uint inDigits = digits;

		ulong remainder = getRemainder(mant, digits, precision);
		int expo = inDigits - digits;

		if (remainder == 0) return expo;

		if (numDigits(remainder) < expo) {
			// no rounding required
			return expo;
		}
		// round half-up
		if (firstDigit(remainder) >= 5) {
			mant++;
			digits = numDigits(mant);
		}
		if (digits > precision) {
			mant /= 10;
			expo++;
			digits--;
		}
		return expo;
	}

	private static ulong getRemainder(ref ulong mant, ref uint digits, int precision) {
		ulong remainder = 0;
		int diff = digits - precision;
		if (diff == 0) {
			return remainder;
		}


		ulong divisor = 10L^^diff;
		ulong dividend = mant;
		ulong quotient = dividend/divisor;
		mant = quotient;
		remainder = dividend % divisor;
		digits = precision;
		return remainder;
	}

	/// Returns the number of digits in the number's coefficient. This number
	/// will always be
	@property
	int digits() const {
		if (isSpecial) return 0;
		if (coefficient > (10^^precision-1)) return precision;
		return numDigits(this.coefficient);
	}

	/// Returns the payload of the number.
	/// If this is a NaN, returns the value of the payload bits.
	/// Otherwise returns zero.
	@property
	ushort payload() const {
		if (this.isNaN) {
			return pyldNaN;
		}
		return 0;
	}

	/// Sets the payload of the number.
	/// If the number is not a NaN (har!) no action is taken and zero
	/// is returned.
	@property
	ushort payload(ushort value) {
		if (isNaN) {
			pyldNaN = value;
			return pyldNaN;
		}
		return 0;
	}

	unittest {	// payload
		write("-- payload..........");
		Dec64 num;
		assertEqual(num.payload,0);
		num = Dec64.snan;
		assertEqual(num.payload,0);
		num.payload = 234;
		assertEqual(num.payload,234);
		assertStringEqual(num,"sNaN234");
		num.payload = 13234;
		assertEqual(num.payload,13234);
		assertStringEqual(num,"sNaN13234");
		num.payload = cast(ushort)218234;
		assertEqual(num.payload,21626);
		assertStringEqual(num,"sNaN21626");
//		num = 1234567;
//		assertEqual(num.payload,0);
		writeln("passed");
	}

//--------------------------------
//	constants
//--------------------------------

	static Dec64 zero(bool signed = false) {
		return signed ? NEG_ZERO : ZERO;
	}

	static Dec64 one(bool signed = false) {
		return signed ? NEG_ONE : ONE;
	}

	static Dec64 max(bool signed = false) {
		return signed ? NEG_MAX : MAX;
	}

	static Dec64 infinity(bool signed = false) {
		return signed ? NEG_INF : INFINITY;
	}

	static Dec64 nan(ushort payload = 0) {
		if (payload) {
			Dec64 result = NAN;
			result.payload = payload;
			return result;
		}
		return NAN;
	}

	static Dec64 snan(ushort payload = 0) {
		if (payload) {
			Dec64 result = SNAN;
			result.payload = payload;
			return result;
		}
		return SNAN;
	}

	/// Maximum length of the coefficient in decimal digits.
	public enum int precision = PRECISION;
	/// Maximum value of the exponent.
	public enum int maxExpo = MAX_EXPO;
	/// Maximum value of the adjusted exponent.
	public enum int maxAdjustedExpo = MAX_EXPO - (PRECISION - 1);
	/// Smallest normalized exponent.
	public enum int minExpo = 1 - MAX_EXPO;
	/// Smallest non-normalized exponent.
	public enum int tinyExpo = 2 - MAX_EXPO - PRECISION;
	/// Rounding mode.
	/// Returns the radix (always 10 for decimal numbers)
	public enum int radix = 10;

	// floating point properties
	public enum Dec64 init    		= NAN;
//	public enum Dec64 epsilon 		= EPSILON;
//	public enum Dec64 min     		= MIN_SUB;
//	public enum Dec64 min_normal	= MIN_NRM;

	static int dig()		{ return 7; }
	static int mant_dig()	{ return 24; }
	enum max_10_exp() { return maxExpo; }
	static int min_10_exp() { return minExpo; }
	static int max_exp()	{ return cast(int)(maxExpo/std.math.LOG2); }
	static int min_exp()	{ return cast(int)(minExpo/std.math.LOG2); }


//--------------------------------
//	classification properties
//--------------------------------

	/// Returns true if the number's representation is canonical.
	/// Finite numbers are always canonical.
	/// Infinities and NaNs are canonical if their unused bits are zero.
	public bool isCanonical() const {
		if (isInfinite) return padInf == 0;
		if (isNaN) return signed == 0 && padNaN == 0;
		// finite numbers are always canonical
		return true;
	}

	/// Returns a copy of the number in canonical form.
	/// Finite numbers are always canonical.
	/// Infinities and NaNs are canonical if their unused bits are zero.
	public Dec64 canonical() const {
		Dec64 copy = this;
		if (this.isFinite) return copy;
		if (this.isInfinite) {
			copy.padInf = 0;
			return copy;
		}
		else { /* isNaN */
			copy.signed = 0;
			copy.padNaN = 0;
			return copy;
		}
	}

	/// Returns true if the number is +\- zero.
	public bool isZero() const {
		return isExplicit && mantEx == 0;
	}

	/// Returns true if the number is a quiet or signaling NaN.
	public bool isNaN() const {
		return testNaN == NAN_BITS || testNaN == SIG_BITS;
	}

	/// Returns true if the number is a signaling NaN.
	public bool isSignaling() const {
		return testNaN == SIG_BITS;
	}

	/// Returns true if the number is a quiet NaN.
	public bool isQuiet() const {
		return testNaN == NAN_BITS;
	}

	/// Returns true if the number is +\- infinity.
	public bool isInfinite() const {
		return testInf == INF_BITS;
	}

	/// Returns true if the number is neither infinite nor a NaN.
	public bool isFinite() const {
		return testSpcl != 0xF;
	}

	/// Returns true if the number is a NaN or infinity.
	public bool isSpecial() const {
		return testSpcl == 0xF;
	}

	/// Returns true if all bits of the coefficient are explicitly represented.
	public bool isExplicit() const {
		return testIm != 0x3;
	}

	/// Returns true if the first bits of the coefficient are implicit.
	public bool isImplicit() const {
		return testIm == 0x3 && testSpcl != 0xF;
	}

	/// Returns true if the number is negative. Negative numbers include
	/// -0 and -infinity. A NaN can be negative but the sign is always ignored.
	public bool isSigned() const {
		return signed;
	}

	/// Returns true if the number is negative. Negative numbers include
	/// -0 and -infinity. A NaN can be negative but the sign is always ignored.
	public bool isNegative() const {
		return signed;
	}

	/// Returns true if the number is not negative. -0 is negative.
	public bool isPositive() const {
		return !isNegative;
	}

	unittest {	// classification
		write("-- classification...");
		Dec64 num;
		num = Dec64.snan;
		assertTrue(num.isSignaling);
		assertTrue(num.isNaN);
		assertFalse(num.isNegative);
		assertFalse(num.isNormal);
		num.sign = true;
		assertTrue(num.isSignaling);
		assertTrue(num.isNaN);
		assertTrue(num.isNegative);
		assertFalse(num.isNormal);
		num = Dec64.nan;
		assertFalse(num.isSignaling);
		assertTrue(num.isNaN);
		assertFalse(num.isNegative);
		assertFalse(num.isNormal);
		num.sign = true;
		assertFalse(num.isSignaling);
		assertTrue(num.isNaN);
		assertTrue(num.isNegative);
		assertTrue(num.isQuiet);
		num = Dec64.infinity;
		assertTrue(num.isInfinite);
		assertFalse(num.isNaN);
		assertFalse(num.isNegative);
		assertFalse(num.isNormal);
		num = Dec64.infinity(true);
		assertFalse(num.isSignaling);
		assertTrue(num.isInfinite);
		assertTrue(num.isNegative);
		assertFalse(num.isFinite);
		num = Dec64.zero;
		assertTrue(num.isFinite);
		assertTrue(num.isZero);
		assertFalse(num.isNegative);
		assertFalse(num.isNormal);
		num = Dec64.zero(true);
		assertFalse(num.isSignaling);
		assertTrue(num.isZero);
		assertTrue(num.isNegative);
		assertTrue(num.isFinite);
		writeln("passed");
	}

	/// Returns true if this number is a true value.
	/// Non-zero finite numbers are true.
	/// Infinity is true and NaN is false.
	public bool isTrue() const {
		return isFinite && !isZero || isInfinite;
	}

	/// Returns true if this number is a false value.
	/// Finite numbers with zero coefficient are false.
	/// Infinity is true and NaN is false.
	public bool isFalse() const {
		return isNaN || isZero;
	}

	unittest {	//isTrue/isFalse
		write("-- isTrue/isFalse...");
		assertTrue(Dec64("1").isTrue);
		assertFalse(Dec64("0").isTrue);
		assertTrue(Dec64.infinity.isTrue);
		assertFalse(Dec64.nan.isTrue);

		assertTrue(Dec64("0").isFalse);
		assertFalse(Dec64("1").isFalse);
		assertFalse(Dec64.infinity.isFalse);
		assertTrue(Dec64.nan.isFalse);
		writeln("passed");
	}

	/// Returns true if the coefficient of this number is zero.
	// TODO: (language) what is the purpose of this function?
	public bool isZeroCoefficient() const {
		return !isSpecial && coefficient == 0;
	}

	unittest {	// isZeroCoefficient
		write("-- isZeroCoefficient..");
		Dec64 num;
		num = Dec64.zero;
		assertTrue(num.isZeroCoefficient);
		num = Dec64.zero(true);
		assertTrue(num.isZeroCoefficient);
		num = Dec64("0E+4");
		assertTrue(num.isZeroCoefficient);
		num = 12345;
		assertFalse(num.isZeroCoefficient);
		num = 1.5;
		assertFalse(num.isZeroCoefficient);
		num = Dec64.nan;
		assertFalse(num.isZeroCoefficient);
		num = Dec64.infinity;
		assertFalse(num.isZeroCoefficient);
		writeln("passed");
	}

	/// Returns true if the number is subnormal.
	/// NOTE: zero is not subnormal.
	public bool isSubnormal() const {
		if (isZero || isSpecial) return false;
		return adjustedExponent < minExpo;
	}

	/// Returns true if the number is normal.
	/// NOTE: zero is not normal.
	public bool isNormal() const {
		if (isZero || isSpecial) return false;
		return adjustedExponent >= minExpo;
	}

	/// Returns the value of the adjusted exponent.
	int adjustedExponent() const {
		if (isSpecial) return 0;
		return exponent + digits - 1;
	}

	/// Returns true if the number is an integer.
	public bool isIntegralValued() const {
		if (isSpecial) return false;
		if (exponent >= 0) return true;
		uint expo = std.math.abs(exponent);
		if (expo >= PRECISION) return false;
		if (coefficient % 10^^expo == 0) return true;
		return false;
	}

	unittest {	// isIntegralValued
		write("-- isIntegralValued.");
		Dec64 num;
		num = 22;
		assertTrue(num.isIntegralValued);
		num = 200E-2;
		assertTrue(num.isIntegralValued);
		num = 201E-2;
		assertFalse(num.isIntegralValued);
		num = Dec64.INFINITY;
		assertFalse(num.isIntegralValued);
		writeln("passed");
	}

//--------------------------------
//	conversions
//--------------------------------

	int toInt() const {
		if (isNaN) {
			contextFlags.setFlags(INVALID_OPERATION);
			return 0;
		}
		if (this > Dec64(int.max) || (isInfinite && !isSigned)) return int.max;
		if (this < Dec64(int.min) || (isInfinite &&  isSigned)) return int.min;
		Dec64 temp = roundToIntegralExact!big64(this);
		int n = cast(int)temp.coefficient;
		return signed ? -n : n;
	}

	unittest {	// toInt
		write("-- toInt............");
		Dec64 num;
		num = 12345;
		assertEqual(num.toInt, 12345);
		num = 123.45;
		assertEqual(num.toInt, 123);
		num = 1.0E6;
		assertEqual(num.toInt, 1000000);
		num = -1.0E60;
		assertEqual(num.toInt, int.min);
		num = Dec64.infinity(true);
		assertEqual(num.toInt, int.min);
		writeln("passed");
	}

	long toLong() const {
		long n;
		if (isNaN) {
			contextFlags.setFlags(INVALID_OPERATION);
			return 0;
		}
		if (this > long.max || (isInfinite && !isSigned)) return long.max;
		if (this < long.min || (isInfinite &&  isSigned)) return long.min;
		Dec64 temp = Dec64(roundToIntegralExact!big64(this));
		n = temp.coefficient;
		return signed ? -n : n;
	}

	unittest {	// toLong
		write("-- toLong...........");
		Dec64 num;
		num = -12345;
		assert(num.toLong == -12345);
		num = 2 * int.max;
		assert(num.toLong == 2 * int.max);
		num = 1.0E6;
		assert(num.toLong == 1000000);
		num = -1.0E60;
		assert(num.toLong == long.min);
		num = Dec64.NEG_INF;
		assert(num.toLong == long.min);
		writeln("passed");
	}

	real toReal() const {
		if (isNaN) {
			return real.nan;
		}
		if (isInfinite) {
			return isNegative ? -real.infinity : real.infinity;
		}
		if (isZero) {
			return isNegative ? -0.0 : 0.0;
		}
		string str = this.toSciString;
		return to!real(str);
	}

	unittest {	// toReal
		write("-- toReal...........");
		Dec64 num;
		real expect, actual;
		num = Dec64(1.5);
		expect = 1.5;
		actual = num.toReal;
		assertEqual(actual, expect);
		writeln("passed");
	}

	// Converts the number to an exact scientific-style string representation.
	string toSciString() const {
		return sciForm(this);
	}

	// Converts the number to an exact engineering-style string representation.
	string toEngString() const {
		return engForm!Dec64(this);
	}

	// Converts a Dec64 to a standard string
	public string toString() const {
		 return toSciString();
	}

	unittest {	// toString
		write("-- toString.........");
		string str;
		str = "-12.345E-42";
		Dec64 num = Dec64(str);
		assertStringEqual(num,"-1.2345E-41");
		writeln("passed");
	}

	/// Creates an exact representation of the number.
	string toExact() const {
		return eris.decimal.conv.toExact(this);
	}

	unittest {	// toExact
		write("-- toExact..........");
		Dec64 num;
		assertEqual(num.toExact,  "+NaN");
		num = Dec64.max;
		assertEqual(num.toExact,  "+9999999999999999E+369");
		num = 1;
		assertEqual(num.toExact,  "+1E+00");
		num = C_MAX_EXPLICIT;
		assertEqual(num.toExact,  "+9007199254740991E+00");
		num = Dec64.infinity(true);
		assertEqual(num.toExact,  "-Infinity");
		writeln("passed");
	}

	/// Creates an abstract representation of the number.
	string toAbstract() const {
		return eris.decimal.conv.toAbstract(this);
	}

	unittest {	// toAbstract
		write("-- toAbstract.......");
		Dec64 num;
		num = Dec64("-25.67E+2");
		assert(num.toAbstract == "[1,2567,0]");
		writeln("passed");
	}

	/// Converts the number to a hexadecimal string representation.
	string toHexString() const {
		 return format("0x%08X", bits);
	}

	/// Converts the number to a binary string.
	string toBinaryString() const {
		return format("%0#32b", bits);
	}

	unittest {	// toHex, toBinary
		write("-- toHex, toBinary..");
		Dec64 num = 12345;
		assertEqual(num.toHexString, "0x31C0000000003039");
		assertEqual(num.toBinaryString, "11000111000000000000000000000000000000000000000011000000111001");
		writeln("passed");
	}
//--------------------------------
//	comparison
//--------------------------------

	/// Returns -1, 0 or 1, if the number is less than, equal to or
	/// greater than the argument, respectively.
	int opCmp(T:Dec64)(in T that) const {
		return compare!big64(this, that);
	}

	/// Returns -1, 0 or 1, if the number is less than, equal to or
	/// greater than the argument, respectively.
	int opCmp(T)(const T that) const {
		return opCmp!Dec64(Dec64(that));
	}

	 /// Returns true if the number is equal to the specified number.
	bool opEquals(T:Dec64) (in T that) const {
		// quick bitwise check
		if (this.bits == that.bits) {
			if (!this.isSpecial) return true;
			if (this.isQuiet) return false;
			// let the main routine handle the signaling NaN
		}
		return equals!big64(this, that);
	}

	 /// Returns true if the number is equal to the specified number.
	bool opEquals(T)(const T that) const {
		return opEquals!Dec64(Dec64(that));
	}

	/// Returns true if the numbers are identical.
	/// Note that this does not guarantee equality -- NaNs are never equal.
	bool isIdentical(const Dec64 that) const {
		return this.bits == that.bits;
	}

	unittest {	// comparison
		write("-- comparison.......");
		Dec64 a, b;
		a = Dec64(104);
		b = Dec64(105);
		assertTrue(a < b);
		assertTrue(b > a);
		a = Dec64(105);
		assertTrue(a == b);
		int c = 105;
		assertTrue(a == c);
		real d = 105.0;
		assertTrue(a == d);
		assertTrue(a == 105);
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a Dec64 (copies that to this).
	void opAssign(T:Dec64)(in T that) {
		this.intBits = that.intBits;
	}

	///    Assigns a numeric value.
	void opAssign(T)(in T that) {
		this = Dec64(that);
	}

	unittest {	// opAssign
		write("-- opAssign.........");
		Dec64 that, lhs;
		that = Dec64(270E-5);
		lhs = that;
		assertEqual(lhs, that);
		that = 332089;
		assertEqual(that.toString, "332089");
		that = 3.1415E+3;
		assertEqual(that.toString, "3141.5");
		writeln("passed");
	}

//--------------------------------
// unary operators
//--------------------------------

	private Dec64 opUnary(string op)() {
		static if (op == "+") {
			return Dec64(plus!big64(this));
		} else static if (op == "-") {
			return Dec64(minus!big64(this));
		} else static if (op == "++") {
			this = Dec64(add!big64(this, 1));
			return this;
		} else static if (op == "--") {
			this = Dec64(sub!big64(this, 1));
			return this;
		}
	}

	unittest {	// opUnary
		write("-- opUnary..........");
		Dec64 num, actual, expect;
		num = 134;
		expect = num;
		actual = +num;
		assertEqual(actual, expect);
		num = 134.02;
		expect = -134.02;
		actual = -num;
		assertEqual(actual, expect);
		num = 134;
		expect = 135;
		actual = ++num;
		assertEqual(actual, expect);
		num = 1.00E12;
		expect = num-1;
		actual = --num;
		assertEqual(actual, expect);
		actual = num--;
		assertEqual(actual, expect);
		num = 1.00E12;
		expect = num+1;
		actual = ++num;
		assertEqual(actual, expect);
		actual = num++;
		assertEqual(actual, expect);
		num = Dec64(9999999, 90);
		expect = num;
		actual = num++;
		assertEqual(actual, expect);
		num = 12.35;
		expect = 11.35;
		actual = --num;
		assertEqual(actual, expect);
		writeln("passed");
	}

//--------------------------------
// binary operators
//--------------------------------

	T opBinary(string op, T:Dec64)(in T that) const
	{
		static if (op == "+") {
			return Dec64(add!big64(this, that));
		} else static if (op == "-") {
			return Dec64(sub!big64(this, that));
		} else static if (op == "*") {
			return Dec64(mul!big64(this, that));
		} else static if (op == "/") {
			return Dec64(div!big64(this, that));
		} else static if (op == "%") {
			return Dec64(remainder!big64(this, that));
		} else static if (op == "&") {
			return Dec64(and!big64(this, that));
		} else static if (op == "|") {
			return Dec64(or!big64(this, that));
		} else static if (op == "^") {
			return Dec64(xor!big64(this, that));
		}
	}

	Dec64 opBinary(string op, T)(in T that) const {
		return opBinary!(op,Dec64)(Dec64(that));
	}

	unittest {	// opBinary
		write("-- opBinary.........");
		Dec64 op1, op2, actual, expect;
		op1 = 4;
		op2 = 8;
		actual = op1 + op2;
		expect = 12;
		assertEqual(actual, expect);
		actual = op1 - op2;
		expect = -4;
		assertEqual(actual, expect);
		actual = op1 * op2;
		expect = 32;
		assertEqual(actual, expect);
		op1 = 5;
		op2 = 2;
		actual = op1 / op2;
		expect = 2.5;
		assertEqual(actual, expect);
		op1 = 10;
		op2 = 3;
		actual = op1 % op2;
		expect = 1;
		assertEqual(actual, expect);
		op1 = Dec64("101");
		op2 = Dec64("110");
		actual = op1 & op2;
		expect = 100;
		assertEqual(actual, expect);
		actual = op1 | op2;
		expect = 111;
		assertEqual(actual, expect);
		actual = op1 ^ op2;
		expect = 11;
		assertEqual(actual, expect);
		Dec64 num = Dec64(591.3);
		Dec64 result = num * 5;
		assertEqual(result, Dec64(2956.5));
		writeln("passed");
	}

//-----------------------------
// operator assignment
//-----------------------------

	ref Dec64 opOpAssign(string op, T:Dec64) (T that) {
		this = opBinary!op(that);
		return this;
	}

 	ref Dec64 opOpAssign(string op, T) (T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- opOpAssign.......");
		Dec64 op1, op2, actual, expect;
		op1 = 23.56;
		op2 = -2.07;
		op1 += op2;
		expect = 21.49;
		actual = op1;
		assertEqual(actual, expect);
		op1 *= op2;
		expect = -44.4843;
		actual = op1;
		assertEqual(actual, expect);
		op1 = 95;
		op1 %= 90;
		actual = op1;
		expect = 5;
		assertEqual(actual, expect);
		writeln("passed");
	}

	/// Returns uint ten raised to the specified power.
	/// (Used in rounding.)
	static uint pow10(const int n) {
		// TODO: limit n to max exponent
		return cast(uint)TENS[n];
	}

	unittest { // pow10
		write("-- pow10............");
		assert(Dec64.pow10(3) == 1000);
		writeln("passed");
	}

/*	///	Returns the BigInt product of the coefficients.
	/// (Used in arithemtic multiply.)
	public static xint bigmul(const Dec64 arg1, const Dec64 arg2) {
		xint big = xint(arg1.coefficient);
		return big * arg2.coefficient;
	}*/
 }	// end Dec64 struct

unittest { // footer
	writeln("==========================");
	writeln("decimal64..............end");
	writeln("==========================");
}

