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

module eris.decimal.decimal;

import std.conv;
import std.string;

import eris.integer.extended;
import eris.decimal.context;
import eris.decimal.arithmetic;
import eris.decimal.rounding;

version(unittest) {
	import std.stdio;
	import eris.assertions;
}

unittest {
	writeln("==========================");
	writeln("decimal..............begin");
	writeln("==========================");
}

alias xint = ExtendedInt;
//alias dec99 = Decimal!(99,999);
alias dec9 = Decimal!(9,99);

// special values for NaN, Inf, etc.
private enum SV { NONE, INF, QNAN, SNAN };


/// A struct representing an arbitrary-precision decimal floating-point number.
///
/// The implementation follows the General Decimal Arithmetic
/// Specification, Version 1.70 (25 Mar 2009),
/// http://www.speleotrove.com/decimal.
/// This specification conforms with IEEE standard 754-2008.
struct Decimal(int PRECISION = 99, int MAX_EXPO = 9999,
		int GUARD_DIGITS = 2, Rounding ROUNDING_MODE = Rounding.HALF_EVEN) {

alias decimal = Decimal!(PRECISION, MAX_EXPO, GUARD_DIGITS, ROUNDING_MODE);

	private SV sval = SV.QNAN;		// special value: default is quiet NaN
	private bool signed = false;	// true if the value is negative, false otherwise.
	private int expo = 0;			// the exponent of the decimal value
	private xint mant;				// the coefficient of the decimal value
	package int digits; 			// the number of decimal digits in this number.
									// (unless the number is a special value)
	private bool guarded = false;	// true if guard digits are in use, false otherwise.

	// static fields
//	package static Rounding rounding = Rounding.HALF_EVEN;
	package static bool verbose = false; // for debugging


	public:
	/// Maximum length of the coefficient in decimal digits.
	enum int precision = PRECISION;
	/// Maximum value of the adjusted exponent.
	enum int maxExpo = MAX_EXPO;
	/// Smallest normalized exponent.
	enum int minExpo = 1 - maxExpo;
	/// Smallest non-normalized exponent.
	enum int tinyExpo = 2 - maxExpo - precision;
	/// Number of additional (decimal) digits added to prevent rounding errors.
	enum int guardDigits = GUARD_DIGITS;
	/// Rounding mode.
	enum Rounding rounding = ROUNDING_MODE;
	/// Struct containing the precision and rounding mode.
	enum Context context = Context(precision, rounding);

	/// Marker used to identify this type irrespective of size.
	private enum bool IS_DECIMAL = true;

	// decimal special values
	enum decimal NAN		= decimal(SV.QNAN);
	enum decimal SNAN		= decimal(SV.SNAN);
	enum decimal INFINITY	= decimal(SV.INF);
	enum decimal NEG_INF	= decimal(SV.INF, true);
	enum decimal ZERO		= decimal(SV.NONE);
	enum decimal NEG_ZERO	= decimal(SV.NONE, true);

	unittest {	// special values
		write("-- special values...");
		decimal num;
		num = NAN;
		assertStringEqual(num, "NaN");
		num = SNAN;
		assertStringEqual(num, "sNaN");
		num = ZERO;
		assertStringEqual(num, "0");
		num = NEG_ZERO;
		assertStringEqual(num, "-0");
		num = INFINITY;
		assertStringEqual(num, "Infinity");
		num = NEG_INF;
		assertStringEqual(num, "-Infinity");
		writeln("passed");
	}

	// common decimal numbers
	enum decimal HALF		= decimal(5, -1);
	enum decimal ONE		= decimal(1);
	enum decimal NEG_ONE	= decimal(-1);
	enum decimal TWO		= decimal(2);
	enum decimal THREE		= decimal(3);
	enum decimal FIVE		= decimal(5);
	enum decimal TEN		= decimal(10);

//--------------------------------
// construction
//--------------------------------

	///
	/// Constructs a new number given a special value and an optional sign.
	///
	@safe
	public this(const SV sv, const bool sign = false) {
		this.signed = sign;
		this.sval = sv;
	}

	unittest {	// special value construction
		write("-- this(special)....");
		dec9 num = dec9(SV.INF, true);
		assertStringEqual(num, "-Infinity");
		assertStringEqual(num.toAbstract(), "[1,inf]");
		writeln("passed");
	}

	/// Creates a decimal from a boolean value.
	/// false == 0, true == 1
	public this(const bool value) {
		this = zero;
        if (value) mant = 1;
	}

	unittest {	// boolean construction
		write("-- this(bool).......");
		dec9 num = dec9(false);
		assertStringEqual(num, "0");
		num = dec9(true);
		assertStringEqual(num, "1");
		writeln("passed");
	}

	/// Constructs a number from a boolean sign, a xint coefficient and
	/// an optional integer exponent.
	/// The sign of the number is the value of the sign parameter
	/// regardless of the sign of the coefficient.
	/// The intial precision of the number is deduced from the number
	/// of decimal digits in the coefficient.
	//@safe
	this(const bool sign, const xint coefficient, const int exponent = 0) {
		this = zero();
		this.signed = sign;
		this.mant = coefficient.abs;
		this.expo = exponent;
		this.digits = numDigits(this.mant);
	}

	unittest {	// bool, xint, int construction
		write("-- this(bool,n,n)...");
		dec9 num;
		num = dec9(true, xint(7254), 94);
		assertStringEqual(num, "-7.254E+97");
		num = dec9(true, xint(7254), 194);
		num = roundToPrecision(num);
		assertStringEqual(num, "-Infinity");
		writeln("passed");
	}

	/// Constructs a decimal from a xint coefficient and an
	/// optional integer exponent. The sign of the number is the sign
	/// of the coefficient. The initial precision is determined by the number
	/// of digits in the coefficient.
	//@safe
	this(const xint coefficient, const int exponent = 0) {
		bool sign = coefficient.sgn < 0;
		this(sign, coefficient.abs, exponent);
	};

	unittest {	// xint, int construction
		write("-- this(big,int)....");
		dec9 num;
		num = dec9(xint(7254), 94);
		assertStringEqual(num, "7.254E+97");
		num = dec9(xint(-7254));
		assertStringEqual(num, "-7254");
		writeln("passed");
	}

	/// Constructs a number from a sign, a long integer coefficient and
	/// an integer exponent.
	//@safe
	this(const bool sign, const long coefficient, const int exponent) {
		this(sign, xint(coefficient), exponent);
	}

/*	/// Constructs a number from a sign, a long integer coefficient and
	/// an integer exponent.
	this(const bool sign, const xint coefficient, const int exponent, const int digits) {
		xint big = coefficient.abs;
		this = zero();
		this.signed = sign;
		this.mant = big;
		this.expo = exponent;
		this.digits = digits;
	}

	unittest {
		write("w/digits...");
	//	const xint mant = xint(314159);
		xint mant = xint("314159");
		dec9 d = dec9(false, mant, -5, 6);
		writefln("d = %s", d);
		mant = xint("3141590000000000000000000000000000000000000");
		d = dec9(false, mant, -42, 43);
		writefln("d = %s", d);

		writeln("test missing");
	}*/

	/// Constructs a number from a long integer coefficient
	/// and an optional integer exponent.
	this(const long coefficient, const int exponent) {
		this(xint(coefficient), exponent);
	}

	/// Constructs a number from a long integer value
	this(const long coefficient) {
		this(xint(coefficient), 0);
	}

	unittest {	// long value construction
		write("-- this(long).......");
		dec9 num;
		num = dec9(7254, 94);
		assertStringEqual(num, "7.254E+97");
		num = dec9(-7254L);
		assertStringEqual(num, "-7254");
		writeln("passed");
	}

	// TODO: this(str): add tests for just over/under int.max, int.min
	// Constructs a decimal number from a string representation
	this(const string str) {
		this = eris.decimal.conv.toNumber!decimal(str);
	};

	unittest {	// string construction
		write("-- this(string).....");
		dec9 num;
		num = dec9("7254E94");
		assertStringEqual(num, "7.254E+97");
		num = dec9("7254.005");
		assertStringEqual(num, "7254.005");
		string str;
		num = dec9(1, 12334, -5);
		str = "-0.12334";
		assertStringEqual(num,str);
		num = dec9(-23456, 10);
		str = "-2.3456E+14";
		assertStringEqual(num,str);
		num = dec9(234568901234);
		str = "234568901234";
		assertStringEqual(num,str);
		num = dec9("123.457E+29");
		str = "1.23457E+31";
		assertStringEqual(num,str);
		num = std.math.E;
		str = "2.71828183";
		assertStringEqual(num,str);
		num = std.math.std.math.LOG2;
		dec9 copy = dec9(num);
		assertEqual(compareTotal(num, copy), 0);
		writeln("passed");
	}

	// TODO: convert real to decimal w/o going to a string.
	/// Constructs a decimal number from a real value.
	this(const real r) {
		string str = format("%.*G", cast(int)precision, r);
		this(str);
	}

    // TODO: add unittest for real value construction

	// copy constructor
	//@safe
	this(const decimal that) {
		this.signed = that.signed;
		this.sval	= that.sval;
		this.digits = that.digits;
		this.expo	= that.expo;
		this.mant	= that.mant.dup;
		this.guarded = that.guarded;
	};

	/// dup property
	//@safe
	const decimal dup() {
		return decimal(this);
	}

	unittest {	// dup
		write("-- this(decimal)....");
		dec9 num, copy;
		num = std.math.std.math.LOG2;
		copy = dec9(num);
		assertZero(compareTotal(num, copy));
		num = dec9(std.math.PI);
		copy = num.dup;
		assertEqual(num, copy);
		writeln("passed");
	}

	public static T guard(T)(const T x = T.init)  {
		T copy = x.dup;
		copy.guarded = true;
		return copy;
	}


//--------------------------------
// casts
//--------------------------------

/*	static decimal opCast(T:Decimal)(T input) {
		decimal output;
		output.sign = input.sign;
		output.mant = input.mant;
		output.expo = input.expo;
		output.digits = input.digits;
		output.sval = input.sval;
		if (T.precision > output.precision || T.maxExpo > output.maxExpo) {
			output = roundToPrecision(output);
		}
		return output;
	}*/

	// NOTE: will cast to true precision, not temp value
	public T opcast(T)() {
		T that;
		that.sign = this.sign;
		that.sval = this.sval;
		that.expo = this.expo;
		that.digits = this.digits;
		return roundToPrecision(that, T.precision);
	}

	unittest {
		write("casts...");
//		alias dec10 = Decimal!(10,99);
		dec9  bingo = 123.45;
//		dec10 result = cast(dec10)bingo;
//writefln("result = %s", result);
		writeln("test missing");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a decimal number (makes a copy)
	void opAssign(T:decimal)(const T that) {
		this.signed  = that.signed;
		this.sval	 = that.sval;
		this.digits  = that.digits;
		this.expo	 = that.expo;
		this.mant	 = that.mant;
		this.guarded = that.guarded;
	}

	///    Assigns an xint value.
	void opAssign(T:xint)(const T that) {
		this = decimal(that);
	}

// TODO: if a ulong is converted to a long, the sign will be wrong.
	///    Assigns a long value.
	void opAssign(T:long)(const T that) {
		this = decimal(that);
	}

	///    Assigns a floating point value.
	void opAssign(T:real)(const T that) {
		this = decimal(that);
	}

	///    Assigns a string value.
	void opAssign(T:string)(const T that) {
		this = decimal(that);
	}

	unittest {	// opAssign
		write("-- opAssign.........");
		dec9 num;
		string str;
		num = dec9(1, 245, 8);
		str = "-2.45E+10";
		assertStringEqual(num,str);
		num = long.max;
		str = "9223372036854775807";
		assertStringEqual(num,str);
		num = real.max;
		str = "1.1897315E+4932";
		assertStringEqual(num, str);
		num = xint("123456098420234978023480");
		str = "123456098420234978023480";
		assertStringEqual(num, str);
		num = "123456098420234978023480";
		assertStringEqual(num, str);
		writeln("passed");
	}


//--------------------------------
// string representations
//--------------------------------

	/// Converts a number to an abstract string representation.
	public const string toAbstract() {
		return eris.decimal.conv.toAbstract(this);
	}

	/// Converts a number to a full string representation.
	const string toExact() {
		return eris.decimal.conv.toExact(this);
	}

	/// Converts a decimal to a "scientific" string representation.
	const string toSciString() {
		return eris.decimal.conv.sciForm(this);
	}

	/// Converts a decimal to an "engineering" string representation.
	const string toEngString() {
   		return eris.decimal.conv.engForm(this);
	}

	/// Converts a number to its string representation.
//	override
	const string toString() {
		return eris.decimal.conv.sciForm(this);
	}

//--------------------------------
// member properties
//--------------------------------

	/// Returns the exponent of this number
	@property
	@safe
	const int exponent() {
		return this.expo;
	}


	// TODO: What does it take to make this an l-value?
	@property
	@safe
	int exponent(const int expo) {
		this.expo = expo;
		return this.expo;
	}

	@property
	@safe
	const xint coefficient() {
		return this.mant.dup;
	}

	@property
	@safe
	xint coefficient(xint mant) {
		this.mant = mant;
		return this.mant;
	}

	@property
	@safe
	const ushort payload() {
		if (this.isNaN) {
			return cast(ushort)(this.mant.toLong);
		}
		return 0;
	}

	@property
	@safe
	ushort payload(const ushort value) {
		if (this.isNaN) {
			this.mant = xint(value);
			return value;
		}
		return 0;
	}

	/// Returns the adjusted exponent of this number
	@property
	@safe
	const int adjustedExponent() {
		return expo + digits - 1;
	}

	/// Returns the number of decimal digits in the coefficient of this number
	@safe
	const int getDigits() {
		return this.digits;
	}

	@property
	@safe
	const bool sign() {
		return signed;
	}

	@property
	@safe
	bool sign(bool value) {
		signed = value;
		return signed;
	}

	@property
	@safe
	const bool isGuarded() {
		return this.guarded;
	}

	@property
	@safe
	bool isGuarded(bool value) {
		guarded = value;
		return guarded;
	}

//--------------------------------
// floating point properties
//--------------------------------

	/// Returns the default value for this type (NaN)
	@safe
	static decimal init() {
		return NAN.dup;
	}

	/// Returns NaN
	@safe
	static decimal nan(ushort payload = 0) {
		if (payload) {
			decimal dec = NAN.dup;
			dec.payload = payload;
			return dec;
		}
		return NAN.dup;
	}

	/// Returns signaling NaN
	@safe
	static decimal snan(ushort payload = 0) {
		if (payload) {
			decimal dec = SNAN.dup;
			dec.payload = payload;
			return dec;
		}
		return SNAN.dup;
	}

	/// Returns infinity.
	@safe
	static decimal infinity(bool signed = false) {
		return signed ? NEG_INF.dup : INFINITY.dup;
	}

	/// Returns the maximum number of decimal digits in this context.
	@safe
	static uint dig() {
		return precision;
	}

	/// Returns the number of binary digits in this context.
	@safe
	static int mant_dig() {
		return cast(int)(precision/std.math.LOG2);
	}

	alias min_exp = minExpo;
	alias max_exp = maxExpo;
	alias min_10_exp = minExpo;
	alias max_10_exp = maxExpo;

	/// Returns the maximum representable normal value in the current context.
	enum xint maxCoefficient =	pow10(PRECISION) - 1;

	/// Returns the maximum representable normal value in the current context.
	enum decimal max = decimal(maxCoefficient, maxExpo);

	/// Returns the minimum representable normal value in this context.
	enum decimal min_normal = decimal(1, minExpo);

	/// Returns the minimum representable subnormal value in this context.
	enum decimal min = decimal(1, tinyExpo);

	/// Returns the smallest available increment to 1.0 in this context
	static decimal epsilon() {return decimal(1, -precision);}

	/// Returns the radix, which is always ten for decimal numbers.
	enum int radix = 10;

	/// Returns zero.
	@safe
	immutable static decimal zero(bool signed = false) {
		return signed ? NEG_ZERO.dup : ZERO.dup;
	}

//	/// Returns 1.
//	//@safe
//	immutable static decimal one(bool signed = false) {
//		return signed ? NEG_ONE.dup : ONE.dup;
//	}

	/// Returns 1.
	//@safe
	immutable static decimal one() {
		return ONE.dup;
	}

	/// Returns 2.
	//@safe
	immutable static decimal two() {
		return TWO.dup;
	}

	/// Returns 1/2.
	//@safe
	immutable static decimal half() {
		return HALF.dup;
	}

	unittest {
		write("-- constants........");
//		assertEqual(dec99.HALF, dec99(1/2));
		assertEqual(dec9.HALF, dec9(0.5));
		writeln("passed");
	}

//--------------------------------
//	classification properties
//--------------------------------

	/// Returns true if this number's representation is canonical (always true).
	@safe
	const bool isCanonical() {
		return true;
	}

	/// Returns the canonical form of the number.
	@safe
	const decimal canonical() {
		return this.dup;
	}

	unittest {	// isCanonical
		write("-- isCanonical......");
		dec9 num = dec9("2.50");
		assertTrue(num.isCanonical);
		dec9 copy = num.canonical;
		assertEqual(compareTotal(num, copy), 0);
		writeln("passed");
	}

	/// Returns true if this number is exactly one.
	//@safe
	const bool isOne() {
		if (isSimpleOne()) {
			return true;
		}
		if (exponent > 0) {
			return false;
		}
		if (this.reduce.isSimpleOne()) {
			return true;
		}
		return false;
	}

	/// Returns true if this number is exactly (false, 1, 0).
	@safe
	const bool isSimpleOne() {
		return isFinite && !isSigned && coefficient == 1 && exponent == 0;
	}

	 unittest { // isOne
		write("-- isOne............");
		dec9 num;
		num = dec9("1");
		assertTrue(num.isOne);
		num = dec9(false, 10, -1);
		assertTrue(num.isOne);
		assertFalse(num.isSimpleOne);
		writeln("passed");
	}

	/// Returns true if this number is + or - zero.
	@safe
	const bool isZero() {
		return isFinite && coefficient == 0;
	}

	unittest {	// isZero
		write("-- isZero...........");
		dec9 num;
		num = dec9("0");
		assertTrue(num.isZero);
		num = dec9("2.50");
		assertFalse(num.isZero);
		num = dec9("-0E+2");
		assertTrue(num.isZero);
		writeln("passed");
	}

	/// Returns true if this number is a quiet or signaling NaN.
	@safe
	const bool isNaN() {
		return this.sval == SV.QNAN || this.sval == SV.SNAN;
	}

	/// Returns true if this number is a signaling NaN.
	@safe
	const bool isSignaling() {
		return this.sval == SV.SNAN;
	}

	/// Returns true if this number is a quiet NaN.
	@safe
	const bool isQuiet() {
		return this.sval == SV.QNAN;
	}

	unittest {	// isNaN, isQuiet, isSignaling
		write("-- isNaN............");
		dec9 num;
		num = dec9("2.50");
		assertFalse(num.isNaN);
		assertFalse(num.isQuiet);
		assertFalse(num.isSignaling);
		num = dec9("NaN");
		assertTrue(num.isNaN);
		assertTrue(num.isQuiet);
		assertFalse(num.isSignaling);
		num = dec9("-sNaN");
		assertTrue(num.isNaN);
		assertFalse(num.isQuiet);
		assertTrue(num.isSignaling);
		writeln("passed");
	}

	/// Returns true if this number is + or - infinity.
	@safe
	const bool isInfinite() {
		return this.sval == SV.INF;
	}

	/// Returns true if this number is not an infinity or a NaN.
	@safe
	const bool isFinite() {
		return sval != SV.INF
			&& sval != SV.QNAN
			&& sval != SV.SNAN;
	}

	unittest {	// isFinite, isInfinite
		write("-- isFinite.........");
		dec9 num;
		num = dec9("2.50");
		assertFalse(num.isInfinite);
		assertTrue(num.isFinite);
		num = dec9("-0.3");
		assertTrue(num.isFinite);
		num = 0;
		assertTrue(num.isFinite);
		num = dec9("-Inf");
		assertTrue(num.isInfinite);
		assertFalse(num.isFinite);
		num = dec9("NaN");
		assertFalse(num.isInfinite);
		assertFalse(num.isFinite);
		writeln("passed");
	}

	/// Returns true if this number is a NaN or infinity.
	@safe
	const bool isSpecial() {
		return sval == SV.INF
			|| sval == SV.QNAN
			|| sval == SV.SNAN;
	}

	unittest {	// isSpecial
		write("-- isSpecial........");
		dec9 num;
		num = dec9.infinity(true);
		assertTrue(num.isSpecial);
		num = dec9.snan(1234);
		assertTrue(num.isSpecial);
		num = 12378.34;
		assertFalse(num.isSpecial);
		writeln("passed");
	}

	/// Returns true if this number is negative. (Includes -0)
	@safe
	const bool isNegative() {
		return this.signed;
	}

	/// Returns true if this number is positive. (Excludes -0)
	@safe
	const bool isPositive() {
		return !this.signed;
	}

	alias isSigned = isNegative;

	unittest {	// isSigned, isNegative
		write("-- isNegative.......");
		dec9 num;
		num = dec9("2.50");
		assertFalse(num.isSigned);
		assertFalse(num.isNegative);
		num = dec9("-12");
		assertTrue(num.isSigned);
		assertTrue(num.isNegative);
		num = dec9("-0");
		assertTrue(num.isSigned);
		assertTrue(num.isNegative);
		writeln("passed");
	}

	/// Returns true if this number is subnormal.
	@safe
	const bool isSubnormal() {
		if (!isFinite) return false;
		return adjustedExponent < minExpo;
	}

	/// Returns true if this number is normal.
	@safe
	const bool isNormal() {
		if (isFinite && !isZero) {
			return adjustedExponent >= minExpo;
		}
		return false;
	}

	unittest { // isNormal, isSubnormal
		write("-- isNormal.........");
		dec9 num;
		num = dec9("2.50");
		assertTrue(num.isNormal);
		assertFalse(num.isSubnormal);
		num = dec9("0.1E-99");
		assertFalse(num.isNormal);
		assertTrue(num.isSubnormal);
		num = dec9("0.00");
		assertFalse(num.isSubnormal);
		assertFalse(num.isNormal);
		num = dec9("-Inf");
		assertFalse(num.isNormal);
		assertFalse(num.isSubnormal);
		num = dec9("NaN");
		assertFalse(num.isSubnormal);
		assertFalse(num.isNormal);
		writeln("passed");
	}

/*	/// Returns true if this number is integral;
	/// that is, if its fractional part is zero.
	 const bool isIntegral() {
	 	// TODO: need to take trailing zeros into account
		return expo >= 0;
	 }*/
/+
	/// Returns true if the number is an integer.
	const bool isIntegralValued() {
writeln(" -------- -------- ");
writefln("expo = %s", expo);
writefln("coefficient = %s", coefficient);
		if (isSpecial) return false;
		if (exponent >= 0) return true;
		uint expo2 = std.math.abs(exponent);
writefln("expo2 = %s", expo2);
		if (expo2 >= context.precision) return false;
//		if (expo2 == 0) return true;
writefln("10^^expo2 = %s", 10^^expo2);
writefln("coefficient mod 10 = %s", coefficient % 10);
		if (coefficient % 10^^expo2 == 0) return true;
		return false;
	}

/*	unittest {	// isIntegralValued
		dec9 num;
		num = 12345;
//writefln("num.isIntegralValued = %s", num.isIntegralValued);
		assertTrue(num.isIntegralValued);
		num = xint("123456098420234978023480");
//writefln("num.isIntegralValued = %s", num.isIntegralValued);
		assertTrue(num.isIntegralValued);
		num = 1.5;
//writefln("num.isIntegralValued = %s", num.isIntegralValued);
		assertTrue(!num.isIntegralValued);
		num = 1.5E+1;
		assertTrue(num.isIntegralValued);
		num = 0;
		assertTrue(num.isIntegralValued);
	}*/
+/
	/// Returns true if this number is a true value.
	/// Non-zero finite numbers are true.
	/// Infinity is true and NaN is false.
	@safe
	const bool isTrue() {
		return isFinite && !isZero || isInfinite;
	}

	/// Returns true if this number is a false value.
	/// Finite numbers with zero coefficient are false.
	/// Infinity is true and NaN is false.
	@safe
	const bool isFalse() {
		return isNaN || isZero;
	}

	unittest {	//isTrue/isFalse
		write("-- isTrue/isFalse...");
		assertTrue(dec9("1").isTrue);
		assertFalse(dec9("0").isTrue);
		assertTrue(infinity.isTrue);
		assertFalse(nan.isTrue);
		assertTrue(dec9("0").isFalse);
		assertFalse(dec9("1").isFalse);
		assertFalse(infinity.isFalse);
		assertTrue(nan.isFalse);
		writeln("passed");
	}

	@safe
	const bool isZeroCoefficient() {
		return !isSpecial && coefficient == 0;
	}

	unittest {	// isZeroCoefficient
		write("-- isZeroCoeff......");
		dec9 num;
		num = 0;
		assertTrue(num.isZeroCoefficient);
		num = xint("-0");
		assertTrue(num.isZeroCoefficient);
		num = dec9("0E+4");
		assertTrue(num.isZeroCoefficient);
		num = 12345;
		assertFalse(num.isZeroCoefficient);
		num = 1.5;
		assertFalse(num.isZeroCoefficient);
		num = dec9.NAN;
		assertFalse(num.isZeroCoefficient);
		num = dec9.INFINITY;
		assertFalse(num.isZeroCoefficient);
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0 or 1, if this number is less than, equal to,
	/// or greater than the argument, respectively. NOTE: The comparison
	/// is made to the current precision.
	const int opCmp(T:decimal)(const T that) {
		// TODO: this is a place where the context is set from the outside.
		return compare(this, that, context);
	}

	/// Returns -1, 0 or 1, if this number is less than, equal to,
	/// or greater than the argument, respectively.
	const int opCmp(T)(const T that) {
		return opCmp(decimal(that));
	}

	/// Returns true if this number is equal to the argument.
	/// Finite numbers are equal if they are numerically equal
	/// to the current precision.
	/// Infinities are equal if they have the same sign.
	/// Zeros are equal regardless of sign.
	/// A NaN is not equal to any number, not even to another NaN.
	/// A number is not even equal to itself (this != this) if it is a NaN.
	const bool opEquals(T:decimal)(const T that) {
		return equals!T(this, that/*, rounding*/);
	}

/*	/// Returns true if this extended integer is equal to the argument.
	const bool opEquals(T:long)( const T that) {
		return opEquals(decimal(that));
	}*/

	/// Returns true if this extended integer is equal to the argument.
	const bool opEquals(T)(const T that) {
		return opEquals(decimal(that));
	}

	unittest {	// comparison
		write("-- comparison.......");
		dec9 num1, num2;
		num1 = 105;
		num2 = 10.543;
		assert(num1 == 105L);
		assertGreaterThan(num1, num2);
		assertNotEqual(num1, num2);
		assertGreaterThan(num1, num2);
		assertLessThan(num2, num1);
		num1 = 10.543;
		assertNotLessThan(num1, num2);
		assertNotGreaterThan(num2, num1);
		assertEqual(num1, num2);
		writeln("passed");
	}


//--------------------------------
// unary arithmetic operators
//--------------------------------

	/// Returns the result of performing the specified
	/// unary operation on this number.
	//@safe
	private decimal opUnary(string op)() {
		static if (op == "+") {
			return plus(this, decimal.context);
		}
		else static if (op == "-") {
			return minus(this, decimal.context);
		}
		else static if (op == "++") {
			this = add(this, 1, decimal.context);
			return this;
		}
		else static if (op == "--") {
			this = sub(this, 1, decimal.context);
			return this;
		}
	}

	unittest {	// opUnary
		write("-- opUnary..........");
		dec9 num, actual, expect;
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
		num = 1.00E8;
		expect = num - 1;
		actual = --num;
		assertEqual(actual, expect);
		num = 1.00E8;
		expect = num;
		actual = num--;
		assertEqual(actual, expect);
		// FIXTHIS: these break the compiler...
		num = dec9(9999999, 90);
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
//	binary arithmetic operators
//--------------------------------

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinary(string op, T:decimal)(const T arg)
	{
		static if (op == "+") {
			return add(this, arg, T.context);
		}
		else static if (op == "-") {
			return sub(this, arg, T.context);
		}
		else static if (op == "*") {
			return mul(this, arg, T.context);
		}
		else static if (op == "/") {
			return div(this, arg, T.context);
		}
		else static if (op == "%") {
			return remainder(this, arg);
		}
		else static if (op == "&") {
			return and(this, arg/*, context*/);
		}
		else static if (op == "|") {
			return or(this, arg/*, context*/);
		}
		else static if (op == "^") {
			return xor(this, arg/*, context*/);
		}
	}

/*	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinaryRight(string op, T:decimal)(const T arg)
	{
		static if (op == "+") {
			return add!decimal(decimal(arg), this, context);
		}
		else static if (op == "-") {
			return sub!decimal(decimal(arg), this, context);
		}
		else static if (op == "*") {
			return mul!decimal(decimal(arg), this, context);
		}
		else static if (op == "/") {
			return div!decimal(decimal(arg), this, context);
		}
		else static if (op == "%") {
			return remainder!decimal(decimal(arg), this, context);
		}
		else static if (op == "&") {
			return and!decimal(decimal(arg), this, context);
		}
		else static if (op == "|") {
			return or!decimal(decimal(arg), this, context);
		}
		else static if (op == "^") {
			return xor!decimal(decimal(arg), this, context);
		}
	}*/

/*	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinary(string op, T)(const T arg) {
		return opBinary(decimal(arg));
	}*/

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinary(string op, T:long)(const T arg)
	{
		static if (op == "+") {
			return add(this, arg, decimal.context);
		}
		else static if (op == "-") {
			return sub(this, arg, decimal.context);
		}
		else static if (op == "*") {
			return mul(this, arg, decimal.context);
		}
		else static if (op == "/") {
			return div(this, decimal(arg), decimal.context);
		}
		else static if (op == "%") {
			return remainder(this, decimal(arg), decimal.context);
		}
		else static if (op == "&") {
			return and(this, decimal(arg), decimal.context);
		}
		else static if (op == "|") {
			return or(this, decimal(arg), decimal.context);
		}
		else static if (op == "^") {
			return xor(this, decimal(arg), decimal.context);
		}
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinaryRight(string op, T:long)(const T arg)
	{
		static if (op == "+") {
			return add(this, arg, decimal.context);
		}
		else static if (op == "-") {
			return sub(decimal(arg), this, decimal.context);
		}
		else static if (op == "*") {
			return mul(this, arg, decimal.context);
		}
		else static if (op == "/") {
			return div(decimal(arg), this, decimal.context);
		}
		else static if (op == "%") {
			return remainder(decimal(arg), this, decimal.context);
		}
		else static if (op == "&") {
			return and(this, decimal(arg), rounding);
		}
		else static if (op == "|") {
			return or(this, decimal(arg), rounding);
		}
		else static if (op == "^") {
			return xor(this, decimal(arg), rounding);
		}
	}

/+	/// Returns true if the type T is promotable to a decimal type.
	private template isPromotable(T) {
		enum bool isPromotable = is(T:ulong) || is(T:real);
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinary(string op, T)(const T arg) if (isPromotable!T)	{
		return opBinary!(op,decimal)(decimal(arg));
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinaryRight(string op, T)(const T arg) if (isPromotable!T)	{
		static if (op == "+") {
			return add(decimal(arg), this, context);
		}
		else static if (op == "-") {
			return sub(decimal(arg), this, context);
		}
		else static if (op == "*") {
			return mul(decimal(arg), this, context);
		}
		else static if (op == "/") {
			return div(decimal(arg), this, context);
		}
		else static if (op == "%") {
			return remainder(decimal(arg), this, context);
		}
		else static if (op == "&") {
			return and(this, decimal(arg), context);
		}
		else static if (op == "|") {
			return or(this, decimal(arg), context);
		}
		else static if (op == "^") {
			return xor(this, decimal(arg), context);
		}
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	// TODO: separate out the long arithmetic
/*	const decimal opBinary(string op, T)(const long arg) if (isPromotable!T)	{
		return opBinary!(op,decimal)(decimal(arg));
	}*/

/*	unittest {	// ???
		decimal num = dec9(591.3);
		dec9 result = num * 5;
		writefln("result = %s", result);

//		assertEqual(result, dec9(2956.5));
	}*/

/*	unittest {	// isPromotable
		write("isPromotable...");
		writeln("test missing");
	}*/
+/
	unittest {	// opBinary
		write("-- opBinary.........");
		dec9 op1, op2, actual, expect;
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
		writeln("passed");
	}

//-----------------------------
// operator assignment
//-----------------------------

	/// Performs the specified binary operation on this number
	/// and the argument then assigns the result to this number.
	ref decimal opOpAssign(string op, T:decimal) (const T arg) {
		this = opBinary!op(arg);
		return this;
	}

	/// Performs the specified binary operation on this number
	/// and the argument then assigns the result to this number.
	ref decimal opOpAssign(string op, T) (const T arg) {
		this = opBinary!op(decimal(arg));
		return this;
	}

	unittest {	// opOpAssign
		write("-- opOpAssign.......");
		dec9 op1, op2, actual, expect;
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
		writeln("passed");
	}

//-----------------------------
// nextUp, nextDown, nextAfter
//-----------------------------

	/// Returns the smallest representable number that is larger than
	/// this number.
	const decimal nextUp() {
		return nextPlus(this, decimal.context);
	}

	/// Returns the largest representable number that is smaller than
	/// this number.
	const decimal nextDown() {
		return nextMinus(this, decimal.context);
	}

	/// Returns the representable number that is closest to the
	/// this number (but not this number) in the
	/// direction toward the argument.
	const decimal nextAfter(const decimal arg) {
		return nextToward(this, arg, decimal.context);
	}

	unittest {	// nextUp, nextDown, nextAfter
		write("-- next.............");
		dec9 big, expect;
		big = 123.45;
		assertEqual(big.nextUp, dec9(123.450001));
		big = 123.45;
		assertEqual(big.nextDown, dec9(123.449999));
		big = 123.45;
		expect = big.nextUp;
		assertEqual(big.nextAfter(dec9(123.46)), expect);
		big = 123.45;
		expect = big.nextDown;
		assertEqual(big.nextAfter(dec9(123.44)), expect);
		writeln("passed");
	}

	// (B)TODO: move this outside the struct
	// TODO: currently multiplies by 5s and shifts bits
	/// Returns a xint value of ten raised to the specified power.
	//@safe
	public static xint pow10(int n) {
		xint num = 1;
		return shiftLeft(num, n/*, precision*/);
	}

//--------------------------------
// decimal constants
//--------------------------------

	/// base of natural logarthims, e = 2.7182818283...
	enum decimal E = roundString("2.71828182845904523536028747135266249775"
		"724709369995957496696762772407663035354759457138217852516643");

/*	/// base 2 logarithm of 10 = 3.32192809...
	enum decimal LG_10 = roundString("3.3219280948873623478703194294893901"
		"7586483139302458061205475639581593477660862521585013974335937016");

	/// base 2 logarithm of e = 1.44269504...
	enum decimal LG_E = roundString("1.44269504088896340735992468100189213"
		"742664595415298593413544940693110921918118507988552662289350634");

	/// base 10 logarithm of 2 = 0.301029996...
	enum decimal LOG_2 = roundString("0.3010299956639811952137388947244930"
		"26768189881462108541310427461127108189274424509486927252118186172");

	/// base 10 logarithm of e  = 4.34294482...
	enum decimal LOG_E = roundString("4.3429448190325182765112891891660508"
		"2294397005803666566114453783165864649208870774729224949338431748");

	/// natural logarithm of 2 = 0.693147806...
	enum decimal LN2 = roundString("0.693147180559945309417232121458176568"
		"075500134360255254120680009493393621969694715605863326996418688");*/

	/// natural logarithm of 10 = 2.30258509...
	enum decimal LN10 = roundString("2.30258509299404568401799145468436420"
		"760110148862877297603332790096757260967735248023599720508959820");

	/// pi = 3.14159266...
	enum decimal PI = roundString("3.1415926535897932384626433832795028841"
		"9716939937510582097494459230781640628620899862803482534211707");

/*	/// pi/2
	enum decimal PI_2 = roundString("1.57079632679489661923132169163975144"
		"209858469968755291048747229615390820314310449931401741267105853");

	/// pi/4
	enum decimal PI_4 = roundString("0.78539816339744830961566084581987572"
		"1049292349843776455243736148076954101571552249657008706335529267");

	/// 1/pi
	enum decimal INV_PI = roundString("0.318309886183790671537767526745028"
		"724068919291480912897495334688117793595268453070180227605532506172");

	/// 1/2*pi
	enum decimal INV_2PI = roundString("0.15915494309189533576888376337251"
		"4362034459645740456448747667344058896797634226535090113802766253086");*/

	/// square root of two = 1.41421357
	enum decimal SQRT2 = roundString("1.4142135623730950488016887242096980"
		"7856967187537694807317667973799073247846210703885038753432764157");

/*	/// square root of one half = 0.707106781...
	immutable decimal SQRT1_2 = roundString("0.70710678118654752440084436210484"
		"9039284835937688474036588339868995366239231053519425193767163820786");*/

	/// Rounds a string representation of a number to specified precision.
	/// Does not convert the string to a number. A decimal point may be
	/// included, but no exponent is allowed.
	/// A string of all nines will throw an exception.
	private static string roundString(
			const string str, int precision = decimal.precision) {

		// make a copy, deleting any whitespace at ends of the string
		char[] copy = strip(str).dup;

		// if the string has a decimal point increment the precision to account
		// for the extra character
		if (copy.indexOf('.') >= 0) precision++;

		// strip out any underscores
		if (indexOf(copy, '_') >= 0) {
					copy = removechars(copy.idup, "_").dup;
		}

		// ignore leading zeros
		int index = 0;
		while (copy[index] == '0') index++;
		precision += index;

		// if the precision is greater than the length return the whole string
		if (precision >= copy.length) return copy.idup;

		// get the last digit in the (to-be-)clipped string,
		// and the following digit
		char last = str[precision-1];
		char follow = str[precision];

		// if the following digit is less than five return the clipped string
		if (follow < '5') return copy[0..precision].idup;

		// otherwise, increment last digit in the string(round half-up)
		copy = copy[0..precision];
		copy[precision-1] += 1;

		// if the increment resulted in a digit return the clipped string
		if (copy[precision-1] <= '9') return copy[0..precision].idup;

		// otherwise, (the last digit increment resulted in a non-digit)
		// set the last digit to '0' and increment its preceding digit,
		// repeat as necessary
		int lix = precision - 1;
		last = copy[lix];
		while (last > '9') {
			copy[lix] = '0';
			lix--;
			copy[lix] += 1;
			last = copy[lix];
		}
		return copy[0..precision].idup;
	}

	unittest {
		write("-- constants........");
		assertStringEqual(dec9.E, "2.71828183");
		assertStringEqual(dec9.PI, "3.14159265");
		writeln("passed");
	}

}	 // end struct Decimal

/*unittest {
	write("cast...");
	Decimal!(7,99) input;
	Decimal!(8,99) output;
	input = "12.34";
	output = cast(Decimal!(7,99))input;
writefln("output = %s", output);

	writeln("test missing");
}*/

unittest {
	writeln("==========================");
	writeln("decimal................end");
	writeln("==========================");
}

