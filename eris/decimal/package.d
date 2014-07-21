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

module eris.decimal;

import std.conv;
import std.string;

import eris.integer.extended;
import eris.decimal.context;
import eris.decimal.arithmetic;
import eris.decimal.rounding;

version(unittest) {
	import std.stdio;
	import eris.assertion;
	import eris.decimal.dec64;
}

alias xint = ExtendedInt;
alias dec99 = BigDecimal!(99,999);
alias dec9 = BigDecimal!(9,99);

// special values for NaN, Inf, etc.
private enum SV { NONE, INF, QNAN, SNAN };

/*public BigInt x2b(in xint x = 0) {
	return BigInt(x.toString);
}

unittest {
	write("x2b...");
writefln("x2b = %s", x2b());
writefln("x2b = %s", x2b(xint(3)));
writefln("x2b = %s", x2b(xint("909239874203948")));
	writeln("test missing");
}*/

/// A struct representing an arbitrary-precision decimal floating-point number.
///
/// The implementation follows the General Decimal Arithmetic
/// Specification, Version 1.70 (25 Mar 2009),
/// http://www.speleotrove.com/decimal.
/// This specification conforms with IEEE standard 754-2008.

struct BigDecimal(int PRECISION = 99, int MAX_EXPO = 9999,
		Rounding ROUNDING_MODE = Rounding.HALF_EVEN) {

	private alias decimal = BigDecimal!(PRECISION, MAX_EXPO, ROUNDING_MODE);

static if (PRECISION == 9) {
unittest {
	writeln("==========================");
	writeln("decimal..............begin");
	writeln("==========================");
}}

 	/// Marker used to identify decimal numbers irrespective of size.
	public enum IS_DECIMAL;

	/// members
	private SV sval = SV.QNAN;		// special value: default is quiet NaN
	private bool signed = false;	// true if the value is negative, false otherwise.
	private int expo = 0;			// the exponent of the decimal value
	private xint mant;				// the coefficient of the decimal value
	package int digits; 			// the number of decimal digits in this number.
									// (unless the number is a special value)

	// static fields
	package static bool verbose = false; // for debugging

	public:
	/// Maximum length of the coefficient in decimal digits.
	enum int precision = PRECISION;
	/// Maximum value of the exponent.
	enum int maxExpo = MAX_EXPO;
	/// Maximum value of the adjusted exponent.
	enum int maxAdjustedExpo = MAX_EXPO - (PRECISION - 1);
	/// Smallest normalized exponent.
	enum int minExpo = 1 - MAX_EXPO;
	/// Smallest non-normalized exponent.
	enum int tinyExpo = 2 - MAX_EXPO - PRECISION;
	/// Rounding mode.
	enum Rounding rounding = ROUNDING_MODE;
	/// Struct containing the precision and rounding mode.
	enum Context context = Context(PRECISION, MAX_EXPO, ROUNDING_MODE);

	// decimal special values
	enum decimal NAN		= decimal(SV.QNAN);
	enum decimal SNAN		= decimal(SV.SNAN);
	enum decimal INFINITY	= decimal(SV.INF);
	enum decimal NEG_INF	= decimal(SV.INF, true);
	enum decimal ZERO		= decimal(SV.NONE);
	enum decimal NEG_ZERO	= decimal(SV.NONE, true);

	static if (PRECISION == 9) {
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
	}}

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

	/// Constructs a new number given a special value and an optional sign.
	///
	@safe
	public this(const SV sv, const bool sign = false) {
		this.signed = sign;
		this.sval = sv;
	}

	static if (PRECISION == 9) {
	unittest {	// special value construction
		write("-- this(special)....");
		dec9 num = dec9(SV.INF, true);
		assertStringEqual(num, "-Infinity");
		assertStringEqual(num.toAbstract(), "[1,inf]");
		writeln("passed");
	}}

	/// Creates a decimal from a boolean value.
	/// false == 0, true == 1
	public this(const bool value) {
		this = zero;
        if (value) {
			mant = 1;
			this.digits = 1;
		}
	}

	static if (PRECISION == 9) {
	unittest {	// boolean construction
		write("-- this(bool).......");
		dec9 num = dec9(false);
		assertStringEqual(num, "0");
		num = dec9(true);
		assertStringEqual(num, "1");
		writeln("passed");
	}}

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

	static if (PRECISION == 9) {
	unittest {	// bool, xint, int construction
		write("-- this(bool,big,int)...");
		dec9 num;
		num = dec9(true, xint(7254), 94);
		assertStringEqual(num, "-7.254E+97");
		num = dec9(true, xint(7254), 194);
		num = dec9(true, xint(1), 194);
		num = roundToPrecision(num);
		assertStringEqual(num, "-Infinity");
		writeln("passed");
	}}

	/// Constructs a decimal from a xint coefficient and an
	/// optional integer exponent. The sign of the number is the sign
	/// of the coefficient. The initial precision is determined by the number
	/// of digits in the coefficient.
	//@safe
	this(const xint coefficient, const int exponent = 0) {
		bool sign = coefficient.sgn < 0;
		this(sign, coefficient.abs, exponent);
	};

	static if (PRECISION == 9) {
	unittest {	// xint, int construction
		write("-- this(big,int)....");
		dec9 num;
		num = dec9(xint(7254), 94);
		assertStringEqual(num, "7.254E+97");
		num = dec9(xint(-7254));
		assertStringEqual(num, "-7254");
		writeln("passed");
	}}

	/// Constructs a number from a sign, a long integer coefficient and
	/// an integer exponent.
	//@safe
	public this(const bool sign, const long coefficient, const int exponent) {
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

	static if (PRECISION == 9) {
	unittest {	// long value construction
		write("-- this(long).......");
		dec9 num;
		num = dec9(7254, 94);
		assertStringEqual(num, "7.254E+97");
		num = dec9(-7254L);
		assertStringEqual(num, "-7254");
		writeln("passed");
	}}

	// Constructs a decimal number from a string representation
	this(const string str) {
		this = eris.decimal.conv.toNumber!decimal(str);
	};

	static if (PRECISION == 9) {
	unittest {	// string construction
	// TODO: (testing) this(str): add tests for just over/under int.max, int.min
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
	}}

	// TODO: (efficiency) fix for small numbers (convert to long, back to real/decimal)
	/// Constructs a decimal number from a real value.
	this(const real r) {
		string str = format("%.*G", cast(int)precision, r);
		this(str);
	}

   // TODO: (testing) add unittest for real value construction

	// Constructs a decimal number from a different type of decimal.
	public this(T)(T from) if (__traits(hasMember, T, "IS_DECIMAL")) {
		bool sign = from.isNegative;
		if (from.isFinite) this(from.sign, from.coefficient, from.exponent);
		else if (from.isInfinite) 	this(SV.INF, sign);
		else if (from.isQuiet)		this(SV.QNAN, sign);
		else if (from.isSignaling)	this(SV.SNAN, sign);
		else this(SV.QNAN);
	}

	static if (PRECISION == 9) {
	unittest {
		write("-- this(decimal)....");
		dec9 abc = 12345;
		dec99 def = dec99(abc);
		assertEqual(abc, def);
		dec9 ghi = dec99(def);
		assertEqual(def, ghi);
		dec9 klm = dec9(-dec99.infinity);
		assertEqual(klm, "-Infinity");
		assertEqual(dec9.infinity, dec99.infinity);
		writeln("passed");
	}}

	// copy constructor
	//@safe
	public this(const decimal that) {
		this.signed = that.signed;
		this.sval	= that.sval;
		this.digits = that.digits;
		this.expo	= that.expo;
		this.mant	= that.mant.dup;
	};

//--------------------------------
// copy functions
//--------------------------------

	/// dup property
	//@safe
	public decimal dup() const {
		return decimal(this);
	}

	static if (PRECISION == 9) {
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
	}}

	/// Returns a copy of the operand.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy' function in the specification. (p. 43)
	//@safe
	public decimal copy() const {
		return dup;
	}

	/// Returns a copy of the operand with a positive sign.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy-abs' function in the specification. (p. 44)
	//@safe
	public decimal copyAbs() const  {
		decimal copy = dup;
		copy.sign = false;
		return copy;
	}

	/// Returns a copy of the operand with the sign inverted.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy-negate' function in the specification. (p. 44)
	//@safe
	public decimal copyNegate() const {
		decimal copy = dup;
		copy.sign = !sign;
		return copy;
	}

	/// Returns a copy of the first operand with the sign of the second operand.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy-sign' function in the specification. (p. 44)
	//@safe
	public decimal copySign()(in decimal x) const {
		decimal copy = dup;
		copy.sign = x.sign;
		return copy;
	}

	unittest {	// copy
		write("-- copy.............");
		dec9 arg, expect;
		arg = dec9("2.1");
		expect = dec9("2.1");
		assertZero(compareTotal(arg.copy,expect));
		arg = dec9("-1.00");
		expect = dec9("-1.00");
		assertZero(compareTotal(arg.copy,expect));
		// copyAbs
		arg = 2.1;
		expect = 2.1;
		assertZero(compareTotal(arg.copyAbs,expect));
		arg = dec9("-1.00");
		expect = dec9("1.00");
		assertZero(compareTotal(arg.copyAbs,expect));
		// copyNegate
		arg	= dec9("101.5");
		expect = dec9("-101.5");
		assertZero(compareTotal(arg.copyNegate,expect));
		// copySign
		dec9 arg1, arg2;
		arg1 = 1.50; arg2 = 7.33; expect = 1.50;
		assertZero(compareTotal(arg1.copySign(arg2),expect));
		arg2 = -7.33;
		expect = -1.50;
		assertZero(compareTotal(arg1.copySign(arg2),expect));
		writeln("passed");
	}

//--------------------------------
// casts
//--------------------------------

 	bool opCast(T:bool)() const {
		return isTrue;
	}

 	T opCast(T)() const
	{
		return T(this);
	}

	static if (PRECISION == 9) {
	unittest {
		write("-- opCast...........");
		assertFalse(dec9.init);
		dec9 abc = dec9(12,4);
		assertTrue(abc);
		dec99 def = cast(dec99)abc;
		assertEqual(abc, def);
		dec9 def2 = cast(dec99)abc;
		assertEqual(def, def2);
		int n = 7;
		dec9 bdn = cast(dec9)n;
		assertEqual(bdn, dec9(7));
		auto tr = cast(dec9)12;
		assertEqual(typeid(tr), typeid(bdn));
		dec99 big = 1234567890123;
		dec9 klm = dec9(big);
		assertEqual(klm, big);	// klm has not been rounded.
		assertNotEqual(abs(klm), big);	// klm has been rounded.
		dec99 spcl = dec99.infinity(true);
		klm = dec9(spcl);
		assertEqual(klm, dec9("-Infinity"));
		writeln("passed");
	}}

/*
	static bool isDecimal(T)(T dummy) {
		return __traits(hasMember, T, "IS_DECIMAL");
	}
*/
	static if (PRECISION == 9) {
	unittest {
		write("-- isDecimal........");
		dec9 dummy;
		assertTrue(isDecimal!dec9);
		assertFalse(isDecimal!int);
		assertTrue(isDecimal!Dec64);
		writeln("passed");
	}}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a decimal number (makes a copy)
	void opAssign(T:decimal)(in T that) {
		this.signed  = that.signed;
		this.sval	 = that.sval;
		this.digits  = that.digits;
		this.expo	 = that.expo;
		this.mant	 = that.mant;
	}

	///    Assigns an xint value.
	void opAssign(T:xint)(in T that) {
		this = decimal(that);
	}

	/// Assigns a long value.
	/// NOTE: Unsigned long integers are first converted to signed.
	void opAssign(T:long)(in T that) {
		this = decimal(that);
	}

	/// Assigns a floating point value.
	void opAssign(T:real)(in T that) {
		this = decimal(that);
	}

	///    Assigns a string value.
	void opAssign(T:string)(in T that) {
		this = decimal(that);
	}

	static if (PRECISION == 9) {
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
		num = ulong.max - 12;
		str = "-13";
		assertStringEqual(num,str);
		num = 237UL;
		str = "237";
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
	}}


//--------------------------------
// string representations
//--------------------------------

	/// Converts a number to an abstract string representation.
	public string toAbstract() const {
		return eris.decimal.conv.toAbstract(this);
	}

	/// Converts a number to a full string representation.
	public string toExact() const {
		return eris.decimal.conv.toExact(this);
	}

	/// Converts a decimal to a "scientific" string representation.
	public string toSciString() const {
		return eris.decimal.conv.sciForm(this);
	}

	/// Converts a decimal to an "engineering" string representation.
	public string toEngString() const {
   		return eris.decimal.conv.engForm(this);
	}

	/// Converts a number to its string representation.
//	override
	public string toString() const {
		return eris.decimal.conv.sciForm(this);
	}

//--------------------------------
// member properties
//--------------------------------

	/// Returns the exponent of this number
	@property
	@safe
	int exponent() const {
		if (isSpecial) return 0;
		return this.expo;
	}


	// NOTE: (language) What does it take to make this an l-value?
	/// sets the exponent of this number
	@property
	@safe
	int exponent(int expo) {
		this.expo = expo;
		return this.expo;
	}

	@property
	@safe
	xint coefficient() const {
		if (isSpecial) return xint(0);
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
	ushort payload() const {
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
	int adjustedExponent() const {
		if (isSpecial) return 0;
		return expo + digits - 1;
	}

	/// Returns the number of decimal digits in the coefficient of this number
	@property
	@safe
	int getDigits() const {
		if (isSpecial) return 0;
		return this.digits;
	}

	@property
	@safe
	bool sign() const {
		return signed;
	}

	@property
	@safe
	bool sign(bool value) {
		signed = value;
		return signed;
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
	static decimal nan(ushort payload = 0, bool sign = false) {
		decimal dec = NAN.dup;
		dec.payload = payload;
		dec.signed = sign;
		return dec;
	}

	/// Returns signaling NaN
	@safe
	static decimal snan(ushort payload = 0, bool sign = false) {
		decimal dec = SNAN.dup;
		dec.payload = payload;
		dec.signed = sign;
		return dec;
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

	/// Returns the maximum integer value of the coefficient in the current context.
	@safe
	enum xint maxCoefficient =	pow10(precision) - 1;

	/// Returns the maximum representable normal value in the current context.
	@safe
	enum decimal max = decimal(maxCoefficient, maxAdjustedExpo);

	/// Returns the minimum representable normal value in this context.
	@safe
	enum decimal min_normal = decimal(1, minExpo);

	/// Returns the minimum representable subnormal value in this context.
	@safe
	enum decimal min = decimal(1, tinyExpo);

	/// Returns the smallest available increment to 1.0 in this context
	static enum decimal epsilon(in Context context = decimal.context) {
		return decimal(1, -context.precision);}

	/// Returns the radix, which is always ten for decimal numbers.
	@safe
	enum int radix = 10;

	/// Returns zero.
	@safe
	static enum decimal zero(bool signed = false) {
		return signed ? NEG_ZERO.dup : ZERO.dup;
	}

//	/// Returns 1.
//	//@safe
//	immutable static decimal one(bool signed = false) {
//		return signed ? NEG_ONE.dup : ONE.dup;
//	}

	/// Returns 1.
	//@safe
	static enum decimal one() {
		return ONE.dup;
	}

	/// Returns 2.
	//@safe
	static enum decimal two() {
		return TWO.dup;
	}

	/// Returns 1/2.
	//@safe
	static enum decimal half() {
		return HALF.dup;
	}

	static if (PRECISION == 9) {
	unittest {
		write("-- constants........");
		assertEqual(dec9.HALF, dec9(0.5));
		writeln("passed");
	}}

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

	static if (PRECISION == 9) {
	unittest {	// isCanonical
		write("-- isCanonical......");
		dec9 num = dec9("2.50");
		assertTrue(num.isCanonical);
		dec9 copy = num.canonical;
		assertEqual(compareTotal(num, copy), 0);
		writeln("passed");
	}}

	/// Returns true if this number is exactly one.
	//@safe
	const bool isOne() {
		if (isSimpleOne) {
			return true;
		}
		if (exponent > 0) {
			return false;
		}
		return this.reduce.isSimpleOne;
	}

	/// Returns true if this number is exactly (false, 1, 0).
	@safe
	const bool isSimpleOne() {
		return isFinite && !isSigned && coefficient == 1 && exponent == 0;
	}

	static if (PRECISION == 9) {
	 unittest { // isOne
		write("-- isOne............");
		dec9 num;
		num = dec9("1");
		assertTrue(num.isOne);
		num = dec9(false, 10, -1);
		assertTrue(num.isOne);
		assertFalse(num.isSimpleOne);
		writeln("passed");
	}}

	/// Returns true if this number is + or - zero.
	@safe
	const bool isZero() {
		return isFinite && coefficient == 0;
	}

	static if (PRECISION == 9) {
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
	}}

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

	static if (PRECISION == 9) {
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
	}}

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

	static if (PRECISION == 9) {
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
	}}

	/// Returns true if this number is a NaN or infinity.
	@safe
	const bool isSpecial() {
		return sval == SV.INF
			|| sval == SV.QNAN
			|| sval == SV.SNAN;
	}

	static if (PRECISION == 9) {
	unittest {	// isSpecial
		write("-- isSpecial........");
		dec9 num;
		num = dec9.infinity(true);
		assertTrue(num.isSpecial);
		num = dec9.snan(1234,true);
		assertTrue(num.isSpecial);
		num = 12378.34;
		assertFalse(num.isSpecial);
		writeln("passed");
	}}

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

	static if (PRECISION == 9) {
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
	}}

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

	static if (PRECISION == 9) {
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
	}}

	/// Returns true if the number is an integer (the fractional part is zero).
	const bool isIntegralValued() {
		if (isSpecial) return false;
		if (exponent >= 0) return true;
		int expo = -exponent;
		if (expo >= context.precision) return false;
		// NOTE: this is an expensive operation if the goal is to reduce calculation cost.
		int zeros = trailingZeros(coefficient, digits);
		if (zeros) {
			expo += zeros;
			if (expo >= 0) return true;
		}
		return false;
	}

	static if (PRECISION == 9) {
	unittest {	// isIntegralValued
		write("-- isIntegralValued.");
		dec9 num;
		num = 12345;
		assertTrue(num.isIntegralValued);
		num = xint("123456098420234978023480");
		assertTrue(num.isIntegralValued);
		num = 1.5;
		assertTrue(!num.isIntegralValued);
		num = 1.5E+1;
		assertTrue(num.isIntegralValued);
		num = 0;
		assertTrue(num.isIntegralValued);
		num = "2.19000000E+5";
		num = "21900.000E-2";
		assertTrue(num.isIntegralValued);
		writeln("passed");
	}}

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

	static if (PRECISION == 9) {
	unittest {	//isTrue/isFalse
		write("-- isTrue/isFalse...");
//		assertTrue(dec9(1));
//		assert(ONE);
//		assertEqual(ONE, true);
//		assertTrue(cast(bool)ONE);
		assertTrue(dec9("1").isTrue);
		assertFalse(dec9("0").isTrue);
		assertTrue(infinity.isTrue);
		assertFalse(nan.isTrue);
		assertTrue(dec9("0").isFalse);
		assertFalse(dec9("1").isFalse);
		assertFalse(infinity.isFalse);
		assertTrue(nan.isFalse);
		writeln("passed");
	}}

/*	@safe
	const bool isZeroCoefficient() {
		return !isSpecial && coefficient == 0;
	}

	static if (PRECISION == 9) {
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
	}}*/

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0 or 1, if this number is less than, equal to,
	/// or greater than the argument, respectively. NOTE: The comparison
	/// is made to the current precision.
	const int opCmp(T:decimal)(T that) {
		return compare(this, that);
	}

	/// Returns -1, 0 or 1, if this number is less than, equal to,
	/// or greater than the argument, respectively.
	const int opCmp(T)(T that) {
		return opCmp(decimal(that));
	}

	/// Returns true if this number is equal to the argument.
	/// Finite numbers are equal if they are numerically equal
	/// to the current precision.
	/// Infinities are equal if they have the same sign.
	/// Zeros are equal regardless of sign.
	/// A NaN is not equal to any number, not even to another NaN.
	/// A number is not even equal to itself (this != this) if it is a NaN.
	const bool opEquals(T:decimal)(T that) {
		return equals(this, that);
	}

	/// Returns true if this extended integer is equal to the argument.
	const bool opEquals(T)(T that) {
		return opEquals(decimal(that));
	}

	static if (PRECISION == 9) {
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
	}}

//--------------------------------
// unary arithmetic operators
//--------------------------------

	/// Returns the result of performing the specified
	/// unary operation on this number.
	//@safe
	private decimal opUnary(string op)() {
		static if (op == "+") {
			return plus(this);
		}
		else static if (op == "-") {
			return minus(this);
		}
		else static if (op == "++") {
			this = add(this, 1);
			return this;
		}
		else static if (op == "--") {
			this = sub(this, 1);
			return this;
		}
	}

	static if (PRECISION == 9) {
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
		num = dec9(9999999, 90);
		expect = num;
		actual = num++;
		assertEqual(actual, expect);
		num = 12.35;
		expect = 11.35;
		actual = --num;
		assertEqual(actual, expect);
		writeln("passed");
	}}

//--------------------------------
//	binary arithmetic operators
//--------------------------------

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinary(string op, T:decimal)(in T x)
	{
		static if (op == "+") {
			return add(this, x);
		}
		else static if (op == "-") {
			return sub(this, x);
		}
		else static if (op == "*") {
			return mul(this, x);
		}
		else static if (op == "/") {
			return div(this, x);
		}
		else static if (op == "%") {
			return remainder(this, x);
		}
		else static if (op == "&") {
			return and(this, x);
		}
		else static if (op == "|") {
			return or(this, x);
		}
		else static if (op == "^") {
			return xor(this, x);
		}
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinary(string op, T:long)(T x)
	{
		static if (op == "+") {
			return add(this, x, decimal.context);
		}
		else static if (op == "-") {
			return sub(this, x, decimal.context);
		}
		else static if (op == "*") {
			return mul(this, x, decimal.context);
		}
		else static if (op == "/") {
			return div(this, x, decimal.context);
		}
		else static if (op == "%") {
			return remainder(this, decimal(x), decimal.context);
		}
		else static if (op == "&") {
			return and(this, decimal(x), decimal.context);
		}
		else static if (op == "|") {
			return or(this, decimal(x), decimal.context);
		}
		else static if (op == "^") {
			return xor(this, decimal(x), decimal.context);
		}
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	const decimal opBinaryRight(string op, T:long)(in T x)
	{
		static if (op == "+") {
			return add(this, x, decimal.context);
		}
		else static if (op == "-") {
			return sub(decimal(x), this, decimal.context);
		}
		else static if (op == "*") {
			return mul(this, x, decimal.context);
		}
		else static if (op == "/") {
			return div(decimal(x), this, decimal.context);
		}
		else static if (op == "%") {
			return remainder(decimal(x), this, decimal.context);
		}
		else static if (op == "&") {
			return and(this, decimal(x), rounding);
		}
		else static if (op == "|") {
			return or(this, decimal(x), rounding);
		}
		else static if (op == "^") {
			return xor(this, decimal(x), rounding);
		}
	}


	// TODO: (testing) more tests to distinguish opBin from opBinRight.
	// TODO: (testing) need complete coverage

	static if (PRECISION == 9) {
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
		actual = op2 - op1;
		expect = 4;
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
	}}

//-----------------------------
// operator assignment
//-----------------------------

	/// Performs the specified binary operation on this number
	/// and the argument then assigns the result to this number.
	ref decimal opOpAssign(string op, T:decimal) (T x) {
		this = opBinary!op(x);
		return this;
	}

	/// Performs the specified binary operation on this number
	/// and the argument then assigns the result to this number.
	ref decimal opOpAssign(string op, T) (T x) {
		this = opBinary!op(decimal(x));
		return this;
	}

	static if (PRECISION == 9) {
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
	}}

//-----------------------------
// nextUp, nextDown, nextAfter
//-----------------------------

	/// Returns the smallest representable number that is larger than
	/// this number.
	decimal nextUp() const {
		return nextPlus(this, decimal.context);
	}

	/// Returns the largest representable number that is smaller than
	/// this number.
	decimal nextDown() const {
		return nextMinus(this, decimal.context);
	}

	/// Returns the representable number that is closest to the
	/// this number (but not this number) in the
	/// direction toward the argument.
	decimal nextAfter(decimal x) const {
		return nextToward(this, x, decimal.context);
	}

	static if (PRECISION == 9) {
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
	}}

//--------------------------------
// decimal constants
//--------------------------------

	/// Constants are computed at compile time to the type precision.
	/// For values of the constant at other precisions use constant(n),
	/// where n is the desired precision.

	/// Returns pi, pi = 3.14159266...
	mixin Constant!("pi");
	enum decimal PI = roundString("3.1415926535897932384626433832795028841"
		"9716939937510582097494459230781640628620899862803482534211707");

	/// Returns 'e' (the base of natural logarthims, e = 2.7182818283...)
	mixin Constant!("e");
	enum decimal E = roundString("2.71828182845904523536028747135266249775"
		"724709369995957496696762772407663035354759457138217852516643");

	/// natural logarithm of 2 = 0.693147806...
	mixin Constant!("ln2");
	enum decimal LN2 = roundString("0.693147180559945309417232121458176568"
		"075500134360255254120680009493393621969694715605863326996418688");

	/// natural logarithm of 10 = 2.30258509...
	mixin Constant!("ln10");
	enum decimal LN10 = roundString("2.30258509299404568401799145468436420"
		"760110148862877297603332790096757260967735248023599720508959820");

	/// base 2 logarithm of e = 1.44269504...
	mixin Constant!("log2_e");
	enum decimal LOG2_E = roundString("1.44269504088896340735992468100189213"
		"742664595415298593413544940693110921918118507988552662289350634");

	/// base 2 logarithm of 10 = 3.32192809...
	mixin Constant!("log2_10");
	enum decimal LOG2_10 = roundString("3.3219280948873623478703194294893901"
		"7586483139302458061205475639581593477660862521585013974335937016");

	/// base 10 logarithm of 2 = 0.301029996...
	enum decimal LOG10_2 = roundString("0.3010299956639811952137388947244930"
		"26768189881462108541310427461127108189274424509486927252118186172");

	/// base 10 logarithm of e  = 4.34294482...
	enum decimal LOG10_E = roundString("4.3429448190325182765112891891660508"
		"2294397005803666566114453783165864649208870774729224949338431748");

	/// pi/2
	mixin Constant!("pi_2");
	enum decimal PI_2 = roundString("1.57079632679489661923132169163975144"
		"209858469968755291048747229615390820314310449931401741267105853");

	/// pi/4
	enum decimal PI_4 = roundString("0.78539816339744830961566084581987572"
		"1049292349843776455243736148076954101571552249657008706335529267");

	/// 1/pi
	mixin Constant!("invPi","INV_PI");
	enum decimal INV_PI = roundString("0.318309886183790671537767526745028"
		"724068919291480912897495334688117793595268453070180227605532506172");

	/// 1/2*pi
	enum decimal INV_2PI = roundString("0.15915494309189533576888376337251"
		"4362034459645740456448747667344058896797634226535090113802766253086");

	/// square root of two = 1.41421357
	mixin Constant!("sqrt2");
	enum decimal SQRT2 = roundString("1.4142135623730950488016887242096980"
		"7856967187537694807317667973799073247846210703885038753432764157");

	/// square root of one half = 0.707106781...
	mixin Constant!("sqrt1_2");
	enum decimal SQRT1_2 = roundString("0.707106781186547524400844362104849"
		"039284835937688474036588339868995366239231053519425193767163820786");

	/// golden ratio = 1.6180339887...
	mixin Constant!("phi");
	enum decimal PHI = roundString("1.6180339887498948482045868343656381177"
		"20309179805762862135448622705260462818902449707207204189391137");

	static if (PRECISION == 9) {
	unittest {
		write("-- constants........");
		assertStringEqual(dec9.E,     "2.71828183");
		assertStringEqual(dec9.pi,    "3.14159265");
		assertStringEqual(dec9.PI,    "3.14159265");
		assertStringEqual(dec9.LN2,   "0.693147181");
		assertStringEqual(dec9.LN10,  "2.30258509");
		assertStringEqual(dec9.SQRT2, "1.41421356");
		assertStringEqual(dec9.INV_PI,"0.318309886");
		assertStringEqual(dec9.invPi, "0.318309886");
		writeln("passed");
	}}

//--------------------------------
// decimal constant boilerplate
//--------------------------------

    /// mixin template to add a constant and a arbitrary precision constant.
	mixin template Constant(string name) {
		mixin ("public static decimal " ~ name ~ "(int precision = decimal.precision) {
			if (precision != decimal.precision) {
				return eris.decimal.math." ~ name ~ "!decimal(precision);
			}
			return " ~ name.toUpper ~ ";
		}");
	}

    /// mixin template to add a constant and a arbitrary precision constant.
	mixin template Constant(string lcName, string ucName) {
		mixin ("public static decimal " ~ lcName ~ "(int precision = decimal.precision) {
			if (precision != decimal.precision) {
				return eris.decimal.math." ~ lcName ~ "!decimal(precision);
			}
			return " ~ ucName ~ ";
		}");
	}

	/// Rounds a string representation of a number to specified precision.
	/// Does not convert the string to a number. A decimal point may be
	/// included, but no exponent is allowed.
	/// A string of all nines will throw an exception.
	private static string roundString(
			string str, int precision = decimal.precision) {

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

	static if (PRECISION == 9) {
	unittest {
		writeln("==========================");
		writeln("decimal................end");
		writeln("==========================");
	}}

}	 // end struct BigDecimal

/// Returns true if the parameter is a type of decimal number.
public bool isDecimal(T)() {
		return __traits(hasMember, T, "IS_DECIMAL");
	}



