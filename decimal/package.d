// Written in the D programming language

/**
 *	A D programming language implementation of the
 *	General Decimal Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 *	http://www.speleotrove.com/decimal/decarith.pdf)
 *
 *	Copyright Paul D. Anderson 2009 - 2015.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.decimal;

import std.conv;
import std.string;
import std.traits;
import std.math;

import eris.integer.extended;
import eris.decimal.context;
import eris.decimal.arithmetic;
import eris.decimal.logical;
import eris.decimal.rounding;

// temporary import
	import std.stdio;

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
}

alias xint = ExtendedInt;

public enum Context DefaultContext = Context(99, 9999, HALF_EVEN);
public enum Context TestContext    = Context(9, 99, HALF_EVEN);
public enum Context Context99      = Context(99, 999, HALF_EVEN);
public enum Context RealContext    = Context(real.dig, real.max_10_exp, HALF_EVEN);
public enum Context DoubleContext  = Context(double.dig, double.max_10_exp, HALF_EVEN);

alias TD = BigDecimal!(TestContext);
alias dec99 = BigDecimal!(Context99);

// special values for NaN, Inf, etc.
private enum SV { None, Inf, qNaN, sNaN };

version(unittest)
{
	struct testStruct(T, U)
	{
		T actual;
		U expect;
	}

alias DD = testStruct!(TD, TD);
alias DS = testStruct!(TD, string);
}
/// A struct representing an arbitrary-precision decimal floating-point number.
///
/// The implementation follows the General Decimal Arithmetic
/// Specification, Version 1.70 (25 Mar 2009),
/// http://www.speleotrove.com/decimal.
/// This specification conforms with IEEE standard 754-2008.

struct BigDecimal(immutable Context _context = DefaultContext)
{

static if (context == TestContext) {
unittest {
	writeln("==========================");
	writeln("decimal..............begin");
	writeln("==========================");
}}

	/// Struct containing the precision and rounding mode.
	enum Context context = _context;

 	/// Marker used to identify decimal numbers irrespective of size.
	public enum IsDecimal;

	/// members
	private:
		SV sval = SV.qNaN;		// special value: default is quiet NaN
		bool signed = false;	// true if the value is negative, false otherwise.
		int expo = 0;			// the exponent of the decimal value
		xint mant;				// the coefficient of the decimal value

	package:
		 int digits; 			// the number of decimal digits in this number.

	public:
		/// Maximum length of the coefficient in decimal digits.
		enum int precision = context.precision;
		/// Maximum value of the exponent.
		enum int maxExpo = context.maxExpo;
		/// Maximum value of the adjusted exponent.
		enum int maxAdjustedExpo = maxExpo - (precision - 1);
		/// Smallest normalized exponent.
		enum int minExpo = 1 - maxExpo;
		/// Smallest non-normalized exponent.
		enum int tinyExpo = 1 - maxExpo - precision;
		/// Rounding mode.
		enum Rounding mode = context.mode;

	private:
		alias decimal = BigDecimal!(context);

	// decimal special values
	public:
		enum decimal NaN		= decimal(SV.qNaN);
		enum decimal sNaN		= decimal(SV.sNaN);
		enum decimal Infinity	= decimal(SV.Inf);
		enum decimal negInf		= decimal(SV.Inf, true);
		enum decimal Zero		= decimal(SV.None);
		enum decimal negZero	= decimal(SV.None, true);

	static if (context == TestContext) {
	unittest {// decimal special values
		write("-- special values...");

//		static struct S { TD num; string val; }

		alias ts = testStruct!(TD, string);
		static DS[] tests =
//		static testStruct!(TD, string)[] tests =
		[
			{ NaN,		"NaN" },
			{ sNaN,		"sNaN" },
			{ Zero,		"0" },
			{ negZero,	"-0" },
			{ Infinity,	"Infinity" },
			{ negInf,	"-Infinity" },
		];

		foreach (i, s; tests)
		{
			assertEqual(s.actual.toString, s.expect, i);
		}
		writeln("passed");
	}}

	// common decimal numbers
	enum decimal Half	= decimal(5, -1);
	enum decimal One	= decimal(1);
	enum decimal negOne	= decimal(-1);
	enum decimal Two	= decimal(2);
	enum decimal Three	= decimal(3);
	enum decimal Five	= decimal(5);
	enum decimal Ten	= decimal(10);

	enum decimal RealMax = decimal("1.1897314953572317649E+4932");
	enum decimal RealMin = RealMax.copyNegate;
	enum decimal RealMinNorm = decimal("3.362103143112093506E-4932");
	enum decimal DoubleMax = decimal("1.7976931348623157079E+308");
	enum decimal DoubleMin = DoubleMax.copyNegate;
	enum decimal DoubleMinNorm = decimal("2.2250738585072013832E-308");
	enum decimal LongMax = decimal("9223372036854775807");
	enum decimal LongMin = decimal("-9223372036854775808");
	enum decimal IntMax = decimal("2147483647");
	enum decimal IntMin = decimal("-2147483648");

//--------------------------------
// construction
//--------------------------------

	/// Constructs a new number given a special value and an optional sign.
	///
	@safe
	public this(const SV sv, const bool sign = false)
	{
		this.signed = sign;
		this.sval = sv;
	}

	/// Creates a decimal from a boolean value.
	/// false == 0, true == 1
	public this(const bool value)
	{
		this = zero;
        if (value) {
			mant = 1;
			this.digits = 1;
		}
	}

	static if (context == TestContext) {
	unittest {	// boolean construction
		write("-- this(bool).......");
		assertEqual(TD(false), TD(0));
		assertEqual(TD(true), TD(1));
		writeln("passed");
	}}

	/// Constructs a number from a boolean sign, a xint coefficient and
	/// an optional integer exponent.
	/// The sign of the number is the value of the sign parameter
	/// regardless of the sign of the coefficient.
	/// The intial precision of the number is deduced from the number
	/// of decimal digits in the coefficient.
	//@safe
	this(bool sign, xint coefficient, int exponent = 0)
	{
		this = zero();
		this.signed = sign;
		this.mant = coefficient.abs;
		this.expo = exponent;
		this.digits = numDigits(this.mant);
	}

	static if (context == TestContext) {
	unittest {	// bool, xint, int construction
		write("-- this(s,big,int)..");
		TD num;
		num = TD(true, xint(7254), 94);
		assertEqual(num, TD("-7.254E+97"));
		num = TD(true, xint(7254), 194);
		num = TD(true, xint(1), 194);
		num = roundToPrecision(num);
		assertEqual(num, TD("-Infinity"));
		writeln("passed");
	}}

	/// Constructs a decimal from a xint coefficient and an
	/// optional integer exponent. The sign of the number is the sign
	/// of the coefficient. The initial precision is determined by the number
	/// of digits in the coefficient.
	//@safe
	this(xint coefficient, int exponent = 0)
	{
		bool sign = coefficient.sgn < 0;
		this(sign, coefficient.abs, exponent);
	}

	static if (context == TestContext) {
	unittest {	// xint, int construction
		write("-- this(big,int)....");
		TD num;
		num = TD(xint(7254), 94);
		assertEqual(num, TD("7.254E+97"));
		num = TD(xint(-7254));
		assertEqual(num, TD("-7254"));
		writeln("passed");
	}}

	/// Constructs a number from a sign, a long integer coefficient and
	/// an integer exponent.
	//@safe
	public this(bool sign, long coefficient, int exponent)
	{
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
		TD d = TD(false, mant, -5, 6);
		writefln("d = %s", d);
		mant = xint("3141590000000000000000000000000000000000000");
		d = TD(false, mant, -42, 43);
		writefln("d = %s", d);

		writeln("test missing");
	}*/

	/// Constructs a number from a long integer coefficient
	/// and an optional integer exponent.
	this(long coefficient, int exponent)
	{
		this(xint(coefficient), exponent);
	}

	/// Constructs a number from a long integer value
	this(long coefficient)
	{
		this(xint(coefficient), 0);
	}

	static if (context == TestContext) {
	unittest {	// long value construction
		write("-- this(long).......");
		TD num;
		num = TD(7254, 94);
		assertEqual(num, TD("7.254E+97"));
		num = TD(-7254L);
		assertEqual(num, TD("-7254"));
		writeln("passed");
	}}

	// Constructs a decimal number from a string representation
	this(const string str)
	{
		this = eris.decimal.conv.toNumber!decimal(str);
	}

	static if (context == TestContext) {
	unittest {	// string construction
		write("-- this(string).....");

		static struct S { string num; string val; }

		static S[] tests =
		[
			{ "7254E94",		"7.254E+97" },
			{ "7254.005",		"7254.005" },
			{ "-2.3456E+14",	"-2.3456E+14" },
			{ "-0.1234",		"-0.1234" },
			{ "234568901234",	"234568901234" },
			{ "123.457E+29",	"1.23457E+31" },
			{ "2.71828183",		"2.71828183" },
			{ "2147483646",		"2147483646" },
			{ "2147483648",		"2147483648" },
			{ "-2147483647",	"-2147483647" },
			{ "-2147483649",	"-2147483649" },
			{ "inf",			"Infinity" },
		];

		foreach (i, s; tests)
		{
			assertEqual(TD(s.num).toString, s.val, i);
		}
		writeln("passed");
	}}

	private static int countZeros(ulong f, int e)
	{
		// quick check for no trailing zeros
		if (f & 1) return 0;
		// quick check for 1 trailing zero
		if (f & 2) return 1;
		// otherwise do a binary search
		int min = 2;
		int max = 64;
		while (min <= max)
		{
			int mid = (min + max)/2;
			long m = ones(mid);
			if (f & m)
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

	private static void trimZeros(ref ulong f, ref int e)
	{
		int zeros = countZeros(f, e);
		if (zeros < 64) {
			e += zeros;
			f >>>= zeros;
		}
	}

	private enum struct RealRep
	{
		union
		{
			real value;
			struct
			{
				ulong fraction;
				mixin(std.bitmanip.bitfields!(
					ushort,  "exponent", 15,
					bool,    "sign",      1));
			}
		}
		enum uint bias = 16383, signBits = 1, exponentBits = 15,
				integerBits = 1, fractionBits = 63;
	}

	/// Constructs a decimal number from a real value.
	this(T)(T r) if (isFloatingPoint!T)
	{
		static if (T.sizeof == 10)	// 80-bit real
		{
			RealRep rep;
		}
		else static if (T.sizeof == 8)	// 64-bit double
		{
			std.bitmanip.DoubleRep rep;
		}
		else static if (T.sizeof == 4)	// 32-bit float
		{
			std.bitmanip.FloatRep rep;
		}
		else // Shouldn't reach here
		{
			// always works but it's slow
			string str = format("%.20G", r);
			this(str);
		}

		// finite numbers
		if (std.math.isFinite(r))
		{
			if (r == 0.0)
			{
				this(SV.None, r < 0.0);
			}
			else if (std.math.abs(r) == 1.0)
			{
				this((r < 0.0) ? -1 : 1);
			}
			else
			{
				rep.value = r;
				ulong f = 1L << rep.fractionBits | rep.fraction;
				int e = rep.exponent - rep.bias - rep.fractionBits;
				this(r, f, e, rep.sign);
			}
		}
		// special values
		else if (std.math.isInfinity(r))
		{
			this(SV.Inf, r < 0.0);
		}
		else
		{
			this(SV.qNaN);
		}
	}

	static private long ones(int n)
	{
		return (1L << n) - 1;
	}

	this(T)(T r, ulong frac, int expo, bool sign)
		if (isFloatingPoint!T)
	{
		trimZeros(frac, expo);
		int c = std.math.abs(expo);
		if (c <= 63)	// if coefficient fits in a long integer
		{
			decimal n = decimal(xint(frac));
			if (sign) n = n.copyNegate;
			// scale factor
			auto scale = decimal(1L << c);
    		// multiply or divide
			if (expo < 0)
			{
				this(n / scale);
			}
			else
			{
				this(n * scale);
			}
		}
		else
		{
			string str = format("%.20G", r);
			this(str);
		}
	}

	// TODO: (testing) need to test this with 15-17 digit precision
	static if (context == TestContext) {
	unittest // real construction
	{
		write("-- this(real).......");

		static struct S { real num; string val; }
		static struct T { double num; string val; }
		static struct U { float num; string val; }

		static S[] tests =
		[
			{ 0.1L,				"0.1" },
			{ 7254E94,			"7.254E+97" },
			{ 7254.005,			"7254.005" },
			{ -2.3456E+14,		"-2.3456E+14" },
			{ -0.1234,			"-0.1234" },

			{ 234568901234.0,	"234568901234" },
			{ 123.457E+29,		"1.23457E+31" },
			{ 2.71828183,		"2.71828183" },
			{ 2.718281832,		"2.718281832" },
			{ 2147483646.0,		"2147483646" },

			{ 2147483648.0,		"2147483648" },
			{ -2147483647.0,	"-2147483647" },
			{ -2147483649.0,	"-2147483649" },
			{ 0.0,				"0" },
			{ -0.0,				"-0" },

			{ 1E54,				"1E54" },
			// TODO: pass these two test cases
			{ double.max, 		"21.79769313E+305" },
			{ real.max, 		"1.1897314953572317649E+4932" },
			{ real.infinity, 	"Infinity" },
		];

		foreach (i, s; tests)
		{
			assertEqual(TD(s.num).reduce, TD(s.val), i);
		}
		writeln("passed");
	}}

/*	public static double dpow10(int n)
	{
		static double[23] tens;
		bool initialized = false;
		if (!initialized)
		{
			tens[0] = 1.0;
			for (size_t i = 1; i < tens.length; i++)
			{
				tens[i] = tens[i-1] * 10.0;
			}
			initialized = true;
		}
		if (n > 22) return double.nan;
		return tens[n];
	}*/

	// Returns a real number constructed from
	// a long coefficient and an integer exponent.
	private static real longToReal(const decimal x)
	{
		// convert the coefficient to a real number
		real r;
		r = cast(ulong)x.coefficient;
		if (x.sign) r = -r;
		if (x.expo == 0) return r;

		// scale by the decimal exponent
		real tens = 10.0L ^^ std.math.abs(x.expo);
		if (x.expo > 0) return r * tens;
		else            return r/tens;
	}


	/// Converts a decimal number to a real number. The decimal will be
	/// rounded to the RealContext before conversion, if necessary.
	public real toReal() const
	{
		// special values
		if (this.isNaN) return real.nan;
		if (this.isInfinite) return sign ? -real.infinity : real.infinity;
		if (this.isZero) return sign ? -0.0 : 0.0;
		int realMinExpo = 1 - RealContext.maxExpo;
		if (this.isSubnormal(realMinExpo)) return real.nan;

		// if this number is larger than the largest real value,
		// return infinity
		if (this.expo >= real.max_10_exp) {
			if (this > RealMax)	return  real.infinity;
			if (this < RealMin) return -real.infinity;
		}

		// if smaller than the smallest value, return zero
		if (this.expo <= real.min_10_exp) {
			if (this.copyAbs < RealMinNorm) return this.sign ? -0.0 : 0.0;
        }

		// will the coefficent fit into a long integer?
		if (this.coefficient.getDigitLength <= 2)
		{
			return longToReal(this);
		}

		// NOTE: There are real numbers that will be rounded unnecessarily
		// (i.e. more than 18 digits but less than long.max)
		// the reduced coefficient will fit
		decimal reduced = this.reduce(RealContext);
		if (reduced.coefficient.getDigitLength <= 2)
		{
			return longToReal(reduced);
		}

		return real.nan;
	}

	public static decimal toDecimal(T)(T fl) if (isFloatingPoint!T) {
		string str = format("%.20G", fl);
		return decimal(str);
	}

	static if (context == TestContext) {
	unittest {	// toReal, toDecimal
		write("-- toReal...........");
		static real[] tests =
		[
			1.0,
			2.0,
			1.0E5,
			0.1,
			123.456,
			32E-27,
			double.max,
			real.max,
		];

		foreach (i, s; tests)
		{
			decimal d = toDecimal(s);
			real r = d.toReal();
			assertEqual(r, d, i);
		}
		writeln("passed");
	}}


	public double toDouble() const
	{
		// try for an exact conversion...
		if (digits <= 15) {
			int absExpo = std.math.abs(expo);
			if (absExpo <= 22) {
				double s = cast(double)(coefficient.toLong);
				if (absExpo == 0) return s;
				double p = dpow10(absExpo);
				if (expo > 0) return s * p;
				if (expo < 0) return s / p;
			}
		}
		// TODO: (behavior) add method for other values
		return double.nan;
	}

	// TODO: (testing) add unit tests
	unittest {
		write("toDouble...");
		TD x = "3.14159";
		writefln("x.toDouble = %s", x.toDouble);
		writeln("test missing");
	}

	// Constructs a decimal number from another decimal number
	/// which may be of a different type.
	public this(T)(T that) if (isDecimal!T)
	{
		bool sign = that.isNegative;
		if (that.isFinite) this(that.sign, that.coefficient, that.exponent);
		else if (that.isInfinite) 	this(SV.Inf, sign);
		else if (that.isQuiet)		this(SV.qNaN, sign);
		else if (that.isSignaling)	this(SV.sNaN, sign);
		else this(SV.qNaN);
	}

	static if (context == TestContext) {
	unittest {
		write("-- this(decimal)....");
/*		TD abc = 12345;
		dec99 def = dec99(abc);
		assertEqual(abc, def);
		TD ghi = dec99(def);
		assertEqual(def, ghi);
		TD klm = TD(-dec99.infinity);
		assertEqual(klm, "-Infinity");
		assertEqual(TD.infinity, dec99.infinity);*/
		writeln("test missing");
	}}

	// copy constructor
	//@safe
	public this(const decimal that)
	{
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
	public decimal dup() const
	{
		return decimal(this);
	}

/*	static if (context == TestContext) {
	version(unittest) {
		private bool assertCopy(T) (T num, T copy)if (isDecimal!T) {
			return assertZero!T(compareTotal(num, copy));
		}
	}}*/

	static if (context == TestContext) {
	unittest {	// dup
		write("-- dup..............");
		TD num, copy;
		// TODO: add tests for these values
		num = std.math.LOG2;
		num = std.math.PI;
		num = std.math.LOG2;
		copy = TD(num);
//		assertCopy!TD(num, copy);
		assertZero(compareTotal(num, copy));
		num = TD(std.math.PI);
		copy = num.dup;
		assertEqual(num, copy);
		writeln("passed");
	}}

	/// Returns a copy of the operand.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy' function in the specification. (p. 43)
	//@safe
	public decimal copy() const
	{
		return dup;
	}

	/// Returns a copy of the operand with a positive sign.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy-abs' function in the specification. (p. 44)
	//@safe
	public decimal copyAbs() const
	{
		decimal copy = dup;
		copy.sign = false;
		return copy;
	}

	/// Returns a copy of the operand with the sign inverted.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy-negate' function in the specification. (p. 44)
	//@safe
	public decimal copyNegate() const
	{
		decimal copy = dup;
		copy.sign = !sign;
		return copy;
	}

	/// Returns a copy of the first operand with the sign of the second operand.
	/// The copy is unaffected by context and is quiet -- no flags are changed.
	/// Implements the 'copy-sign' function in the specification. (p. 44)
	//@safe
	public decimal copySign()(in decimal x) const
	{
		decimal copy = dup;
		copy.sign = x.sign;
		return copy;
	}

	static if (context == TestContext) {
	unittest {	// copy
		write("-- copy.............");
		TD arg, expect;
		arg = TD("2.1");
		expect = TD("2.1");
		assertZero(compareTotal(arg.copy,expect));
		arg = TD("-1.00");
		expect = TD("-1.00");
		assertZero(compareTotal(arg.copy,expect));
		// copyAbs
		arg = 2.1;
		expect = 2.1;
		assertZero(compareTotal(arg.copyAbs,expect));
		arg = TD("-1.00");
		expect = TD("1.00");
		assertZero(compareTotal(arg.copyAbs,expect));
		// copyNegate
		arg	= TD("101.5");
		expect = TD("-101.5");
		assertZero(compareTotal(arg.copyNegate,expect));
		// copySign
		TD arg1, arg2;
		arg1 = 1.50; arg2 = 7.33; expect = 1.50;
		assertZero(compareTotal(arg1.copySign(arg2),expect));
		arg2 = -7.33;
		expect = -1.50;
		assertZero(compareTotal(arg1.copySign(arg2),expect));
		writeln("passed");
	}}

//--------------------------------
// casts
//--------------------------------

 	bool opCast(T:bool)() const
	{
		return isTrue;
	}

 	T opCast(T)() const if (isDecimal!T)
	{
		return T(this);
	}

 	T opCast(T)() const if (isFloatingPoint!T)
	{
		return T(this);
	}

	static if (context == TestContext) {
	unittest {
		write("-- opCast...........");
/*		assertFalse(TD.init);
		TD abc = TD(12,4);
		assertTrue(abc);
		dec99 def = cast(dec99)abc;
		assertEqual(abc, def);
		TD def2 = cast(dec99)abc;
		assertEqual(def, def2);
		int n = 7;
		TD bdn = cast(TD)n;
		assertEqual(bdn, TD(7));
		auto tr = cast(TD)12;
		assertEqual(typeid(tr), typeid(bdn));
		dec99 big = 1234567890123;
		TD klm = TD(big);
		assertEqual(klm, big);	// klm has not been rounded.
		assertNotEqual(abs(klm), big);	// klm has been rounded.
		dec99 spcl = dec99.infinity(true);
		klm = TD(spcl);
		assertEqual(klm, TD("-Infinity"));*/
		writeln("test missing");
	}}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a decimal number (makes a copy)
	// COMPILER BUG
//	void opAssign(T:decimal)(in T that)
	void opAssign(T:decimal)(in T that)
	{
		this.signed  = that.signed;
		this.sval	 = that.sval;
		this.digits  = that.digits;
		this.expo	 = that.expo;
		this.mant	 = that.mant;
	}

	///    Assigns an xint value.
	void opAssign(T:xint)(T that)
	{
		this = decimal(that);
	}

	/// Assigns a long value.
	/// NOTE: Unsigned long integers are first converted to signed.
	void opAssign(T:long)(in T that)
	{
		this = decimal(that);
	}

	/// Assigns a floating point value.
	void opAssign(T:real)(in T that)
	{
		this = decimal(that);
	}

	///    Assigns a string value.
	void opAssign(T:string)(in T that)
	{
		this = decimal(that);
	}

	static if (context == TestContext) {
	unittest {	// opAssign
		write("-- opAssign.........");
/*		TD num;
		string str;
		num = TD(1, 245, 8);
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
		str = "1.1897314953572317649E+4932";
		assertStringEqual(num, str);
		num = xint("123456098420234978023480");
		str = "123456098420234978023480";
		assertStringEqual(num, str);
		num = "123456098420234978023480";
		assertStringEqual(num, str);*/
		writeln("test missing");
	}}


//--------------------------------
// string representations
//--------------------------------

	/// Converts a number to an abstract string representation.
	public string abstractForm() const
	{
		return eris.decimal.conv.abstractForm(this);
	}

	/// Converts a number to a full string representation.
	public string fullForm() const
	{
		return eris.decimal.conv.fullForm(this);
	}

	/// Converts a decimal to a "scientific" string representation.
	public string toSciString() const
	{
		return eris.decimal.conv.sciForm(this);
	}

	/// Converts a decimal to an "engineering" string representation.
	public string toEngString() const
	{
   		return eris.decimal.conv.engForm(this);
	}

	/// Converts a number to the default string representation.
	public string toString() const
	{
		return eris.decimal.conv.sciForm(this);
	}

//--------------------------------
// member properties
//--------------------------------

	/// Returns the exponent of this number
	@property
	@safe
	int exponent() const
	{
		if (isSpecial) return 0;
		return this.expo;
	}


	// NOTE: (language) What does it take to make this an l-value?
	/// sets the exponent of this number
	@property
	@safe
	int exponent(int expo)
	{
		this.expo = expo;
		return this.expo;
	}

	@property
	@safe
	xint coefficient() const
	{
		if (isSpecial) return xint(0);
		return this.mant.dup;
	}

	@property
	@safe
	xint coefficient(xint mant)
	{
		this.mant = mant;
		return this.mant;
	}

	@property
	@safe
	xint coefficient(long mant)
	{
		this.mant = mant;
		return this.mant;
	}

	@property
	@safe
	ushort payload() const
	{
		if (this.isNaN) {
			return cast(ushort)(this.mant.toLong);
		}
		return 0;
	}

	@property
	@safe
	ushort payload(const ushort value)
	{
		if (this.isNaN) {
			this.mant = xint(value);
			return value;
		}
		return 0;
	}

	/// Returns the adjusted exponent of this number
	@property
	@safe
	int adjustedExponent() const
	{
		if (isSpecial) return 0;
		return expo + digits - 1;
	}

	/// Returns the number of decimal digits in the coefficient of this number
	@property
	@safe
	int getDigits() const
	{
		if (isSpecial) return 0;
		return this.digits;
	}

	@property
//	@safe
	bool sign() const
	{
		return signed;
	}

	@property
//	@safe
	bool sign(const bool value)
	{
		signed = value;
		return signed;
	}

//--------------------------------
// floating point properties
//--------------------------------

	/// Returns the default value for this type (NaN)
	@safe
	static decimal init()
	{
		return NaN.dup;
	}

	/// Returns NaN
	@safe
	static decimal nan(ushort payload = 0, bool sign = false)
	{
		decimal dec = NaN.dup;
		dec.payload = payload;
		dec.signed = sign;
		return dec;
	}

	/// Returns signaling NaN
	@safe
	static decimal snan(ushort payload = 0, bool sign = false)
	{
		decimal dec = sNaN.dup;
		dec.payload = payload;
		dec.signed = sign;
		return dec;
	}

	/// Returns infinity.
	@safe
	static decimal infinity(bool signed = false)
	{
		return signed ? negInf.dup : Infinity.dup;
	}

	/// Returns the maximum number of decimal digits in this context.
	@safe
	static uint dig()
	{
		return precision;
	}

	/// Returns the number of binary digits in this context.
	@safe
	static int mant_dig()
	{
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
	static enum decimal epsilon(in Context inContext = context) {
		return decimal(1, -inContext.precision);}

	/// Returns the radix, which is always ten for decimal numbers.
	@safe
	enum int radix = 10;

	/// Returns zero.
	@safe
	static enum decimal zero(bool signed = false)
	{
		return signed ? negZero.dup : Zero.dup;
	}

//	/// Returns 1.
//	//@safe
//	immutable static decimal one(bool signed = false) {
//		return signed ? negOne.dup : One.dup;
//	}

	/// Returns 1.
	@safe
	static enum decimal one()
	{
		return One.dup;
	}

	/// Returns 2.
	@safe
	static enum decimal two()
	{
		return Two.dup;
	}

	/// Returns 1/2.
	@safe
	static enum decimal half()
	{
		return Half.dup;
	}

	static if (context == TestContext) {
	unittest {
		write("-- constants........");
		assertEqual(TD.Half, TD(0.5));
		writeln("passed");
	}}

//--------------------------------
//	classification properties
//--------------------------------

	/// Returns true if this number's representation is canonical.
	///
	/// All decimal numbers are canonical, whether or not they are
	/// reduced to their simplest form. However, the representation
	/// of the coefficient may have leading zeros.
	/// If this is the case, this function returns `false`.
	/// Otherwise it returns `true`.
	@safe
	const bool isCanonical()
	{
		// if the coefficient array has only one digit, return true;
		if (coefficient.getDigitLength == 1) return true;
		// if 1st digit of the coefficient array is zero, return false
		return coefficient.getDigit(0) ? true : false;
	}

	/// Returns the canonical form of the number.
	@safe
	const decimal canonical()
	{
		// copy the number
		decimal copy = this.dup;
		// if it's already canonical, return the copy
		if (copy.isCanonical) return copy;
		// otherwise, trim the leading zeros in the coefficient
		// and return the copy
		copy.coefficient = copy.coefficient.trim;
		return copy;
	}

	static if (context == TestContext)
	{
	unittest
	{	// isCanonical
		write("-- isCanonical......");
		TD num = TD("2.50");
		// string constructions generally have a leading zero
		assertFalse(num.isCanonical);
		TD copy = num.canonical;
		assertTrue(copy.isCanonical);
		assertEqual(compareTotal(num, copy), 0);
		writeln("passed");
	}}

	/// Returns true if this number is exactly one.
	//@safe
	const bool isOne()
	{
		if (isNegative || isZero || isSpecial) {
			return false;
		}
		if (coefficient == 1 && exponent == 0) {
			return true;
		}
/*		if (exponent > 0) {
			return false;
		}*/
		return this.reduce.isSimpleOne;
	}

	/// Returns true if this number is exactly (false, 1, 0).
	@safe
	const bool isSimpleOne()
	{
		return isFinite && !isSigned && coefficient == 1 && exponent == 0;
	}

	static if (context == TestContext) {
	 unittest { // isOne
		write("-- isOne............");
		TD num;
		num = TD("1");
		assertTrue(num.isOne);
		num = TD(false, 10, -1);
		assertTrue(num.isOne);
		assertFalse(num.isSimpleOne);
		writeln("passed");
	}}

	/// Returns true if this number is + or - zero.
	@safe
	const bool isZero()
	{
		return isFinite && coefficient == 0;
	}

	static if (context == TestContext) {
	unittest {	// isZero
		write("-- isZero...........");
		TD num;
		num = TD("0");
		assertTrue(num.isZero);
		num = TD("2.50");
		assertFalse(num.isZero);
		num = TD("-0E+2");
		assertTrue(num.isZero);
		writeln("passed");
	}}

	/// Returns true if this number is a quiet or signaling NaN.
	@safe
	const bool isNaN()
	{
		return this.sval == SV.qNaN || this.sval == SV.sNaN;
	}

	/// Returns true if this number is a signaling NaN.
	@safe
	const bool isSignaling()
	{
		return this.sval == SV.sNaN;
	}

	/// Returns true if this number is a quiet NaN.
	@safe
	const bool isQuiet()
	{
		return this.sval == SV.qNaN;
	}

	static if (context == TestContext) {
	unittest {	// isNaN, isQuiet, isSignaling
		write("-- isNaN............");
		TD num;
		num = TD("2.50");
		assertFalse(num.isNaN);
		assertFalse(num.isQuiet);
		assertFalse(num.isSignaling);
		num = TD("NaN");
		assertTrue(num.isNaN);
		assertTrue(num.isQuiet);
		assertFalse(num.isSignaling);
		num = TD("-sNaN");
		assertTrue(num.isNaN);
		assertFalse(num.isQuiet);
		assertTrue(num.isSignaling);
		writeln("passed");
	}}

	/// Returns true if this number is + or - infinity.
	@safe
	const bool isInfinite()
	{
		return this.sval == SV.Inf;
	}

	/// Returns true if this number is not an infinity or a NaN.
	@safe
	const bool isFinite()
	{
		return sval != SV.Inf
			&& sval != SV.qNaN
			&& sval != SV.sNaN;
	}

	static if (context == TestContext) {
	unittest {	// isFinite, isInfinite
		write("-- isFinite.........");
		TD num;
		num = TD("2.50");
		assertFalse(num.isInfinite);
		assertTrue(num.isFinite);
		num = TD("-0.3");
		assertTrue(num.isFinite);
		num = 0;
		assertTrue(num.isFinite);
		num = TD("-Inf");
		assertTrue(num.isInfinite);
		assertFalse(num.isFinite);
		num = TD("NaN");
		assertFalse(num.isInfinite);
		assertFalse(num.isFinite);
		writeln("passed");
	}}

	/// Returns true if this number is a NaN or infinity.
	@safe
	const bool isSpecial()
	{
		return sval == SV.Inf
			|| sval == SV.qNaN
			|| sval == SV.sNaN;
	}

	static if (context == TestContext) {
	unittest {	// isSpecial
		write("-- isSpecial........");
		TD num;
		num = TD.infinity(true);
		assertTrue(num.isSpecial);
		num = TD.snan(1234,true);
		assertTrue(num.isSpecial);
		num = 12378.34;
		assertFalse(num.isSpecial);
		writeln("passed");
	}}

	/// Returns true if this number is negative. (Includes -0)
	@safe
	const bool isNegative()
	{
		return this.signed;
	}

	/// Returns true if this number is positive. (Excludes -0)
	@safe
	const bool isPositive()
	{
		return !this.signed;
	}

	alias isSigned = isNegative;

	static if (context == TestContext) {
	unittest {	// isSigned, isNegative
		write("-- isNegative.......");
		TD num;
		num = TD("2.50");
		assertFalse(num.isSigned);
		assertFalse(num.isNegative);
		num = TD("-12");
		assertTrue(num.isSigned);
		assertTrue(num.isNegative);
		num = TD("-0");
		assertTrue(num.isSigned);
		assertTrue(num.isNegative);
		writeln("passed");
	}}

	/// Returns true if this number is subnormal.
	@safe
	const bool isSubnormal(int minExponent = minExpo)
	{
		if (!isFinite) return false;
		return adjustedExponent < minExponent;
	}

	/// Returns true if this number is normal.
	@safe
	const bool isNormal(int minExponent = minExpo)
	{
		if (isFinite && !isZero) {
			return adjustedExponent >= minExponent;
		}
		return false;
	}

	static if (context == TestContext) {
	unittest { // isNormal, isSubnormal
		write("-- isNormal.........");
		TD num;
		num = TD("2.50");
		assertTrue(num.isNormal);
		assertFalse(num.isSubnormal);
		num = TD("0.1E-99");
		assertFalse(num.isNormal);
		assertTrue(num.isSubnormal);
		num = TD("0.00");
		assertFalse(num.isSubnormal);
		assertFalse(num.isNormal);
		num = TD("-Inf");
		assertFalse(num.isNormal);
		assertFalse(num.isSubnormal);
		num = TD("NaN");
		assertFalse(num.isSubnormal);
		assertFalse(num.isNormal);
		writeln("passed");
	}}

	/// Returns true if the number is an integer (the fractional part is zero).
	const bool isIntegralValued()
	{
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

	static if (context == TestContext) {
	unittest {	// isIntegralValued
		write("-- isIntegralValued.");
		TD num;
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
	const bool isTrue()
	{
		return isFinite && !isZero || isInfinite;
	}

	/// Returns true if this number is a false value.
	/// Finite numbers with zero coefficient are false.
	/// Infinity is true and NaN is false.
	@safe
	@property
	const bool isFalse()
	{
		return isNaN || isZero;
	}

	static if (context == TestContext) {
	unittest {	//isTrue/isFalse
		write("-- isTrue/isFalse...");
//		assertTrue(TD(1));
//		assert(One);
//		assertEqual(One, true);
//		assertTrue(cast(bool)One);
		assertTrue(TD("1").isTrue);
		assertFalse(TD("0").isTrue);
		assertTrue(infinity.isTrue);
		assertFalse(nan.isTrue);
		assertTrue(TD("0").isFalse);
		assertFalse(TD("1").isFalse);
		assertFalse(infinity.isFalse);
		assertTrue(nan.isFalse);
		writeln("passed");
	}}

/*	@safe
	const bool isZeroCoefficient() {
		return !isSpecial && coefficient == 0;
	}

	static if (context == TestContext) {
	unittest {	// isZeroCoefficient
		write("-- isZeroCoeff......");
		TD num;
		num = 0;
		assertTrue(num.isZeroCoefficient);
		num = xint("-0");
		assertTrue(num.isZeroCoefficient);
		num = TD("0E+4");
		assertTrue(num.isZeroCoefficient);
		num = 12345;
		assertFalse(num.isZeroCoefficient);
		num = 1.5;
		assertFalse(num.isZeroCoefficient);
		num = TD.NaN;
		assertFalse(num.isZeroCoefficient);
		num = TD.Infinity;
		assertFalse(num.isZeroCoefficient);
		writeln("passed");
	}}*/

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0 or 1, if this number is less than, equal to,
	/// or greater than the argument, respectively. NOTE: The comparison
	/// is made to the current precision.
	const int opCmp(T:decimal)(T that)
	{
		return compare(this, that);
	}

	/// Returns -1, 0 or 1, if this number is less than, equal to,
	/// or greater than the argument, respectively.
	const int opCmp(T)(T that)
	{
		return opCmp(decimal(that));
	}

	/// Returns true if this number is equal to the argument.
	/// Finite numbers are equal if they are numerically equal
	/// to the current precision.
	/// Infinities are equal if they have the same sign.
	/// Zeros are equal regardless of sign.
	/// A NaN is not equal to any number, not even to another NaN.
	/// A number is not even equal to itself (this != this) if it is a NaN.
	const bool opEquals(T:decimal)(T that)
	{
		return equals(this, that);
	}

	/// Returns true if this extended integer is equal to the argument.
	const bool opEquals(T)(T that)
	{
		return opEquals(decimal(that));
	}

	static if (context == TestContext) {
	unittest {	// comparison
		write("-- comparison.......");
		TD num1, num2;
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

	/// Returns the result of the
	/// unary operation on this number.
	//@safe
	private decimal opUnary(string op)()
	{
		static if (op == "+")
		{
			return plus(this);
		}
		else static if (op == "-")
		{
			return minus(this);
		}
		else static if (op == "++")
		{
			this = add(this, 1);
			return this;
		}
		else static if (op == "--")
		{
			this = sub(this, 1);
			return this;
		}
	}

	static if (context == TestContext) {
	unittest {	// opUnary
		write("-- opUnary..........");
		TD num, actual, expect;
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
		num = TD(9999999, 90);
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

	/// Returns the result of the specified
	/// binary operation on this number and the argument.
	decimal opBinary(string op, T:decimal)(in T x) const
	{
		static if (op == "+")
		{
			return add(this, x);
		}
		else static if (op == "-")
		{
			return sub(this, x);
		}
		else static if (op == "*")
		{
			return mul(this, x);
		}
		else static if (op == "/")
		{
			return div(this, x);
		}
		else static if (op == "%")
		{
			return remainder(this, x);
		}
		else static if (op == "&")
		{
			return and(this, x);
		}
		else static if (op == "|")
		{
			return or(this, x);
		}
		else static if (op == "^")
		{
			return xor(this, x);
		}
	}

	static if (context == TestContext)
	{
	unittest
	{	// opBinary
		write("-- opBinary.........");
		struct S { string op; string x; string y; string z; }
		S[] tests =
		[
			{ "+", "4", "8", "12" },
			{ "-", "4", "8", "-4" },
			{ "*", "4", "8", "32" },
			{ "/", "5", "2", "2.5" },
			{ "/", "2", "5", "0.4" },

			{ "%", "10", "3", "1" },
			{ "%", "3", "10", "3" },
			{ "&", "00011010", "10001110", "1010" },
			{ "|", "00011010", "10001110", "10011110" },
			{ "^", "00011010", "10001110", "10010100" },
		];
		foreach (i, s; tests)
		{
			testBinaryOp(s.op, s.x, s.y, s.z);
		}
		writeln("passed");
	}}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	decimal opBinary(string op, T)(T x) const
	{
		return opBinary!op(decimal(x));
	}

	/// Returns the result of performing the specified
	/// binary operation on this number and the argument.
	decimal opBinaryRight(string op, T)(in T x) const
	{
		static if (op == "+")
		{
			return add(this, x, decimal.context);
		}
		else static if (op == "-")
		{
			return sub(decimal(x), this, decimal.context);
		}
		else static if (op == "*")
		{
			return mul(this, x, decimal.context);
		}
		else static if (op == "/")
		{
			return div(decimal(x), this, decimal.context);
		}
		else static if (op == "%")
		{
			return remainder(decimal(x), this, decimal.context);
		}
		else static if (op == "&")
		{
			return and(this, decimal(x), mode);
		}
		else static if (op == "|")
		{
			return or(this, decimal(x), mode);
		}
		else static if (op == "^")
		{
			return xor(this, decimal(x), mode);
		}
		assert(false);
	}

//-----------------------------
// operator assignment
//-----------------------------

	/// Performs the specified binary operation on this number
	/// and the argument then assigns the result to this number.
	ref decimal opOpAssign(string op, T:decimal) (T x)
	{
		this = opBinary!op(x);
		return this;
	}

	/// Performs the specified binary operation on this number
	/// and the argument then assigns the result to this number.
	ref decimal opOpAssign(string op, T) (T x)
	{
		this = opBinary!op(decimal(x));
		return this;
	}

	static if (context == TestContext) {
	unittest {	// opOpAssign
		write("-- opOpAssign.......");
		TD op1, op2, actual, expect;
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
	decimal nextUp() const
	{
		return nextPlus(this, decimal.context);
	}

	/// Returns the largest representable number that is smaller than
	/// this number.
	decimal nextDown() const
	{
		return nextMinus(this, decimal.context);
	}

	/// Returns the representable number that is closest to the
	/// this number (but not this number) in the
	/// direction toward the argument.
	decimal nextAfter(decimal x) const
	{
		return nextToward(this, x, decimal.context);
	}

	static if (context == TestContext) {
	unittest {	// nextUp, nextDown, nextAfter
		write("-- next.............");
		TD big = 123.45;
		assertEqual(big.nextUp,   TD(123.450001));
		assertEqual(big.nextDown, TD(123.449999));
		assertEqual(big.nextAfter(TD(123.46)), big.nextUp);
		assertEqual(big.nextAfter(TD(123.44)), big.nextDown);
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

//	/// base 2 logarithm of 10 = 3.32192809...
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

	/// 2/pi
	mixin Constant!("twoInvPi","TWO_INV_PI");
	enum decimal TWO_INV_PI = roundString("0.63661977236758134307553505349005"
		"74481378385829618257949906693762355871905369061403604552110650123438");

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

	static if (context == TestContext) {
	unittest {
		write("-- constants........");
		assertEqual(TD.E,     "2.71828183");
		assertEqual(TD.pi,    "3.14159265");
		assertEqual(TD.PI,    "3.14159265");
		assertEqual(TD.LN2,   "0.693147181");
		assertEqual(TD.LN10,  "2.30258509");
		assertEqual(TD.SQRT2, "1.41421356");
		assertEqual(TD.INV_PI,"0.318309886");
		assertEqual(TD.invPi, "0.318309886");
		writeln("passed");
	}}

//--------------------------------
// decimal constant boilerplate
//--------------------------------

    /// mixin template to create a constant at the type precision,
	/// with an option to create an arbitrary precision constant.
	mixin template Constant(string name)
	{
		mixin ("public static decimal " ~ name ~ "(int precision = decimal.precision) {
			if (precision != decimal.precision) {
				return eris.decimal.math." ~ name ~ "!decimal(precision);
			}
			return " ~ name.toUpper ~ ";
		}");
	}

    /// mixin template to create a constant at the type precision,
	/// with an option to create an arbitrary precision constant.
	mixin template Constant(string lcName, string ucName)
	{
		mixin ("public static decimal " ~ lcName ~ "(int precision = decimal.precision) {
			if (precision != decimal.precision) {
				return eris.decimal.math." ~ lcName ~ "!decimal(precision);
			}
			return " ~ ucName ~ ";
		}");
	}

	/// Rounds a decimal string representation of a number
	/// to a specified precision.
	/// Does not convert the string to a number. A decimal point may be
	/// included, but no exponent is allowed.
	/// A string of all nines will throw an exception.
	private static string roundString(
			string str, int precision = decimal.precision)
		{

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

	static if (context == TestContext) {
	unittest {
		writeln("==========================");
		writeln("decimal................end");
		writeln("==========================");

	}}

}	 // end struct BigDecimal

	public enum double dpow10(int n)
	{
		static double[23] tens;
		bool initialized = false;
		if (!initialized)
		{
			tens[0] = 1.0;
			for (size_t i = 1; i < tens.length; i++)
			{
				tens[i] = tens[i-1] * 10.0;
			}
			initialized = true;
		}
		if (n > 22) return double.nan;
		return tens[n];
	}

/// Returns true if the parameter is a decimal number.
public enum bool isDecimal(T) = hasMember!(T, "IsDecimal");

unittest {
	write("-- isDecimal........");
	TD dummy;
	assertTrue(isDecimal!TD);
	assertFalse(isDecimal!int);
//	assertTrue(isDecimal!Dec64);
	writeln("passed");
}

/// Returns true if the parameter is convertible to a decimal number.
public enum bool isConvertible(T) = isNumeric!T || is(T:string) || isBoolean!T;

unittest {
	write("-- isConvertible...");
	assertTrue(isConvertible!int);
	assertTrue(isConvertible!bool);
	assertTrue(isConvertible!string);
	assertTrue(isConvertible!double);
	assertFalse(isConvertible!creal);
	assertFalse(isConvertible!TD);
	writeln("passed");
}

version(unittest)
{

	public enum bool testBinaryOp(T)(string op, T x, T y, T z,
			string file = __FILE__, int line = __LINE__)
		if (isDecimal!T)
	{
		switch (op)
		{
		// infix operators
		case "+":
			return assertBinaryOp("plus", x, y, x + y, z, file, line);
		case "-":
			return assertBinaryOp("minus", x, y, x - y, z, file, line);
		case "*":
			return assertBinaryOp("times", x, y, x * y, z, file, line);
		case "/":
			return assertBinaryOp("divided by", x, y, x / y, z, file, line);
		case "%":
			return assertBinaryOp("mod", x, y, x % y, z, file, line);
		case "&":
			return assertBinaryOp("and", x, y, x & y, z, file, line);
		case "|":
			return assertBinaryOp("or", x, y, x | y, z, file, line);
		case "^":
			return assertBinaryOp("xor", x, y, x ^ y, z, file, line);
		default:
			return false;
		}
	}

	public enum bool testBinaryOp(T)
		(string op, T x, T y, T z, string file = __FILE__, int line = __LINE__)
		if (!isDecimal!T && isConvertible!T)
	{
		return testBinaryOp!TD
			(op, TD(x), TD(y), TD(z), file, line);
	}

	private bool assertBinaryOp(T)
		(string op, T x, T y, T computed, T expected, string file, int line)
	{
		if (computed == expected)
		{
			return true;
		}
		else
		{
			writeln("failed at ", baseName(file), "(", line, "): \"",
				x , "\" " , op, " \"", y , "\" equals \"",
				computed, "\" not \"", expected, "\".");
			return false;
		}
	}

}



