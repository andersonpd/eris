// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2013 - 2014.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

/++
http://www.scala-lang.org/api/current/index.html#scala.math.BigInt
http://msdn.microsoft.com/en-us/library/system.numerics.biginteger.aspx
http://docs.oracle.com/javase/7/docs/api/java/math/BigInteger.html
http://www.oakcircle.com/xint_docs/
++/
module eris.integer.extended;

import std.ascii;
import std.conv;
import std.format;
import std.string;
import std.traits;

import eris.integer.digits;
import eris.integer.exception;

unittest {
	writeln("==========================");
	writeln("extended integers....begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
}

alias xint = ExtendedInt;

public struct ExtendedInt {

//--------------------------------
// structure
//--------------------------------

	private bool sign;
	private uint[] digits;

//--------------------------------
// private constructors
//--------------------------------

	/// Private constructor for internal use.
	/// Constructs an extended integer from an array of
	/// unsigned integers. In the internal array
	/// digits[0] is the least significant digit while
	/// digits[$-1] is most significant digit.
	@safe
	private this(T:uint[])(const T digits) {
		this.digits = digits.dup;
	}

	/// Private constructor for internal use.
	@safe
	private this(T:uint[])(bool sign, const T digits) {
		this.digits = digits.dup;
		this.sign = sign;
	}

//--------------------------------
// public constructors
//--------------------------------

	/// Constructs an extended integer from a signed integer type.
	@safe
	public this(T)(const T num)
			if (__traits(isIntegral, T) && !__traits(isUnsigned, T)) {
		long value = cast(long)num;
		if (value >= 0) {
			if (value > int.max) {
				digits.length = 2;
				digits[0] = low(value);
				digits[1] = high(value);
			}
			else {
				digits.length = 1;
				digits[0] = cast(uint)value;
			}
		}
		else {	// value < 0
			if (value < int.min) {
				digits.length = 2;
				digits[0] = low(value);
				digits[1] = high(value);
			}
			else {
				digits.length = 1;
				digits[0] = cast(uint)value;
			}
			sign = true;
			digits = negateDigits(digits);
		}
	}

	/// Constructs an extended integer from a boolean value.
	@safe
	public this(T:bool)(const T value) {
		this(cast(long)value);
	}

	/// Constructs an extended integer from an unsigned integer type.
	@safe
	public this(T)(const T num)
			if (__traits(isIntegral, T) && __traits(isUnsigned, T)) {
		ulong value = cast(ulong)num;
		if (value > uint.max) {
			digits.length = 2;
			digits[0] = low(value);
			digits[1] = high(value);
		}
		else {
			digits.length = 1;
			digits[0] = cast(uint)value;
		}
	}

	unittest {
		write("-- this(long).......");
		xint num;
		num = xint(1);
		assertEqual(num, 1);
		num = xint(12);
		assertEqual(num, 12);
		num = xint(-12);
		assertEqual(num, -12);
		num = xint([12u]);
		assertEqual(num, 12);
		num = xint([0xFFFFFFFF, 12]);
		assertEqual(num, 0x0000000C_FFFFFFFF);
		num = xint([12U, 9, -33]);
		assertEqual(num, xint("0xFFFFFFDF_00000009_0000000C"));

		// TODO: move most of these to the test module
		long slg;
		slg = long.max;
		assertStringEqual(xint(slg), slg);
		slg = short.max;
		assertStringEqual(xint(slg), slg);
		slg = long.min;
		assertStringEqual(xint(slg), slg);
		slg = long.min+1;
		assertStringEqual(xint(slg), slg);
		slg = long.min+2;
		assertStringEqual(xint(slg), slg);
		slg = long.min+3;
		assertStringEqual(xint(slg), slg);
		slg = int.min;
		assertStringEqual(xint(slg), slg);
		slg = long.max;
		assertStringEqual(xint(slg), slg);

		int sin;
		sin = short.min;
		assertStringEqual(xint(sin), cast(long)sin);
		sin = -1;
		assertStringEqual(xint(sin), cast(long)sin);
		sin = 1;
		assertStringEqual(xint(sin), cast(long)sin);

		short ssh;
		ssh = short.min;
		assertStringEqual(xint(ssh), cast(long)ssh);
		ssh = -1;
		assertStringEqual(xint(ssh), cast(long)ssh);
		ssh = 1;
		assertStringEqual(xint(ssh), cast(long)ssh);

		bool test = true;
		assertStringEqual(xint(test), test);

		ulong unl;
		unl = ulong.max;
		assertStringEqual(xint(unl), unl);
		unl = short.max;
		assertStringEqual(xint(unl), unl);
		unl = ulong.min;
		assertStringEqual(xint(unl), unl);
		unl = -1;
		assertStringEqual(xint(unl), unl);
		unl = 2;
		assertStringEqual(xint(unl), unl);

		uint uni;
		uni = uint.max;
		assertStringEqual(xint(uni), cast(ulong)uni);
		uni = short.max;
		assertStringEqual(xint(uni), cast(ulong)uni);
		uni = uint.min;
		assertStringEqual(xint(uni), cast(ulong)uni);
		uni = -1;
		assertStringEqual(xint(uni), cast(ulong)uni);
		uni = 2;
		assertStringEqual(xint(uni), cast(ulong)uni);

		ushort uns;
		uns = ushort.min;
		assertStringEqual(xint(uns), cast(ulong)uns);
		uns = ushort.max;
		assertStringEqual(xint(uns), cast(ulong)uns);
		uns = 1;
		assertStringEqual(xint(uns), cast(ulong)uns);
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// copy constructor
	@safe
	public this(T:xint)(const T that) {
		this.digits = that.digits.dup;
		this.sign = that.sign;
	}

	/// Returns a copy of an extended integer.
	@safe
	public const xint dup() {
		return xint(this);
	}

	/// Returns a copy of an extended integer.
	@safe
	public const xint copy() {
		return xint(this);
	}

	/// Returns a copy of an extended integer.
	@safe
	public const xint copyAbs() {
		xint copy = this.dup;
		copy.sign = false;
		return copy;
	}

	/// Returns a copy of an extended integer.
	@safe
	public const xint copyNegate() {
		xint copy = this.dup;
		copy.sign = !copy.sign;
		return copy;
	}

	unittest {
		write("-- copy.............");
		xint x = 3;
		assertEqual(x.dup, 3);
		assertEqual(xint(x), 3);
		assertEqual(x.copy, 3);
		assertEqual(x.copyAbs, 3);
		assertEqual(x.copyNegate, -3);
		assertEqual(x.copyNegate.copyAbs, 3);
		writeln("passed");
	}

//--------------------------------
// opCast
//--------------------------------

    ///
	@safe
	T opCast(T:bool)() {
		return !isZero();
	}

//--------------------------------
// constants
//--------------------------------

	public enum xint ZERO  = xint(0);
	public enum xint ONE   = xint(1);
	public enum xint TWO   = xint(2);
	public enum xint TEN   = xint(10);

	public enum xint MINUS_ONE = xint(-1);

	public enum xint UINT_MIN  = ZERO;
	public enum xint UINT_MAX  = xint(uint.max);
	public enum xint ULONG_MIN = ZERO;
	public enum xint ULONG_MAX = xint(ulong.max);

	public enum xint INT_MIN   = xint(int.min);
	public enum xint INT_MAX   = xint(int.max);
	public enum xint LONG_MIN  = xint(long.min);
	public enum xint LONG_MAX  = xint(long.max);

//--------------------------------
// properties
//--------------------------------

	/// Returns the initial value of an extended integer.
	@safe
	public static xint init() {
		return ZERO.dup;
	}

	/// Returns this extended integer with sign bit cleared.
	@safe
	public const xint abs() {
		xint copy = this.dup;
		copy.sign = false;
		return copy;
	}

	/// Returns the square of this extended integer.
	@safe
	public const xint sqr() {
		return xint(sqrDigits(this.digits));
	}

	unittest {
		write("-- init, abs, sqr...");
		xint a;
		assertZero(a);
		a = long.min;
		assertEqual(a.toHexString, "-0x80000000_00000000");
		assertEqual(a.abs.toHexString, "0x80000000_00000000");
		assertEqual(a.sqr.toHexString, "0x40000000_00000000_00000000_00000000");
		a = -a;
		assertEqual(a.toHexString, "0x80000000_00000000");
		assertEqual(a.abs.toHexString, "0x80000000_00000000");
		assertEqual(a.sqr.toHexString, "0x40000000_00000000_00000000_00000000");
		a = long.max;
		assertEqual(a.toHexString, "0x7FFFFFFF_FFFFFFFF");
		assertEqual(a.abs.toHexString, "0x7FFFFFFF_FFFFFFFF");
		assertEqual(a.sqr.toHexString, "0x3FFFFFFF_FFFFFFFF_00000000_00000001");
        a = -1;
		assertEqual(a, -1);
		assertEqual(a.sqr, 1);
		assertEqual(a.abs, 1);
		writeln("passed");
	}

//--------------------------------
// classification
//--------------------------------

	/// Returns one, zero, or minus one if this extended integer is
	/// positive, zero, or negative, respectively.
	@safe
	public const xint sgn() {
		if (this.isZero) {
			return ZERO.dup;
		}
		else {
			return this.sign ? MINUS_ONE.dup : ONE.dup;
		}
	}

	/// Returns true if this extended integer is zero, false otherwise.
	@safe
	public const bool isZero() {
		return digits.isZero;
	}

	/// Returns true if this extended integer is less than zero, false otherwise.
	@safe
	public const bool isNegative() {
		return sign;
	}

	/// Returns true if this extended integer is odd, false otherwise.
	@safe
	public const bool isOdd() {
		return digits.isOdd;
	}

	/// Returns true if this extended integer is even, false otherwise.
	@safe
	public const bool isEven() {
		return !isOdd;
	}

	/// Returns true if this extended integer is false (zero), false otherwise.
	@safe
	public const bool isFalse() {
		return digits.isZero;
	}

	/// Returns true if this extended integer is true (non-zero), false otherwise.
	@safe
	public const bool isTrue() {
		return !digits.isZero;
	}

//--------------------------------
// conversion to integral
//--------------------------------

	/// Returns true if the value of this extended integer
	/// is a valid unsigned integer value.
	@safe
	public const bool isValidUint() {
		return this >= 0 && this <= UINT_MAX;
	}

	/// Returns an unsigned integer with the value of this extended integer.
	/// If the value is too large to be represented uint.max is returned.
	@safe
	public const uint toUint() {
		if (this < 0) return 0U;
		if (this > UINT_MAX) return uint.max;
		return cast(uint)digits[0];
	}

	/// Returns true if the value of this extended integer
	/// is a valid unsigned long integer value.
	@safe
	public const bool isValidUlong() {
		return this >= 0 && this <= ULONG_MAX;
	}

	/// Returns an unsigned long int with the value of this extended integer.
	/// If the value is too large to be represented ulong.max is returned.
	@safe
	public const ulong toUlong() {
		if (this < 0) return 0L;
		// if too big to represent...
		if (this > ULONG_MAX) return ulong.max;
		// if single digit...
		if (this <= UINT_MAX) return cast(ulong)digits[0];
		// else two digits...
		return pack(digits[1],digits[0]);
	}

	unittest {	// conversion
		write("-- to unsigned int..");
		xint a;
		a = 8754;
		assertEqual(a.toUint, 8754);
		a = "0x12345678_12345678_12345678";
		assertEqual(a.toUint,  0xFFFFFFFF);
		assertEqual(a.toUlong, 0xFFFFFFFF_FFFFFFFF);
		a = 0x7FFFFFFE;
		assertEqual(a.toUint, 0x7FFFFFFE);
		a = 0x80000000;
		assertEqual(a.toUint, 0x80000000);
		a = 0x80000000_00000000;
		assertEqual(a.toUlong, 0x80000000_00000000);
		a = -a;
		assertEqual(a.toUlong,  0);
		writeln("passed");
	}

	/// Returns true if the value of this extended integer
	/// is a valid integer value.
	@safe
	public const bool isValidInt() {
		return this <= INT_MAX && this >= INT_MIN;
	}

	/// Returns a signed integer with the value of this extended integer.
	/// If the value is too large to be represented int.max/min is returned.
	@safe
	public const int toInt() {
		if (this > INT_MAX) return int.max;
		if (this < INT_MIN) return int.min;
		if (this.isZero) return 0;
		return cast(int)digits[0];
	}

	/// Returns true if the value of this extended integer
	/// is a valid long integer value.
	@safe
	public const bool isValidLong() {
		return this <= LONG_MAX && this >= LONG_MIN;
	}

	/// Returns a signed long integer with the value of this extended integer.
	/// If the value is too large to be represented long.max is returned.
	@safe
	public const long toLong() {
		// if too big to represent...
		if (this > LONG_MAX) return long.max;
		if (this < LONG_MIN) return long.min;
		if (this.isZero) return 0;
		// positive integers
		if (this >= 0) {
			// if single digit...
			if (this <= UINT_MAX) return cast(long)digits[0];
			// else two digits...
			return cast(long)pack(digits[1],digits[0]);
		}
		// negative integers
		// if single digit...
		if (this >= INT_MIN) return cast(long)digits[0];
		// else two digits...
		return cast(long)pack(digits[1],digits[0]);
	}


	unittest {	// conversion
		write("-- to signed int....");
		xint a;
		a = 9100;
		assertEqual(a.toInt, 9100);
		assertEqual(a.toLong, 9100);
		a = "0x12345678_12345678_12345678";
		assertEqual(a.toInt,   0x7FFFFFFF);
		assertEqual(a.toLong,  0x7FFFFFFF_FFFFFFFF);
		a = 0x7FFFFFFE;
		assertEqual(a.toInt,  0x7FFFFFFE);
		a = 0x80000000;
		assertEqual(a.toInt,  0x7FFFFFFF);
		a = -a;
		assertEqual(a.toInt,  0x80000000);
		a = 0x80000000_00000000;
		assertEqual(a.toLong, 0x7FFFFFFF_FFFFFFFF);
		a = -a;
		assertEqual(a.toLong, 0x80000000_00000000);
		a = 0xFFFFFFFF_FFFFFFFF;
		assertEqual(a.toLong, 0x7FFFFFFF_FFFFFFFF);
		a = 0x80000000;
		assertEqual(a.toLong, 0x80000000);
		a = -a;
		assertEqual(a.toLong, 0x80000000);
		a = LONG_MIN;
		assertEqual(a.toLong, 0x80000000_00000000);
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns an extended integer value to an extended integer.
	@safe
	private void opAssign(T:xint)(const T that) {
		this.digits = that.digits.dup;
		this.sign = that.sign;
	}

	/// Assigns a compatible value to an extended integer.
	@safe
	private void opAssign(T)(const T that) {
		opAssign(xint(that));
	}

	unittest {	// assignment
		write("-- opAssign.........");
		xint actual = xint(123);
		xint expect = actual;
		assertEqual(actual, expect);
		actual = 123L;
		assertEqual(actual, expect);
		actual = "123";
		assertEqual(actual, expect);
		writeln("passed");
	}

	/// Performs an operation on this and assigns the result to this.
	@safe
	private ref xint opOpAssign(string op, T:xint)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	@safe
	private ref xint opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

//--------------------------------
// equality
//--------------------------------

	/// Returns true if the argument is equal to this extended integer.
	@safe
	private const bool opEquals(T:xint)(const T that) {
		// if signs differ the integers cannot be equal.
		if (!this.isZero && this.sign != that.sign) {
			return false;
		}
		if (this.digits.length == that.digits.length) {
			return (this.digits == that.digits);
		}
        else {
			return trimDigits(this.digits) == trimDigits(that.digits);
		}
	}

	 /// Returns true if the argument is equal to this extended integer.
	@safe
	private const bool opEquals(T)(const T that) {
		return opEquals(xint(that));
	}

	unittest { // equality
		write("-- opEquals.........");
		assertNotEqual(xint(5), xint(6));
		assertNotEqual(xint(5), 6);
		assertNotEqual(xint(3), xint(10));
		assertEqual(xint(195), xint(195));
		xint pzero = ZERO.dup;
		xint mzero = ZERO.dup;
		mzero.sign = true;
		assertEqual(pzero, mzero);
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this extended integer is, respectively,
	/// less than, equal to or greater than the argument.
	@safe
	public static int compare(const xint x, const xint y) {
		if (x.isNegative && !y.isNegative) return -1;
		if (y.isNegative && !x.isNegative) return  1;
		if (x.isNegative) {
			return compareDigits(y.digits, x.digits) ;
		}
		else {
			return compareDigits(x.digits, y.digits);
		}
	}

	private const int opCmp(T:xint)(const T that) {
		return compare(this, that);
	}

	/// Returns -1, 0, or 1, if this extended integer is, respectively,
	/// less than, equal to or greater than the argument.
	@safe
	private const int opCmp(T)(const T that) {
		return opCmp(xint(that));
	}

	unittest { // comparison
		write("-- opCmp............");
		assertLessThan(xint(9100), xint(long.max));
		assertLessThan(xint(5), xint(6));
		assertLessThan(xint(5), 6);
		assertLessThan(xint(3), xint(10));
		assert(xint(195) >= xint(195));
		assert(xint(195) >= 195);

		assert(xint(5) < xint(6));
		assert(xint(5) < 6);
		assert(xint(3) < xint(10));
		assert(xint(195) >= xint(195));
		assert(xint(195) >= 195);

		assertGreaterThan(xint(-5), xint(-6));
		assert(xint(-5) < xint(6));
		assertGreaterThan(xint(3), xint(-10));
		assertGreaterThan(xint(10), xint(-3));
		assert(xint(195) >= xint(195));
		assert(xint(195) >= -195);
		writeln("passed");
	}

//--------------------------------
// byte functions
//--------------------------------

	/// Returns the unsigned value of the extended integer as
	///	a big-endian array of bytes. Sign is ignored.
	public const ubyte[] toByteArray() {
		uint index = 0;
		ubyte[] buffer = new ubyte[digits.length * 4];
		foreach (uint digit; digits) {
			std.bitmanip.write!uint(buffer, digit, &index);
		}
		return buffer;
	}

	/// Constructs an extended integer from an array of bytes. Sign is not set.
	public this(T:ubyte[])(T buffer) {
		int n = buffer.length / 4;
		int m = buffer.length % 4;
		if (m != 0) {
			n++;
			buffer.length = n * 4;
		}
		this.digits.length = n;
		foreach (ref uint digit; digits) {
			digit = std.bitmanip.read!uint(buffer);
		}
	}

	unittest {
		write("-- byte array.......");
		xint a = xint("12345678901234567890");
		ubyte[] b = a.toByteArray;
		xint c = xint(b);
		assertEqual(a,c);
		writeln("passed");
	}

//--------------------------------
// string functions
//--------------------------------

	/// Constructs an extended integer from a string representation.
	/// The constructor recognizes decimal, hexadecimal and binary strings.
	/// Non-decimal strings are treated as unsigned integers. That is,
	/// they are always positive.
	///	For negative hexadecimal, etc., values use xint(bool, string).
	@safe
	public this(T:string)(const T str) {
		char[] chars = strip(str.dup);
		this.sign = false;
		// parse sign character, if any.
		if (chars[0] == '-') {
			chars = chars[1..$];
			this.sign = true;
		}
		if (chars[0] == '+') {
			chars = chars[1..$];
		}
		// if the string starts withs a non-zero digit,
		// it should be a decimal number.
		if (chars[0] != '0') {
			this.digits = parseDecimal(chars);
			return;
		}
		// Otherwise it may be hex or binary
		toLowerInPlace(chars);
		if (startsWith(chars, "0x")) {
			digits = parseHex(chars[2..$].dup);
			return;
		}
		else if (startsWith(chars, "0b")) {
			digits = parseBinary(chars[2..$]);
			return;
		}
		this.digits = parseDecimal(chars);
	}

	/// Constructs an extended integer from a sign and a string value.
	@safe
	public this(T:string)(bool sign, const T str) {
		this(str);
		this.sign = sign;
	}

	@safe
	private static uint[] parseHex(char[] chars) {
		uint[] digits;
		uint digit = 0;
		foreach (char ch; chars) {
			if (ch == '_') continue;
			if (!isHexDigit(ch)) {
				throw new ConvException("Invalid hexadecimal char: [" ~ ch ~ "]");
			}
			digits = mulDigit(digits, 16U);
			if (isDigit(ch)) {
				digit = ch - '0';
			}
			else {
				digit = ch - 'a' + 10;
			}
			digits = addDigit(digits, digit);
		}
		return digits;
	}

	@safe
	private static uint[] parseBinary(char[] chars) {
		uint[] digits;
		foreach (char ch; chars) {
			if (ch == '_') continue;
			if (ch != '0' && ch != '1') {
				throw new ConvException("Invalid binary char: [" ~ ch ~ "]");
			}
			digits = mulDigit(digits, 2U);
			if (ch == '1') digits = addDigit(digits, 1);
		}
		return digits;
	}

	@safe
	private static uint[] parseDecimal(char[] chars) {
		uint[] digits;
		if (!isDigit(chars[0])) {
			throw new ConvException("Invalid start char: [" ~ chars[0] ~ "]");
		}
		foreach (char ch; chars) {
			if (ch == '_') continue;
			if (!isDigit(ch)) {
				throw new ConvException("Invalid decimal char: [" ~ ch ~ "]");
			}
			digits = mulDigit(digits, 10U);
			digits = addDigit(digits, cast(uint)(ch - '0'));
		}
		return digits;
	}

	unittest
	{
		write("-- this(string).....");
		string str;
		xint unum = 123;
		assertEqual(xint("123"), unum);
		assertEqual(xint("0x7B"), unum);
		assertEqual(xint("-0b1111011"), -unum);
		assertEqual(xint("0_234_445"), 234445);
		xint snum = -123;
		assertEqual(xint("-123"), snum);
		xint pos = xint("0x7B");
		xint neg = xint(true, "0x7B");
		neg = "-0x0024";
//		neg = xint(negateDigits(snum.digits));
//		assertEqual(xint("0xFFFFFF85"), snum);
//		assertEqual(xint("0b11111111111111111111111111111111_11111111111111111111111111111111_11111111111111111111111111111111_11111111111111111111111110000101"), snum);
		assertEqual(xint("0_234_445"), 234445);
		writeln("passed");
	}

	public const string toString(string fmt = "%s") {
		import std.exception : assumeUnique;
		char[] buf;
		buf.reserve(100);
		toString((const(char)[] s) { buf ~= s; }, fmt);
		return assumeUnique(buf);
	}

	public const void toString(scope void delegate(const(char)[]) sink,
		string formatString) {

		auto f = singleSpec!char(formatString.dup);
		switch (f.spec) {
			case 's':
			case 'd': formatDecimal(sink, f);
						break;
			case 'x':
			case 'X': formatHex(sink);
						break;
			case 'b': formatBinary(sink);
						break;
			default: throw new FormatException("Format specifier not recognized %" ~ f.spec);
		}
	}

    public const void formatDecimal(
			scope void delegate(const (char)[]) sink, ref FormatSpec!char f) {

        string str = toDecimalString(false);
		char signChar = isNegative() ? '-' : 0;
        auto minw = str.length + (signChar ? 1 : 0);

        if (!signChar && (f.width == 0 || minw < f.width))
        {
            if (f.flPlus)
                signChar = '+', ++minw;
            else if (f.flSpace)
                signChar = ' ', ++minw;
        }

        auto maxw = minw < f.width ? f.width : minw;
        auto difw = maxw - minw;

        if (!f.flDash && !f.flZero)
            foreach (i; 0 .. difw)
                sink(" ");

        if (!f.flDash && f.flZero)
            foreach (i; 0 .. difw)
                sink("0");

        if (signChar)
            sink((&signChar)[0..1]);

        sink(str);

        if (f.flDash)
            foreach (i; 0 .. difw)
                sink(" ");
	}

    public const void formatHex(
			scope void delegate(const (char)[]) sink) {
		sink(toHexString);
	}

    public const void formatBinary(
			scope void delegate(const (char)[]) sink) {
		sink(toBinaryString);
	}

	public const string toDecimalString(bool addSign = true) {
		char[] str;
		uint[] from = this.digits.dup;
		uint n = numDigits(from);
		if (n == 0) return "0";
		while (n > 0)
		{
			uint mod;
			char[1] ch;
			from = divmodDigit(from, n, 10, mod);
			std.string.sformat(ch, "%d", mod);
			str = ch ~ str;
			n = numDigits(from);
		}
		if (addSign && sign) str = "-" ~ str;
		return str.idup;
	}

	/// Converts the extended integer value to a hexadecimal string.
	public const string toHexString() {
		char[] str;
		int length = numDigits(digits);
		if (length == 0) {
			return ("0x00000000");
		}
		for (int i = 0; i < length; i++) {
			str = std.string.format("_%08X", digits[i]) ~ str;
		}
//		if (sign) str = '-' ~ str;
		return sign ? "-0x" ~ str[1..$].idup : "0x" ~ str[1..$].idup;
	}

	/// Converts the extended integer value to a binary string.
	// FIXTHIS: doesn't do signed
	public const string toBinaryString() {
		char[] str;
		int n = numDigits(digits);
		if (n == 0) {
			return ("0b00000000");
		}
		for (int i = 0; i < n; i++) {
			uint num = digits[i];
			for (int j = 0; j < 4; j++ ) {
				uint chunk = num % 256;
				str = std.string.format("_%08b", chunk) ~ str;
				num /= 256;
			}
		}
		return "0b" ~ str[1..$].idup;
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	@safe
	private xint opUnary(string op)() {
		static if (op == "+") {
			return plus();
		}
		else static if (op == "-") {
			return minus();
		}
		else static if (op == "~") {
			return complement();
		}
		else static if (op == "++") {
			this = add(this, ONE);
			return this;
		}
		else static if (op == "--") {
			this = sub(this, ONE);
			return this;
		}
	}



	unittest {	// opUnary
		write("-- opUnary..........");
		xint num = 4;
		assertEqual(+num, num);
		assertEqual(-(-num), xint(4));
		assertEqual(~(~num), xint(4));
		assertEqual(++num, xint(5));
		assertEqual(--num, xint(4));
		assertFalse(!num);
		assertTrue(cast(bool)num);
		xint snum = -4;
		assertEqual(+snum, snum);
		assertEqual(-(-snum), xint(-4));
		assertEqual(~(~snum), xint(-4));
		assertEqual(++snum, xint(-3));
		assertEqual(--snum, xint(-4));
		snum = xint(286331136);
		assertEqual(~snum, 4008636159);
		assertEqual(-snum, -286331136);
		writeln("passed");
	}

	/// Returns a copy of this extended integer.
	@safe
	public const xint plus() {
		return this.dup;
	}

	/// Returns a copy of this extended integer with sign inverted.
	@safe
	public const xint minus() {
		if (this.isZero) return ZERO.dup;
		auto copy = this.dup;
		copy.sign = !this.sign;
		return copy;
	}

	/// Returns the ones complement of this extended integer
	@safe
	public const xint complement() {
		auto temp = digits.dup;
		eris.integer.digits.reduce(temp);
		auto comp = compDigits(temp);
		return xint(sign, comp);
	}

	/// Returns the twos complement of this extended integer
	@safe
	public const xint negate() {
		if (this.isZero) return ZERO.dup;
		auto comp = this.complement;
		comp++;
		return comp;
	}

	/// Converts the integer to twos complement representation.
	/// If the number is positive or zero, a copy is returned.
	/// Otherwise, the number is negated, the sign is set to false,
	/// and the result is returned.
	@safe
	public const xint toTwosComplement() {
		if (!this.sign) return this.dup;
		auto copy = this.negate;
		copy.sign = false;
		return copy;
	}

//--------------------------------
// binary operations
//--------------------------------

	@safe
	private const xint opBinary(string op, T:xint)(const T that) {
		static if (op == "+") {
			return add(this, that);
		}
		else static if (op == "-") {
			return sub(this, that);
		}
		else static if (op == "*") {
			return mul(this, that);
		}
		else static if (op == "/") {
			return div(this, that);
		}
		else static if (op == "%") {
			return mod(this, that);
		}
		else static if (op == "^^") {
			return pow(this, that);
		}
		else static if (op == "&") {
			return and(this, that);
		}
		else static if (op == "|") {
			return or(this, that);
		}
		else static if (op == "^") {
			return xor(this, that);
		}
		else static if (op == "<<") {
			return shl(this, that);
		}
		else static if (op == ">>") {
			return shr(this, that);
		}
		else static if (op == ">>>") {
			return lshr(this, that);
		}
	}	// end opBinary

	@safe
	private const xint opBinary(string op, T)(const T that) {
		return opBinary!(op, xint)(xint(that));
	}

//--------------------------------
// binary operations
//--------------------------------

	unittest {	// opBinary
		write("-- opBinary.........");
		xint x, y;
		x = 4; y = 8;
		// test addition
		assertEqual(y + x, 12);
		// test subtraction
		assertEqual(y - x, 4);
		// test multiplication
		assertEqual(x * y, 32);
		// test division
		x = 5; y = 2;
		assertEqual(x / y, 2);
		assertEqual(x % y, 1);
		// test power function
		assertEqual(x ^^ y, 25);
		// test logical operations
		x = 10101; y = 10001;
		assertEqual((x & y), 10001);
		assertEqual((x | y), 10101);
		assertEqual((x ^ y), 100);
		// test left and right shifts
//		y = 2;
//		assertEqual(x << y, 40404);
//		assertEqual(x >> y, 2525);
		x = 4; y = xint([0u,1u]);
		assertEqual(x + y, 0x100000004);
		writeln("passed");
	}

//--------------------------------
// arithmetic operations
//--------------------------------

	/// Adds two extended integers and returns the sum.
	@safe
	private static xint add(const xint x, const xint y)
	{
		xint sum;
		if (x.sign == y.sign) {
			sum = xint(addDigits(x.digits, y.digits));
			sum.sign = x.sign;
		}
		else {
			int comp = compareDigits(x.digits, y.digits);
			if (comp > 0) { // x > y.
				sum = xint(subDigits(x.digits, y.digits));
				sum.sign = x.sign;
			}
			else if (comp < 0) { // y > x.
				sum = xint(subDigits(y.digits, x.digits));
				sum.sign = y.sign;
			}
		}
		return sum;
	}

	unittest {
		write("-- addition.........");
		xint x, y;
		x = 4; y = 8;
		assertEqual(x + y, 12);
		x = -4; y = -8;
		assertEqual(x + y, -12);
		x = -4; y = 8;
		assertEqual(x + y, 4);
		x = -4; y = -8;
		assertEqual(x + y, -12);
		writeln("passed");
	}

	/// Subtracts one extended integer from another and returns the difference.
	@safe
	private static xint sub(const xint x, const xint y) {
		if (x.sign == y.sign) {
			return add(x, y.minus);
		}
		else {
			auto diff = xint(addDigits(x.digits, y.digits));
			diff.sign = x.sign;
			return diff;
		}
	}

	unittest {
		write("-- subtraction......");
		xint x, y;
		x = 4; y = 8;
		assertEqual(y - x, xint(4));
		// both pos, x > y
		x = 15; y = 8;
		assertEqual(x - y, 7);
		// both pos, y > x
		x = 4; y = 8;
		assertEqual(x - y, -4);
		// both neg, |x| > |y|
		x = -11; y = -8;
		assertEqual(x - y, -3);
		// both neg, |x| < |y|
		x = -9; y = -28;
		assertEqual(x - y, 19);
		// signs differ, x pos, x > |y|
		x = 15; y = -8;
		assertEqual(x - y, 23);
		// signs differ, x pos, x < |y|
		x = 5; y = -8;
		assertEqual(x - y, 13);
		// signs differ, y pos, |x| > y
		x = -24; y = 8;
		assertEqual(x - y, -32);
		// signs differ, y pos, |x| < y
		x = -4; y = 18;
		assertEqual(x - y, -22);
		// interoperability
		x = 4; y = 8;
		assertEqual(x - y, -4);
		writeln("passed");
	}

	/// Multiplies two extended integers and returns the product.
	@safe
	private static xint mul(const xint x, const xint y) {
		uint[] xd;
		int nx = copyDigits(x.digits, xd);
		// special cases: x = 0, x = 1, x = -1
		if (nx == 0) return ZERO.dup;
		if (nx == 1 && xd[0] == 1) {
			return x.sign ? y.minus : y.dup;
		}

		uint[] yd;
		int ny = copyDigits(y.digits, yd);
		// special cases: y = 0, y = 1, y = -1
		if (ny == 0) return ZERO.dup;
		if (ny == 1 && yd[0] == 1) {
			return y.sign ? x.minus : x.dup;
		}

		xint product = xint(mulDigits(xd, nx, yd, ny));
		product.sign = x.sign ^ y.sign;
		return product;
	}

	unittest {
		write("-- multiplication...");
		xint x = 2;
		xint y = 6;
		assertEqual(x * y, 12);
		assertEqual(x * y, xint(12));
		x = -x;
		assertEqual(x * y, xint(-12));
		y = -y;
		assertEqual(x * y, xint(12));
		x = -x;
		assertEqual(x * y, xint(-12));
		// TODO: test special cases
		writeln("passed");
	}

	/// Divides one extended integer by another and returns the integer quotient.
	@safe
	private static xint div(const xint x, const xint y) {
        if (y == 0) throw new DivByZeroException("division by zero");
		uint[] xd;
		int nx = copyDigits(x.digits, xd);
		if (nx == 0) return ZERO.dup;
		uint[] yd;
		int ny = copyDigits(y.digits, yd);
		auto quotient = xint(divDigits(xd, nx, yd, ny));
		quotient.sign = x.sign ^ y.sign;
		return quotient;
	}

	unittest {
		write("-- division.........");
		xint x = 6;
		xint y = 2;
		assertEqual(x/y, xint(3));
		x = -x;
		assertEqual(x/y, xint(-3));
		y = -y;
		assertEqual(x/y, xint(3));
		x = -x;
		assertEqual(x/y, xint(-3));
		x = 0;
		assertEqual(x/y, ZERO);
		x = xint("1234567890");
		y = xint("123456789");
		assertEqual(x/y, 10);
		y = "10000000000";
		x = "22345678901234"; //89012345";
		assertEqual(x/y, 2234);
		x = xint("145678901234567890");
		y = xint("14567890123456789");
		assertEqual(x/y, 10);
		assertThrows!DivByZeroException(x/0);
		writeln("passed");
	}

	/// Divides one extended integer by another and returns the remainder.
	@safe
	private static xint mod(const xint x, const xint y) {
		// FIXTHIS: check for division by zero.
		uint[] xd;
		int nx = copyDigits(x.digits, xd);
		if (nx == 0) return ZERO.dup;
		uint[] yd;
		int ny = copyDigits(y.digits, yd);
		if (x == y) return ZERO.dup;
		auto remainder = xint(modDigits(xd, nx, yd, ny));
		remainder.sign = x.sign;	// FIXTHIS: is this okay?
		return remainder;
	}

	unittest {
		write("-- remainder........");
		xint ux = 2;
		xint uy = 2;
		assertEqual(ux % uy, 0);
		ux = 7;
		uy = 2;
		assertEqual(ux % uy, 1);
		xint x = 7;
		xint y = 2;
		assertEqual(x % y, 1);
		x = -x;
		assertEqual(x % y, -1);
		y = -y;
		assertEqual(x % y, -1);
		x = -x;
		assertEqual(x % y, 1);
		x = "5000000000000000000000";
		y = "1000000000000000000000";
		assertEqual(x % y, 0);
		x = "500000000000000000000";
		y = "100000000000000000000";
		assertEqual(x % y, 0);
		x = "50000000000000000000";
		y = "10000000000000000000";
		assertEqual(x % y, 0);

		writeln("passed");
	}

	/// Raises an extended integer to an integer power.
	@safe
	private static xint pow(const xint x, const xint y) {
		return xint(pow(x, y.toUint));
	}

	/// Raises an extended integer to an integer power.
	@safe
	private static xint pow(const xint x, const int n) {
		if (n < 0) throw new InvalidOperationException();
		if (n == 0) return ONE.dup;
		if (n == 1) return x.dup;
		if (n == 2) return x.sqr;
		return xint(powDigits(x.digits, n));
	}

	unittest {
		write("-- power............");
		xint x, y;
		x = 3;
		y = 2;
		assertEqual(x^^y, 9);
		x = 10;
		y = 12;
		xint z = x^^y;
		assertEqual(z, xint(1000000000000));
		writeln("passed");
	}

//--------------------------------
// right and left shifts
//--------------------------------

	/// Shifts an extended integer left by an integral value.
	/// No check for overflow is made.
	@safe
	private static xint shl(const xint x, const xint y) {
		return shl(x, y.toUint);
	}

	/// Shifts an extended integer left by an integral value.
	/// No check for overflow is made.
	@safe
	private static xint shl(const xint x, const uint n) {
		int nDigs = n / 32;
		int nBits = n % 32;
		uint [] array = x.digits.dup;
		if (nDigs != 0) {
			array = shlDigits(array, nDigs);
		}
		array = shlBits(array, nBits);
		return xint(array);
	}

	/// Shifts an extended integer right by an integral value.
	@safe
	private static xint shr(const xint x, const xint y) {
		return shr(x, y.toInt);
	}

	/// Shifts an extended integer right by an integral value.
	@safe
	private static xint shr(const xint x, const uint n) {
		int digits = n / 32;
		int nBits = n % 32;
		// FIXTHIS: see fixed integer
		uint [] array = x.digits.dup;
		array = shrDigits(array, digits);
		array = shrBits(array, nBits);
		return xint(array);
	}

unittest {
	write("shr...");
	xint a = "1_000_000_000_000_000";
	uint b = 15;
	xint c = shr(a,b);
	assertEqual(c, xint(0x00000007_1AFD498D));
	writeln("passed");
}
	// FIXTHIS: no logical shift??

//--------------------------------
// logical operations
//--------------------------------

	/// Performs a bitwise AND of the integers and returns the result.
	/// Negative integers are converted to their twos-complement representation.
	/// If the integers are of unequal lengths the shorter is sign-extended.
	@safe
	private static xint and(const xint x, const xint y) {
		auto xd =  x.isNegative ? negateDigits(x.digits) : x.digits.dup;
		auto yd =  y.isNegative ? negateDigits(y.digits) : y.digits.dup;
		matchLengthSigned(xd, yd);
		xint z = xint(andDigits(xd, yd));
		z.sign = x.sign & y.sign;
		if (z.sign) {
			z.digits = negateDigits(z.digits);
		}
		return z;
	}

	/// Performs a bitwise OR of the integers and returns the result.
	/// Negative integers are converted to their twos-complement representation.
	/// If the integers are of unequal lengths the shorter is sign-extended.
	@safe
	private static xint or(const xint x, const xint y) {
		auto xd =  x.isNegative ? negateDigits(x.digits) : x.digits.dup;
		auto yd =  y.isNegative ? negateDigits(y.digits) : y.digits.dup;
		matchLengthSigned(xd, yd);
		xint z = xint(orDigits(xd, yd));
		z.sign = x.sign | y.sign;
		if (z.sign) {
			z.digits = negateDigits(z.digits);
		}
		return z;
	}

	/// Performs a bitwise XOR of the integers and returns the result.
	/// Negative integers are converted to their twos-complement representation.
	/// If the integers are of unequal lengths the shorter is sign-extended.
	@safe
	private static xint xor(const xint x, const xint y) {
		auto xd =  x.isNegative ? negateDigits(x.digits) : x.digits.dup;
		auto yd =  y.isNegative ? negateDigits(y.digits) : y.digits.dup;
		matchLengthSigned(xd, yd);
		xint z = xint(xorDigits(xd, yd));
		z.sign = x.sign ^ y.sign;
		if (z.sign) {
			z.digits = negateDigits(z.digits);
		}
		return z;
	}

	unittest {
		write("-- logical ops......");

		xint A = "0x80000000_00000000_00001111";
		xint B = "0xFF00FF00_88883333_AAAA5555";
		xint C = "0x80000000";
		xint D = "0x7FFFFFFF_FFFFFFFF_FFFFFFFF_BEEFCAFE";

		assertEqual(A & B, xint("0x80000000_00000000_00001111"));
		assertEqual(A | B, xint("0xFF00FF00_88883333_AAAA5555"));
		assertEqual(A ^ B, xint("0x7F00FF00_88883333_AAAA4444"));

		assertEqual(A & C, xint("0x00000000"));
		assertEqual(A | C, xint("0x80000000_00000000_80001111"));
		assertEqual(A ^ C, xint("0x80000000_00000000_80001111"));

		assertEqual(B & C, xint("0x80000000"));
		assertEqual(B | C, xint("0xFF00FF00_88883333_AAAA5555"));
		assertEqual(B ^ C, xint("0xFF00FF00_88883333_2AAA5555"));

		assertEqual(B & D, xint("0x7FFFFFFF_FF00FF00_88883333_AAAA4054"));
		assertEqual(B | D, xint("0xFFFFFFFF_FFFFFFFF_FFFFFFFF_BEEFDFFF"));
		assertEqual(B ^ D, xint("0x80000000_00FF00FF_7777CCCC_14459FAB"));

		assertEqual(C & D, xint("0x80000000"));
		assertEqual(C | D, xint("0x7FFFFFFF_FFFFFFFF_FFFFFFFF_BEEFCAFE"));
		assertEqual(C ^ D, xint("0x7FFFFFFF_FFFFFFFF_FFFFFFFF_3EEFCAFE"));

		long ai,bi,ci,di;
		ai = 3;
		bi = -7;
		ci = 0xFF;
		di = -12;
		xint a,b,c,d;
		a = 3;
		b = -7;
		c = 0xFF;
		d = -12;
		assertEqual(a, ai);
		assertEqual(b, bi);
		assertEqual(c, ci);
		assertEqual(d, di);
		assertEqual(a & b, ai & bi);
		assertEqual(a & c, ai & ci);
		assertEqual(b & d, bi & di);
		assertEqual(a | b, ai | bi);
		assertEqual(a | c, ai | ci);
		assertEqual(b | d, bi | di);
		assertEqual(a ^ b, ai ^ bi);
		assertEqual(a ^ c, ai ^ ci);
		assertEqual(b ^ d, bi ^ di);
		writeln("passed");
	}

//--------------------------------
// bit operations
//--------------------------------

	// Copies the digit array to a std.bitmanip.BitArray and returns
	// the bit array.
	public static std.bitmanip.BitArray toBitArray(xint a) {
		digit[] copy = a.digits.dup;
        std.bitmanip.BitArray ba;
		ba.init(cast(void[])copy, copy.length*32);
		return ba;
	}

	/// Sets (or clears) a single bit in an extended integer.
	/// Bits are counted right-to-left (lsb is bit zero).
	/// If the index of the bit is longer than the integer size,
	/// no change is made.
	@safe
	public void setBit(int n, bool value = true) {
		uint d = n / 32;
		if (d >= digits.length) return;
		uint b = n % 32;
		uint mask = 1 << b;
		if (value) {
			digits[d] |= mask;
		}
		else {
			digits[d] &= ~mask;
		}
	}

	/// Tests a single bit in an unsigned extended integer.
	/// Bits are counted right-to-left (lsb is bit zero).
	/// Returns true if the bit == 1, false otherwise.
	/// Returns false if the index of the bit is longer than the integer size.
	@safe
	public const bool testBit(int n) {
		uint d = n / 32;
		if (d >= digits.length) return false;
		uint digit = digits[d];
		uint b = n % 32;
		uint mask = 1 << b;
		return cast(bool) (digit &= mask);
	}

	unittest {
		write  ("-- bit ops..........");
		xint x = 0x00100000;
		x.setBit(3);
		assertEqual(x, xint(0x00100008));
		x.setBit(3, false);
		assertEqual(x, xint(0x00100000));
		xint y = xint([0x123U, 0x345U, 0x123456U]);
        y.setBit(60);
		assertEqual(y, xint("0x00123456_10000345_00000123"));
        assertFalse(y.testBit(95));
		y.setBit(95);
        assertTrue(y.testBit(95));
		assertEqual(y, xint("0x80123456_10000345_00000123"));
        y.setBit(100);
		assertEqual(y, xint("0x80123456_10000345_00000123"));
		y.setBit(1,false);
		assertEqual(y, xint("0x80123456_10000345_00000121"));
		writeln("passed");
	}

//--------------------------------
// digit manipulation
//--------------------------------

	/// Returns the digit at the specified index.
	/// The index is zero-based and increases in left-to-right order.
	/// For example, getDigit(0) returns the most significant digit
	///	(which may be zero), while getDigit(digits.length - 1)
	/// returns the least significant digit.
	/// If the index is larger than the current length of the
	/// internal array of digits, zero is returned.
	@safe
	public uint getDigit(int n) {
		if (n >= digits.length) return 0;
		return digits[$ - n - 1];
	}

	/// Sets the value of a single uint digit in left-to-right order.
	/// setDigit(0, value) sets the most significant digit,
	/// setDigit(digits.length - 1) sets the least significant digit.
	@safe
	public void setDigit(int n, uint value) {
		if (n >= digits.length - 1) digits.length = n + 1;
		digits[$ - n - 1] = value;
	}


	/// Returns the length of the uint digit array. Includes leading
	/// zeros, if any.
	@safe
	public const int getDigitLength() {
		return digits.length;
	}

	/// Returns the number of significant digits in this integer.
	/// Leading zeros are ignored.
	@safe
	private const int getDigitCount() {
		return this.digits.numDigits();
	}

	/// Returns this extended integer with leading zeros removed.
	@safe
	public const xint trim() {
		return xint(trimDigits(this.digits));
	}

	/// Returns this extended integer with leading zeros removed.
	@safe
	public void reduce() {
		eris.integer.digits.reduce(this.digits);
	}

	/// Increases the length of the digit array of this extended integer
	/// to at least the specified value. If the array is already as long
	/// or longer than the specified length the integer is not modified.
	@safe
	public void extend(int n) {
		if (n > this.digits.length) {
			this.digits.length = n;
		}
	}

	/// Increases the length of the digit array of this extended integer
	/// to at least the specified value. If the array is already as long
	/// or longer than the specified length the integer is not modified.
	@safe
	public void extendTwosComplement(int n) {
		if (n < this.digits.length) {
			this.digits.length = n;
		}
	}

	unittest {
//		write("digit manipulation...");
		write("-- digit ops........");
		xint x = "0x12345678_87654321";
		assertEqual(x.getDigitCount, 2);
		x.extend(4);
		assertEqual(x.getDigitLength, 4);
		assertEqual(x.getDigit(0), 0);
		assertEqual(x.getDigit(1), 0);
		assertEqual(x.getDigit(2), 0x12345678);
		assertEqual(x.getDigit(3), 0x87654321);
		x.reduce;
		assertEqual(x.getDigit(0), 0x12345678);
		assertEqual(x.getDigit(1), 0x87654321);
		writeln("passed");
	}

	public static xint min(const xint a, const xint b) {
		return b > a ? b.dup : a.dup;
	}

/*	/// Returns a copy of the entire array of uint values, unmodified
	public const uint[] toArray()
	{
		return digits.dup;
	}

	unittest {
		write("to/from array...");
		xint a = 27;
		xint b = xint([0xFFFF, 0xFF00, 0x00FF]);
		writeln("test missing!!");
	}*/

}	// end struct ExtendedInt

unittest {
	writeln("==========================");
	writeln("extended integers......end");
	writeln("==========================");
}

