// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.integer.fixed;

import std.ascii: isDigit, isHexDigit;
import std.conv: ConvException;
import std.format;
import std.string: startsWith, strip, toLower;

import eris.integer.digits;
import eris.integer.exception;

unittest {
	writeln("==========================");
	writeln("fixed int module.....begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;

	alias uint128 = FixedInt!(128, false);
	alias int128  = FixedInt!(128, true);
	alias cint64  = FixedInt!( 64, true, true);
}

public static const ulong BASE = 1UL << 32;

public struct FixedInt(uint BITS, bool SIGNED = false, bool CHECKED = false) {

alias fixed = FixedInt!(BITS,SIGNED,CHECKED);
private enum bool UNSIGNED  = !SIGNED;
private enum bool UNCHECKED = !CHECKED;

unittest {
	writeln("==========================");
	if (CHECKED) {
		writefln("cint%d...............begin", BITS);
	} else if (SIGNED) {
		writefln("int%d...............begin", BITS);
	} else {
		writefln("uint%d..............begin", BITS);
	}
	writeln("==========================");
}

//--------------------------------
// structure
//--------------------------------

	// The number of whole uint digits in a fixed-size integer
	private static const uint N = BITS/32;

	// TODO: allow arbitrary lengths
//	// The number of additional bits in a fixed-size integer
//	private static const uint M = BITS%32;

	// Array of the uint digits of a fixed-size integer
	// The digits are stored in increasing value:
	//   least significant digit is digits[0],
	//   most significiant digit is digits[N-1]
	private uint[N] digits = 0;

	/// Marker used to identify this type irrespective of size.
	private static immutable bool IS_FIXED_INTEGER = true;

//--------------------------------
// construction
//--------------------------------

	/// Private constructor for internal use.
	/// Constructs a fixed-size integer from an array of
	/// unsigned integer values. In the internal array
	/// digits[0] is the least significant digit while
	/// digits[$-1] is most significant digit. This
	/// is the reverse of the public constructor and
	/// of the get/set digit and word methods.
	private this(T:uint[])(const T digits) {
		uint len = digits.length >= N ? N : digits.length;
		for (int i = 0; i < len; i++)
			this.digits[i] = digits[i];
	}

	unittest {	// construction
		write("-- this(uint[]).....");
		static if (UNSIGNED)	{
			uint128 num;
			num = uint128([1u,2u,3u,4u]);
			assertEqual(num.size, 4);
			uint first = num.getDigit(0);
			assertEqual(first, 4);
			uint last = num.getDigit(3);
			assertEqual(last, 1);
		}
		static if (SIGNED && UNCHECKED) {
			int128 num;
			num = int128([1u,2u,3u,4u]);
			assertEqual(num.size, 4);
			uint first = num.getDigit(0);
			assertEqual(first, 4);
			uint last = num.getDigit(3);
			assertEqual(last, 1);
		}
		static if (CHECKED) {
			cint64 num;
			num = cint64([1u,2u]);
			assertEqual(num.size, 2);
			uint first = num.getDigit(0);
			assertEqual(first, 2);
			uint last = num.getDigit(1);
			assertEqual(last, 1);
		}
		writeln("passed");
	}

	/// Constructs a fixed-size integer from a signed long integer.
	public this(T)(const T num)
			if (__traits(isIntegral, T) && !__traits(isUnsigned, T)) {
		long value = cast(long)num;
		digits[0] = low(value);
		digits[1] = high(value);
		if (digits[1] > int.max) {
			for (int i = 2; i < N; i++) {
				digits[i] = uint.max;
			}
		}
	}

	/// Constructs a fixed-size integer from a boolean value.
	public this(T:bool)(const T value) {
		this(cast(long)value);
	}

	// TODO: move most of these test cases to the test module.
	unittest {	// construction
	static if (SIGNED && UNCHECKED) {
		write("-- this(long).......");

		long longValue;
		longValue = long.max;
		assertStringEqual(int128(longValue), longValue);
		longValue = int.max;
		assertStringEqual(int128(longValue), longValue);
		longValue = 1;
		assertStringEqual(int128(longValue), longValue);
		longValue = 0;
		assertStringEqual(int128(longValue), longValue);
		longValue = -1;
		assertStringEqual(int128(longValue), longValue);
		longValue = int.min;
		assertStringEqual(int128(longValue), longValue);
		longValue = long.min;
		assertStringEqual(int128(longValue), longValue);

		ulong ulongValue;
		ulongValue = 1;
		assertStringEqual(int128(ulongValue), ulongValue);
		ulongValue = 0;
		assertStringEqual(int128(ulongValue), ulongValue);
		ulongValue = -1;
		assertStringEqual(int128(ulongValue), ulongValue);

		int intValue;
		intValue = int.max;
		assertStringEqual(int128(intValue), intValue);
		intValue = int.min;
		assertStringEqual(int128(intValue), intValue);

		uint uintValue;
		uintValue = -1;
		assertStringEqual(int128(uintValue), uintValue);
		uintValue = uint.min;
		assertStringEqual(int128(uintValue), uintValue);

		short shortValue;
		shortValue = short.max;
		assertStringEqual(int128(shortValue), shortValue);
		shortValue = short.min;
		assertStringEqual(int128(shortValue), shortValue);

		byte byteValue;
		byteValue = byte.max;
		assertStringEqual(int128(byteValue), byteValue);
		byteValue = byte.min;
		assertStringEqual(int128(byteValue), byteValue);

		bool boolValue;
		boolValue = bool.max;
		assertStringEqual(int128(boolValue), boolValue);
		boolValue = bool.min;
		assertStringEqual(int128(boolValue), boolValue);

		writeln("passed");
	}}

	/// Constructs a fixed-size integer from an unsigned long integer.
	public this(T)(const T num)
			if (__traits(isIntegral, T) && __traits(isUnsigned, T)) {
		ulong value = cast(ulong)num;
		digits[0] = low(value);
		digits[1] = high(value);
	}

	// TODO: move most of these test cases to the test module.
	unittest {	// construction
	static if (UNSIGNED && UNCHECKED) {
 		write("-- this(ulong)......");

		ulong ulongValue;
		ulongValue = ulong.max;
		assertStringEqual(uint128(ulongValue), ulongValue);
		ulongValue = uint.max;
		assertStringEqual(uint128(ulongValue), ulongValue);
		ulongValue = 1;
		assertStringEqual(uint128(ulongValue), ulongValue);
		ulongValue = 0;
		assertStringEqual(uint128(ulongValue), ulongValue);
		ulongValue = -1;
		assertStringEqual(uint128(ulongValue), ulongValue);
		ulongValue = uint.min;
		assertStringEqual(uint128(ulongValue), ulongValue);
		ulongValue = ulong.min;
		assertStringEqual(uint128(ulongValue), ulongValue);

		long longValue = -1;
		assertStringEqual(uint128(longValue), uint128([-1u, -1u, -1u, -1u]));

		uint uintValue;
		uintValue = -1;
		assertStringEqual(uint128(uintValue), uintValue);
		uintValue = uint.min;
		assertStringEqual(uint128(uintValue), uintValue);

		int intValue = -1;
		assertStringEqual(uint128(intValue), uint128([-1u, -1u, -1u, -1u]));

		ushort ushortValue;
		ushortValue = ushort.max;
		assertStringEqual(uint128(ushortValue), ushortValue);
		ushortValue = ushort.min;
		assertStringEqual(uint128(ushortValue), ushortValue);

		ubyte ubyteValue;
		ubyteValue = ubyte.max;
		assertStringEqual(uint128(ubyteValue), ubyteValue);
		ubyteValue = ubyte.min;
		assertStringEqual(uint128(ubyteValue), ubyteValue);

		bool boolValue;
		boolValue = bool.max;
		assertStringEqual(uint128(boolValue), boolValue);
		boolValue = bool.min;
		assertStringEqual(uint128(boolValue), boolValue);

		writeln("passed");
	}}

 	/// Constructs a fixed-size integer from a fixed-size integer
	/// of a different size.
	public this(T)(const T that)
			if (__traits(hasMember, that, "IS_FIXED_INTEGER")) {
		this(that.digits);
	}

	unittest {
		write("-- mixed............");
		uint128 unum;
		unum = 5;
		int128 snum;
		snum = 17;
		unum *= snum;
		assertEqual(unum, uint128(85));
		snum = unum * -snum;
		assertEqual(snum, int128(-1445));
		writeln("passed");
	}


//--------------------------------
// copy
//--------------------------------

	/// Copy constructor.
	public this(T:fixed)(const T that) {
		this.digits = that.digits;
	}

	/// Returns a copy of a fixed-size integer.
	public const fixed dup() {
		return fixed(this);
	}

	unittest {	// copy
	static if (UNCHECKED) {
		write("-- copying..........");
		static if (UNSIGNED)	{
			int128 num = int128(2715);
			uint128 unum = uint128(9305);
			assertEqual(uint128(unum), unum);
			assertEqual(unum.dup, unum);
		}
		static if (SIGNED) {
			int128 snum = int128(-9305);
			assertEqual(int128(snum), snum);
			assertEqual(snum.dup, snum);
		}
		writeln("passed");
	}}

//--------------------------------
// byte operations
//--------------------------------

	public const ubyte[] toByteArray() {
		uint index = 0;
		ubyte[] buffer = new ubyte[digits.length * 4];
		foreach (uint digit; digits) {
			std.bitmanip.write!uint(buffer, digit, &index);
		}
		return buffer;
	}

	/// Constructs a fixed integer from an array of bytes.
	public this(T:ubyte[])(T buffer) {
		int n = buffer.length / 4;
		int m = buffer.length % 4;
		if (m != 0)	{
			n++;
			buffer.length = 4 * n;
		}
		uint[] array = new uint[n];
		foreach (ref uint digit; array) {
			digit = std.bitmanip.read!uint(buffer);
		}
		this(array);
	}

	unittest {
	static if (UNSIGNED && UNCHECKED) {
		write("-- byte array.......");
		uint128 before = 21354577;
		ubyte[] bytes = before.toByteArray;
		uint128 after = uint128(bytes);
		assertEqual(before,after);
		writeln("passed");
	}}

//--------------------------------
// digit manipulation
//--------------------------------

	/// Returns the number of significant digits in this fixed-size integer.
	/// I.e., leading zeros are not counted.
	public const uint sigDigits() {
		return numDigits(digits);
	}

	/// Returns the value of the nth digit in the fixed-size integer.
	/// getDigit(0) returns the most significant digit.
	/// getDigit(size-1) returns the least significant digit.
	public const uint getDigit(uint n) {
        int i = N-n-1;
		return cast(uint)digits[i];
	}

	/// Sets the nth digit in the fixed-size integer to the specified value.
	/// setDigit(size-1, value) sets the value of the least significant digit.
	public void setDigit(uint n, uint value) {
        int i = N-n-1;
		digits[i] = value;
	}

	unittest {
	static if (UNSIGNED && UNCHECKED) {
		write("-- digits...........");
		uint128 num;
		num = uint128([1u,2u,3u,4u]);
		assertEqual(num.size, 4);
		assertEqual(num.sigDigits, 4);
		uint first = num.getDigit(0);
		assertEqual(first, 4);
		uint last = num.getDigit(3);
		assertEqual(last, 1);
		writeln("passed");
	}}

//--------------------------------
// bit manipulation
//--------------------------------

	import std.bitmanip: BitArray;

	// Copies the digit array to a std.bitmanip.BitArray and returns
	// the bit array.
	public static BitArray toBitArray(fixed a) {
		digit[] copy = a.digits.dup;
        BitArray ba;
		ba.init(cast(void[])copy, copy.length*32);
		return ba;
	}

	// TODO: add conversion of bit array to byte array and thence to fixed.

	/// Sets a single bit in a fixed-size integer.
	public void setBit(int n, bool value = true) {
		if (value) {
			this |= shl(ONE, n);
		} else {
			this &= (shl(ONE, n).complement);
		}
	}

	/// Tests a single bit in a fixed-size integer.
	public const bool testBit(int n) {
		fixed value = this & shl(ONE, n);
		return cast(bool)value;
	}

	/// Flips a single bit in a fixed-size integer.
	public void flipBit(int n) {
		fixed mask = shl(ONE, n);
		fixed value = this & mask;
		if (value) {
			this &= mask.complement;
		} else {
			this |= mask;
		}
	}

	unittest {	// bit manipulation
	static if (UNSIGNED && UNCHECKED) {
		write("-- bits.............");
		uint128 test = uint128(1234567);
		assertFalse(test.testBit(5));
		test.setBit(5);
		assertTrue(test.testBit(5));
		test.flipBit(5);
		assertFalse(test.testBit(5));
		assertEqual(test, 1234567);
		writeln("passed");
	}}

//--------------------------------
// constants
//--------------------------------

	private enum uint[N-2] ONES = uint.max;
	private enum uint[N-2] ZEROS = 0u;

	public enum fixed ZERO = fixed([0u]);
	public enum fixed ONE  = fixed([1u]);

	static if (UNSIGNED) {
		public enum fixed MAX = fixed(ONES  ~ [uint.max, uint.max]);
		public enum fixed MIN = ZERO;
	}
	static if (SIGNED) {
		public enum fixed MAX = fixed(ONES  ~ [uint.max, int.max]);
		public enum fixed MIN = fixed(ZEROS ~ [0u, int.min]);
	}

	public enum fixed UINT_MAX  = fixed(uint.max);
	public enum fixed UINT_MIN  = ZERO;
	public enum fixed ULONG_MAX = fixed(ulong.max);
	public enum fixed ULONG_MIN = ZERO;

	public enum fixed INT_MAX   = fixed(int.max);
	public enum fixed INT_MIN   = fixed([int.min, uint.max] ~ ONES);
	public enum fixed LONG_MAX  = fixed(long.max);
	public enum fixed LONG_MIN  = fixed([0u, int.min] ~ ONES);

	// min/max properties
   	/// Returns the maximum value for this type.
	/// For signed fixed-size integers the maximum value is 0x7F...FF.
	/// For unsigned fixed-size integers the maximum value is 0xFF...FF.
	@property
	public static fixed max() {
		return MAX;
	}

	/// Returns the minimum value for this type.
	/// For signed fixed-size integers the minimum value is 0x80...00.
	/// For unsigned fixed-size integers the maximum value is 0x00...00.
	@property
	public static fixed min() {
		return MIN;
	}

	unittest {	// min/max values
		write("-- min/max values...");
		static if (UNCHECKED && UNSIGNED) {
			assertEqual(uint128.max,
				uint128("0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF"));
			assertEqual(uint128.min,
				uint128("0x00000000_00000000_00000000_00000000"));
		}
		static if (UNCHECKED && SIGNED) {
			assertEqual(int128.max,
				int128("0x7FFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF"));
			assertEqual(int128.min,
				int128("0x80000000_00000000_00000000_00000000"));
		}
		static if (CHECKED) {
			assertEqual(cint64.max,
				cint64("0x7FFFFFFF_FFFFFFFF"));
			assertEqual(cint64.min,
				cint64("0x80000000_00000000"));
		}
		writeln("passed");
	}

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the this fixed-size integer is a signed integer.
	public const bool isSigned() {
		return SIGNED;
	}

	/// Returns true if the value of the fixed-size integer is zero.
	public const bool isZero() {
		return this.sigDigits == 0;
//		return numDigits(this.digits) == 0;
	}

	/// Returns true if the value of the fixed-size integer is less than zero.
	/// For unsigned fixed-size integers the return value is always false.
	public const bool isNegative()
	{
		static if (SIGNED) {
			return cast(int)digits[N-1] < 0;
		} else {
			return false;
		}
	}

	/// Returns true if the value of the fixed-size integer is odd.
	public const bool isOdd() {
		return digits[0] & 1;
	}

	/// Returns true if the value of the fixed-size integer is even.
	public const bool isEven() {
		return !isOdd();
	}

	unittest {	// classification
	static if (UNCHECKED) {
		write("-- classification...");
		static if (UNSIGNED) {
			uint128 unum = uint128(0);
			assertTrue(unum.isZero);
			assertFalse(unum.isNegative);
			assertTrue(unum.isEven);
			unum = uint128(7);
			assertFalse(unum.isZero);
			assertFalse(unum.isNegative);
			assertTrue(unum.isOdd);
		}
		static if (SIGNED) {
			int128 snum = int128(0);
			assertTrue(snum.isZero);
			assertFalse(snum.isNegative);
			assertTrue(snum.isEven);
			snum = int128(-7);
			assertFalse(snum.isZero);
			assertTrue(snum.isNegative);
			assertTrue(snum.isOdd);
		}
		writeln("passed");
	}}

//--------------------------------
// properties
//--------------------------------

	/// Returns the size (number of uint digits) of the fixed-size integer type.
	public static uint size() {
		return N;
// TODO: return M == 0 ? N : N+1;
	}

	/// Returns the size (number of bits) of the fixed-size integer type.
	@property
	public static uint bits() {
//		return N * 32 + M;
		return BITS;
	}

	/// Returns true if the fixed-size integer is signed.
	@property
	public static bool signed() {
		return SIGNED;
	}

	/// Returns true if the fixed-size integer is checked.
	@property
	public static bool checked() {
		return CHECKED;
	}


	/// Returns zero, the initial value for fixed-size integer types.
	@property
	public static fixed init() {
		return ZERO;
	}

//--------------------------------
// conversion to/from strings
//--------------------------------

	/// Constructs a fixed-size integer from a string.
	public this(T:string)(const T str) {
		auto digs = new uint[N];
		char[] chars = strip(str.dup);
		bool signed = false;
		// if the string starts withs a non-zero digit,
		// it should be a decimal number.
		if (chars[0] != '0') {
			parseDecimal(chars, signed);
		}
writefln("chars = %s", chars);
		// Otherwise it may be hex or binary
		chars = toLower(chars);
		if (startsWith(chars, "0x")) {
			digs = parseHex(chars[2..$].dup);
		} else if (startsWith(chars, "0b")) {
			digs = parseBinary(chars[2..$]);
		} else {
			digs = parseDecimal(chars, signed);
		}
		// Note that signed argument is valid
		// for unsigned integers.
		this = signed ? -this(digs) : this(digs);
	}

	private static uint[] parseHex(char[] chars) {
		auto value = new uint[N];
		uint charValue = 0;
		foreach (char ch; chars) {
			if (ch == '_') continue;
			if (!isHexDigit(ch)) {
			throw new ConvException("Invalid hexadecimal char: [" ~ ch ~ "]");
			}
			value = mulDigit(value, 16);
			if (isDigit(ch)) {
				charValue = ch - '0';
			} else {
				charValue = ch - 'a' + 10;
			}
			value = addDigit(value, charValue);
		}
		return value;
	}

	private static uint[] parseBinary(/*ref*/ char[] chars) {
		auto value = new uint[N];
		uint charValue = 0;
		foreach (char ch; chars) {
			if (ch == '_') continue;
			if (ch != '0' && ch != '1') throw
				new ConvException("Invalid binary char: [" ~ ch ~ "]");
			value = mulDigit(value, 2);

		}
		return value;
	}

	private static uint[] parseDecimal(char[] chars, out bool signed) {
		signed = false;
		auto digits = new uint[N];
		if (SIGNED && (chars[0] == '-')) {
			signed = true;
			chars = chars[1..$];
		}
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

	unittest {
		write("-- this(string).....");
		string str;
		uint128 unum = 123;
		assertEqual(uint128("123"), unum);
		assertEqual(uint128("0x7B"), unum);
		assertEqual(uint128("0b1111011"), unum);
		assertEqual(uint128("0_234_445"), 234445);
		int128 snum = -123;
		assertEqual(int128("-123"), snum);
		assertEqual(int128("0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFF85"), snum);
		assertEqual(int128("0b11111111111111111111111111111111"
			"_11111111111111111111111111111111"
			"_11111111111111111111111111111111"
			"_11111111111111111111111110000101"), snum);
		assertEqual(int128("0_234_445"), 234445);
		writeln("passed");
	}

	public const string toString(string fmt = "%s") {
		import std.exception : assumeUnique;
		char[] buf;
		buf.reserve(100);
		toString((const(char)[] s) { buf ~= s; }, fmt);
		return assumeUnique(buf);
/*    	auto a = std.array.appender!(const(char)[])();
		void sink(const(char)[] s) {
			a.put(s);
		char ch = fmt[$-1];
//		writefln("ch = %s", ch);
		switch (ch) {
			case 's':
			case 'd': return toDecimalString();
			case 'x': return toHexString();
			case 'b': return toBinaryString();
			default: throw new Exception("format not recognized");*/
//		}
	}


	public const void toString(scope void delegate(const(char)[]) sink,
		string formatString) {

		auto f = singleSpec!char(formatString.dup);
/*writefln("sink = %s", sink);
writefln("f.width = %s", f.width);	// min width. if negative, left justify
writefln("f.spec = %s", f.spec);		// b,d,o?,x,X
writefln("f.flDash = %s", f.flDash);	// left justify
writefln("f.flZero = %s", f.flZero);	// pad with zeros
writefln("f.flSpace = %s", f.flSpace);	// prefix positive #s with a space
writefln("f.flPlus = %s", f.flPlus);    // prefix positive #s with a '+'
writefln("f.flHash = %s", f.flHash);    // if not zero, prefix with 0x, 0X, etc.*/
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

	unittest {
		write("formatting...");

		int n;
		fixed num = fixed();
    	auto a = std.array.appender!(const(char)[])();
		void sink(const(char)[] s) {
			a.put(s);
		}
		writeln("test missing");
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

	/// Converts this fixed-size integer to a decimal string.
	public const string toDecimalString(bool addSign = true) {

		char[] str;
		bool sign = this.isNegative;
		uint[] from = sign ? negate.digits : dup.digits;
		uint n = numDigits(from);
		if (n == 0) return "0";
		while (n > 0) {
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

	/// Converts this fixed-size integer to a hexadecimal string.
	public const string toHexString() {
		char[] str;
		int n = sigDigits;
		if (n == 0) return ("0x00000000");
		for (int i = 0; i < n; i++) {
			str = std.string.format("_%08X", digits[i]) ~ str;
		}
		return "0x" ~ str[1..$].idup;
	}

	/// Converts this fixed-size integer to a binary string.
	public const string toBinaryString() {
		char[] str;
		int n = sigDigits;
		if (n == 0) return ("0b00000000");
		for (int i = 0; i < n; i++) {
			uint digit = digits[i];
			for (int j = 0; j < 4; j++ ) {
				uint chunk = digit % 256;
				str = std.string.format("_%08b", chunk) ~ str;
				digit /= 256;
			}
		}
		return "0b" ~ str[1..$].idup;
	}

	unittest // toString
	{
		write("-- toString.........");
		uint128 unum;
		unum = uint128(11);
		assertStringEqual(unum, "11");
		unum = uint128(1234567890123);
		assertStringEqual(unum, "1234567890123");
		unum = uint128(0x4872EACF123346FFU);
		assertEqual(unum.toString("%x"), "0x4872EACF_123346FF");
		assertEqual(unum.toString("%b"), "0b01001000_01110010_11101010"
			"_11001111_00010010_00110011_01000110_11111111");
		int128 snum;
		snum = int128(-156);
		assertStringEqual(snum, "-156");
		assertEqual(snum.toHexString, "0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFF64");
		assertEqual(snum.toBinaryString, "0b11111111_11111111_11111111_"
			"11111111_11111111_11111111_11111111_11111111_11111111_"
			"11111111_11111111_11111111_11111111_11111111_11111111_01100100");
		writeln("passed");
	}

	unittest {
		writeln("isFixedInt...");
		uint128 num;
		int128 snum;
		FixedInt!(256) dnum;
		dnum = 1234;
//writefln("dnum = %s", dnum);
		num = uint128(dnum);
//writefln("num = %s", num);
		num = uint128(5);
		snum = int128(-17);
		num = uint128(snum);
//writefln("num = %s", num);
		cint64 cnum;
		num = long.max;
		num *= 3;
//writefln("num = %s", num.toHexString);
		cnum = cint64(num);
//writefln("cnum = %s", cnum.toHexString);
		writeln("test missing");
	}

//--------------------------------
// conversion to integral
//--------------------------------

	/// Returns true if the value of this fixed-size integer
	/// is a valid unsigned integer value.
	public const bool isValidUint() {
		return this <= UINT_MAX;
	}

	/// Returns an unsigned integer with the value of this fixed-size integer.
	/// If the value is too large to be represented uint.max is returned.
	public const uint toUint() {
		return isValidUint ? cast(uint)digits[0] : uint.max;
	}

	/// Returns true if the value of this fixed-size integer
	/// is a valid unsigned long integer value.
	public const bool isValidUlong() {
		return this <= ULONG_MAX;
	}

	/// Returns an unsigned long integer with the value of this fixed-size integer.
	/// If the value is too large to be represented ulong.max is returned.
	public const ulong toUlong() {
		// if too big to represent...
		if (this > ULONG_MAX) return ulong.max;
		// if single digit...
		if (this <= UINT_MAX) return cast(ulong)digits[0];
		// else two digits...
		return pack(digits[1],digits[0]);
	}

	/// Returns true if the value of this fixed-size integer
	/// is a valid integer value.
	public const bool isValidInt() {
		return this <= INT_MAX && this >= INT_MIN;
	}

	/// Returns a signed integer with the value of this fixed-size integer.
	/// If the value is too large to be represented int.max/min is returned.
	public const int toInt() {
		if (this > INT_MAX) return int.max;
		if (this < INT_MIN) return int.min;
		return cast(int)digits[0];
	}

	/// Returns true if the value of this fixed-size integer
	/// is a valid long integer value.
	public const bool isValidLong() {
		return this <= LONG_MAX && this >= LONG_MIN;
	}

	/// Returns a signed long integer with the value of this fixed-size integer.
	/// If the value is too large to be represented long.max is returned.
	public const long toLong() {
		// if too big to represent...
		if (this > LONG_MAX) return long.max;
		if (this < LONG_MIN) return long.min;
		// if single digit...
		if (this <= UINT_MAX) return cast(long)digits[0];
		// else two digits...
		return cast(long)pack(digits[1],digits[0]);
	}

	unittest {	// conversion
		write("-- to integral......");
		static if (UNSIGNED) {
		uint128 unum;
		unum = uint128(8754);
		assertTrue(unum.isValidUint);
		assertEqual(unum.toUint, 8754);
		unum = uint128(9100);
		assertTrue(unum.isValidUint);
		assertEqual(unum.toUlong, 9100U);
		unum = uint128([2u,2u]);
		assertFalse(unum.isValidUint);
		assertTrue(unum.isValidUlong);
		assertEqual(unum.toUlong, 0x00000002_00000002);
		unum = uint128([2u,2u, 2u]);
		assertFalse(unum.isValidUint);
		assertFalse(unum.isValidUlong);
		assertEqual(unum.toUlong, ULONG_MAX);
		}

		static if (SIGNED && UNCHECKED) {
		int128 num;
		num = int128(-8754);
		assertNotGreaterThan(num, INT_MAX);
		assertNotLessThan(num, INT_MIN);
		assertTrue(num.isValidInt);
		assertEqual(num.toInt, -8754);
		num = int128(9100);
		assertTrue(num.isValidInt);
		assertTrue(num.isValidLong);
		assertEqual(num.toLong, 9100U);
		num = int128([2u,2u]);
		assertFalse(num.isValidInt);
		assertTrue(num.isValidLong);
		assertEqual(num.toLong, 0x00000002_00000002);
		num = int128([2u,2u,2u]);
		assertFalse(num.isValidInt);
		assertFalse(num.isValidLong);
		assertEqual(num.toLong, LONG_MAX);
		assertEqual(num.toInt, INT_MAX);
		num = num.negate;
		assertEqual(num.toLong, LONG_MIN);
		assertEqual(num.toInt, INT_MIN);
		assertTrue(cast(bool)num);
		}

		static if (CHECKED) {
		// TODO: test with int128, other types...
		cint64 num = -8754;
		assertNotGreaterThan(num, int.max);
		assertNotLessThan(num, int.min);
		assertNotGreaterThan(num, INT_MAX);
		assertNotLessThan(num, INT_MIN);
		assertTrue(num.isValidInt);
		assertEqual(num.toInt, -8754);
		int128 n2 = int128(-8754);
		assertTrue(n2 <= int128.INT_MAX);
		assertTrue(n2 >= int128.INT_MIN);
		assertTrue(n2.isValidInt);
		assertEqual(n2.toInt, -8754);
		}
		writeln("passed");
	}

//--------------------------------
// opCast
//--------------------------------

	/// Casts this fixed-size integer to a boolean value.
    T opCast(T:bool)() {
        return !isZero();
    }

	/// Casts this fixed-size integer to an unsigned integer.
    T opCast(T:uint)() {
		return digits[0];
	}

	/// Casts this fixed-size integer to a signed integer.
    T opCast(T:int)() {
		return cast(int)digits[0];
	}

	/// Casts this fixed-size integer to an unsigned long integer.
    T opCast(T:ulong)() {
		// if num is single digit return the digit...
		if (sigDigits < 2) return cast(ulong)digits[0];
		// else return last two digits...
		return pack(digits[1],digits[0]);
	}

	/// Casts this fixed-size integer to an signed long integer.
    T opCast(T:long)() {
		// if num is single digit return the digit...
		if (sigDigits < 2) return cast(long)digits[0];
		// else return last two digits...
		return cast(long)pack(digits[1],digits[0]);
	}

	unittest {
		write("opCast...");
		writeln("test missing");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this fixed-size integer is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T:fixed)(const T that) {
		static if (SIGNED) {
			bool negThis = this.isNegative;
			bool negThat = that.isNegative;
			if (negThis && !negThat) return -1;
			if (negThat && !negThis) return +1;
		}
		return compareDigits(this.digits, that.digits);
	}

	/// Returns -1, 0, or 1, if this fixed-size integer is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T)(const T that) {
		return opCmp(fixed(that));
	}

	 /// Returns true if this fixed-size integer is equal to the argument.
	private const bool opEquals(T:fixed)(const T that) {
		return this.digits == that.digits;
	}

	 /// Returns true if this fixed-size integer is equal to the argument.
	private const bool opEquals(T:string)(const T that){
		return opEquals(fixed(that));
	}

	 /// Returns true if this fixed-size integer is equal to the argument.
	private const bool opEquals(T:bool)(const T that){
		return opEquals(fixed(that));
	}

	/// Constructs a fixed-size integer from a signed long value.
	private const bool opEquals(T)(const T that)
			if (__traits(isIntegral, T) && !__traits(isUnsigned, T)) {
		return opEquals(fixed(cast(long)that));
	}

	/// Constructs a fixed-size integer from a signed long value.
	private const bool opEquals(T)(const T that)
			if (__traits(isIntegral, T) && __traits(isUnsigned, T)) {
		return opEquals(fixed(cast(ulong)that));
	}

	unittest { // comparison
		static if (UNCHECKED) {
		write("-- comparison.......");
		// TODO: fix naked asserts
		static if (UNSIGNED) {
			assertLessThan(uint128(5), uint128(6));
			assertLessThan(uint128(5), 6);
			assertLessThan(uint128(3), uint128(10));
			assert(uint128(195) >= uint128(195));
			assert(uint128(195) >= 195);
		}
		static if (SIGNED) {
			assertLessThan(int128(5), int128(6));
			assertLessThan(int128(5), 6);
			assertLessThan(int128(3), int128(10));
			assert(int128(195) >= int128(195));
			assert(int128(195) >= 195);

			assertGreaterThan(int128(-5), int128(-6));
			assert(int128(-5) < int128(6));
			assert(int128(3) > int128(-10));
			assert(int128(10) > int128(-3));
			assert(int128(195) >= int128(195));
			assert(int128(195) >= -195);
			assert(int128(2) >= int.min);

			assertGreaterThan(INT_MIN, LONG_MIN);
		}
		writeln("passed");
	}}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a fixed-size integer value to this.
	private void opAssign(T:fixed)(const T that) {
		this.digits = that.digits;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(fixed(that));
	}

	unittest {	// assignment
		write("-- opAssign.........");
		uint128 actual = uint128(123);
		uint128 expect = actual;
		assertEqual(actual, expect);
		actual = 123L;
		assertEqual(actual, expect);
		actual = "123";
		assertEqual(actual, expect);
		writeln("passed");
	}

	/// Performs an operation on this and assigns the result to this.
	private ref fixed opOpAssign(string op, T:fixed)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref fixed opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		static if (UNSIGNED) {
		write("-- opOpAssign.......");
		uint128 u1, u2;
		u1 = 8; u2 = 3;
		// test subtraction
		u1 -= u2;
		assertEqual(u1, 5);
		// test multiplication
		u1 *= u2;
		assertEqual(u1, 15);
		// test division
		u2 = 2;
		u1 /= u2;
		assertEqual(u1, 7);
		u1 %= u2;
		assertEqual(u1, 1);
		// test power function
		u1 = 5;
		u1 ^^= 2;
		assertEqual(u1, 25);
		// test logical operations
		u1 = 10101; u2 = 10001;
		assertEqual((u1 & u2), 10001);
		assertEqual((u1 | u2), 10101);
		assertEqual((u1 ^ u2), 100);
		// test left and right shifts
		u2 = 2;
		assertEqual(u1 << u2, 40404);
		assertEqual(u1 >> u2, 2525);
		u1 = 4; u2 = uint128([0u,1u]);
		assertEqual(u1 + u2, 0x100000004);
		writeln("passed");
	}}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private fixed opUnary(string op)() {
		static if (op == "+") {
			return plus();
		} else static if (op == "-") {
			return negate();
		} else static if (op == "~") {
			return complement();
		}else static if (op == "++") {
			this = add(this, ONE);
			return this;
		} else static if (op == "--") {
			this = sub(this, ONE);
			return this;
		}
	}

	/// Returns a copy of this fixed-size integer
	public const fixed plus() {
		return fixed(this.digits);
	}

	/// Returns the ones complement of this fixed-size integer
	public const fixed complement() {
		auto comp = compDigits(digits);
		return fixed(comp);
	}

	/// Returns the twos complement of this fixed-size integer
	public const fixed negate() {
		if (isZero) return ZERO;
		auto neg = negateDigits(digits);
		return fixed(neg);
	}

	unittest {	// opUnary
		static if (UNCHECKED) {
		write("-- opUnary..........");
		static if (UNSIGNED) {
			uint128 op1 = 4;
			assertEqual(+op1, op1);
			assertEqual(-(-op1), uint128(4));
			assertEqual(~(~op1), uint128(4));
			assertEqual(++op1, uint128(5));
			assertEqual(--op1, uint128(4));
			uint128 op2 = uint128(0x000011111100UL);
			assertEqual(~op2, "0xFFFFFFFF_FFFFFFFF_FFFFFFFF_EEEEEEFF");
			assertEqual(-op2, "0xFFFFFFFF_FFFFFFFF_FFFFFFFFEEEEEF00");
		}
		static if (SIGNED) {
			int128 snum = -4;
			assertEqual(+snum, snum);
			assertEqual(-(-snum), int128(-4));
			assertEqual(~(~snum), int128(-4));
			assertEqual(++snum, int128(-3));
			assertEqual(--snum, int128(-4));
			snum = int128(0x000011111100UL);
			assertEqual(~snum, "0xFFFFFFFF_FFFFFFFF_FFFFFFFF_EEEEEEFF");
			assertEqual(-snum, "0xFFFFFFFF_FFFFFFFF_FFFFFFFFEEEEEF00");
		}
		writeln("passed");
	}}

//--------------------------------
// binary operations
//--------------------------------

	private const fixed opBinary(string op, T:fixed)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {
			return sub(this, that);
		} else static if (op == "*") {
			return mul(this, that);
		} else static if (op == "/") {
			return div(this, that);
		} else static if (op == "%") {
			return mod(this, that);
		} else static if (op == "^^") {
			return pow(this, that);
		} else static if (op == "&") {
			return and(this, that);
		} else static if (op == "|") {
			return or(this, that);
		} else static if (op == "^") {
			return xor(this, that);
		} else static if (op == "<<") {
			return shl(this, that);
		} else static if (op == ">>") {
			return shr(this, that);
		} else static if (op == ">>>") {
			return lshr(this, that);
		}
	}

	private const fixed opBinary(string op, T)(const T that) {
		return opBinary!(op, fixed)(fixed(that));
	}

	unittest {	// opBinary
		write("-- opBinary.........");
		uint128 u1, u2;
		u1 = 4; u2 = 8;
		// test subtraction
		assertEqual(u2 - u1, 4);
		// test multiplication
		assertEqual(u1 * u2, 32);
		// test division
		u1 = 5; u2 = 2;
		assertEqual(u1 / u2, 2);
		assertEqual(u1 % u2, 1);
		// test power function
		assertEqual(u1 ^^ u2, 25);
		// test logical operations
		u1 = 10101; u2 = 10001;
		assertEqual((u1 & u2), 10001);
		assertEqual((u1 | u2), 10101);
		assertEqual((u1 ^ u2), 100);
		// test left and right shifts
		u2 = 2;
		assertEqual(u1 << u2, 40404);
		assertEqual(u1 >> u2, 2525);
		u1 = 4; u2 = uint128([0u,1u]);
		assertEqual(u1 + u2, 0x100000004);
		writeln("passed");
	}

	static if (CHECKED) {
	/// Returns false if the result did not overflow,
	/// true if the result is too large for the fixed-size integer.)
	public static bool overflowOccurred(uint[] digits) {
		// quick checks
		if (digits.length <= N) return false;
		// many operations return N + 1 digits.
		if (digits.length == N + 1 && digits[N] == 0) return false;
		// if number of significant digits > N, return true.
		int num = numDigits(digits);
		return (num > N);
	}}

	/// Adds two fixed-size integers and returns the sum.
	/// Tests the result for overflow.
	private static fixed add(const fixed x, const fixed y) {
		if (x.isZero) return y.dup;
		if (y.isZero) return x.dup;
		uint[] sum = addDigits(x.digits, y.digits);
		static if (UNCHECKED) {
			return fixed(sum);
		} else {
			if (overflowOccurred(sum)) throw new IntegerOverflowException();
			fixed z = fixed(sum);
			if ((y >= 0 && z < x) || (y <= 0 && z > x)) {
			 	throw new IntegerOverflowException();
			}
			return z;
		}
	}

	unittest {
		write("-- addition.........");
		uint128 u1, u2;
		u1 = 4; u2 = 8;
		assertEqual(u1 + u2, 12);
		u1 = 4; int n1 = 8;
		assertEqual(u1 + n1, 12);
		int128 i1, i2;
		i1 = -4; i2 = -8;
		assertEqual(i1 + i2, -12);
		i1 = -4; i2 = 8;
		assertEqual(i1 + i2, 4);
		i1 = 4; i2 = -8;
		assertEqual(i1 + i2, -4);
		i1 = -4; n1 = -8;
		assertEqual(i1 + n1, -12);
		cint64 c1 = long.max;
		cint64 c2 = long.max;
		assertThrows(c1 + c2);
		writeln("passed");
	}

	/// Subtracts one fixed-size integer from another and returns the difference.
	/// Performs a pre-test for overflow.
	private static fixed sub(const fixed x, const fixed y) {

		// quick check
		if (x == y) return ZERO;

		 // unsigned arithmetic
		static if (UNSIGNED)	{
			bool overflow = (y > x);
			static if (CHECKED) {
				if (overflow) {
					throw new IntegerOverflowException();
				}
			} else {
				uint[] diff;
				if (overflow) {
					diff = subDigits(y.digits, x.digits);
					return fixed(diff).negate;
				} else {
					diff = subDigits(x.digits, y.digits);
					return fixed(diff);
				}
        	}
		}

		// signed arithmetic
		else {
			bool xneg = x.isNegative;
			bool yneg = y.isNegative;

			if (!xneg && !yneg) {	// both positive
				if (x > y) {
					uint[] diff = subDigits(x.digits, y.digits);
					return fixed(diff);
				} else {
					uint[] diff = subDigits(y.digits, x.digits);
					return fixed(diff).negate;
				}
			}

			if (xneg && yneg) {	// both negative
				fixed negx = x.negate;
				fixed negy = y.negate;

				if (negx > negy) {
					uint[] diff = subDigits(negx.digits, negy.digits);
					return fixed(diff).negate;
				} else {
					uint[] diff = subDigits(negy.digits, negx.digits);
					return fixed(diff);
				}
			}

			else {	// signs differ
				if (xneg) {
					return add(x.negate, y).negate;
				} else {
					return add(x, y.negate);
				}
			}
		}
	}

	unittest {
		static if (UNCHECKED) {
		write("-- subtraction......");
		static if (UNSIGNED) {
			uint128 op1, op2;
			op1 = 4; op2 = 8;
			assertEqual(op2 - op1, uint128(4));
			assertEqual(op1 - op2, uint128(-4));
        }
		static if (SIGNED) {
			int128 op3, op4;
			// both pos, x > y
			op3 = 15; op4 = 8;
			assertEqual(op3 - op4, 7);
			// both pos, y > x
			op3 = 4; op4 = 8;
			assertEqual(op3 - op4, -4);
			// both neg, |x| > |y|
			op3 = -11; op4 = -8;
			assertEqual(op3 - op4, -3);
			// both neg, |x| < |y|
			op3 = -9; op4 = -28;
			assertEqual(op3 - op4, 19);
			// signs differ, x pos, x > |y|
			op3 = 15; op4 = -8;
			assertEqual(op3 - op4, 23);
			// signs differ, x pos, x < |y|
			op3 = 5; op4 = -8;
			assertEqual(op3 - op4, 13);
			// signs differ, y pos, |x| > y
			op3 = -24; op4 = 8;
			assertEqual(op3 - op4, -32);
			// signs differ, y pos, |x| < y
			op3 = -4; op4 = 18;
			assertEqual(op3 - op4, -22);
			// interoperability
			op3 = 4; int intOp = 8;
			assertEqual(op3 - intOp, -4);
		}
		writeln("passed");
	}}

	/// Multiplies two fixed-size integers and returns the product.
	/// Tests the result for overflow.
	private static fixed mul(const fixed x, const fixed y) {
		// special cases
		if (x == ZERO || y == ZERO) return ZERO;
		if (y == ONE) return x;
		if (x == ONE) return y;
		static if (SIGNED)
		{
			// special cases
			auto NEG_ONE = -ONE;
			if (x == NEG_ONE) return y.negate;
			if (y == NEG_ONE) return x.negate;
			auto xx = x.dup;
			auto yy = y.dup;
			bool sign = false;
			if (x.isNegative) {
				sign = !sign;
				xx = x.negate;
			}
			if (y.isNegative) {
				sign = !sign;
				yy = y.negate;
			}
			uint[] w = mulDigits(xx.digits, yy.digits);
			static if (CHECKED) {
				if (overflowOccurred(w)) {
					throw new IntegerOverflowException();
				}
			}
			auto product = fixed(w[0..N-1]);
			return sign ? -product : product;
		} else {
			uint[] w = mulDigits(x.digits, y.digits);
			static if (CHECKED) {
				if (overflowOccurred(w)) {
					throw new IntegerOverflowException();
				}
			}
			return fixed(w);
		}
	}

	unittest {
		static if (UNCHECKED) {
		write("-- multiplication...");
		static if (UNSIGNED) {
			uint128 ux = 2;
			uint128 uy = 6;
			assertEqual(ux*uy, uint128(12));
		}
		static if (SIGNED) {
			int128 x = 2;
			int128 y = 6;
			assertEqual(x*y, int128(12));
			x = -x;
			assertEqual(x*y, int128(-12));
			y = -y;
			assertEqual(x*y, int128(12));
			x = -x;
			assertEqual(x*y, int128(-12));
		}
		writeln("passed");
	}}

	/// Divides one fixed-size integer by another
	/// and returns the quotient.
	private static fixed div(const fixed x, const fixed y) {
		if (x.isZero) return ZERO;
		if (x == y) return ONE;
		static if (SIGNED) {
			bool sign = x.isNegative ^ y.isNegative;
			auto xx = x.isNegative ? x.negate : x.dup;
			auto yy = y.isNegative ? y.negate : y.dup;
			auto quotient = fixed(divDigits(xx.digits, yy.digits));
			return sign ? quotient.negate : quotient;
		} else {
			return fixed(divDigits(x.digits, y.digits));
		}
	}

	unittest {
		static if (SIGNED && UNCHECKED) {
		write("-- division.........");
		int128 x = 6;
		int128 y = 2;
		assertEqual(x/y, int128(3));
		x = -x;
		assertEqual(x/y, int128(-3));
		y = -y;
		assertEqual(x/y, int128(3));
		x = -x;
		assertEqual(x/y, int128(-3));
		x = 0;
		assertEqual(x/y, int128(0));
		writeln("passed");
	}}

	/// Divides one fixed-size integer by another
	/// and returns the remainder.
	private static fixed mod(const fixed x, const fixed y) {
		if (x.isZero) return ZERO;
		if (x == y) return ZERO;
		static if (SIGNED) {
			bool sign = x.isNegative ^ y.isNegative;
			auto xx = x.isNegative ? x.negate : x.dup;
			auto yy = y.isNegative ? y.negate : y.dup;
			auto remainder = fixed(modDigits(xx.digits, yy.digits));
			return x.isNegative ? remainder.negate : remainder;
		} else {
			return fixed(modDigits(x.digits, y.digits));
		}
	}

	unittest {
		static if (UNSIGNED && UNCHECKED) {
		write("-- remainder........");
		uint128 ux = 2;
		uint128 uy = 2;
		assertEqual(ux % uy, 0);
		ux = 7;
		uy = 2;
		assertEqual(ux % uy, 1);
		int128 x = 7;
		int128 y = 2;
		assertEqual(x % y, 1);
		x = -x;
		assertEqual(x % y, -1);
		y = -y;
		assertEqual(x % y, -1);
		x = -x;
		assertEqual(x % y, 1);
		writeln("passed");
	}}

	/// Raises a fixed-size integer to an integer power
	/// and returns the result. Tests the result for overflow.
	private static fixed pow(const fixed x, const fixed y) {
		return fixed(pow(x, y.toUint));
	}

	/// Raises a fixed-size integer to an integer power
	/// and returns the result. Tests the result for overflow.
	private static fixed pow(const fixed x, const uint n) {

		if (n < 0) throw new InvalidOperationException();

		// NOTE: 0^^0 == 1.
		if (n == 0) return ONE;
		if (x.isZero) return ZERO;
		if (n >= BITS) throw new IntegerOverflowException();

		uint[] digits = x.digits.dup;
		if (x.isNegative) digits = negateDigits(digits);
		digit[] result = powDigits(digits, n);
		static if (CHECKED) {
			if (overflowOccurred(result)) {
				throw new IntegerOverflowException();
			}
		}
		return fixed(result);
	}

	unittest {
	static if (SIGNED) {
		write("-- exponentiation...");
		static if (UNCHECKED) {
			int128 x, y, z;
			x = 0; y = 0; z = x^^y;
			assertEqual(z, 1);
			x = 0; y = 1; z = x^^y;
			assertEqual(z, 0);
			x = 1; y = 0; z = x^^y;
			assertEqual(z, 1);
			x = 2; y = 8; z = x^^y;
			assertEqual(z, 256);
			x = -2; y = 8; z = x^^y;
			assertEqual(z, 256);
			x = 2; y = 100; z = x^^y;
			assertEqual(z, int128([0u, 0u, 0u, 16u]));
			x = 2; y = 126; z = x^^y;
			assertEqual(z, "85070591730234615865843651857942052864");
		}
		static if (CHECKED) {
			cint64 x, y, z;
			x = 2; y = 60; z = x^^y;
			assertEqual(z, "1152921504606846976");
			x = 100; y = 126;
			assertThrows(x^^y);
		}
		writeln("passed");
	}}

	/// Returns the logical AND of two fixed-size integers
	private static fixed and(const fixed x, const fixed y) {
		return fixed(andDigits(x.digits, y.digits));
	}

	/// Returns the logical OR of two fixed-size integers
	private static fixed or(const fixed x, const fixed y) {
		return fixed(orDigits(x.digits, y.digits));
	}

	/// Returns the logical XOR of two fixed-size integers
	private static fixed xor(const fixed x, const fixed y) {
		return fixed(xorDigits(x.digits, y.digits));
	}

	unittest {
	static if (UNSIGNED && UNCHECKED) {
		write("-- bitwise..........");
		uint128 a, b;
		a = 0x25A9;
		b = 0x03F7;
		assertEqual(a ^ b, 0x265E);
		assertEqual(a & b, 0x01A1);
		assertEqual(a | b, 0x27FF);
		writeln("passed");
	}}

	/// Shifts a fixed-size integer left by an integral value.
	/// No check for overflow is made.
	private static fixed shl(const fixed x, const fixed y) {
/*writefln("\nx = %s", x.toHexString);
writefln("y = %s", y.toHexString);
// FIXTHIS
writefln("y.toInt = %016X", y.toInt);
writefln("y.toUint = %016X", y.toUint);*/
		return shl(x, y.toUint);
	}

	/// Shifts a fixed-size integer left by an integral value.
	/// No check for overflow is made.
	private static fixed shl(const fixed x, const uint n) {
		int nDigs = n / 32;
		int nBits = n % 32;
		uint [] array = x.digits.dup;
		if (nDigs != 0) {
			array = shlDigits(array, nDigs);
		}
		array = shlBits(array, nBits);
		return fixed(array);
	}

	/// Shifts a fixed-size integer right by an integral value.
	private static fixed shr(const fixed x, const fixed y) {
		return shr(x, y.toUint);
	}

	/// Shifts a fixed-size integer right by an integral value.
	private static fixed shr(const fixed x, const uint n) {
		static if (SIGNED) {
			if (n < 0 || n > BITS) throw new InvalidOperationException();
			int digits = n / 32;
			int nBits = n % 32;
			uint [] array = x.digits.dup;
			array = shrDigits(array, digits);
			array = shrBits(array, nBits);
			return fixed(array);
		} else {
			return lshr(x,n);
		}
	}

	/// Shifts a fixed-size integer right by an integral value.
	private static fixed lshr(const fixed x, const fixed y) {
		return lshr(x, y.toUint);
	}

	/// Shifts a fixed-size integer right by an integral value.
	private static fixed lshr(const fixed x, const uint n) {
		if (n < 0 || n > BITS) throw new InvalidOperationException();
		int nd = n / 32;
		int nb = n % 32;
		uint [] array = x.digits.dup;
		if (nd > 0 ) {
			array = lshrDigits(array, nd);
		}
		array = lshrBits(array, nb);
		return fixed(array);
	}

	unittest {
		static if (UNCHECKED) {
		// TODO: add checked tests.
		write("-- shifts...........");
		static if (UNSIGNED) {
			uint128 x, y; int n;
			x = 2;
			y = x >> 3;
			assertEqual(y, 0);
			y = x << 3;
			assertEqual(y, 16);
			x = uint.max;
			y = x << 3;
			assertEqual(y, uint128(0x00000007_FFFFFFF8));
			y = x >> 3;
			assertEqual(y, uint128(0x1FFFFFFF));
			y = x >>> 3;
			assertEqual(y, uint128(0x1FFFFFFF));
			x = ulong.max;
			y = x << 3;
			assertEqual(y, uint128("0x00000007_FFFFFFFF_FFFFFFF8"));
			y = x >> 3;
			assertEqual(y, uint128("0x1FFFFFFF_FFFFFFFF"));
        }
		static if (SIGNED) {
			int128 sx, sy;
			sx = 2;
			sy = sx >> 3;
			assertEqual(sy, 0);
			sy = sx << 3;
			assertEqual(sy, 16);
			sx = uint.max;
			sy = sx << 3;
			assertEqual(sy, int128(0x00000007_FFFFFFF8));
			sy = sx >> 3;
			assertEqual(sy, int128(0x1FFFFFFF));
			sy = sx >>> 3;
			assertEqual(sy, int128(0x1FFFFFFF));
			sx = ulong.max;
			sy = sx << 3;
			assertEqual(sy, int128("0x00000007_FFFFFFFF_FFFFFFF8"));
			sy = sx >> 3;
			assertEqual(sy, int128("0xFFFFFFFF_FFFFFFFF"));
			sy = sx >>> 3;
			// TODO: add an assertEqualHex to assertions? or add formatting?
			assertEqual(sy, int128("0x1FFFFFFF_FFFFFFFF"));
		}
/*		long z = long.max;
writeln();
writefln("z       = %016X", z);
writefln("z >>  3 = %016X", z >> 3);
writefln("z >>> 3 = %016X", z >>> 3);
		z = long.min;
writefln("z       = %016X", z);
writefln("z >>  3 = %016X", z >> 3);
writefln("z >>> 3 = %016X", z >>> 3);*/

		writeln("passed");
	}}

	/// Returns the absolute value of the value of the fixed-size integer.
	/// No effect on unsigned fixed-size integers -- returns a copy.
	public const fixed abs() {
		static if (SIGNED) {
			return isNegative ? this.negate : this.dup;
		} else {
			return this.dup;
		}
	}

	unittest {
	static if (UNCHECKED) {
		write("-- abs..............");
		static if (UNSIGNED) {
			uint128 unum = 1234567;
			assertEqual(unum.abs, unum);
		}
		static if (SIGNED) {
			int128 snum = -1234567;
			assertEqual(snum.abs, -snum);
		}
		writeln("passed");
	}}

	public const fixed sqr() {
		uint[] arg = this.isNegative ? negateDigits(this.digits) : this.digits.dup;
		uint[] sq = sqrDigits(arg);
		static if (CHECKED) {
			if (overflowOccurred(sq)) {
				throw new IntegerOverflowException();
			}
		}
		return fixed(sq);
	}

	unittest {
		write("-- square...........");
		uint128 unum = 25;
		assertEqual(unum.sqr, 625);
		int128 snum = -25;
		assertEqual(snum.sqr, 625);
		cint64 cnum = -25;
		assertEqual(cnum.sqr, 625);
		cnum = 0x0000000_FFFFFFFF;
		assertEqual(cnum.sqr, 0xFFFFFFFE_00000001);
		cnum = 0x0000001_00000000;
		assertThrows!IntegerOverflowException(cnum.sqr);
		writeln("passed");
	}

	unittest {
		writeln("==========================");
		if (CHECKED) {
			writefln("cint%d.................end", BITS);
		} else if (SIGNED) {
			writefln("int%d.................end", BITS);
		} else {
			writefln("uint%d................end", BITS);
		}
		writeln("==========================");
	}

}	// end FixedInt

//--------------------------------
// fixed operations
//--------------------------------

/*	unittest {
		write("min, maxof...");
		uint128 a = 12345;
		uint128 b = 23456;
	writefln("a = %s", a);
	writefln("b = %s", b);
	writefln("maxof(a,b) = %s", uint128.maxof(a,b));
		writeln("test missing");
	}*/

	/// Returns the absolute value of the value of the fixed-size integer.
	/// No effect on unsigned fixed-size integers -- returns a copy.
	public  T abs(T)(const T arg) {
		return arg < 0 ? -arg.dup : arg.dup;
	}

	/// Returns the square of the value of the fixed-size integer.
	public  T sqr(T)(const T arg) {
		return arg.dup.sqr;
	}

	public T max(T)(const T arg1, const T arg2) {
		return arg1 < arg2 ? arg2 : arg1;
	}

	public T min(T)(const T arg1, const T arg2) {
		return arg1 > arg2 ? arg2 : arg1;
	}

	unittest {
		write("min/max...");
		uint128 a = 5;
		uint128 b = 7;
		assert(max(a,b) == b);
		assert(min(a,b) == a);
		writeln("passed");
	}

	public T divmod(T)(const T x, const T y, out T mod) {
		return divmodDigits(x.digits, y.digits, mod.digits);
	}

	unittest {	// divmod
		write("divmod...");
		uint128 a = 5;
		uint128 b = 7;
		uint128 c = 0;
	//writefln("abs!uint128(5) = %s", abs(a));
	//writefln("sqr!uint128(uint128(7)) = %s", uint128.sqr(b ));

	//	writefln("max!128(a,b) = %s", max!128(a,b));

		writeln("test missing");
	}

unittest {
	writeln("==========================");
	writeln("fixed int module.......end");
	writeln("==========================");
}

