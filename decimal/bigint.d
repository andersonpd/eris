module eris.decimal.bigint;

import std.stdio;
import std.bigint;
import std.conv;
import std.ascii;
import std.string;

//import eris.integer.extended;

//alias xint = ExtendedInt;

alias bigint = BigInt;

unittest {
	writeln("==========================");
	writeln("decimal bigint.........begin");
	writeln("==========================");
}

version (unittest)
{
	import eris.decimal.test;
}

///
/// Returns a decimal string representation of the argument.
///
public string toString(in bigint bi) {
	string outbuff = "";
	void sink(const(char)[] s) {
		outbuff ~= s;
	}
	bi.toString(&sink, "%d");
	return outbuff;
}

///
/// Returns -1, 0, or 1 if the argument is
/// negative, zero, or positive, respectively.
/// The sign of zero is ignored: returns 0 for +0 or -0.
///
public int sgn(in bigint x)
{
	if (x == 0) return 0;
	return x < 0 ? -1 : 1;
}

unittest
{	// sgn
	static struct S { bigint x; int expect; }
	S[] s =
	[
		{  "-123", -1 },
		{  "2345",  1 },
		{ "-2345", -1 },
		{     "0",  0 },
	];
	auto f = FunctionTest!(S,int)("sgn");
	foreach (t; s) f.test(t, sgn(t.x));
    writefln(f.report);
}

///
/// Returns the absolute value of the argument.
///
public bigint abs(in bigint x)
{
//	if (x == 0) return 0;
	return x < 0 ? -x : x;
}

unittest
{	// abs
	static struct S { bigint x; bigint expect; }
	S[] s =
	[
		{  "-123", "123" },
		{  "2345", "2345" },
		{ "-2345", "2345" },
		{     "0", "0" },
		{    "-0", "0" },
	];
	auto f = FunctionTest!(S,bigint)("abs");
	foreach (t; s) f.test(t, abs(t.x));
    writefln(f.report);
}

///
/// Returns the square of the argument.
///
public bigint sqr(in bigint x)
{
	return x^^2;
}

unittest
{	// sqr
	static struct S { bigint x; bigint expect; }
	S[] s =
	[
		{  "-123", "15129" },
		{  "2345", "5499025" },
		{ "-2345", "5499025" },
		{     "0",  "0" },
		{    "-0",  "0" },
	];
	auto f = FunctionTest!(S,bigint)("sqr");
	foreach (t; s) f.test(t, sqr(t.x));
    writefln(f.report);
}

///
/// Returns true if the argument is zero.
///
public bool isZero(bigint)(in bigint x)
{
	return x == 0;
}

unittest
{	// isZero
	static struct S { bigint x; bool expect; }
	S[] s =
	[
		{  "-123", false },
		{  "2345", false },
		{     "0", true  },
		{    "-0", true  },
	];
	auto f = FunctionTest!(S,bool)("isZero");
	foreach (t; s) f.test(t, isZero(t.x));
    writefln(f.report);
}

/*public xint toXint(in bigint x)
{
	return xint(toString(x));
}*/

///
/// Returns the argument as an unsigned long integer
///
public ulong toUlong(in bigint x)
{
	bigint y = abs(x);
	if (y == 0) return 0;
	if (y <= long.max) return cast(ulong)y.toLong;
	if (y >= ulong.max) return ulong.max;
	bool odd = cast(bool)(y & 1);
	bigint z = y / 2;
	ulong a = cast(ulong)z.toLong;
	ulong n = 2 * a;
	if (odd) n++;
	return n;
}

unittest
{	// toUlong
	static struct S { bigint x; ulong expect; }
	S[] s =
	[
		// zero
		{                    "0", 0 },
		// less than long.max
		{                "23456", 23456 },
		// long.max
		{  "9223372036854775807", long.max },
		// long.max + 1
		{  "9223372036854775808", 9223372036854775808UL },
		// ulong.max
		{ "18446744073709551615", ulong.max },
		// ulong.max - 1
		{ "18446744073709551614", ulong.max-1 },
		// ulong.max + 1
		{ "18446744073709551616", ulong.max },
		// less than ulong.max
		{ "10000000000000000000", 10000000000000000000UL },
		// odd number less than ulong.max
		{ "10000000000000000001", 10000000000000000001UL },
		// greater than ulong.max
		{ "20000000000000000000", ulong.max },
	];
	auto f = FunctionTest!(S,ulong)("toUlong");
	foreach (t; s) f.test(t, toUlong(t.x));
    writefln(f.report);
}

/// Reduces the argument to an unsigned long
public ulong reduceDigits(in bigint x) {
	bigint big = x;
	while (big > QUINTILLION) {
		big /= QUINTILLION;
	}
	return big.toUlong;
}

/// Reduces the argument to an unsigned long
public ulong reduceDigits(in bigint x, out uint count) {
	count = 0;
	bigint big = x;
	while (big > QUINTILLION) {
		big /= QUINTILLION;
		count += 18;
	}
	return big.toUlong;
}

unittest
{	// reduceDigits
	static struct S { bigint x; ulong expect; }
	S[] s =
	[
		{		   long.max, 9 },
		{		  ulong.max, 18 },
		{		  ulong.min, 0 },
		{				 13, 13 },
		{			   9999, 9999 },
		{		10000000000, 10000000000 },
		{123456789012345678, 123456789012345678 },
		{ "5000000000000000000000", 5000 },
		{ "18690473486004564289165545643685440097", 18 },
		{ "823456789012345678901234567890123456789012345678901234567890"
			"12345678901234567890123456789012345678905", 82345678901 },
	];
	auto f = FunctionTest!(S,ulong)("redDigits");
	foreach (t; s) f.test(t, reduceDigits(t.x));
    writefln(f.report);
}

unittest {
	static struct S { bigint x; uint expect; }
	S[] s =
	[
		{		   long.max, 18 },
		{		  ulong.max, 18 },
		{		  ulong.min, 0 },
		{				 13, 0 },
		{			   9999, 0 },
		{		10000000000, 0 },
		{123456789012345678, 0 },
		{ "5000000000000000000000", 18 },
		{ "18690473486004564289165545643685440097", 36 },
		{ "823456789012345678901234567890123456789012345678901234567890"
			"12345678901234567890123456789012345678905", 90 },
	];
	auto f = FunctionTest!(S,ulong)("redDigits");
	foreach (t; s)
	{
		uint count = 0;
		reduceDigits(t.x, count);
		f.test(t, count);
	}
    writefln(f.report);
}

// NOTE: for some reason POW10 is limited to 10^^18 in xint version
/// unsigned long integer powers of ten from 10^^0 to 10^^19
public enum ulong[20] POW10 = [10UL^^0,
		10UL^^1,  10UL^^2,  10UL^^3,  10UL^^4,
		10UL^^5,  10UL^^6,	10UL^^7,  10UL^^8,
		10UL^^9,  10UL^^10, 10UL^^11, 10UL^^12,
		10UL^^13, 10UL^^14, 10UL^^15, 10UL^^16,
		10UL^^17, 10UL^^18,	10UL^^19];

private enum bigint QUINTILLION  = 1_000_000_000_000_000_000UL;

/// Returns the number of digits in the argument,
/// where the argument is an unsigned long integer.
public uint numDigits(ulong n) {
	if (n == 0) return 0;
	if (n < 10) return 1;
	if (n >= POW10[19]) return 20;
	// binary search
	uint min = 2;
	uint max = 19;
	while (min <= max) {
		uint mid = (min + max)/2;
		if (n < POW10[mid]) {
			max = mid - 1;
		}
		else {
			min = mid + 1;
		}
	}
	return min;
}

unittest
{	// numDigits
	static struct S { ulong n; uint expect; }
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
	auto f = FunctionTest!(S,uint)("numDigits");
	foreach (t; s) f.test(t, numDigits(t.n));
    writefln(f.report);
}

///
/// Returns the number of decimal digits in the argument
///
public int numDigits(in bigint x)
{
	if (x == 0) return 0;
	if (abs(x) < 10) return 1;
	if (abs(x) < 100) return 2;
	if (x == 0) return 0;

	uint count = 0;
	ulong n = reduceDigits(x, count);
	return count + numDigits(n);
//	return str.length;
}

unittest
{	// numDigits
	static struct S { bigint x; int expect; }
	S[] s =
	[
		{     "0", 0 },
		{    "-0", 0 },
		{    "-5", 1 },
		{    "12", 2 },
		{  "-123", 3 },
		{ "-2345", 4 },
		{ "23456", 5 },
		{ "123456789012345678901234567890", 30 },
	];
	auto f = FunctionTest!(S,int)("numDigits");
	foreach (t; s) f.test(t, numDigits(t.x));
    writefln(f.report);
}

/// Returns the first digit of the argument.
public uint firstDigit(ulong n) { //, int maxValue = 19) {
	if (n == 0) return 0;
	if (n < 10) return cast(uint) n;
	uint digits = numDigits(n); //, maxValue);
	return cast(uint)(n/POW10[digits-1]);
}

/*/// Returns the first digit of the argument.
public uint firstDigit(ulong n) { //, int maxValue = 19) {
	if (n == 0) return 0;
	if (n < 10) return cast(uint) n;
	uint digits = numDigits(n); //, maxValue);
	return cast(uint)(n/POW10[digits-1]);
}*/

unittest
{	// firstDigit(xint)
	static struct S { ulong n; uint expect; }
	S[] s =
	[
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
		{long.max,  9 },
		{ulong.max, 1 },
	];
	auto f = FunctionTest!(S,uint)("1stDigit");
	foreach (t; s) f.test(t, firstDigit(t.n));
    writefln(f.report);
}

/// Returns the first digit of the argument.
public int firstDigit(in bigint x) {
	return firstDigit(reduceDigits(x));
}

unittest
{	// firstDigit(xint)
	static struct S { bigint n; uint expect; }
	S[] s =
	[
		{ "5000000000000000000000", 5 },
		{ "8234567890123456789012345678901234567890123456789012"
			"3456789012345678901234567890123456789012345678905", 8 },
	];
	auto f = FunctionTest!(S,uint)("1stDigBig");
	foreach (t; s) f.test(t, firstDigit(t.n));
    writefln(f.report);
}

/// Returns a big integer from a string representation.
/// The function recognizes decimal, hexadecimal and binary strings.
/// The sign character, if any, is ignored with non-decimal strings;
/// i.e., there is no sign extension.
public static bigint toBigInt(string str) {
	char[] chars = strip(str.dup);
	bigint big = 0;
	bool sign = false;
	// parse sign character, if any.
	if (chars[0] == '-') {
		chars = chars[1..$];
		sign = true;
	}
	if (chars[0] == '+') {
		chars = chars[1..$];
	}
	// if the string starts withs a non-zero digit,
	// it should be a decimal number.
	if (chars[0] != '0') {
		big = parseDecimal(chars);
	}
	else
	{	// first char is zero; it may be hex or binary
		toLowerInPlace(chars);
		if (startsWith(chars, "0x")) {
			big = parseHex(chars[2..$].dup);
		}
		else if (startsWith(chars, "0b")) {
			big = parseBinary(chars[2..$]);
		}
		else
		{	// assume decimal
			big = parseDecimal(chars);
		}
	}
	return sign ? -big : big;
}

/// Constructs a big integer from a sign and a string value.
public bigint toBigInt(bool sign, string str) {
	bigint big = toBigInt(str);
	return sign ? -big : big;
}

private static bigint parseHex(char[] chars) {
	bigint big = 0;
	uint digit = 0;
	foreach (char ch; chars) {
		if (ch == '_') continue;
		if (!isHexDigit(ch)) {
			throw new ConvException("Invalid hexadecimal char: [" ~ ch ~ "]");
		}
		big *= 16;
		if (isDigit(ch)) {
			digit = ch - '0';
		}
		else {
			digit = ch - 'a' + 10;
		}
		big += digit;
	}
	return big;
}

private static bigint parseBinary(char[] chars) {
	bigint big = 0;
	foreach (char ch; chars) {
		if (ch == '_') continue;
		if (ch != '0' && ch != '1') {
			throw new ConvException("Invalid binary char: [" ~ ch ~ "]");
		}
		big *= 2;
		if (ch - '0') big += 1;
	}
	return big;
}

private static bigint parseDecimal(char[] chars) {
	bigint big = 0;
	uint digit = 0;
	if (!isDigit(chars[0])) {
		throw new ConvException("Invalid start char: [" ~ chars[0] ~ "]");
	}
	foreach (char ch; chars) {
		if (ch == '_') continue;
		if (!isDigit(ch)) {
			throw new ConvException("Invalid decimal char: [" ~ ch ~ "]");
		}
		big *= 10;
		big += ch - '0';
	}
	return big;
}

unittest
{	// toBigInt
	static struct S { string str; bigint expect; }
	S[] s =
	[
		{         "7", "7" },
		{       "123", "123" },
		{      "-123", "-123" },
		{      "0x7B", "123" },
		{ "0b1111011", "123" },
		{ "0_234_445", "234445" },
		{ "0xFFFFFF85", "4294967173" },
		{ "0xFFFFFFFF_FFFFFF85", "18446744073709551493" },
		{ "0b11111111111111111111111110000101",
			"4294967173" },
		{ "0b11111111111111111111111111111111"
			"11111111111111111111111110000101",
			"18446744073709551493" },
		{ "0b11111111111111111111111111111111"
			"11111111111111111111111111111111"
			"11111111111111111111111111111111"
			"11111111111111111111111110000101",
			"340282366920938463463374607431768211333" },
		{ "1000000000000000000000000000001",
			"1000000000000000000000000000001" },
		{ "10000000000111111111111111111111111111",
			"10000000000111111111111111111111111111" },
		{ "10000000000999999999999999999999999999999",
			"10000000000999999999999999999999999999999" },
		{ "-8234567890123451354886321000483300584897",
			"-8234567890123451354886321000483300584897" },
		{ "623456789012345678", "623456789012345678" },
	];
	auto f = FunctionTest!(S,bigint)("toBigInt");
	foreach (t; s) f.test(t, toBigInt(t.str));
    writefln(f.report);
}

unittest
{

// these lines cause a compile error due
// to implementation of std.bigint
// problem is that no arithmetic functions
// work at compile time

//	enum BigInt aa = toBigInt("0xabcde");
//	writefln("aa = %s", aa);

	enum BigInt ab = BigInt("1234345");
	writefln("ab = %s", ab);

}

unittest {
	writeln("==========================");
	writeln("decimal bigint.........end");
	writeln("==========================");
}
