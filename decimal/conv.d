// Written in the D programming language

/**
 * Conversion of floating-point decimals to/from strings.
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

module eris.decimal.conv;

import std.array: insertInPlace, replicate;
//static import std.array;
import std.ascii: isDigit;
import std.string;
import std.format;
import std.stdio;
static import std.uni;
static import std.conv;
static import std.math;

import eris.decimal;
import eris.decimal.context;
import eris.decimal.rounding;

unittest {
	writeln("==========================");
	writeln("decimal conversion...begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.decimal.test;
}

public enum DEFAULT_PRECISION = 6;

//--------------------------------
//   to!string conversions
//--------------------------------

/// to!string(bigint).
public T to(T:string)(in bigint x) {
	string outbuff = "";
	void sink(const(char)[] s) {
		outbuff ~= s;
	}
	x.toString(&sink, "%d");
	return outbuff;
}

/// to!string(long).
private T to(T:string)(in long n) {
	return format("%d", n);
}

/**
 * Returns a string representing the value of the number, formatted as
 * specified by the format string.
 * Params: num, fmtstr.
 */
public string toString(T)(in T num, string fmStr = "%s") if (isDecimal!T)
{
    auto fm = singleSpec!char(fmStr.dup);
	string str = "";
	if (num.isSigned) str = "-";
	else if (fm.flPlus)  str = "+";
	else if (fm.flSpace) str = " ";

	bool noPrecision = (fm.precision == fm.UNSPECIFIED);
	// if precision is unspecified it defaults to 6
	int precision = noPrecision ? DEFAULT_PRECISION : fm.precision;

	// FIXTHIS: if num is special this may give error. see formatDecimal
	str ~= formatDecimal(num, fm.spec, precision);

	// add trailing zeros
	if (fm.flHash && str.indexOf('.' < 0)) {
		str ~= ".0";
	}
	// if precision is unspecified the zero flag is ignored
	bool zero = noPrecision ? fm.flZero : false;
	// adjust width
	str = setWidth(str, fm.width, fm.flDash, zero);
	return str;
}

unittest
{	// toString
	static struct S { TD num; string fmt; string str; }
	S[] s =
	[
		// default format is scientific form
		{ "123",		"%s",	"123" },
		{ "-123",		"%S",	"-123" },
		{ "12.3E1",		"%S",	"123" },
		{ "123E1",		"%S",	"1.23E+3" },
		{ "123E3",		"%S",	"1.23E+5" },
		{ "123E-1",		"%S",	"12.3" },
		{ "50E-7",		"%S",	"0.0000050" },
		{ "5E-7",		"%S",	"5E-7" },
		{ "12.3456789",	"%S",	"12.3456789" },
		{ "12.34567",	"%s",	"12.34567" },
		{ "12.345",		"%S",	"12.345"   },
		// exponential form
		{ "12.3456789",	"%E",	"1.234568E+01" },
		{ "12.34567",	"%e",	"1.234567e+01" },
		{ "12.345",		"%E",	"1.2345E+01"   },
		// decimal form
		{ "12.3456789",	"%F",	"12.345679" },
		{ "12.34567",	"%F",	"12.345670" },
		{ "12.345",		"%F",	"12.345000" },
		// decimal or exponential. note change in meaning of precision
		{ "12.3456789",	"%G",	"1.234568E+01" },
		{ "12.34567",	"%G",	"1.234567E+01" },
		{ "12.345",		"%G",	"12.345000"    },
		// width
		{ "12.34567",	"%12.4G",	"  1.2346E+01" },
		{ "12.345",		"%12G",	"   12.345000" },
		// flags
		{ "12.34567",	"% G",		" 1.234567E+01"},
		{ "12.345",		"%+G",		"+12.345000"   },
		{ "12.345",		"%-12G",	"12.345000   " },
		{ "12.34567",	"%-12.4G",	"1.2346E+01  " },
		{ "12.345",		"%012G",	"00012.345000" },
		// zero flag ignored if precision is specified
		{ "12.345",		 "%012.4G",	"     12.3450" },
		// zero flag, upper/lower case  ignored if infinity or nan
		// FIXTHIS: returns Orphan format specifier error message
		// not just for G either
		// I think it's a bug in std.format
//		{ "Inf",		 "%012.4G",	"    Infinity" },
		{ "NaN",		 "%012.4g",	"         NaN" },
		// if hash, print trailing zeros.
		{ "1234567.89",	 "%.0G",	"1234568" },
		{ "1234567.89",	 "%.0F",	"1234568" },
		{ "1234567",	 "%.0F",	"1234567" },
		{ "123",		 "%#.0F",	"123.0" },
	];
	auto f = FunctionTest!(S,string)("toString");
	foreach (t; s) f.test(t, toString(t.num, t.fmt));
    writefln(f.report);
}

/*    void toString(T)(
		scope void delegate(const(char)[]) sink,
		ref FormatSpec!char f) if (isDecimal!T) const
	{

	}*/
//--------------------------------
//  formatting
//--------------------------------

/**
 *  Converts a decimal number to a string
 *  using "scientific" notation, per the spec.
 */
public string sciForm(T)(in T num) if (isDecimal!T)
{
	if (num.isSpecial) {
		return specialForm(num);
	}

	char[] coff = to!string(num.coefficient).dup;
	int  expo = num.exponent;
	bool signed = num.isSigned;

	int adjx = expo + cast(int)coff.length - 1;
	// if the exponent is small use decimal notation
	if (expo <= 0 && adjx >= -6) {
		// if the exponent is not zero, insert a decimal point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if the coefficient is too small, pad with zeroes
			if (point > coff.length) {
				coff = rightJustify(coff, point, '0');
			}
			// if no chars precede the decimal point, prefix a zero
			if (point == coff.length) {
				coff = "0." ~ coff;
			}
			// otherwise insert the decimal point into the string
			else {
				insertInPlace(coff, coff.length - point, ".");
			}
		}
		return signed ? ("-" ~ coff).dup : coff.dup;
	}
	// if the exponent is large enough use exponential notation
	if (coff.length > 1) {
		insertInPlace(coff, 1, ".");
	}
	string expStr = to!string(adjx);
	if (adjx >= 0) {
		expStr = "+" ~ expStr;
	}
	string str = (coff ~ "E" ~ expStr).dup;
	return signed ? "-" ~ str : str;
};  // end sciForm

unittest
{	// sciForm
	static struct S { TD num; string str; }
	S[] s =
	[
		{ "123",		"123" },
		{ "-123",		"-123" },
		{ "12.3E1",		"123" },
		{ "123E1",		"1.23E+3" },
		{ "123E3",		"1.23E+5" },
		{ "123E-1",		"12.3" },
		{ "inf",		"Infinity" },
	];
	auto f = FunctionTest!(S,string)("sciForm");
	foreach (t; s) f.test(t, sciForm(t.num));
    writefln(f.report);
}

/**
 *  Converts a decimal number to a string
 *  using "engineering" notation, per the spec.
 */
public string engForm(T)(in T num) if (isDecimal!T)
{
	if (num.isSpecial) {
		return specialForm(num);
	}

	char[] coff = to!string(num.coefficient).dup;
	int  expo = num.exponent;
	bool signed = num.isSigned;

	int adjx = expo + cast(int)coff.length - 1;
	// if exponent is small, don't use exponential notation
	if (expo <= 0 && adjx >= -6) {
		// if exponent is not zero, insert a decimal point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if coefficient is too small, pad with zeroes
			if (point > coff.length) {
				coff = rightJustify(coff, point, '0');
			}
			// if no chars precede the decimal point, prefix a zero
			if (point == coff.length) {
				coff = "0." ~ coff;
			}
			// otherwise insert a decimal point
			else {
				insertInPlace(coff, coff.length - point, ".");
			}
		}
		return signed ? ("-" ~ coff).idup : coff.idup;
	}
	// use exponential notation
	if (num.isZero) {
		adjx += 2;
	}
	int mod = adjx % 3;
	// the % operator rounds down; we need it to round to floor.
	if (mod < 0) {
		mod = -(mod + 3);
	}
	int dot = std.math.abs(mod) + 1;
	adjx = adjx - dot + 1;
	if (num.isZero) {
		dot = 1;
		int count = 3 - std.math.abs(mod);
		coff.length = 0;
		for (size_t i = 0; i < count; i++) {
			coff ~= '0';
		}
	}
	while (dot > coff.length) {
		coff ~= '0';
	}
	if (coff.length > dot) {
		insertInPlace(coff, dot, ".");
	}
	string str = coff.idup;
	if (adjx != 0) {
		string expStr = to!string(adjx);
		if (adjx > 0) {
			expStr = '+' ~ expStr;
		}
		str = str ~ "E" ~ expStr;
	}
	return signed ? "-" ~ str : str;
}  // end engForm()

unittest
{	// engForm
	static struct S { TD num; string str; }
	S[] s =
	[
		{ "123",		"123" },
		{ "-123",		"-123" },
		{ "12.3E1",		"123" },
		{ "123E1",		"1.23E+3" },
		{ "123E3",		"123E+3" },
		{ "123E-1",		"12.3" },
		{ "1.23E+3",	"1.23E+3" },
		{ "1.23E-9",	"1.23E-9" },
		{ "-1.23E-12",	"-1.23E-12" },
		{ "9.999999E+96","9.999999E+96" },
		{ "1.000",		"1.000" },
		{ "NaN",		"NaN" },
		{ "-NaN102",	"-NaN102" },
		{ "-Infinity",	"-Infinity" },
		{ "inf",		"Infinity" },
		{ "-0",			"-0" },
	];
	auto f = FunctionTest!(S,string)("engForm");
	foreach (t; s) f.test(t, engForm(t.num));
    writefln(f.report);
}

/**
 *  Returns a string representing the number, formatted as specified.
 */
// TODO: why have a short form
// Should there be an upper/lower case flag?
private string specialForm(T)(in T num, bool shortForm = false) if (isDecimal!T)
{
	string str = num.sign ? "-" : "";
	if (num.isInfinite)
	{
		str ~= shortForm ? "Inf" : "Infinity";
	}
	else if (num.isNaN)
	{
		str ~= num.isSignaling ? "sNaN" : "NaN";
		if (num.payload)
		{
			str ~= to!string(num.payload);
		}
	}
	return str;
}

unittest
{	// specialForm
	static struct S { TD num; string str; }
	S[] s =
	[
		{ "NaN",		"NaN" },
		{ "-sNaN102",	"-sNaN102" },
		{ "-Infinity",	"-Infinity" },
	];
	auto f = FunctionTest!(S,string)("specForm");
	foreach (t; s) f.test(t, specialForm(t.num));
	S[] r =
	[
		{ "Infinity",	"Inf" },
	];
	foreach (t; r) f.test(t, specialForm(TD(t.num),true));
    writefln(f.report);
}

/**
 *  Converts a decimal number to a string in decimal format.
 *
 *  Returns e.g. "125E-5" as "0.001250" with no exponent.
 *  Numbers with large or small exponents will return long strings.
 *  Numbers with very large or very small exponents
 *  will return very long strings.
 */
private string decimalForm(T)(in T number,
	int precision = DEFAULT_PRECISION) if (isDecimal!T)
{
	if (number.isSpecial)
	{
		return specialForm(number);
	}
	T num = number.dup;
	// check if rounding is needed:
	int diff = num.exponent + precision;
	if (diff < 0) {
		int numPrecision = num.digits + num.exponent + precision;
		num = roundToPrecision(num, numPrecision, T.maxExpo, T.mode);
	}

	// convert the coefficient to a string
	char[] str = to!string(num.coefficient).dup;
	int expo = num.exponent;
	bool sign = num.isSigned;
	if (expo >= 0) {
		if (expo > 0) {
			// add zeros up to the decimal point
			str ~= replicate("0", expo);
		}
		if (precision) {
			// add zeros trailing the decimal point
			str ~= "." ~ replicate("0", precision);
		}
	}
	else { // (expo < 0)
		int point = -expo;
		// if coefficient is too small, pad with zeros on the left
		if (point > str.length) {
			str = rightJustify(str, point, '0');
			}
		// if no chars precede the decimal point, prefix a zero
		if (point == str.length) {
			str = "0." ~ str;
		}
		// otherwise insert a decimal point
		else {
			insertInPlace(str, str.length - point, ".");
		}
		// if result is less than precision, add zeros
		if (point < precision) {
			str ~= replicate("0", precision - point);
		}
	}
	return sign ? ("-" ~ str).idup : str.idup;
}

unittest
{	// decimalForm
	static struct S { TD num; int precision; string str; }
	S[] s =
	[
		{ "12.345",	6,	"12.345000" },
		{ "125",	3,	"125.000" },
		{ "-125",	3,	"-125.000" },
		{ "125E5",	0,	"12500000" },
		{ "1.25",	2,	"1.25" },
		{ "125E-5",	6,	"0.001250" },
		{ "-0",		6,	"-0.000000" },
		{ "Inf",	0,	"Infinity" },
		{ "-NaN",	4,	"-NaN" },
		{ "123.4567890123",	6,	"123.456789" },
		{ "123.4567895123",	6,	"123.456790" },
	];
	auto f = FunctionTest!(S,string)("decForm");
	foreach (t; s) f.test(t, decimalForm(t.num, t.precision));
    writefln(f.report);
}

/**
 *  Converts a decimal number to a string using exponential notation.
 */
private string exponentForm(T)(in T number, int precision = DEFAULT_PRECISION,
	const bool lowerCase = false, const bool padExpo = true) if (isDecimal!T)
{

	if (number.isSpecial) {
		return specialForm(number);
	}
	T num = number.dup;
	num = roundToPrecision(num, precision + 1);
	char[] coff = to!string(num.coefficient).dup;
	int expo = num.exponent;
	bool sign = num.isSigned;
	int adjx = expo + cast(int)coff.length - 1;
	if (coff.length > 1) {
		insertInPlace(coff, 1, ".");
	}
	string expStr = to!string(std.math.abs(adjx));
	if (padExpo && expStr.length < 2) {
		expStr = "0" ~ expStr;
	}
	expStr = adjx < 0 ? "-" ~ expStr : "+" ~ expStr;
	string expoChar = lowerCase ? "e" : "E";
	string str = (coff ~ expoChar ~ expStr).idup;
	return sign ? "-" ~ str : str;
}  // end exponentForm

unittest
{	// exponentForm
	static struct S { TD num; int precision; string str; }
	S[] s =
	[
		{ "125",	3,	"1.25E+02" },
		{ "-125",	3,	"-1.25E+02" },
		{ "125E5",	4,	"1.25E+07" },
		{ "125E-5",	6,	"1.25E-03" },
		{ "Inf",	0,	"Infinity" },
		{ "-NaN",	4,	"-NaN" },
		{ "1.25",	1,	"1.2E+00" },
		{ "1.35",	1,	"1.4E+00" },
		{ "123.4567890123",	6,	"1.234568E+02" },
		{ "123.456789500",	6,	"1.234568E+02" },
		{ "123.4567895123",	8,	"1.23456790E+02" },
	];
	auto f = FunctionTest!(S,string)("expForm");
	foreach (t; s) f.test(t, exponentForm(t.num, t.precision));
    writefln(f.report);
}

/**
 *  Returns a string representing the number, formatted as specified.
 */
private string formatDecimal(T)(in T num,
	char formatChar, int precision) if (isDecimal!T)
{
	bool lowerCase = std.uni.isLower(formatChar);
	bool upperCase = std.uni.isUpper(formatChar);

	// special values
	if (num.isSpecial)
	{
//		FIXTHIS: this return hoses the toString process
		return specialForm(num.copyAbs, false);
	}

	switch (std.uni.toUpper(formatChar))
	{
	case 'F':
		return decimalForm(num, precision);
	case 'G':
		int expo = num.exponent;
		if (expo > -5 && expo < precision) {
			return decimalForm(num, precision);
		}
		break;
	case 'E':
		break;
	case 'S':
		return sciForm(num.copyAbs);
	default:
		break;
	}
	return exponentForm(num, precision, lowerCase, true);
}

/**
 *  Returns a string that is at least as long as the specified width. If the
 *  string is already greater than or equal to the specified width the original
 *  string is returned. If the specified width is negative or if the
 *  flag is set the widened string is left justified.
 */
private string setWidth(string str, int width,
		bool justifyLeft = false, bool padZero = false) {

	if (str.length >= std.math.abs(width)) return str;
	char fillChar = padZero ? '0' : ' ';
	if (width < 0) {
		justifyLeft = true;
		width = -width;
	}
	if (justifyLeft) {
		fillChar = ' ';
		return leftJustify!string(str, width, fillChar);
	}
	return rightJustify!string(str, width, fillChar);
}

unittest
{	// setWidth
	static struct S { string num; int width; bool left; bool zeros; string str; }
	S[] s =
	[
		{ "10+E5",	 8,	false, false, "   10+E5" },
		{ "10E+05",	-8,	false, false, "10E+05  " },
		{ "10E+05",	 8,	true,  false, "10E+05  " },
		{ "10E+05",	 8,	true,  true,  "10E+05  " },
		{ "10E+05",	 8,	false, true,  "0010E+05" },
	];
	auto f = FunctionTest!(S,string)("setWidth");
	foreach (t; s) f.test(t, setWidth(t.num, t.width, t.left, t.zeros));
    writefln(f.report);
}

/**
 *  Returns an abstract string representation of a number.
 *  The abstract representation is described in the specification. (p. 9-12)
 */
public string abstractForm(T)(in T num) if (isDecimal!T)
{
	if (num.isFinite) {
		return format("[%d,%s,%d]", num.sign ? 1 : 0,
		              to!string(num.coefficient), num.exponent);
	}
	if (num.isInfinite) {
		return format("[%d,%s]", num.sign ? 1 : 0, "inf");
	}
	if (num.isQuiet) {
		if (num.payload) {
			return format("[%d,%s%d]", num.sign ? 1 : 0, "qNaN", num.payload);
		}
		return format("[%d,%s]", num.sign ? 1 : 0, "qNaN");
	}
	if (num.isSignaling) {
		if (num.payload) {
			return format("[%d,%s%d]", num.sign ? 1 : 0, "sNaN", num.payload);
		}
		return format("[%d,%s]", num.sign ? 1 : 0, "sNaN");
	}
	return "[0,qNAN]";
}

unittest
{	// abstractForm
	static struct S { TD num; string str; }
	S[] s =
	[
		{ "-inf",	  "[1,inf]" },
		{ "nan",	  "[0,qNaN]" },
		{ "snan1234", "[0,sNaN1234]" },
	];
	auto f = FunctionTest!(S,string)("absForm");
	foreach (t; s) f.test(t, abstractForm(t.num));
    writefln(f.report);
}

/**
 *  Returns a full, exact representation of a number. Similar to abstractForm,
 *  but it provides a valid string that can be converted back into a number.
 */
public string fullForm(T)(in T num) if (isDecimal!T)
{
	if (num.isFinite) {
		return format("%s%sE%s%02d", num.sign ? "-" : "+",
		              to!string(num.coefficient),
		              num.exponent < 0 ? "-" : "+", std.math.abs(num.exponent));
	}
	if (num.isInfinite)
	{
		return format("%s%s", num.sign ? "-" : "+", "Infinity");
	}
	if (num.isQuiet)
	{
		if (num.payload) {
			return format("%s%s%d", num.sign ? "-" : "+", "NaN", num.payload);
		}
		return format("%s%s", num.sign ? "-" : "+", "NaN");
	}
	if (num.isSignaling) {
		if (num.payload) {
			return format("%s%s%d", num.sign ? "-" : "+", "sNaN", num.payload);
		}
		return format("%s%s", num.sign ? "-" : "+", "sNaN");
	}
	return "+NaN";
}

unittest
{	// fullForm
	static struct S { TD num; string str; }
	S[] s =
	[
		{ "-inf",      "-Infinity" },
		{ "nan",       "+NaN" },
		{ "+NaN",      "+NaN" },
		{ "text",      "+NaN" },
		{ "1E+00",     "+1E+00" },
		{ "1000E-03",  "+1000E-03" },
		{ "-NaN102",   "-NaN102" },
		{ "-0E+00",    "-0E+00" },
		{ "9999999E+90", "+9999999E+90" },
	];
	auto f = FunctionTest!(S,string)("fullForm");
	foreach (t; s) f.test(t, fullForm(t.num));
    writefln(f.report);
}

/**
 *  Converts a string into a decimal number. This departs from the
 *  specification in that the coefficient string may contain underscores.
 *  A leading or trailing "." is allowed by the specification even though
 *  it is not valid as a D language real number.
 */
public T toNumber(T)(string inStr, bool round = true) if (isDecimal!T)
{
	T num;

//writefln("inStr = %s", inStr);
	// strip, copy, tolower
	char[] str = strip(inStr).dup;
	toLowerInPlace(str);

	// check for minus sign or plus sign
	bool sign = false;
	if (startsWith(str, "-"))
	{
		sign = true;
		str = str[1..$];
	}
	else if (startsWith(str, "+"))
	{
		str = str[1..$];
	}

	// check for NaN
	if (startsWith(str, "nan"))
	{
		num = T.nan(0, sign);
		// check for payload
		if (str.length > 3) {
			return setPayload(num, str, 3);
		}
		return num;
	}

	// check for sNaN
	if (startsWith(str, "snan"))
	{
		num = T.snan(0, sign);
		// check for payload
		if (str.length > 4) {
			return setPayload(num, str, 4);
		}
		return num;
	}

	// check for infinity
	if (str == "inf" || str == "infinity")
	{
		num = T.infinity(sign);
		return num;
	}

	// at this point, num must be finite
	num = T.zero(sign);

	// check for exponent
	ptrdiff_t  pos = indexOf(str, 'e');
	if (pos > 0) 	// if exponent string found...
	{
		// exponent string must be at least two chars
		if (pos == str.length - 1)
		{
			return T.nan;
		}

		// split str into coefficient and exponent strings
		char[] expStr = str[pos + 1..$];
		str = str[0..pos];

		// check exponent for minus sign or plus sign
		bool expSign = false;
		if (startsWith(expStr, "-"))
		{
			expSign = true;
			expStr = expStr[1..$];
		}
		else if (startsWith(expStr, "+"))
		{
			expStr = expStr[1..$];
		}

		// ensure it's not now empty
		if (expStr.length < 1)
		{
			return T.nan;
		}
		// ensure exponent is all digits
		foreach (char c; expStr)
		{
			if (!isDigit(c)) {
				return T.nan;
			}
		}
		// trim leading zeros
		while (expStr[0] == '0' && expStr.length > 1) {
			expStr = expStr[1..$];
		}
		// make sure it will fit into an int
		if (expStr.length > 10) {
//			writefln("expStr = %s", expStr);
			return T.nan;
		}
		if (expStr.length == 10) {
			// try to convert it to a long (should work) and
			// then see if the long value is too big (or small)
			long lex = std.conv.to!long(expStr);
			if ((expSign && (-lex < int.min)) || lex > int.max) {
				return T.nan;
			}
			num.exponent = cast(int) lex;
		} else {
			// everything should be copacetic at this point
			num.exponent = std.conv.to!int(expStr);
		}
		if (expSign)
		{
			num.exponent = -num.exponent;
		}
	}
	else	// no exponent
	{
		num.exponent = 0;
	}
//if (!__ctfe) writefln("num.exponent = %s", num.exponent);

	// remove trailing decimal point
	if (endsWith(str, "."))
	{
		str = str[0..$ -1];
		// check for empty string (input was ".")
		if (str.length == 0) {
			return T.nan;
		}
	}
	// TODO: better done with a range?
	// strip leading zeros
	while (str[0] == '0' && str.length > 1)
	{
		str = str[1..$];
	}
	// make sure first char is a digit
	// (or a decimal point, in which case check the second char)
	if (!isDigit(str[0]))
	{
		// check for single non-digit char
		if (str.length == 1)
		{
			return T.nan;
		}
		// ensure first char is a decimal point and second char is a digit
		if (str[0] == '.' && !isDigit(str[1]))
		{
		 return T.nan;
		}
	}
	// strip underscores
	if (indexOf(str, '_') >= 0)
	{
  		str = removechars(str.idup, "_").dup;
	}
	// remove internal decimal point
	int point = cast(int)indexOf(str, '.');
	if (point >= 0)
	{
		// excise the point and adjust the exponent
		str = str[0..point] ~ str[point + 1..$];
		int diff = cast(int)str.length - point;
		num.exponent = num.exponent - diff;
	}
	// ensure string is not empty
	if (str.length < 1) {
		return T.nan;
	}
	// ensure chars are all digits
	foreach (char c; str) {
		if (!isDigit(c)) {
			return T.nan;
		}
	}
	// convert coefficient string to bigint
//if (!__ctfe) writefln("str = %s", str);
	num.coefficient = bigint(str.idup);
//if (!__ctfe) writefln("num.coefficient = %s", num.coefficient);
	num.digits = decDigits(num.coefficient);
//if (!__ctfe) writefln("num.digits = %s", num.digits);
	return num;
}

unittest
{	// toNumber
	static struct S { string num; TD str; }
	S[] s =
	[
		{ "2.50",		"2.50" },
		{ "1.0",		"1.0" },
		{ "-123",		"-123" },
		{ "1.23E3",		"1.23E+3" },
		{ "1.23E-3",	"0.00123" },
		{ "1.2_3E3",	"1.23E+3" },
		{ ".1",			"0.1" },
		{ ".",			"NaN" },
		{ ".E3",		"NaN" },
		{ "+.",			"NaN" },
		{ "1.7976931348623157079E+308", "1.7976931348623157079E+308" },
	];
	auto f = FunctionTest!(S,TD)("toNumber");
	foreach (t; s) f.test(t, toNumber!TD(t.num));
    writefln(f.report);
}

private T setPayload(T)(in T num, char[] str, int len) if (isDecimal!T)
{
	T copy = num.copy;
	// if finite number or infinity, return
	if (!num.isNaN) return copy;
	// if no payload, return
	if (str.length == len) return copy;
	// otherwise, get payload string
	str = str[len..$];
	// trim leading zeros
	while (str[0] == '0' && str.length > 1) {
		str = str[1..$];
	}
	// payload has a max length of 6 digits
	if (str.length > 6) return copy;
	// ensure string is all digits
	foreach (char c; str) {
		if (!isDigit(c)) {
			return copy;
		}
	}
	// convert string to number
	uint payload = std.conv.to!uint(str);
	// check for overflow
	if (payload > ushort.max) {
		return copy;
	}
	copy.payload = cast(ushort)payload;
	return copy;
}

unittest
{	// setPayload
	static struct S { string num; string str; }
	S[] s =
	[
		{ "NaN167",		"NaN167" },
		// invalid payload is ignored
		{ "SNAN135ee",		"sNaN" },
		// leading zeros in payload are excised
		{ "-snan0170",		"-sNaN170" },
	];
	auto f = FunctionTest!(S,string)("setPayload");
	foreach (t; s) f.test(t, TD(t.num).toString);
    writefln(f.report);
}

unittest {
	writeln("==========================");
	writeln("decimal conversion.....end");
	writeln("==========================");
}
