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

module eris.decimal.conv;

import std.array: insertInPlace, replicate;
import std.ascii: isDigit;
import std.string;
import std.format;

import eris.decimal;
import eris.decimal.context;
import eris.decimal.rounding;
import eris.integer.extended;

unittest {
	writeln("==========================");
	writeln("decimal conversion...begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.test.assertion;
}

public enum DefaultPrecision = 6;

//--------------------------------
//   to!string conversions
//--------------------------------

/// to!string(xint).
private T to(T:string)(const xint num) {
	string outbuff = "";
	void sink(const(char)[] s) {
		outbuff ~= s;
	}
	num.toString(&sink, "%d");
	return outbuff;
}

/// to!string(int).
private T to(T:string)(const long n) {
	return format("%d", n);
}

/// Returns a string representing the value of the number, formatted as
/// specified by the formatString.
public string toString(T)(in T num, string fmStr = "%S") if (isDecimal!T)
{
	// TODO: (behavior) is singleSpec okay?
    auto fm = singleSpec!char(fmStr.dup);
	string str = "";
	if (num.isSigned)   str = "-";
	else if (fm.flPlus)  str = "+";
	else if (fm.flSpace) str = " ";
	bool noPrecision = (fm.precision == fm.UNSPECIFIED);
	// if precision is unspecified it defaults to 6
	int precision = noPrecision ? DefaultPrecision : fm.precision;
	str ~= formatDecimal!T(num, fm.spec, precision);
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

unittest  // toString
{
	write("-- toString.........");

	static struct S { string num; string fmt; string str; }

	static S[] tests =
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
		{ "12.34567",	"%E",	"1.234567E+01" },
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
		{ "Inf",		 "%012.4G",	"    Infinity" },
		{ "NaN",		 "%012.4g",	"         NaN" },
		// if hash, print trailing zeros.
		{ "1234567.89",	 "%.0G",	"1234568" },
		{ "1234567.89",	 "%.0F",	"1234568" },
		{ "1234567",	 "%.0F",	"1234567" },
		{ "123",		 "%#.0F",	"123.0" },
	];

	foreach (int i, s; tests)
	{

		assertEqual(toString(dec9(s.num), s.fmt), s.str, i);
	}
	writeln("passed");
}

//--------------------------------
//  formatting
//--------------------------------

/// Converts a decimal number to a string
/// using "scientific" notation, per the spec.
public string sciForm(T)(in T num) if (isDecimal!T)
{
	if (num.isSpecial) {
		return specialForm!T(num);
	}

	char[] mant = to!string(num.coefficient).dup;
	int  expo = num.exponent;
	bool signed = num.isSigned;

	int adjx = expo + cast(int)mant.length - 1;
	// if the exponent is small use decimal notation
	if (expo <= 0 && adjx >= -6) {
		// if the exponent is not zero, insert a decimal point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if the coefficient is too small, pad with zeroes
			if (point > mant.length) {
				mant = rightJustify(mant, point, '0');
			}
			// if no chars precede the decimal point, prefix a zero
			if (point == mant.length) {
				mant = "0." ~ mant;
			}
			// otherwise insert the decimal point into the string
			else {
				insertInPlace(mant, mant.length - point, ".");
			}
		}
		return signed ? ("-" ~ mant).dup : mant.dup;
	}
	// if the exponent is large enough use exponential notation
	if (mant.length > 1) {
		insertInPlace(mant, 1, ".");
	}
	string xstr = to!string(adjx);
	if (adjx >= 0) {
		xstr = "+" ~ xstr;
	}
	string str = (mant ~ "E" ~ xstr).dup;
	return signed ? "-" ~ str : str;
};  // end sciForm

unittest // sciForm
{
	write("-- sciForm..........");

	static struct S { string num; string str; }

	static S[] tests =
	[
		{ "123",		"123" },
		{ "-123",		"-123" },
		{ "12.3E1",		"123" },
		{ "123E1",		"1.23E+3" },
		{ "123E3",		"1.23E+5" },
		{ "123E-1",		"12.3" },
		{ "inf",		"Infinity" },
	];

	foreach (int i, s; tests)
	{
		assertEqual(sciForm(dec9(s.num)), s.str, i);
	}
	writeln("passed");
}

/// Converts a decimal number to a string
/// using "engineering" notation, per the spec.
public string engForm(T)(in T num) if (isDecimal!T)
{
	if (num.isSpecial) {
		return specialForm!T(num);
	}

	char[] mant = to!string(num.coefficient).dup;
	int  expo = num.exponent;
	bool signed = num.isSigned;

	int adjx = expo + cast(int)mant.length - 1;
	// if exponent is small, don't use exponential notation
	if (expo <= 0 && adjx >= -6) {
		// if exponent is not zero, insert a decimal point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if coefficient is too small, pad with zeroes
			if (point > mant.length) {
				mant = rightJustify(mant, point, '0');
			}
			// if no chars precede the decimal point, prefix a zero
			if (point == mant.length) {
				mant = "0." ~ mant;
			}
			// otherwise insert a decimal point
			else {
				insertInPlace(mant, mant.length - point, ".");
			}
		}
		return signed ? ("-" ~ mant).idup : mant.idup;
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
		mant.length = 0;
		for(size_t i = 0; i < count; i++) {
			mant ~= '0';
		}
	}
	while(dot > mant.length) {
		mant ~= '0';
	}
	if (mant.length > dot) {
		insertInPlace(mant, dot, ".");
	}
	string str = mant.idup;
	if (adjx != 0) {
		string xstr = to!string(adjx);
		if (adjx > 0) {
			xstr = '+' ~ xstr;
		}
		str = str ~ "E" ~ xstr;
	}
	return signed ? "-" ~ str : str;
}  // end engForm()

unittest // engForm
{
	write("-- engForm..........");

	static string[] tests =
	[
		"1.23E+3",
		"1.23E-9",
		"-1.23E-12",
		"9.999999E+96",
		"1.000",
		"NaN",
		"-NaN102",
		"-Infinity",
		"-0",
	];

	foreach (i, str; tests)
	{
		assertEqual(engForm(dec9(str)), str, i);
	}
	writeln("passed");
}

private string specialForm(T)(in T num, bool shortForm = false) if (isDecimal!T)
{
	string str = num.sign ? "-" : "";
	if (num.isInfinite) {
		str ~= shortForm ? "Inf" : "Infinity";
	}
	else if (num.isNaN) {
		str ~= num.isSignaling ? "sNaN" : "NaN";
		if (num.payload) {
			str ~= to!string(num.payload);
		}
	}
//	if (lower) str = toLower(str);
//	else if (upper) str = toUpper(str);
	return str;
}

unittest  // specialForm
{
	write("-- specialForm......");

	static string[] tests =
	[
		"NaN",
		"-sNaN102",
		"-Infinity",
	];

	foreach (i, str; tests)
	{
		assertEqual(dec9(str).specialForm, str, i);
	}

	tests =
	[
		"Inf",
		"-Inf",
	];

	foreach (i, str; tests)
	{
		assertEqual(dec9(str).specialForm(true), str, i);
	}
	writeln("passed");
}

/// Converts a decimal number to a string in decimal format (xxx.xxx).
private string decimalForm(T)(in T number,
	int precision = DefaultPrecision) if (isDecimal!T)
{
	if (number.isSpecial) {
		return specialForm(number);
	}
	T num = number.dup;
	// check if rounding is needed:
	int diff = num.exponent + precision;
	if (diff < 0) {
		int numPrecision = num.digits + num.exponent + precision;
		num = roundToPrecision(num, numPrecision, T.maxExpo, T.rounding);
	}

	// convert the coefficient to a string
	char[] str = to!string(num.coefficient).dup;
	auto expo = num.exponent;
	auto sign = num.isSigned;
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

unittest // decimalForm
{
	write("-- decimalForm......");

	static struct S { string num; int precision; string str; }

	static S[] tests =
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

	foreach (i, s; tests)
	{
		assertEqual(decimalForm(dec9(s.num), s.precision), s.str, i);
	}
	writeln("passed");
}


/// Converts a decimal number to a string using exponential notation.
private string exponentForm(T)(in T number, int precision = DefaultPrecision,
	const bool lowerCase = false, const bool padExpo = true) if (isDecimal!T)
{

	if (number.isSpecial) {
		return specialForm(number);
	}
	T num = number.dup;
	num = roundToPrecision(num, precision + 1);
	char[] mant = to!string(num.coefficient).dup;
	auto expo = num.exponent;
	auto sign = num.isSigned;
	auto adjx = expo + mant.length - 1;
	if (mant.length > 1) {
		insertInPlace(mant, 1, ".");
	}
	string xstr = to!string(std.math.abs(adjx));
	if (padExpo && xstr.length < 2) {
		xstr = "0" ~ xstr;
	}
	xstr = adjx < 0 ? "-" ~ xstr : "+" ~ xstr;
	string expoChar = lowerCase ? "e" : "E";
	string str = (mant ~ expoChar ~ xstr).idup;
	return sign ? "-" ~ str : str;
}  // end exponentForm

unittest	// exponentForm
{
	write("-- exponentForm.....");

	static struct S { string num; int precision; string str; }

	static S[] tests =
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

	foreach (i, s; tests)
	{
		assertEqual(exponentForm(dec9(s.num), s.precision), s.str, i);
	}
	writeln("passed");
}

/// Returns a string representing the number, formatted as specified.
private string formatDecimal(T)(in T num,
	char formatChar, int precision) if (isDecimal!T)
{
	bool lowerCase = std.uni.isLower(formatChar);
	bool upperCase = std.uni.isUpper(formatChar);

	// special values
	if (num.isSpecial) {
		return specialForm!T(num, false);
	}

	switch (std.uni.toUpper(formatChar)) {
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
//		writeln("S branch");

			return sciForm(num.copyAbs);
		default:
			break;
	}
	return exponentForm(num, precision, lowerCase, true);
}

/// Returns a string that is at least as long as the specified width. If the
/// string is already greater than or equal to the specified width the original
/// string is returned. If the specified width is negative or if the
/// flag is set the widened string is left justified.
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

unittest // setWidth
{
	write("-- setWidth.........");
	auto str = "10E+05";
	assertEqual(setWidth(str,  8),			"  10E+05");
	assertEqual(setWidth(str, -8), 			"10E+05  ");
	assertEqual(setWidth(str,  8, true), 	"10E+05  ");
	assertEqual(setWidth(str,  8, false, true), "0010E+05");
	writeln("passed");
}

/// Returns an abstract string representation of a number.
/// The abstract representation is described in the specification. (p. 9-12)
public string toAbstract(T)(in T num) if (isDecimal!T)
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

unittest // toAbstract
{
	write("-- toAbstract.......");
	static struct S { string num; string abs; }

	static S[] tests =
	[
		{     "-inf",	"[1,inf]" },
		{      "nan",	"[0,qNaN]" },
		{ "snan1234",	"[0,sNaN1234]" },
	];

	foreach (i, s; tests)
	{
		assertEqual(dec9(s.num).toAbstract, s.abs, i);
	}
	writeln("passed");
}

/// Returns a full, exact representation of a number. Similar to toAbstract,
/// but it provides a valid string that can be converted back into a number.
public string toExact(T)(in T num) if (isDecimal!T)
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

unittest // toExact
{
	write("-- toExact..........");

	static string[] tests =
	[
		"+9999999E+90",
		"+1E+00",
		"+1000E-03",
		"+NaN",
		"-NaN102",
		"-Infinity",
		"-0E+00",
	];

	foreach (i, str; tests)
	{
		assertEqual(dec9(str).toExact, str, i);
	}
	writeln("passed");
}

/// Converts a string into a decimal number. This departs from the
/// specification in that the coefficient string may contain underscores.
/// A leading or trailing "." is allowed by the specification even though
/// it is not valid as a D language real number.
public T toNumber(T)(string inStr) if (isDecimal!T)
{
	T num;
	bool sign = false;
	// strip, copy, tolower
	char[] str = strip(inStr).dup;
	toLowerInPlace(str);
	// get sign, if any
	if (startsWith(str, "-")) {
		sign = true;
		str = str[1..$];
	} else if (startsWith(str, "+")) {
		str = str[1..$];
	}
	// check for NaN
	if (startsWith(str, "nan")) {
		num = T.nan(0, sign);
		// check for payload
		if (str.length > 3) {
			return setPayload(num, str, 3);
		}
		return num;
	}
	// check for sNaN
	if (startsWith(str, "snan")) {
		num = T.snan(0, sign);
		// check for payload
		if (str.length > 4) {
			return setPayload(num, str, 4);
		}
		return num;
	}
	// check for infinity
	if (str == "inf" || str == "infinity") {
		num = T.infinity(sign);
		return num;
	}
	// at this point, num must be finite
	num = T.zero(sign);
	// check for exponent
	ptrdiff_t  pos = indexOf(str, 'e');
	if (pos > 0) {
		// if it's just a trailing 'e', return NaN
		if (pos == str.length - 1) {
			return T.nan;
		}
		// split the string into coefficient and exponent
		char[] xstr = str[pos + 1..$];
		str = str[0..pos];
		// assume exponent is positive
		bool xneg = false;
		// check for minus sign
		if (startsWith(xstr, "-")) {
			xneg = true;
			xstr = xstr[1..$];
		}
		// check for plus sign
		else if (startsWith(xstr, "+")) {
			xstr = xstr[1..$];
		}
		// ensure it's not now empty
		if (xstr.length < 1) {
			return T.nan;
		}
		// ensure exponent is all digits
		foreach(char c; xstr) {
			if (!isDigit(c)) {
				return T.nan;
			}
		}
		// trim leading zeros
		while(xstr[0] == '0' && xstr.length > 1) {
			xstr = xstr[1..$];
		}
		// make sure it will fit into an int
		if (xstr.length > 10) {
			return T.nan;
		}
		if (xstr.length == 10) {
			// try to convert it to a long (should work) and
			// then see if the long value is too big (or small)
			long lex = std.conv.to!long(xstr);
			if ((xneg && (-lex < int.min)) || lex > int.max) {
				return T.nan;
			}
			num.exponent = cast(int) lex;
		} else {
			// everything should be copacetic at this point
			num.exponent = std.conv.to!int(xstr);
		}
		if (xneg) {
			num.exponent = -num.exponent;
		}
	} else {
		num.exponent = 0;
	}
	// remove trailing decimal point
	if (endsWith(str, ".")) {
		str = str[0..$ -1];
		// check for empty string (input was ".")
		if (str.length == 0) {
			return T.nan;
		}
	}
	// strip leading zeros
	while(str[0] == '0' && str.length > 1) {
		str = str[1..$];
	}
	// make sure first char is a digit
	// (or a decimal point, in which case check the second char)
	if (!isDigit(str[0])) {
		if (str[0] == '.' && !isDigit(str[1])) {
		 return T.nan;
		}
	}
	// strip underscores
	if (indexOf(str, '_') >= 0) {
  		str = removechars(str.idup, "_").dup;
	}
	// remove internal decimal point
	int point = cast(int)indexOf(str, '.');
	if (point >= 0) {
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
	foreach(char c; str) {
		if (!isDigit(c)) {
			return T.nan;
		}
	}
	// convert coefficient string to xint
	num.coefficient = xint(str.idup); //.trim; TODO: determine if trimming is too costly
	num.digits = numDigits(num.coefficient);
	return num;
}

unittest // toNumber
{
	write("-- toNumber.........");
	static struct S { string num; string str; }

	static S[] tests =
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
	];

	foreach (i, s; tests)
	{
		assertEqual(dec9(s.num).toString, s.str, i);
		auto num = dec9(s.num);
	auto len = num.coefficient.getDigitLength;
	for (int j = 0; j < len; j++)
	{
		auto digit = num.coefficient.getDigit(j);
		writefln("digit = %s", digit);
//			writefln("num.coefficient.getDigit(i) = %s", num.coefficient.getDigit(i));
	}
	}
	writeln("passed");
}

private T setPayload(T)(T num, char[] str, int len) if (isDecimal!T)
{
	// if finite number or infinity, return
	if (!num.isNaN) return num;
	// if no payload, return
	if (str.length == len) return num;
	// otherwise, get payload string
	str = str[len..$];
	// trim leading zeros
	while(str[0] == '0' && str.length > 1) {
		str = str[1..$];
	}
	// payload has a max length of 6 digits
	if (str.length > 6) return num;
	// TODO: can put previous checks into this foreach
	// ensure string is all digits
	foreach(char c; str) {
		if (!isDigit(c)) {
			return num;
		}
	}
	// convert string to number
	uint payload = std.conv.to!uint(str);
	// check for overflow
	if (payload > ushort.max) {
		return num;
	}
	num.payload = cast(ushort)payload;
	return num;
}

unittest // setPayload
{
	write("-- setPayload.......");
	static struct S { string num; string str; }

	static S[] tests =
	[
		{ "NaN167",		"NaN167" },
		// invalid payload is ignored
		{ "SNAN135ee",		"sNaN" },
		// leading zeros in payload are excised
		{ "-snan0170",		"-sNaN170" },
	];

	foreach (i, s; tests)
	{
		assertEqual(dec9(s.num).toString, s.str, i);
	}
	writeln("passed");
}


unittest {
	writeln("==========================");
	writeln("decimal conversion.....end");
	writeln("==========================");
}
