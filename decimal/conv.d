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
import std.ascii: isDigit;
import std.bitmanip : bitfields;
import std.string;
import std.format;
import std.stdio;
import std.traits;
static import std.uni;
static import std.conv;
static import std.math;

import eris.decimal;

unittest {
	writeln("     conversion tests     ");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.decimal.test;
  import eris.decimal.math : M_PI;
}

public enum DEFAULT_PRECISION = 6;

//--------------------------------
//   to!string conversions
//--------------------------------

/// to!string(BigInt).
public T to(T:string)(in BigInt x) {
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
 */
public string toString(D)(in D num, string fmStr = "%s") if (isDecimal!D)
{
    auto fm = singleSpec!char(fmStr.dup);
  string str = "";
  if (num.isNegative) str = "-";
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
{  // toString
  static struct S { TD num; string fmt; string str; }
  S[] s =
  [
    // default format is scientific form
    { "123",    "%s",  "123" },
    { "-123",    "%S",  "-123" },
    { "12.3E1",    "%S",  "123" },
    { "123E1",    "%S",  "1.23E+3" },
    { "123E3",    "%S",  "1.23E+5" },
    { "123E-1",    "%S",  "12.3" },
    { "50E-7",    "%S",  "0.0000050" },
    { "5E-7",    "%S",  "5E-7" },
    { "12.3456789",  "%S",  "12.3456789" },
    { "12.34567",  "%s",  "12.34567" },
    { "12.345",    "%S",  "12.345"   },
    // exponential form
    { "12.3456789",  "%E",  "1.234568E+01" },
    { "12.34567",  "%e",  "1.234567e+01" },
    { "12.345",    "%E",  "1.2345E+01"   },
    // decimal form
    { "12.3456789",  "%F",  "12.345679" },
    { "12.34567",  "%F",  "12.345670" },
    { "12.345",    "%F",  "12.345000" },
    // decimal or exponential. note change in meaning of precision
    { "12.3456789",  "%G",  "1.234568E+01" },
    { "12.34567",  "%G",  "1.234567E+01" },
    { "12.345",    "%G",  "12.345000"    },
    // width
    { "12.34567",  "%12.4G",  "  1.2346E+01" },
    { "12.345",    "%12G",  "   12.345000" },
    // flags
    { "12.34567",  "%G",  "1.234567E+01"},
    { "12.345",    "%+G",    "+12.345000"   },
    { "12.345",    "%-12G",  "12.345000   " },
    { "12.34567",  "%-12.4G",  "1.2346E+01  " },
    { "12.345",    "%012G",  "00012.345000" },
    // zero flag ignored if precision is specified
    { "12.345",     "%012.4G",  "     12.3450" },
    // zero flag, upper/lower case  ignored if infinity or nan
    { "Inf",     "%012.4G",  "    Infinity" },
    { "NaN",     "%012.4g",  "         NaN" },
    // if hash, print trailing zeros.
    { "1234567.89",   "%.0G",  "1234568" },
    { "1234567.89",   "%.0F",  "1234568" },
    { "1234567",   "%.0F",  "1234567" },
    { "123",     "%#.0F",  "123.0" },
  ];
  auto f = FunctionTest!(S,string)("toString");
  foreach (t; s) f.test(t, toString(t.num, t.fmt));
  writefln(f.report);
}

/*    void toString(D)(
		scope void delegate(const(char)[]) sink,
		ref FormatSpec!char f) if (isDecimal!D) const
	{

	}*/
//--------------------------------
//  formatting
//--------------------------------

/**
 *  Converts a decimal number to a string
 *  using "scientific" notation, per the spec.
 */
public string sciForm(D)(in D num) if (isDecimal!D)
{
	if (num.isSpecial)
    {
		return specialForm(num);
	}

	char[] str = to!string(num.coff).dup;
	int  expo = num.expo;
	bool signed = num.isNegative;

	int adjx = expo + cast(int)str.length - 1;
	// if the exponent is small use decimal notation
	if (expo <= 0 && adjx >= -6)
    {
		// if the exponent is not zero, insert a decimal point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if the coefficient is too small, pad with zeroes
			if (point > str.length) {
				str = rightJustify(str, point, '0');
			}
			// if no chars precede the decimal point, prefix a zero
			if (point == str.length) {
				str = "0." ~ str;
			}
			// otherwise insert the decimal point into the string
			else {
				insertInPlace(str, str.length - point, ".");
			}
		}
		return signed ? ("-" ~ str).dup : str.dup;
	}
	// if the exponent is large enough use exponential notation
	if (str.length > 1) {
		insertInPlace(str, 1, ".");
	}

	string expStr = to!string(adjx);
	if (adjx >= 0) {
		expStr = "+" ~ expStr;
	}
	string s = (str ~ "E" ~ expStr).dup;
	return signed ? "-" ~ s : s;
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
public string engForm(D)(in D num) if (isDecimal!D)
{
	if (num.isSpecial) {
		return specialForm(num);
	}

	char[] cof = to!string(num.coff).dup;
	int  expo = num.expo;
	bool signed = num.isNegative;

	int adjx = expo + cast(int)cof.length - 1;
	// if exponent is small, don't use exponential notation
	if (expo <= 0 && adjx >= -6) {
		// if exponent is not zero, insert a decimal point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if coefficient is too small, pad with zeroes
			if (point > cof.length) {
				cof = rightJustify(cof, point, '0');
			}
			// if no chars precede the decimal point, prefix a zero
			if (point == cof.length) {
				cof = "0." ~ cof;
			}
			// otherwise insert a decimal point
			else {
				insertInPlace(cof, cof.length - point, ".");
			}
		}
		return signed ? ("-" ~ cof).idup : cof.idup;
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
		cof.length = 0;
		for (size_t i = 0; i < count; i++) {
			cof ~= '0';
		}
	}
	while (dot > cof.length) {
		cof ~= '0';
	}
	if (cof.length > dot) {
		insertInPlace(cof, dot, ".");
	}
	string str = cof.idup;
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
private string specialForm(D)(in D num, bool shortForm = false) if (isDecimal!D)
{
	string str = num.sign ? "-" : "";
	if (num.isInfinite)
	{
		str ~= shortForm ? "Inf" : "Infinity";
	}
	else if (num.isNaN)
	{
		str ~= num.isSignal ? "sNaN" : "NaN";
		if (num.coff)
		{
			str ~= to!string(num.coff);
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
		{ "-Infinity",	"-Inf" },
	];
	foreach (t; r) f.test(t, specialForm(TD(t.num),true));
  writefln(f.report);
}

/**
 *  Converts a decimal number to a string in decimal format.
 *
 *  Returns e.g. "125E-5" as "0.001250" with no exponent.
 *  Numbers with large or small exponents will return long strings.
 *  Numbers with very large or very small exponents will return very long strings.
 */
private string decimalForm(D)(in D num, int precision = DEFAULT_PRECISION)
    if (isDecimal!D)
{
  // handle special numbers
  if (num.isSpecial)
  {
    return specialForm(num);
  }

  // create a mutable copy
  D copy = num.copy;

  // check if rounding is needed:
  if (copy.expo + precision < 0)
  {
    int numPrecision = copy.digits + copy.expo + precision;
    copy = round(copy, numPrecision);
  }

  // convert the coefficient to a string
  char[] str = to!string(copy.coff).dup;
  int exp = copy.expo;
  bool sign = copy.isNegative;
  if (exp >= 0) {
    if (exp > 0) {
      // add zeros up to the decimal point
      str ~= replicate("0", exp);
    }
    if (precision) {
      // add zeros trailing the decimal point
      str ~= "." ~ replicate("0", precision);
    }
  }
  else { // (exp < 0)
    int point = -exp;
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
{  // decimalForm
  static struct S { TD num; int precision; string str; }
  S[] s =
  [
    { "12.345",  6,  "12.345000" },
    { "125",  3,  "125.000" },
    { "-125",  3,  "-125.000" },
    { "125E5",  0,  "12500000" },
    { "1.25",  2,  "1.25" },
    { "125E-5",  6,  "0.001250" },
    { "-0",    6,  "-0.000000" },
    { "Inf",  0,  "Infinity" },
    { "-NaN",  4,  "-NaN" },
    { "123.4567890123",  6,  "123.456789" },
    { "123.4567895123",  6,  "123.456790" },
  ];
  auto f = FunctionTest!(S,string)("decForm");
  foreach (t; s) f.test(t, decimalForm(t.num, t.precision));
  writefln(f.report);
}

/**
 *  Converts a decimal number to a string using exponential notation.
 */
private string exponentForm(D)(in D number, int precision = DEFAULT_PRECISION,
	const bool lowerCase = false, const bool padExpo = true) if (isDecimal!D)
{

	if (number.isSpecial) {
		return specialForm(number);
	}
	D num = number.dup;
	num = round(num, precision + 1);
	char[] cof = to!string(num.coff).dup;
	int exp = num.expo;
	bool sign = num.isNegative;
	int adjx = exp + cast(int)cof.length - 1;
	if (cof.length > 1) {
		insertInPlace(cof, 1, ".");
	}
	string expStr = to!string(std.math.abs(adjx));
	if (padExpo && expStr.length < 2) {
		expStr = "0" ~ expStr;
	}
	expStr = adjx < 0 ? "-" ~ expStr : "+" ~ expStr;
	string expoChar = lowerCase ? "e" : "E";
	string str = (cof ~ expoChar ~ expStr).idup;
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
private string formatDecimal(D)(in D num,
	char formatChar, int precision) if (isDecimal!D)
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
//	return exponentForm(num, precision, lowerCase, true);
	case 'G':
		int exp = num.expo;
		if (exp > -5 && exp < precision) {
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
public string abstractForm(D)(in D num) if (isDecimal!D)
{
	if (num.isFinite) {
		return format("[%d,%s,%d]", num.sign ? 1 : 0,
		              to!string(num.coff), num.expo);
	}
	if (num.isInfinite) {
		return format("[%d,%s]", num.sign ? 1 : 0, "inf");
	}
	if (num.isQuiet) {
		if (num.coff) {
			return format("[%d,%s%d]", num.sign ? 1 : 0, "qNaN", num.coff);
		}
		return format("[%d,%s]", num.sign ? 1 : 0, "qNaN");
	}
	if (num.isSignal) {
		if (num.coff) {
			return format("[%d,%s%d]", num.sign ? 1 : 0, "sNaN", num.coff);
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
public string fullForm(D)(in D num) if (isDecimal!D)
{
	if (num.isFinite) {
		return format("%s%sE%s%02d", num.sign ? "-" : "+",
		              to!string(num.coff),
		              num.expo < 0 ? "-" : "+", std.math.abs(num.expo));
	}
	if (num.isInfinite)
	{
		return format("%s%s", num.sign ? "-" : "+", "Infinity");
	}
	if (num.isQuiet)
	{
		if (num.coff) {
			return format("%s%s%d", num.sign ? "-" : "+", "NaN", num.coff);
		}
		return format("%s%s", num.sign ? "-" : "+", "NaN");
	}
	if (num.isSignal) {
		if (num.coff) {
			return format("%s%s%d", num.sign ? "-" : "+", "sNaN", num.coff);
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
 *  not valid as a D language real.
 */
public D fromString(D)(string inStr, bool round = true) if (isDecimal!D)
{
	D num;

	// copy, strip, tolower
	char[] str = inStr.dup;
  str = strip(str);
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
		num = D.nan(0, sign);
		// check for payload
		if (str.length > 3) {
			return setPayload(num, str, 3);
		}
		return num;
	}

	// check for sNaN
	if (startsWith(str, "snan"))
	{
		num = D.snan(0, sign);
		// check for payload
		if (str.length > 4) {
			return setPayload(num, str, 4);
		}
		return num;
	}

	// check for infinity
	if (str == "inf" || str == "infinity")
	{
		num = D.infinity(sign);
		return num;
	}

	// at this point, num must be finite
	num = D.zero(sign);

	// check for exponent
	ptrdiff_t  pos = indexOf(str, 'e');
	if (pos > 0) 	// if exponent string found...
	{
		// exponent string must be at least two chars
		if (pos == str.length - 1)
		{
			return D.nan;
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
			return D.nan;
		}
		// ensure exponent is all digits
		foreach (char c; expStr)
		{
			if (!isDigit(c)) {
				return D.nan;
			}
		}
		// trim leading zeros
		while (expStr[0] == '0' && expStr.length > 1) {
			expStr = expStr[1..$];
		}
		// make sure it will fit into an int
		if (expStr.length > 10) {
//			writefln("expStr = %s", expStr);
			return D.nan;
		}
		if (expStr.length == 10) {
			// try to convert it to a long (should work) and
			// then see if the long value is too big (or small)
			long lex = std.conv.to!long(expStr);
			if ((expSign && (-lex < int.min)) || lex > int.max) {
				return D.nan;
			}
			num.expo = cast(int) lex;
		} else {
			// everything should be copacetic at this point
			num.expo = std.conv.to!int(expStr);
		}
		if (expSign)
		{
			num.expo = -num.expo;
		}
	}
	else	// no exponent
	{
		num.expo = 0;
	}

	// remove trailing decimal point
	if (endsWith(str, "."))
	{
		str = str[0..$ -1];
		// check for empty string (input was ".")
		if (str.length == 0) {
			return D.nan;
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
			return D.nan;
		}
		// ensure first char is a decimal point and second char is a digit
		if (str[0] == '.' && !isDigit(str[1]))
		{
		 return D.nan;
		}
	}
	// strip out any underscores
	str = str.replace("_", "");

	// remove internal decimal point
	int point = indexOf(str, '.');
	if (point >= 0)
	{
		// excise the point and adjust the exponent
		str = str[0..point] ~ str[point + 1..$];
		int diff = cast(int)str.length - point;
		num.expo = num.expo - diff;
	}
	// TODO: how can this happen? is it possible? assert?
	// ensure string is not empty
	if (str.length < 1) {
		return D.nan;
	}
	// strip leading zeros again
	while (str[0] == '0' && str.length > 1)
	{
		str = str[1..$];
	}
	// ensure chars are all digits
	foreach (char c; str) {
		if (!isDigit(c)) {
			return D.nan;
		}
	}
	// convert coefficient string to BigInt
	num.coff = BigInt(str.idup);
	// by convention, a zero coefficient has zero digits
	num.digits = (num.coff) ? str.length : 0;
	return num;
}

unittest
{	// fromString
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
	auto f = FunctionTest!(S,TD)("fromString");
	foreach (t; s) f.test(t, fromString!TD(t.num));
  writefln(f.report);
}

private D setPayload(D)(in D num, char[] str, int len) if (isDecimal!D)
{
//if (!__ctfe) writefln("str = %s", str);
	D copy = num.copy;
	// if finite number or infinity, return
	if (!num.isNaN) return copy;
	// if no payload, return
	if (str.length == len) return copy;
	// otherwise, get payload string
	str = str[len..$];
	// trim leading zeros
//if (!__ctfe) writefln("str = %s", str);
//	BigInt payload = BigInt(str);
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
//	uint payload = std.conv.to!uint(str);
	BigInt payload = std.conv.to!uint(str);
//if (!__ctfe) writefln("payload = %d", payload);
//	BigInt payload2 = BigInt(payload);
//if (!__ctfe) writefln("payload2 = %s", payload2);
	// check for overflow
	if (payload > ushort.max) {
		return copy;
	}
//if (!__ctfe) writefln("copy.coff = %s", copy.coff);
	copy.coff = payload;
//if (!__ctfe) writefln("copy.coff = %s", copy.coff);
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

// Binary Integer Decimal (BID) representation

struct Bid32Rep
{
    union
    {
        uint bid;
		// NaN
        mixin(bitfields!(
            uint,  "padNan", 25,
            ubyte, "testNan", 6,
            bool,  "signNan", 1));
        // infinity
		mixin(bitfields!(
            uint,  "padInf", 26,
            ubyte, "testInf", 5,
            bool,  "signInf", 1));
        // explicit representation
		mixin(bitfields!(
            uint,  "coffEx", 23,
            ushort,"expoEx",  8,
            bool,  "signEx",  1));
		// implicit representation
        mixin(bitfields!(
            uint,  "coffIm", 21,
            ushort,"expoIm",  8,
			ubyte, "testIm",  2,
            bool,  "signIm",  1));
    }
    enum uint bias = 101, fractionBits = 23, exponentBits = 8, signBits = 1;
}

struct BiD16Rep
{
    union
    {
        ulong bid;
		// NaN
        mixin(bitfields!(
            ulong, "padNan", 57,
            ubyte, "testNan", 6,
            bool,  "signNan", 1));
        // infinity
		mixin(bitfields!(
            ulong, "padInf", 58,
            ubyte, "testInf", 5,
            bool,  "signInf", 1));
        // explicit representation
		mixin(bitfields!(
            ulong, "coffEx", 53,
            ushort,"expoEx", 10,
            bool,  "signEx",  1));
		// implicit representation
        mixin(bitfields!(
            ulong, "coffIm", 51,
            ushort,"expoIm", 10,
			ubyte, "testIm",  2,
            bool,  "signIm",  1));
    }
    enum uint bias = 398, fractionBits = 53, exponentBits = 10, signBits = 1;
}

/// binary integer decimal form
public U toBid(D, U = ulong)(const D num)
	if (isDecimal!D && (is(U == ulong) || is(U == uint)))
{

	U sig;	// value of a signaling NaN
	U nan;	// value of a quiet NaN
	U inf;	// value of infinity
	U maxExpl;	// maximum explicit coefficient
	U maxImpl;	// maximum implicit coefficient
	U impBits;	// set if implicit
	U impMask;	// implicit coefficient mask
	U signBit;	// set if signed

	uint bits;	// number of bits in the coefficient
	uint bias;	// the exponent bias

	D rnum;		// a rounded copy of the argument

    static if (is(U == ulong))
    {
		sig  = 0x7E00000000000000;
		nan  = 0x7C00000000000000;
		inf  = 0x7800000000000000;
		maxExpl = 0x1FFFFFFFFFFFFF;  // = 9007199254740991
		maxImpl = 9999999999999999;  // = 0x2386F26FC0FFFF
		impBits = 0x6000000000000000;
		impMask = 0x7FFFFFFFFFFFFF;
		signBit = 0x8000000000000000;
		bits = 53;
        bias = 398;
	}
    else if (is(U == uint))
    {
		sig  = 0x7E000000;
		nan  = 0x7C000000;
		inf  = 0x78000000;
		maxExpl = 0x7FFFFF;  // = 8388607
		maxImpl = 9999999;  // = 0x98967F
		impBits = 0x60000000;
		impMask = 0x7FFFFF;
		signBit = 0x80000000;
		bits = 23;
        bias = 101;
	}

	// NaNs : sign bit is ignored
	if (num.isNaN)
	{
		return num.isQuiet ? nan : sig;
	}

	// infinities
	if (num.isInfinite)
	{
		return num.sign ? inf | signBit : inf;
	}

    static if (is(U == ulong))
    {
		rnum = round(num, Bid64);
	}
    else if (is(U == uint))
    {
		rnum = round(num, Bid32);
	}

	// check for overflow
	if (rnum.isInfinite)
	{
		return rnum.sign ? inf | signBit : inf;
	}

	U bid = 0;
	U coff = cast(U)rnum.coff.toLong;
	U expo = rnum.expo + bias;

	// explicit representation
	if (coff <= maxExpl)
	{
		bid |= coff;
		bid |= expo << bits;
	}
	// implicit representation
	else
	{
		bid = impBits; 	// set the implicit bits
		coff &= impMask;	// remove the three leading bits
		bid |= coff;		// coefficient is always < long.max
		bid |= expo << (bits - 2);
	}
	if (num.isNegative)
	{
		bid |= signBit;	// set sign bit
	}
	return bid;
}

unittest
{	// toBid
    static struct S { TD num; ulong bid; }
    S[] s =
    [
        { "NaN",	0x7C00000000000000 },
        { "-NaN",	0x7C00000000000000 },
        { "0.0",	0x31A0000000000000 },
        { "0.0E1",	0x31C0000000000000 },
        { "0.0E2",	0x31E0000000000000 },
        { "0.0E3",	0x3200000000000000 },
        { "sNaN",	0x7E00000000000000 },
        { "-sNaN",	0x7E00000000000000 },
        { "Inf",	0x7800000000000000 },
        { "-Inf",	0xF800000000000000 },
        { "0",		0x31C0000000000000 },
        { "1",		0x31C0000000000001 },
        { "2",		0x31C0000000000002 },
        { ".1",		0x31A0000000000001 },
        { "-1",		0xB1C0000000000001 },
        { "2.0E3",	0x3200000000000014 },
        { "2E3",	0x3220000000000002 },
        { "20E2",	0x3200000000000014 },
        { M_PI!TD, 	0x2FEB29430A256D21 },
        { 9007199254740991, 0x31DFFFFFFFFFFFFF },
        { 9007199254740992, 0x6C70000000000000 },
    ];
    auto f = FunctionTest!(S, ulong, "%016X")("toBid");
    foreach (t; s) f.test(t, toBid!(TD,ulong)(t.num));
  writefln(f.report);
}

unittest
{	// toBid
	static struct S { TD num; uint bid; }
	S[] s =
	[
		{ "NaN",		0x7C000000 },
		{ "-NaN",		0x7C000000 },
		{ "0",			0x32800000 },
		{ "0.0",		0x32000000 },
		{ "0E1",		0x33000000 },
		{ "0E2",		0x33800000 },
		{ "0.0E3",		0x33800000 },
		{ "sNaN",		0x7E000000 },
		{ "-sNaN",		0x7E000000 },
		{ "Inf",		0x78000000 },
		{ "-Inf",		0xF8000000 },
		{ "0",			0x32800000 },
		{ "1",			0x32800001 },
		{ "2",			0x32800002 },
		{ ".1",			0x32000001 },
		{ "-1",			0xB2800001 },
		{ "2.0E3",		0x33800014 },
		{ "2E3",		0x34000002 },
		{ "20E2",		0x33800014 },
		{ "3.14159265", 0x2FAFEFD9 },
		{ "3.141593",   0x2FAFEFD9 },
		{ 8388607, 		0x32FFFFFF },
		{ 8388608, 		0x6CA00000 },
	];
	auto f = FunctionTest!(S, uint, "%08X")("toBid");
	foreach (t; s) f.test(t, toBid!(TD,uint)(t.num));
  writefln(f.report);
}


public D fromBid(D, U = ulong)(U bid)
	if (isDecimal!D && (is(U == ulong) || is(U == uint)))
{
	int bits;
	bool sign;

    static if (is(U == ulong))
    {
		BiD16Rep rep;
		rep.bid = bid;
		bits = rep.fractionBits;
	}
	else
	{
		Bid32Rep rep;
		rep.bid = bid;
		bits = rep.fractionBits;
	}

	// NaN
	if (rep.testNan == 0x3E) return D.nan;
	if (rep.testNan == 0x3F) return D.snan;

	// infinity
	sign = rep.signInf;
	if (rep.testInf == 0x1E)
	{
		return sign ? -D.infinity : D.infinity;
    }

	D num = 0;	// initialize to zero -- not NaN
    // explicit coefficient
	if (rep.testIm < 3)
	{
		num.coff = rep.coffEx;
		num.expo = cast(int)rep.expoEx - rep.bias;
		num.sign = sign;
	}
	else
	{
		// implicit coefficient
		immutable one = cast(U)4 << (bits - 2);
        num.coff = one | rep.coffIm;
		num.expo = rep.expoIm - rep.bias;
		num.sign = sign;
	}
	return sign ? -num : num;
}

unittest
{	// fromBid
	static struct S {ulong bid; TD expect; }
	S[] s =
	[
		{ 0x7C00000000000000,  TD.nan  },
		{ 0xFE00000000000000,  TD.snan },	// sign is ignored
		{ 0x7800000000000000,  TD.infinity  },
		{ 0xF800000000000000,  -TD.infinity  },
		{ 0x31C0000000000000,  0 },
		{ 0x3220000000000002,  "2E3"  },
		{ 0x31DFFFFFFFFFFFFF,  9007199254740991 },	// largest explicit
		{ 0x6C70000000000000,  9007199254740992 },   // smallest implicit
		{ 0x2FEB29430A256D21,  M_PI!TD    },
	];
	auto f = FunctionTest!(S, TD, "%s")("fromBid(UL)");
	foreach (t; s) f.test(t, fromBid!(TD,ulong)(t.bid));
  writefln(f.report);
}

unittest
{	// fromBid
	static struct S {uint bid; TD expect; }
	S[] s =
	[
		{ 0x7C000000,  TD.nan  },
		{ 0xFE000000,  TD.snan },	// sign is ignored
		{ 0x78000000,  TD.infinity  },
		{ 0xF8000000, -TD.infinity  },
		{ 0x32800000,  0 },
		{ 0x34000002,  "2E3"  },
//		{ 0x34000002,  2000  },
		{ 0x32FFFFFF,  8388607 },	// largest explicit
		{ 0x6CA00000,  8388608 },   // smallest implicit
	];
	auto f = FunctionTest!(S, TD, "%s")("fromBid(U)");
	foreach (t; s) f.test(t, fromBid!(TD,uint)(t.bid));
  writefln(f.report);
}

unittest
{
	real r = 123.456E2;
	TD num;
	num = TD(2.0L);
	num = TD(r);
	num = TD(231.89E+112);
}

struct RealRep
{
	union
	{
		real value;
		ulong[2] word;
	}
	ulong fraction;
	ushort exponent;
	enum uint bias = 16383, signBits = 1, fractionBits = 64, exponentBits = 15;

	this(real bin) {
		value = bin;
		fraction = word[0];
		exponent = cast(ushort)word[1];	// TODO: remove sign bit?
	}
}

public D fromBinary(D,U)(in U bin)
	if (isDecimal!D && isFloatingPoint!U)
{
	if (std.math.isNaN(bin)) return D.nan;

	bool sign = bin < 0;
	if (!std.math.isFinite(bin))
	{
		if (sign)
		{
			return -D.infinity;
   		}
		else
		{
			return D.infinity;
		}
	}
	if (bin == 0.0) return D(0,0,sign);

	D num;
	int expo;
	BigInt one;
	U bing = sign ? -bin : bin;

    static if (is(U == real))
    {
		RealRep rep = RealRep(bing);
		num = BigInt(rep.fraction);
   		expo = rep.exponent - rep.bias - rep.fractionBits + 1;
	}
    else if (is(U == float))
    {
		std.bitmanip.FloatRep rep;
		rep.value = bing;
		one = BigInt(1) << rep.fractionBits;
		num = one | rep.fraction;
   		expo = rep.exponent - rep.bias - rep.fractionBits;
	}
    else	// double
    {
		std.bitmanip.DoubleRep rep;
		rep.value = bing;
		one = BigInt(1) << rep.fractionBits;
		num = one | rep.fraction;
   		expo = rep.exponent - rep.bias - rep.fractionBits;
	}

	bool divide = expo < 0;
	if (divide) {
		expo = -expo;
	}
	// NOTE: at some point the toString/fromString method must be more
	// efficient than an enormous shifted BigInt. 1 << 16384 ??
	// say expo > 255?
	if (expo > 127)
	{
		string str = std.format.format!"%.18G"(bin);
		return fromString!D(str);
	}
	auto mult = BigInt(1) << expo;
	num = divide ? num/mult : num*mult;

    static if (is(U == real))
    {
		num = reduce(num, RealContext);
	}
	else if (is(U == double))
    {
		num = reduce(num, DoubleContext);
	}
	else
	{
		num = reduce(num, FloatContext);
	}
	return sign ? -num : num;
}

unittest {
	writeln("==========================");
}
