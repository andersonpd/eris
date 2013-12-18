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

import eris.decimal.context;
import eris.decimal.decimal;
import eris.decimal.rounding;
import eris.integer.extended;

unittest {
	writeln("==========================");
	writeln("decimalconversion...begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
}

//--------------------------------
//   to!string conversions
//--------------------------------

/// to!string(ExtendedInt).
T to(T:string)(const ExtendedInt num) {
	string outbuff = "";
	void sink(const(char)[] s) {
		outbuff ~= s;
	}
	num.toString(&sink, "%d");
	return outbuff;
}

/// to!string(int).
T to(T:string)(const long n) {
	return format("%d", n);
}

//--------------------------------
//  formatting
//--------------------------------

/// Converts a decimal number to a string
/// using "scientific" notation, per the spec.
public string sciForm(T)(const T num) {

	if (num.isSpecial) {
		string str = toSpecialString!T(num);
		return num.isSigned ? "-" ~ str : str;
	}

	char[] mant = to!string(num.coefficient).dup;
	int  expo = num.exponent;
	bool signed = num.isSigned;

	int adjx = expo + mant.length - 1;
	// if the exponent is small use dec9 notation
	if (expo <= 0 && adjx >= -6) {
		// if the exponent is not zero, insert a dec9 point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if the coefficient is too small, pad with zeroes
			if (point > mant.length) {
				mant = rightJustify(mant, point, '0');
			}
			// if no chars precede the dec9 point, prefix a zero
			if (point == mant.length) {
				mant = "0." ~ mant;
			}
			// otherwise insert the dec9 point into the string
			else {
				insertInPlace(mant, mant.length - point, ".");
			}
		}
		return signed ? ("-" ~ mant).idup : mant.idup;
	}
	// if the exponent is large enough use exponential notation
	if (mant.length > 1) {
		insertInPlace(mant, 1, ".");
	}
	string xstr = to!string(adjx);
	if (adjx >= 0) {
		xstr = "+" ~ xstr;
	}
	string str = (mant ~ "E" ~ xstr).idup;
	return signed ? "-" ~ str : str;
};  // end sciForm

unittest {	// sciForm
	write("-- toSciString......");
	dec9 num = dec9(123);
	assertStringEqual(sciForm(num), "123");
	assertStringEqual(num.toAbstract(), "[0,123,0]");
	num = dec9(-123, 0);
	assertStringEqual(sciForm(num), "-123");
	assertStringEqual(num.toAbstract(), "[1,123,0]");
	num = dec9(123, 1);
	assertStringEqual(sciForm(num), "1.23E+3");
	assertStringEqual(num.toAbstract(), "[0,123,1]");
	num = dec9(123, 3);
	assertStringEqual(sciForm(num), "1.23E+5");
	assertStringEqual(num.toAbstract(), "[0,123,3]");
	num = dec9(123, -1);
	assertStringEqual(sciForm(num), "12.3");
	assertStringEqual(num.toAbstract(), "[0,123,-1]");
	num = dec9("inf");
	assertStringEqual(sciForm(num), "Infinity");
	assertStringEqual(num.toAbstract(), "[0,inf]");
	writeln("passed");
}


/// Converts a decimal number to a string
/// using "engineering" notation, per the spec.
public string engForm(T)(const T num) /+if (isDecimal!T)+/ {

	if (num.isSpecial) {
		string str = toSpecialString!T(num);
		return num.isSigned ? "-" ~ str : str;
	}

	char[] mant = to!string(num.coefficient).dup;
	int  expo = num.exponent;
	bool signed = num.isSigned;

	int adjx = expo + mant.length - 1;
	// if exponent is small, don't use exponential notation
	if (expo <= 0 && adjx >= -6) {
		// if exponent is not zero, insert a dec9 point
		if (expo != 0) {
			int point = std.math.abs(expo);
			// if coefficient is too small, pad with zeroes
			if (point > mant.length) {
				mant = rightJustify(mant, point, '0');
			}
			// if no chars precede the dec9 point, prefix a zero
			if (point == mant.length) {
				mant = "0." ~ mant;
			}
			// otherwise insert a dec9 point
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
		for(int i = 0; i < count; i++) {
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

unittest {
	write("-- toEngString......");
	string str = "1.23E+3";
	dec9 dec = dec9(str);
	assertStringEqual(engForm(dec), str);
	str = "123E+3";
	dec = dec9(str);
	assertStringEqual(engForm(dec), str);
	str = "12.3E-9";
	dec = dec9(str);
	assertStringEqual(engForm(dec), str);
	str = "-123E-12";
	dec = dec9(str);
	assertStringEqual(engForm(dec), str);
	writeln("passed");
}

/// Returns a string representation of a special value.
/// If the number is not a special value an empty string is returned.
/// NOTE: The sign of the number is not included in the string.
private string toSpecialString(T)(const T num,
		bool shortForm = false, bool lower = false, bool upper = false) {

	string str = "";
	if (num.isInfinite) {
		str = shortForm ? "Inf" : "Infinity";
	}
	else if (num.isNaN) {
		str = !shortForm && num.isSignaling ? "sNaN" : "NaN";
		if (num.payload) {
			str ~= to!string(num.payload);
		}
	}
	if (lower) str = toLower(str);
	else if (upper) str = toUpper(str);
	return str;
}

unittest {
	write("-- toSpecialString..");
	dec9 num;
	string expect, actual;
	num = dec9("inf");
	actual = toSpecialString(num);
	expect = "Infinity";
	assertEqual(actual, expect);
	actual = toSpecialString(num, true);
	expect = "Inf";
	assertEqual(actual, expect);
	writeln("passed");
}

/// Converts a decimal number to a string in dec9 format (xxx.xxx).
/// NOTE: The sign of the number is not included in the string.
private string decimalForm(T)
	(const T number, const int precision = 6) {

	T num = number.dup;
	// check if rounding is needed:
	int diff = num.exponent + precision;
	if (diff < 0) {
		int numPrecision = num.digits + num.exponent + precision;
		num = roundToPrecision(num, numPrecision, contextMode);
	}

	// convert the coefficient to a string
	char[] mant = to!string(num.coefficient).dup;
	auto expo = num.exponent;
	auto sign = num.isSigned;
	if (expo >= 0) {
		if (expo > 0) {
			// add zeros up to the dec9 point
			mant ~= replicate("0", expo);
		}
		if (precision) {
			// add zeros trailing the dec9 point
			mant ~= "." ~ replicate("0", precision);
		}
	}
	else { // (expo < 0)
		int point = -expo;
		// if coefficient is too small, pad with zeros on the left
		if (point > mant.length) {
			mant = rightJustify(mant, point, '0');
			}
		// if no chars precede the dec9 point, prefix a zero
		if (point == mant.length) {
			mant = "0." ~ mant;
		}
		// otherwise insert a dec9 point
		else {
			insertInPlace(mant, mant.length - point, ".");
		}
		// if result is less than precision, add zeros
		if (point < precision) {
			mant ~= replicate("0", precision - point);
		}
	}
	return mant.idup;
//	return sign ? ("-" ~ mant).idup : mant.idup;
}

unittest {
	write("-- toDecimalForm....");
	dec9 num;
	string expect, actual;
	expect = "123.456789";
	num = "123.4567890123";
	actual = decimalForm(num);
	assertEqual(actual, expect);
	expect = "123.456790";
	num = "123.456789500";
	actual = decimalForm(num);
	assertEqual(actual, expect);
	num = 125;
	expect = "125.000";
	actual = decimalForm(num, 3);
	assertEqual(actual, expect);
	num = 125E5;
	expect = "12500000";
	actual = decimalForm(num, 0);
	assertEqual(actual, expect);
	num = 1.25;
	expect = "1.25";
	actual = decimalForm(num, 2);
	assertEqual(actual, expect);
	num = 125E-5;
	expect = "0.001250";
	actual = decimalForm(num, 6);
	assertEqual(actual, expect);
	writeln("passed");
}


/// Converts a decimal number to a string using exponential notation.
private string exponentForm(T)(const T number, const int precision = 6,
	const bool lowerCase = false, const bool padExpo = true) /+if (isDecimal!T)+/ {

	T num = number.dup;
	if (T.precision > precision + 1) {
		int numPrecision = precision + 1;
		num = roundToPrecision(num, numPrecision, contextMode);
	}
	char[] mant = to!string(num.coefficient).dup;
	auto expo = num.exponent;
	auto sign = num.isSigned;
	int adjx = expo + mant.length - 1;
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

unittest {
	write("-- toExponentForm...");
	dec9 num;
	string expect, actual;
	num = dec9("123.4567890123");
	actual = exponentForm!dec9(num);
	expect = "1.234568E+02";
	assertEqual(actual, expect);
	num = dec9("123.456789500");
	actual = exponentForm!dec9(num);
	assertEqual(actual, expect);
	num = dec9(125);
	expect = "1.25E+02";
	actual = exponentForm(num);
	assertEqual(actual, expect);
	expect = "1.25e+2";
	actual = exponentForm(num, 6, true, false);
	assertEqual(actual, expect);
	num = dec9(125E5);
	expect = "1.25E+07";
	actual = exponentForm(num,2);
	assertEqual(actual, expect);
	num = dec9(1.25);
	expect = "1.25E+00";
	actual = exponentForm(num);
	assertEqual(actual, expect);
	num = dec9(125E-5);
	expect = "1.25E-03";
	actual = exponentForm(num);
	assertEqual(actual, expect);
	writeln("passed");
}

private void writeTo(T)(const T num, scope void delegate(const(char)[]) sink,
	const char formatChar, const int precision) /+if (isDecimal!T)+/ {
}

unittest {
	write("writeTo...");
	writeln("test missing");
}

/// toString(num, width, precision, expo)
private string formatDecimal(T)(const T num,
	const char formatChar, const int precision) /+if (isDecimal!T)+/ {

	bool lowerCase = std.uni.isLower(formatChar);
	bool upperCase = std.uni.isUpper(formatChar);

	// special values
	if (num.isSpecial) {
		return toSpecialString!T(num, false, lowerCase, upperCase);
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
		default:
			break;
	}
	return exponentForm(num, precision, lowerCase, true);
}

/// Returns the string with the prefix inserted at the front. If the
/// prefix string is empty, returns the original string.
private string addPrefix(string str, string prefix) {
	if (prefix == "") {
		return str;
	}
	return prefix ~ str;
}

/*
/// Returns the string with a prefix inserted at the front.
/// The prefix character is based on the value of the flags.
/// If none of the flags are set, the original string is returned.
private string addPrefix(string str, bool minus, bool plus, bool space) {

	if (!minus && !plus && !space) return str;

	string prefix;
	if      (minus) prefix = "-";
	else if (plus) prefix = "+";
	else if (space) prefix = " ";
	return prefix ~ str;
}

unittest {	//addPrefix
	string str, expect, actual;
	str = "100.54";
	expect = "100.54";
	actual = addPrefix(str, "");
	assertEqual(actual, expect);
	assert(actual is expect);
	expect = "-100.54";
	actual = addPrefix(str, "-");
	assertEqual(actual, expect);
	expect = " 100.54";
	actual = addPrefix(str, " ");
	assertEqual(actual, expect);
	expect = "+100.54";
	actual = addPrefix(str, "+");
	assertEqual(actual, expect);
}*/

/// Returns a string that is at least as long as the specified width. If the
/// string is already greater than or equal to the specified width the original
/// string is returned. If the specified width is negative or if the
/// flag is set the widened string is left justified.
private string setWidth(const string str, int width,
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

unittest { // setWidth
	write("-- setWidth.........");
	string str, expect, actual;
	str = "10E+05";
	expect = "  10E+05";
	actual = setWidth(str, 8);
	assertEqual(actual, expect);
	expect = "10E+05  ";
	actual = setWidth(str, 8, true);
	assertEqual(actual, expect);
	expect = "10E+05  ";
	actual = setWidth(str, -8);
	assertEqual(actual, expect);
	expect = "0010E+05";
	actual = setWidth(str, 8, false, true);
	assertEqual(actual, expect);
	writeln("passed");
}

private void sink(const(char)[] str) {
    auto app = std.array.appender!(string)();
	app.put(str);
}

/// Returns a string representing the value of the number, formatted as
/// specified by the formatString.
public string toString(T)(const T num, const string formatString = "")
		/+if (isDecimal!T)+/ {

    auto a = std.array.appender!(const(char)[])();
	void sink(const(char)[] s) {
		a.put(s);
	}
	writeTo!T(num, &sink, formatString);
    auto f = FormatSpec!char(formatString);
    f.writeUpToNextSpec(a);
    string str = formatDecimal!T(num, f.spec, f.precision);
	// add trailing zeros
	if (f.flHash && str.indexOf('.' < 0)) {
		str ~= ".0";
	}
	// add prefix
	string prefix;
	if (num.isSigned)   prefix = "-";
	else if (f.flPlus)  prefix = "+";
	else if (f.flSpace) prefix = " ";
	else prefix = "";
	str = addPrefix(str, prefix);
	// adjust width
	str = setWidth(str, f.width, f.flZero, f.flDash);
	return str;
}

unittest {
/*	write("toString...");
	string expect, actual;
	Dec32 num = 2;
	actual = toString!Dec32(num, "%9.6e"); //3.3g41");
	actual = toString!Dec32(num, "%-9.6e"); //3.3g41");
	actual = toString!Dec32(num, "%9.6e"); //3.3g41");
	expect = "    2e+00";
	assertEqual(actual, expect);
	writeln("passed");*/
}


// TODO: Doesn't work yet. Uncertain how to merge the string versions
// with the sink versions.
/// Converts a decimal number to a string representation.
void writeTo(T)(const T num, scope void delegate(const(char)[]) sink,
		string fmt = "") /+if (isDecimal!T)+/ {


};  // end writeTo

/// Converts a string into a Decimal. This departs from the specification
/// in that the coefficient string may contain underscores.
/// A leading or trailing "." is allowed by the specification even though
/// it is not valid as a D language real number.
public T toNumber(T)(const string inStr) {
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
		num = T.nan(sign);
		// check for payload
		if (str.length > 3) {
			return setPayload(num, str, 3);
		}
		return num;
	}
	// check for sNaN
	if (startsWith(str, "snan")) {
		num = T.snan(sign);
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
	};
	// at this point, num must be finite
	num = T.zero(sign);
	// check for exponent
	int pos = indexOf(str, 'e');
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
	// remove trailing dec9 point
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
	// remove internal dec9 point
	int point = indexOf(str, '.');
	if (point >= 0) {
		// excise the point and adjust the exponent
		str = str[0..point] ~ str[point + 1..$];
		int diff = str.length - point;
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
	// convert coefficient string to ExtendedInt
	num.coefficient = ExtendedInt(str.idup);
	num.digits = numDigits(num.coefficient);
	return num;
}

unittest {	// toNumber
	write("-- toNumber.........");
	dec9 big;
	string expect, actual;
	big = dec9("1.0");
	expect = "1.0";
	actual = big.toString();
	assertEqual(actual, expect);
	big = dec9("-123");
	expect = "-123";
	actual = big.toString();
	assertEqual(actual, expect);
	big = dec9("1.23E3");
	expect = "1.23E+3";
	actual = big.toString();
	assertEqual(actual, expect);
	big = dec9("1.23E-3");
	expect = "0.00123";
	actual = big.toString();
	assertEqual(actual, expect);
	big = dec9("1.2_3E3");
	expect = "1.23E+3";
	actual = big.toString();
	assertEqual(actual, expect);
	// not valid for real numbers
	big = dec9(".1");
	expect = "0.1";
	actual = big.toString();
	assertEqual(actual, expect);
	// not valid for real numbers
	big = dec9("1.");
	expect = "1";
	actual = big.toString();
	assertEqual(actual, expect);
	// not valid for dec9 numbers
	big = dec9(".");
	expect = "NaN";
	actual = big.toString();
	assertEqual(actual, expect);
	// not valid for dec9 numbers
	big = dec9(".E3");
	expect = "NaN";
	actual = big.toString();
	assertEqual(actual, expect);
	// not valid for dec9 numbers
	big = dec9("+.");
	expect = "NaN";
	actual = big.toString();
	assertEqual(actual, expect);
	writeln("passed");
}

//public decPX toNumber(int P, int X)(const string inStr) {
private T setPayload(T)(T num, char[] str, int len) {
	// if no payload, return
	if (str.length == len) {
			return num;
	}
	// get payload
	str = str[len..$];
	// trim leading zeros
	while(str[0] == '0' && str.length > 1) {
		str = str[1..$];
	}
	// payload has a max length of 6 digits
	if (str.length > 6) return num;
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

unittest {
	write("setPayload...");
	writeln("test missing");
}

/// Returns an abstract string representation of a number.
/// The abstract representation is described in the specification. (p. 9-12)
public string toAbstract(T)(const T num) /*/+if (isDecimal!T)+/*/ {
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

unittest {	// toAbstract
	write("-- toAbstract.......");
	dec9 num;
	string str;
	num = dec9("-inf");
	str = "[1,inf]";
	assert(num.toAbstract == str);
	num = dec9("nan");
	str = "[0,qNaN]";
	assert(num.toAbstract == str);
	num = dec9("snan1234");
	str = "[0,sNaN1234]";
	assert(num.toAbstract == str);
	writeln("passed");
}

// (V)TODO: Does exact representation really return a round-trip value?
/// Returns a full, exact representation of a number. Similar to toAbstract,
/// but it provides a valid string that can be converted back into a number.
public string toExact(T)(const T num) {
	if (num.isFinite) {
		return format("%s%sE%s%02d", num.sign ? "-" : "+",
		              to!string(num.coefficient),
		              num.exponent < 0 ? "-" : "+", std.math.abs(num.exponent));
	}
	if (num.isInfinite) {
		return format("%s%s", num.sign ? "-" : "+", "Infinity");
	}
	if (num.isQuiet) {
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

unittest {
	write("-- toExact..........");
	dec9 num, copy;
	string expect, actual;
	assertStringEqual(num.toExact, "+NaN");
	copy = num.toExact;
	assertStringEqual(num.toAbstract, copy.toAbstract);
	num = +9999999E+90;
	actual = num.toExact;
	expect = "+9999999E+90";
	assertEqual(actual, expect);
	copy = dec9(actual);
	assertEqual(num, copy);
	assertStringEqual(num.toAbstract, copy.toAbstract);
	num = 1;
	actual = num.toExact;
	expect = "+1E+00";
	assertEqual(actual, expect);
	copy = dec9(actual);
	assertEqual(num, copy);
	assertStringEqual(num.toAbstract, copy.toAbstract);
	num = dec9("1.000");
	actual = num.toExact;
	expect = "+1000E-03";
	assertEqual(actual, expect);
	copy = dec9(actual);
	assertEqual(num, copy);
	assertStringEqual(num.toAbstract, copy.toAbstract);
	num = dec9.infinity(true);
	actual = num.toExact;
	expect = "-Infinity";
	assertEqual(actual, expect);
	copy = dec9(actual);
	assertEqual(num, copy);
	assertStringEqual(num.toAbstract, copy.toAbstract);
	writeln("passed");
}

unittest {
	writeln("==========================");
	writeln("decimal conversion.....end");
	writeln("==========================");
}
