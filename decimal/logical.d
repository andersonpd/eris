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

/*
	TODO: For each function --
		1. Ensure algorithm is properly implemented.
		2. Ensure context flags are being set properly.
		3. Ensure function passes GDA tests.
		4. Determine additional tests needed and implement them.
		5. Ensure all special cases are enumerated.
		6. Automate all tests, if possible.
		7. Determine what effect the context has on the function and
			if it should be explained.
		8. Ensure all documentation is complete:
			a. Header - description, inputs, return value(s)
			b. Code - variables, control statements, branches, return points.
		9. Move most tests to the test module.
*/

module eris.decimal.logical;

import std.string;

import eris.decimal;
import eris.decimal.arithmetic: invalidOperation, invalidOperand;

unittest {
	writeln("==========================");
	writeln("decimal logic........begin");
	writeln("==========================");
}

// temporary import
import std.stdio;

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
}

//--------------------------------
// logical operations
//--------------------------------

/// Returns true if the argument is a valid logical string.
/// All characters in a valid logical string must be either '1' or '0'.
private bool isLogicalString(const string str) {
	foreach(char ch; str) {
		if (ch != '0' && ch != '1') return false;
	}
	return true;
}

/// Returns true if the argument is a valid logical decimal number.
/// The sign and exponent must both be zero, and all decimal digits
/// in the coefficient must be either '1' or '0'.
public bool isLogical(T)(in T arg) if (isDecimal!T)
{
	if (arg.sign != 0 || arg.exponent != 0) return false;
	string str = arg.coefficient.toString;
	return isLogicalString(str);
}

/// Returns true and outputs a valid logical string if the argument is
/// a valid logical decimal number.
/// The sign and exponent must both be zero, and all decimal digits
/// in the coefficient must be either '1' or '0'.
private bool isLogicalOperand(T)(T x, string str) if (isDecimal!T)
{
	if (x.sign != 0 || x.exponent != 0) return false;
	return isLogicalString(str);
}

unittest {	// logical string/number tests
 	write("-- logical tests....");
	assertTrue(isLogicalString("010101010101"));
	assertTrue(isLogical(dec9("1010101")));
	writeln("passed");
}

//--------------------------------
// unary logical operations
//--------------------------------

/// Inverts and returns a decimal logical number.
/// Implements the 'invert' function in the specification. (p. 44)
public T invert(T)(T x) if (isDecimal!T)
{
	string xstr = x.coefficient.toString;
	if (!isLogicalOperand(x, xstr))	return invalidOperand(x);
	return T(invert(xstr));
}

/// Inverts and returns a logical string.
/// Each '1' is changed to a '0', and vice versa.
private T invert(T: string)(T str)
{
	char[] result = new char[str.length];
	for(size_t i = 0; i < str.length; i++)
	{
		result[i] = str[i] == '0' ? '1' : '0';
	}
	return result.idup;
}

unittest {	// inverse
 	write("-- logical inverse..");
	// TODO: (behavior, language) why can't we compare ints and decimals?
	dec9 num;
	num = invert(dec9(101001));
	assertEqual(num, 10110);
	num = invert(dec9(1));
	assertEqual(num, dec9(0));
//	assertEqual(num, 0);
	num = invert(dec9(0));
	assertEqual(num, 1);
	writeln("passed");
}

//--------------------------------
// binary logical operations
//--------------------------------

/// called by opBinary.
private T opLogical(string op, T)(in T x, in T y)
		if (isDecimal!T)
{
//	int precision = T.precision;
	string xstr = x.coefficient.toString;
	if (!isLogicalOperand(x, xstr)) {
		return invalidOperation!T;
	}
	string ystr = y.coefficient.toString;
	if (!isLogicalOperand(y, ystr)) {
		return invalidOperation!T;
	}
	static if (op == "and") {
		string str = and(xstr, ystr);
	}
	static if (op == "or") {
		string str = or(xstr, ystr);
	}
	static if (op == "xor") {
		string str = xor(xstr, ystr);
	}
	return T(str);
}

/// Performs a logical 'and' of the arguments and returns the result
/// Implements the 'and' function in the specification. (p. 41)
public T and(T)(in T x, in T y) if(isDecimal!T)
{
	return opLogical!("and", T)(x, y);
}

/// Performs a logical 'and' of the (string) arguments and returns the result
T and(T: string)(in T x, in T y)
{
	string str1, str2;
	size_t length, diff;
	if (x.length > y.length) {
		length = y.length;
		diff = x.length - y.length;
		str2 = x;
		str1 = rightJustify(y, x.length, '0');
	}
	else if (x.length < y.length) {
		length = x.length;
		diff = y.length - x.length;
		str1 = rightJustify(x, y.length, '0');
		str2 = y;
	} else {
		length = x.length;
		diff = 0;
		str1 = x;
		str2 = y;
	}
	char[] result = new char[length];
	for(int i = 0; i < length; i++) {
		if (str1[i + diff] == '1' && str2[i + diff] == '1') {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return result.idup;
}

/// Performs a logical 'or' of the arguments and returns the result
/// Implements the 'or' function in the specification. (p. 47)
public T or(T)(in T x, in T y) if (isDecimal!T)
{
	return opLogical!("or", T)(x, y);
}

/// Performs a logical 'or' of the (string) arguments and returns the result
private T or(T: string)(T xstr, T ystr)
{
	string str1, str2;
	size_t length;
	if (xstr.length > ystr.length) {
		length = xstr.length;
		str1 = xstr;
		str2 = rightJustify(ystr, xstr.length, '0');
	}
	if (xstr.length < ystr.length) {
		length = ystr.length;
		str1 = rightJustify(xstr, ystr.length, '0');
		str2 = ystr;
	} else {
		length = xstr.length;
		str1 = xstr;
		str2 = ystr;
	}
	char[] zstr = new char[length];
	for(int i = 0; i < length; i++) {
		if (str1[i] == '1' || str2[i] == '1') {
			zstr[i] = '1';
		} else {
			zstr[i] = '0';
		}
	}
	return zstr.idup;
}

/// Performs a logical 'xor' of the arguments and returns the result
/// Implements the 'xor' function in the specification. (p. 49)
public T xor(T)(in T x, in T y) if (isDecimal!T)
{
	return opLogical!("xor", T)(x, y);
}

/// Performs a logical 'xor' of the (string) arguments
/// and returns the result.
T xor(T: string)(in T x, in T y)
{
	string str1, str2;
	size_t length;
	if (x.length > y.length) {
		length = x.length;
		str1 = x;
		str2 = rightJustify(y, x.length, '0');
	}
	if (x.length < y.length) {
		length = y.length;
		str1 = rightJustify(x, y.length, '0');
		str2 = y;
	} else {
		length = x.length;
		str1 = x;
		str2 = y;
	}
	char[] result = new char[length];
	for(int i = 0; i < length; i++) {
		if (str1[i] != str2[i]) {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return result.idup;
}

unittest { // binary logical ops
	dec9 op1, op2;
	op1 = 10010101;
	op2 = 11100100;
//	assertEqual((op1 & op2), dec9(10000100));
//	assert((op1 | op2) == dec9(11110101));
//	assert((op1 ^ op2) == dec9( 1110001));
	op1 =   100101;
	op2 = 11100100;
//	assert((op1 & op2) == dec9(  100100));
//	assert((op1 | op2) == dec9(11100101));
//	assert((op1 ^ op2) == dec9(11000001));
}

/+
version(unittest)
{

	public struct BinaryTestData
	{
		testDecimal x;
		testDecimal y;
		testDecimal z;
	}

	public struct TestResults
	{
		string name;
		int passed;
		int failed;
		string[] messages;

		this(string name)
		{
			this.name = name;
		}

		string report()
		{
			string str = format("%s: %s (%d pass, %d fail).",
				name, tests(passed+failed), passed, failed);
			if (failed == 0) return str;
			foreach(msg; messages)
			{
				str ~= format("\n  %s", msg);
			}
			return str ~ "\n";
		}

		private string tests(int n)
		{
			if (n == 1) return "1 test";
			else return format("%d tests", n);
		}
		private string plural(string str, int n)
		{
			if (n != 1) str ~= "s";
			return str;
		}
	}

	public enum TestResults testBinaryFctn(T)
		(string name, T function(in T, in T) fctn, BinaryTestData[] tests,
		string file = __FILE__, int line = __LINE__) if (isDecimal!T)
	{
		TestResults r;
		r.name = name;
		foreach(i, t; tests)
		{
			assertBinaryFctn(r, name, t.x, t.y,
				fctn(t.x,t.y), t.z,	i, file, line);
		}
		return r;
	}

	public enum TestResults testBinaryFctn(T)
		(string name, T function(in T,in T, Context) fctn, BinaryTestData[] tests,
		string file = __FILE__, int line = __LINE__) if (isDecimal!T)
	{
		TestResults r;
		r.name = name;
		foreach(i, t; tests)
		{
			assertBinaryFctn(r, name, t.x, t.y,
				fctn(t.x, t.y, T.context), t.z,	i, file, line);
		}
		return r;
	}

	public enum TestResults testBinaryFctn(T)
		(string name, T function(in T, in T, Context, bool) fctn, BinaryTestData[] tests,
		string file = __FILE__, int line = __LINE__) if (isDecimal!T)
	{
		TestResults r;
		r.name = name;
		foreach(i, t; tests)
		{
			assertBinaryFctn(r, name,
				t.x, t.y, fctn(t.x, t.y, T.context, false), t.z,
				i, file, line);
		}
		return r;
	}

	private bool assertBinaryFctn(T)(ref TestResults r,
		string fctn, T x, T y, T act, T exp,
		int index = -1,	string file = __FILE__, int line = __LINE__)
	{
		if (act == exp)
		{
			r.passed++;
			return true;
		}
		else
		{
			r.failed++;
			string str = format("failed at %s(%d)", baseName(file), line);
			if (index >= 0) str ~= format(", test %d", index+1);
			str ~= format(": <%s(%s, %s)> equals <%s> not <%s>.", fctn, x, y, act, exp);
			r.messages.length++;
			r.messages[$-1] = str;
			return false;
		}
	}

	public enum bool testUnaryFctn(T)(string fctn, T input, T expected,
		int index = -1,	string file = __FILE__, int line = __LINE__)
		if (isDecimal!T)
	{
		switch (fctn)
		{
        // functions
		case "reduce":
			return assertUnaryFctn("reduce", input.toString,
				reduce(input).toString, expected.toString, index, file, line);
		case "abs":
			return assertUnaryFctn("abs", input,
				abs(input), expected, index, file, line);
		default:
			return false;
		}
	}

	private bool assertUnaryFctn(T)
		(string fctn, T input, T computed, T expected,
		int index = -1,	string file = __FILE__, int line = __LINE__)
	{
		if (computed == expected)
		{
			return true;
		}
		else
		{
			if (index >= 0)
			{
				write("failed at ", baseName(file), "(", line, ")[", index+1, "]: \"");
			}
			else
			{
				write("failed at ", baseName(file), "(", line, "): \"");
			}
			writeln(fctn, "(" , input, ")\" equals \"",
				computed, "\" not \"", expected, "\".");
			return false;
		}
	}

}+/

unittest {
	writeln("==========================");
	writeln("decimal logic..........end");
	writeln("==========================");
}


