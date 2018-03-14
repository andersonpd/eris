// Written in the D programming language

/**
 * Decimal logical functions.
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
import std.math;

import eris.decimal;

unittest {
	writeln("      logical tests       ");
	writeln("==========================");
}

// temporary import
import std.stdio;

version(unittest)
{
	import std.stdio;
	import eris.decimal.test;
}

//--------------------------------
// logical operations
//--------------------------------

/**
 *  Returns true if the argument is a valid logical decimal number.
 *  The sign and exponent must both be zero, and all decimal digits
 *  in the coefficient must be either '1' or '0'.
 */
public bool isLogical(D)(in D arg, out string str) if (isDecimal!D)
{
	if (arg.sign != 0 || arg.expo != 0) return false;
	str = std.string.format("%d", arg.coff);
	size_t n = str.length;
	int p = D.precision;
	// if length > precision, truncate left chars
	if (n > p)
	{
		str = str[n-p..n];
	}
	// if length < precision, pad left with zeros
	if (n < p)
	{
		str = rightJustify(str, p, '0');
	}
	foreach (char ch; str) {
		if (ch != '0' && ch != '1') return false;
	}
	return true;
}

/**
 *  Returns true if the argument is a valid logical decimal number.
 *  The sign and exponent must both be zero, and all decimal digits
 *  in the coefficient must be either '1' or '0'.
 */
public bool isLogical(D)(in D arg) if (isDecimal!D)
{
	string str;
	return isLogical!D(arg, str);
}

unittest {	// logical string/number tests
 	write("-- logical tests....");
	assertTrue(isLogical(TD("010101110101")));
	assertTrue(isLogical(TD("1011101")));
	writeln("passed");
}

//--------------------------------
// unary logical operations
//--------------------------------

/**
 *  Inverts and returns a decimal logical number.
 *  Implements the 'invert' function in the specification. (p. 44)
 */
public D not(D)(D arg) if (isDecimal!D)
{
	string str;
	if (!isLogical(arg, str)) {
		return invalidOperation!D;
	}
	char[] result = new char[str.length];
	for (size_t i = 0; i < str.length; i++)
	{
		result[i] = str[i] == '0' ? '1' : '0';
	}
	return D(result.idup);
}

/**
 *  Inverts and returns a logical string.
 *  Each '1' is changed to a '0', and vice versa.
 */
private S not(S: string)(S str)
{
	char[] result = new char[str.length];
	for (size_t i = 0; i < str.length; i++)
	{
		result[i] = str[i] == '0' ? '1' : '0';
	}
	return result.idup;
}

unittest
{	// inverse
	static struct S { TD num; TD expect; }
	S[] s =
	[
/*		{ "101001",	"10110" },
		{ "1",		"0" },
		{ "0",		"1" },*/
		{ "101001",	"1111111111010110" },
		{ "1",		"1111111111111110" },
		{ "0",		"1111111111111111" },
	];
	auto f = FunctionTest!(S,TD)("not");
	foreach (t; s) f.test(t, not!TD(t.num));
    writefln(f.report);
}

//--------------------------------
// binary logical operations
//--------------------------------

/**
 *  Performs a logical and of the arguments and returns the result.
 *  Implements the 'and' function in the specification. (p. 41)
 */
public D and(D)(in D x, in D y) if (isDecimal!D)
{
	string xstr;
	if (!isLogical(x, xstr)) {
		return invalidOperation!D;
	}
	string ystr;
	if (!isLogical(y, ystr)) {
		return invalidOperation!D;
	}
	auto len = xstr.length;
	char[] result = new char[len];
	for (size_t i = 0; i < len; i++)
	{
		if (xstr[i] == '1' && ystr[i] == '1') {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return D(result.idup);
}

/**
 *  Performs a logical or of the arguments and returns the result.
 *  Implements the 'or' function in the specification. (p. 41)
 */
public D or(D)(in D x, in D y) if (isDecimal!D)
{
	string xstr;
	if (!isLogical(x, xstr)) {
		return invalidOperation!D;
	}
	string ystr;
	if (!isLogical(y, ystr)) {
		return invalidOperation!D;
	}
	auto len = xstr.length;
	char[] result = new char[len];
	for (size_t i = 0; i < len; i++)
	{
		if (xstr[i] == '1' || ystr[i] == '1') {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return D(result.idup);
}

/**
 *  Performs a logical xor of the arguments and returns the result.
 *  Implements the 'xor' function in the specification. (p. 41)
 */
public D xor(D)(in D x, in D y) if (isDecimal!D)
{
	string xstr;
	if (!isLogical(x, xstr)) {
		return invalidOperation!D;
	}
	string ystr;
	if (!isLogical(y, ystr)) {
		return invalidOperation!D;
	}
	auto len = xstr.length;
	char[] result = new char[len];
	for (size_t i = 0; i < len; i++)
	{
		if (xstr[i] != ystr[i]) {
			result[i] = '1';
		} else {
			result[i] = '0';
		}
	}
	return D(result.idup);
}

unittest { // binary logical ops
	TD op1, op2;
	op1 = 10010101;
	op2 = 11100100;
	assert(and(op1, op2) == TD(10000100));
	assert(or(op1, op2)  == TD(11110101));
	assert(xor(op1, op2) == TD( 1110001));
	op1 =   100101;
	op2 = 11100100;
//	assert((op1 & op2) == TD(  100100));
//	assert((op1 | op2) == TD(11100101));
//	assert((op1 ^ op2) == TD(11000001));
}

//--------------------------------
// logical shift and rotate
//--------------------------------

/// Performs a logical shift of the argument by the specified number of
/// decimal digits. Positive values of the second operand shift the argument
///  to the left; negative values shift the argument to the right.
/// If the argument is not a valid logical operand, or if the absolute value of
/// the shift is greater than precision, an INVALID_OPERATION is signaled.
/// Implements the 'shift' function in the specification. (p. 49)
public D lsh(D)(in D arg, int n,
		Context context = D.context) if (isDecimal!D)
{
	string str;
	int p = context.precision;

	if (n > p || n < -p || !isLogical(arg, str))
	{
		return invalidOperation!D;
	}

	if (n == 0)
	{
	 	return arg;
	}

	if (n > 0)
	{
		str = str[n..$];
		str = leftJustify(str, p, '0');
		return D(str);
	}
	else
	{
		str = str[0..$+n];
		str = rightJustify(str, p, '0');
		return D(str);
	}
}

unittest
{	// shift  -- depends on context
	static struct S { TD x; int n; TD expect; }
	S[] s =
	[
		{ 111, 8, 11100000000 },
		{ 11111, -3, 11 },
		{ 1101, 0, 1101 },
		{ 1101, -16, 0 },
		{ 1101, -17, "NaN" },
	];
	auto f = FunctionTest!(S,TD)("lsh");
	foreach (t; s) f.test(t, lsh(t.x, t.n));
    writefln(f.report);
}

/// Rotates the argument by the specified number of decimal digits.
/// Positive values of the second operand rotate the argument to the left;
/// negative values rotate the argument to the right. If the argument
/// is not a valid logical operand, or if the absolute value of the rotation
/// is greater than precision, an INVALID_OPERATION is signaled.
/// Implements the 'rotate' function in the specification. (p. 47)
public D rot(D)(in D arg, int n,
		Context context = D.context) if (isDecimal!D)
{
	string str;
	int p = context.precision;
	if (n > p || n < -p || !isLogical(arg, str))
	{
		return invalidOperation!D;
	}

	if (n == 0)
	{
	 	return arg;
	}

	if (n > 0)
	{
		string str1 = str[n..$];
		string str2 = str[0..n];
		return D(str1 ~ str2);
	}
	else
	{
		string str1 = str[$+n..$];
		string str2 = str[0..$+n];
		return D(str1 ~ str2);
	}
}

unittest
{	// shift  -- depends on context
	static struct S { TD x; int n; TD expect; }
	S[] s =
	[
		{ 1, 1, 10 },
		{ 1011001110001111, 4, 11100011111011 },
		{ 11111, -3, 1110000000000011 },
		{ 1101, 0, 1101 },
	];
	auto f = FunctionTest!(S,TD)("rot");
	foreach (t; s) f.test(t, rot(t.x, t.n));
    writefln(f.report);
}


unittest
{
	writeln("==========================");
}


