// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2013 - 2014.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.rational.assertion;

import std.path: baseName;
import std.stdio;
import std.traits: isIntegral;


/// Returns true if the actual value equals the expected value.
/// Otherwise prints an error message and returns false.
bool assertEqual(T, U:T)(in T actual, in T expected, size_t index = -1,
		string file = __FILE__, int line = __LINE__ )
	{
	if (expected == actual)
	{
		return true;
	}
	else
	{
		if (index >= 0)
		{
			write("failed at ", baseName(file), "(", line, ")[", index + 1, "]:");
		}
		else
		{
			write("failed at ", baseName(file), "(", line, "):");
		}
		writeln(" expected \"", expected, "\" but found \"", actual, "\".");
		return false;
	}
}

/// Returns true if the actual value equals the expected value.
/// Otherwise prints an error message and returns false.
bool assertEqual(T, U)(in T actual, in U expected, size_t index = 0,
		string file = __FILE__, int line = __LINE__ ) {
	if (expected == actual)	{
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, ")[", index, "]:",
				" expected \"", expected, "\"",
				" but found \"", actual, "\".");
		return false;
	}
}

/*/// Returns true if the actual value equals the expected value.
/// Otherwise prints an error message and returns false.
bool assertEqual(T, U)(in T actual, in U expected, uint index = 0,
		string file = __FILE__, int line = __LINE__ )
{
	return assertEqual!T(actual, T(expected), index, file, line);
}*/

private string toString(T)(T n) if (__traits(isIntegral, T)){
	return std.string.format("%d", n);
}

/*private string toString(T:bool)(T v) {
	return std.string.format("%d", v);
}*/

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringNotEqual(T,U)(in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertNotEqual(actual.toString, expected.toString, file, line);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U)(in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual!string(actual.toString, expected.toString, 0, file, line);
}

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T:long)(in T actual, in T expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T:ulong)(in T actual, in T expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:uint)(in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:bool)(in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:string)(in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, expected, file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:string)(int index, in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqualIndexed(index, actual.toString, expected, file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqualIndexed(T:string)(in T actual, in T expected, int index,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual, expected, index, file, line);
}*/

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T:string)(in T actual, in T expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual, expected, file, line);
}*/

/*bool assertStringEqual(T)(T expected, string actual,
		string file = __FILE__, int line = __LINE__ ) {
	if (expected.toString == actual) {
		return true;
	}
	writeln("failed at ", baseName(file), "(", line, "):",
			" expected \"", expected, "\"",
			" but found \"", actual, "\".");
	return false;
}*/

/// Returns true if the actual value does not equal the expected value.
/// Otherwise prints an error message and returns false.
bool assertNotEqual(T,U = T)(in T actual, in U expected,
		string file = __FILE__, int line = __LINE__ ) {
	if (expected != actual) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" \"", expected, "\" is equal to \"", actual, "\".");
		return false;
	}
}

/// Returns true if the actual value is true.
/// Otherwise prints an error message and returns false.
bool assertTrue(bool actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual, true, 0, file, line);
}

/// Returns true if the actual value is true.
/// Otherwise prints an error message and returns false.
bool assertTrue(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(cast(bool)actual, true, 0, file, line);
}

/// Returns true if the actual value is false.
/// Otherwise prints an error message and returns false.
bool assertFalse(bool actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual, false, 0, file, line);
}

/// Returns true if the actual value is false.
/// Otherwise prints an error message and returns false.
bool assertFalse(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(cast(bool)actual, false, 0, file, line);
}

/// Returns true if the value is null.
/// Otherwise prints an error message and returns false.
bool assertNull(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	if (isNull(actual)) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" expected null ",
				" but found \"", actual, "\".");
		return false;
	}
}

/// Returns true if the value is not null.
/// Otherwise prints an error message and returns false.
bool assertNotNull(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	if (!isNull(actual)) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" expected \"", actual, "\"",
				" but found null.");
		return false;
	}
}

/// Returns true if the value is zero.
/// Otherwise prints an error message and returns false.
bool assertZero(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	if (actual == 0) 	{
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" expected zero ",
				" but found \"", actual, "\".");
		return false;
	}
}

/// Returns true if the value is greater than or equal to zero.
/// Otherwise prints an error message and returns false.
bool assertPositive(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	if (actual >= zero) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" expected a positive value ",
				" but found \"", actual, "\".");
		return false;
	}
}

/// Returns true if the value is less than zero.
/// Otherwise prints an error message and returns false.
bool assertNegative(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	if (actual < zero) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" expected a negative value ",
				" but found \"", actual, "\".");
		return false;
	}
}

/// Returns true if the first value is greater than the second value.
/// Otherwise prints an error message and returns false.
bool assertGreaterThan(T,U)(in T first, in U second,
		string file = __FILE__, int line = __LINE__ ) {
	if (first > second) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" \"", first, "\"",
				" is not greater than \"", second, "\".");
		return false;
	}
}

/// Returns true if the first value is greater than the second value.
/// Otherwise prints an error message and returns false.
bool assertNotGreaterThan(T,U)(in T first, in U second,
		string file = __FILE__, int line = __LINE__ ) {
	if (first <= second) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" \"", first, "\"",
				" is greater than \"", second, "\".");
		return false;
	}
}

/// Returns true if the first value is less than the second value.
/// Otherwise prints an error message and returns false.
bool assertLessThan(T,U)(in T first, in U second,
		string file = __FILE__, int line = __LINE__ ) {
	if (first < second) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" \"", first, "\"",
				" is not less than \"", second, "\".");
		return false;
	}
}

/// Returns true if the first value is less than the second value.
/// Otherwise prints an error message and returns false.
bool assertNotLessThan(T,U)(in T first, in U second,
		string file = __FILE__, int line = __LINE__ ) {
	if (first >= second) {
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
				" \"", first, "\"",
				" is less than \"", second, "\".");
		return false;
	}
}

/// Returns true if the expression throws.
bool assertThrows(T:Throwable = Exception, E)(lazy E expression,
		string msg = T.stringof, string file = __FILE__, int line = __LINE__ ) {
	try {
		std.exception.assertThrown!T(expression, msg, file, line);
		return true;
	}
	catch (Throwable exc) {
		writeln("failed at ", baseName(file), "(", line, "):",
				" Did not throw \"", msg, "\".");
		return false;
	}
}

