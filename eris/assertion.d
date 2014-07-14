// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2013 - 2014.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.assertion;

import std.path: baseName;
import std.stdio;
import std.traits: isIntegral;


/// Returns true if the actual value equals the expected value.
/// Otherwise prints an error message and returns false.
bool assertEqual(T,U)(const T actual, const U expected,
		string file = __FILE__, int line = __LINE__ ) {
	if (expected == actual)	{
		return true;
	}
	else {
		writeln("failed at ", baseName(file), "(", line, "):",
	        	" expected \"", expected, "\"",
	        	" but found \"", actual, "\".");
		return false;
	}
}

private string toString(T)(T n) if (__traits(isIntegral, T)){
	return std.string.format("%d", n);
}

private string toString(T:bool)(T v) {
	return std.string.format("%d", v);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringNotEqual(T,U)(const T actual, const U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertNotEqual(actual.toString, expected.toString, file, line);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U)(const T actual, const U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, expected.toString, file, line);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T:long)(const T actual, const T expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T:ulong)(const T actual, const T expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}

/*/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:uint)(const T actual, const U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}*/

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:bool)(const T actual, const U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, toString(expected), file, line);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:string)(const T actual, const U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, expected, file, line);
}

/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T:string)(const T actual, const T expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual, expected, file, line);
}

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
bool assertNotEqual(T,U = T)(const T actual, const U expected,
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
	return assertEqual(actual, true, file, line);
}

/// Returns true if the actual value is true.
/// Otherwise prints an error message and returns false.
bool assertTrue(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(cast(bool)actual, true, file, line);
}

/// Returns true if the actual value is false.
/// Otherwise prints an error message and returns false.
bool assertFalse(bool actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual, false, file, line);
}

/// Returns true if the actual value is false.
/// Otherwise prints an error message and returns false.
bool assertFalse(bool actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(cast(bool)actual, false, file, line);
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
bool assertGreaterThan(T,U)(const T first, const U second,
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
bool assertNotGreaterThan(T,U)(const T first, const U second,
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
bool assertLessThan(T,U)(const T first, const U second,
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
bool assertNotLessThan(T,U)(const T first, const U second,
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
		// FIXTHIS: unhelpful error message
		writeln("failed at ", baseName(file), "(", line, "):",
	        	" Did not throw \"", msg, "\".");
		return false;
	}
}


