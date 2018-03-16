// Written in the D programming language

/**
 * Test routines for floating-point decimal arithmetic.
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

module eris.decimal.test;

version(unittest)
{

	import std.path : baseName;
	import std.stdio;
	import std.string;
	import std.traits;

	import eris.decimal;
	import eris.decimal.arithmetic;

public static bool initialized = false;
public static int  totalTests;
public static int  totalPass;
public static int  totalFail;

/*	package struct Totals()
	{
			string report()
		{
			string rep = format("%-11s: %s", name, tests(testCount));
			if (failCount == 0)
			{
				rep ~= format(" (%2d pass)", passCount);
			}
			else
			{
				rep ~= format(" (%2d pass, %d fail).", passCount, failCount);
				foreach (msg; messages)
				{
					rep ~= format("\n  %s", msg);
				}
			}
			return rep;
		}

	}*/
//-----------------------------
// tests
//-----------------------------

	/**
	 * S is a struct containing input data and the expected value
	 * T is the return type of the function being tested
	 * N is the number of inputs
	 */
	package struct FunctionTest(S,T,string fmt = "%s", int N = S.tupleof.length-1)
	{
		string name;
		int testCount;
		int passCount;
		int failCount;
		string[] messages;

		@disable this();

		this(string name)
		{
			this.name = name;
		}

		void test(S s, T actual, int precision = 0,
			string file = __FILE__, int line = __LINE__)
		{
			testCount++;
			totalTests++;
			auto input = s.tupleof[0..$-1];
			auto expect = s.tupleof[$-1];
			bool passed = false;

			static if (isDecimal!T)
			{
				if (precision > 0)
				{
//	if (!__ctfe) writefln("precision = %s", precision);
				// must be equal at specified precision
					passed = true; //precisionEquals!T(expect, actual, precision);
				}
				else
				{
					// equal at context precision
					passed = (actual == expect);
				}

				if (!passed)	// check for NaN, NaN
				{
					passed = actual.isNaN && expect.isNaN;
				}
			}
			else
			{
				passed = (actual == expect);
			}
			// end static if/else

			if (passed)
			{
				passCount++;
				totalPass++;
			}
			else
			{
				failCount++;
				totalFail++;
				string msg = format("failed at %s(%d)", baseName(file), line);
				msg ~= format(", test %d", testCount);
				string description = format(" should be <" ~ fmt ~ "> not <" ~ fmt ~ ">.", expect, actual);

				static if (N == 0)
                {
					msg ~= format(": <%s()>", name);
                }
				else static if (N == 1)
				{
					msg ~= format(": <%s(%s)>", name, input[0]);
				}
				else static if (N == 2)
				{
                    msg ~= format(": <%s(%s,%s)>", name, input[0], input[1]);
				}
				else static if (N == 3)
                {
					msg ~= format(": <%s(%s,%s,%s)>", name, input[0], input[1], input[2]);
                }
				else
                {
					msg ~= format(": <%s(%s,%s,%s, ...)>", name, input[0], input[1], input[2]);
                }
				messages.length++;
				messages[$-1] = msg ~ description;
			}
		}

		string report()
		{
			string rep = format("%-11s: %s", name, tests(testCount));
			if (failCount == 0)
			{
				rep ~= format(" (%2d pass)", passCount);
			}
			else
			{
				rep ~= format(" (%2d pass, %d fail).", passCount, failCount);
				foreach (msg; messages)
				{
                    // escape any formatting
                    msg = msg.replace("%","%%");
                    rep ~= format("\n  %s", msg);
				}
			}
			return rep;
		}

		string tests(int n)
		{
			if (n == 1) return " 1 test ";
			else return format("%2d tests", n);
		}
	}

/*	/// Returns true if the actual value equals the expected value to the specified precision.
	/// Otherwise prints an error message and returns false.
	public bool assertPrecisionEqual(T, U)(T actual, U expected, int precision,
			string file = __FILE__, int line = __LINE__ ) if (isDecimal!T && isDecimal!U){
	//	auto context = Context(precision);
		if (precisionEquals!T(expected, actual, precision))	{
			return true;
		}
		else {
			writeln("failed at ", baseName(file), "(", line, "):",
	        		" expected \"", expected, "\"",
	        		" but found \"", actual, "\".");
			return false;
		}
	}

	/// Returns true if the actual value equals the expected value to the specified precision.
	/// Otherwise prints an error message and returns false.
	public bool assertPrecisionEqual(T, U:string)(T actual, U expected, int precision,
			string file = __FILE__, int line = __LINE__ ) {
		auto context = Context(precision, T.maxExpo, T.mode);
		if (equals(T(expected), actual, context))	{
			return true;
		}
		else {
			writeln("failed at ", baseName(file), "(", line, "):",
	        		" expected \"", expected, "\"",
	        		" but found \"", actual, "\".");
			return false;
		}
	}*/

}
//-----------------------------
// asserts
//-----------------------------


// TODO: assertCompare? assertCompareTotal?

/// Returns true if the actual value equals the expected value.
/// Otherwise prints an error message and returns false.
bool assertEqual(T, U: T)(in T actual, T expected, size_t index = -1,
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
bool assertEqual(T, U)(in T actual, U expected, size_t index = 0,
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


/// Returns true if the string representation of the actual value
/// equals the string representation of the expected value.
/// Otherwise prints an error message and returns false.
bool assertStringEqual(T,U:string)(in T actual, U expected,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(actual.toString, expected, -1, file, line);
}

/// Returns true if the actual value is true.
/// Otherwise prints an error message and returns false.
bool assertTrue(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(cast(bool)actual, true, 0, file, line);
}

/// Returns true if the actual value is false.
/// Otherwise prints an error message and returns false.
bool assertFalse(T)(T actual,
		string file = __FILE__, int line = __LINE__ ) {
	return assertEqual(cast(bool)actual, false, 0, file, line);
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


