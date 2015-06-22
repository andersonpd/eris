// Written in the D programming language

/**
 *	A D programming language implementation of the
 *	General Decimal Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 *	http://www.speleotrove.com/decimal/decarith.pdf)
 *
 *	Copyright Paul D. Anderson 2009 - 2015.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.decimal.test;

version(unittest)
{

	import std.path: baseName;
	import std.stdio;
	import std.string;

	import eris.decimal;
	import eris.decimal.context;
	import eris.decimal.arithmetic;

	// The TestResults structure accumulates the results of an array of tests
	// and generates a report (as a string). The report includes the number of
	// tests that were run and the numbers of tests that passed and failed.
	// Failing tests include a message with actual results and expected results.
	public struct TestResults
	{
		string name;
		int pass;
		int fail;
		string[] messages;

		@disable this();

		this(string name)
		{
			this.name = name;
		}

		string report()
		{
			string rep = format("%-10s: %s", name, tests(pass + fail));
			if (fail == 0)
			{
				rep ~= format(" (%2d pass)", pass);
			}
			else
			{
				rep ~= format(" (%2d pass, %d fail).", pass, fail);
				foreach (msg; messages)
				{
					rep ~= format("\n  %s", msg);
				}
			}
			return rep;
		}

		private string tests(int n)
		{
			if (n == 1) return " 1 test ";
			else return format("%2d tests", n);
		}
	}


	// test data structure for n-ary functions.
	// { x, y, z, expected value }
	package struct ArithTestData(T, int N, bool P = false)
	{
		static if (N > 0) T x;
		static if (N > 1) T y;
		static if (N > 2) T z;
		T expect;
		static if (P) int precision;
	}

    /// Mixin template to create a test of a function.
	/// Needs to be tailored to match the function's call parameters.
	/// See following mixins.
	///
	/// The test iterates through a range of inputs and expected results
	/// and returns a report of the passes and failures.
	mixin template ArithTest(string signature, string actual)
	{
		mixin
		(
		"package enum TestResults testArith(T, int N, bool S = false)"
			"(string name, " ~ signature ~ ", ArithTestData!(T, N)[] tests,"
			"string file = __FILE__, int line = __LINE__) if (isDecimal!T)"
		"{"
			"auto tr = TestResults(name);"
			"foreach (int i, t; tests)"
			"{"
				"assertArith!(T,N,S)(tr, name, t, " ~  actual ~ ", i, file, line);"
			"}"
			"return tr;"
		"}"
		);
	}

    /// Mixin template to create a test of a function.
	/// Needs to be tailored to match the function's call parameters.
	/// See following mixins.
	///
	/// The test iterates through a range of inputs and expected results
	/// and returns a report of the passes and failures.
	mixin template FunctionTest(string signature, string actual)
	{
		mixin
		(
		"package enum TestResults testArith(T, int N, bool S = false)"
			"(string name, " ~ signature ~ ", ArithTestData!(T, N)[] tests,"
			"string file = __FILE__, int line = __LINE__) if (isDecimal!T)"
		"{"
			"auto tr = TestResults(name);"
			"foreach (int i, t; tests)"
			"{"
				"assertArith!(T,N,S)(tr, name, t, " ~  actual ~ ", i, file, line);"
			"}"
			"return tr;"
		"}"
		);
	}

/*	// test data structure for n-ary functions.
	// { x, y, z, expected value }
	package struct PrecisionTestData(T, int N)
	{
		static if (N > 0) T x;
		static if (N > 1) T y;
		static if (N > 2) T z;
		T expect;
		int precision;
	}*/

    /// Mixin template to create a test of a function.
	/// Needs to be tailored to match the function's call parameters.
	/// See following mixins.
	///
	/// The test iterates through a range of inputs and expected results
	/// and returns a report of the passes and failures.
	mixin template PrecisionTest(string signature, string actual)
	{
		mixin
		(
		"package enum TestResults testPrecision(T, int N, bool P = true)"
			"(string name, " ~ signature ~ ", ArithTestData!(T, N, P)[] tests,"
			"string file = __FILE__, int line = __LINE__) if (isDecimal!T)"
		"{"
			"auto tr = TestResults(name);"
			"foreach (int i, t; tests)"
			"{"
				"assertPrecision!(T,N,P)(tr, name, t, " ~  actual ~ ", i, file, line);"
			"}"
			"return tr;"
		"}"
		);
	}

	// one argument, no context
	mixin PrecisionTest!
	(
		"T function(in T, int precision) fctn",
		"fctn(t.x, t.precision)"
	);

	// one argument, no context
	mixin ArithTest!
	(
		"T function(in T) fctn",
		"fctn(t.x)"
	);

	// one argument with rounding
	mixin ArithTest!
	(
		"T function(in T, in Rounding) fctn",
		"fctn(t.x, T.rounding)"
	);

	// one argument, no context, returns int
	mixin ArithTest!
	(
		"int function(in T) fctn",
		" T(fctn(t.x))"
	);

	// one argument with context
	mixin ArithTest!
	(
		"T function(in T, Context) fctn",
		"fctn(t.x,T.context)"
	);

	// two arguments, no context
	mixin ArithTest!
	(
		"T function(in T, in T) fctn",
		"fctn(t.x,t.y)"
	);

	// two arguments, no context, returns int
	mixin ArithTest!
	(
		"int function(in T, in T) fctn",
		"T(fctn(t.x, t.y))"
	);

	// two arguments, no context, returns bool
	mixin ArithTest!
	(
		"bool function(in T, in T) fctn",
		"T(fctn(t.x, t.y))"
	);

	// two arguments with context
	mixin ArithTest!
	(
		"T function(in T, in T, Context) fctn",
		"fctn(t.x,t.y,T.context)"
	);

	// two arguments with context, returns int
	mixin ArithTest!
	(
		"int function(in T, in T, Context) fctn",
		"T(fctn(t.x,t.y,T.context))"
	);

	// two arguments with context, returns bool
	mixin ArithTest!
	(
		"bool function(in T, in T, Context) fctn",
		"T(fctn(t.x,t.y,T.context))"
	);

	// two arguments with context and a boolean flag
	mixin ArithTest!
	(
		"T function(in T, in T, Context, bool) fctn",
		"fctn(t.x,t.y,T.context,false)"
	);

	// three arguments with context
	mixin ArithTest!
	(
		"T function(in T, in T, in T, Context) fctn",
		"fctn(t.x, t.y, t.z, T.context)"
	);

	private bool assertArith(T, int N, bool S = false)(ref TestResults tr,
		string fctn, ArithTestData!(T, N) test, T actual,
		int index = -1,	string file = __FILE__, int line = __LINE__)
	{
		bool pass;
		static if (S)
		{
			pass = (actual.toString == test.expect.toString);
		}
		else
		{
			pass = (actual == test.expect);
		}
		if (pass)
		{
			tr.pass++;
			return true;
		}
		else
		{
			tr.fail++;
			string msg = format("failed at %s(%d)", baseName(file), line);
			if (index >= 0) msg ~= format(", test %d", index+1);
			static if (N == 0)
				msg ~= format(": <%s()> should be <%s> not <%s>.",
					fctn, actual, test.expect);
			static if (N == 1)
				msg ~= format(": <%s(%s)> should be <%s> not <%s>.",
					fctn, test.x, actual, test.expect);
			else static if (N == 2)
				msg ~= format(": <%s(%s, %s)> should be <%s> not <%s>.",
					fctn, test.x, test.y, actual, test.expect);
			else static if (N == 3)
				msg ~= format(": <%s(%s, %s, %s)> should be <%s> not <%s>.",
					fctn, test.x, test.y, test.z, actual, test.expect);
			tr.messages.length++;
			tr.messages[$-1] = msg;
			return false;
		}
	}

	private bool assertPrecision(T, int N, bool P = true, bool S = false)(ref TestResults tr,
		string fctn, ArithTestData!(T, N, P) test, T actual,
		int index = -1,	string file = __FILE__, int line = __LINE__)
	{
		bool pass;
		static if (S)
		{
			pass = (actual.toString == test.expect.toString);
		}
		else
		{
			pass = (precisionEquals(actual, test.expect, test.precision));
		}
		if (pass)
		{
			tr.pass++;
			return true;
		}
		else
		{
			tr.fail++;
			string msg = format("failed at %s(%d)", baseName(file), line);
			if (index >= 0) msg ~= format(", test %d", index+1);
			static if (N == 0)
				msg ~= format(": <%s()> should be <%s> not <%s>.",
					fctn, actual, test.expect);
			static if (N == 1)
				msg ~= format(": <%s(%s)> should be <%s> not <%s>.",
					fctn, test.x, actual, test.expect);
			else static if (N == 2)
				msg ~= format(": <%s(%s, %s)> should be <%s> not <%s>.",
					fctn, test.x, test.y, actual, test.expect);
			else static if (N == 3)
				msg ~= format(": <%s(%s, %s, %s)> should be <%s> not <%s>.",
					fctn, test.x, test.y, test.z, actual, test.expect);
			tr.messages.length++;
			tr.messages[$-1] = msg;
			return false;
		}
	}

	/// Returns true if the actual value equals the expected value to the specified precision.
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
		auto context = Context(precision, T.maxExpo, T.rounding);
		if (equals(T(expected), actual, context))	{
			return true;
		}
		else {
			writeln("failed at ", baseName(file), "(", line, "):",
	        		" expected \"", expected, "\"",
	        		" but found \"", actual, "\".");
			return false;
		}
	}

}

