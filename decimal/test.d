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

module eris.decimal.test;

version(unittest)
{

	import std.path: baseName;
	import std.string;

	import eris.decimal;
	import eris.decimal.context;

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


	// test data structure for n-ary functions
	package struct ArithTestData(T, int N)
	{
		static if (N > 0) T x;
		static if (N > 1) T y;
		static if (N > 2) T z;
		T expect;
	}

    /// mixin template to create a test of a function
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
				"assertFctn!(T,N,S)(tr, name, t, " ~  actual ~ ", i, file, line);"
			"}"
			"return tr;"
		"}"
		);
	}

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

	private bool assertFctn(T, int N, bool S = false)(ref TestResults tr,
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
				msg ~= format(": <%s()> equals <%s> not <%s>.",
					fctn, actual, test.expect);
			static if (N == 1)
				msg ~= format(": <%s(%s)> equals <%s> not <%s>.",
					fctn, test.x, actual, test.expect);
			else static if (N == 2)
				msg ~= format(": <%s(%s, %s)> equals <%s> not <%s>.",
					fctn, test.x, test.y, actual, test.expect);
			else static if (N == 3)
				msg ~= format(": <%s(%s, %s, %s)> equals <%s> not <%s>.",
					fctn, test.x, test.y, test.z, actual, test.expect);
			tr.messages.length++;
			tr.messages[$-1] = msg;
			return false;
		}
	}
}

