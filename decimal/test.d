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
	import std.traits;

	import eris.decimal;
	import eris.decimal.context;
	import eris.decimal.arithmetic;

	// S is a struct containing input data and the expected value
	// T is the return type of the function being tested
	// N is the number of inputs
	package struct FunctionTest(S,T,int N = S.tupleof.length-1)
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

		void test(S s, T actual,
			string file = __FILE__, int line = __LINE__)
		{
			testCount++;
			auto input = s.tupleof[0..$-1];
			auto expect = s.tupleof[$-1];

			if (actual == expect)
			{
				passCount++;
			}
			else
			{
				failCount++;
				string msg = format("failed at %s(%d)", baseName(file), line);
				msg ~= format(", test %d", testCount);
				static if (N == 0)
					msg ~= format(": <%s()> should be <%s> not <%s>.",
					name, actual, expect);
				else static if (N == 1)
					msg ~= format(": <%s(%s)> should be <%s> not <%s>.",
					name, input[0], actual, expect);
				else static if (N == 2)
					msg ~= format(": <%s(%s, %s)> should be <%s> not <%s>.",
					name, input[0], input[1], actual, expect);
				else static if (N == 3)
					msg ~= format(": <%s(%s, %s, %s)> should be <%s> not <%s>.",
					name, input[0], input[1], input[2], actual, expect);
				else
					msg ~= format(": <%s(%s, %s, %s, ...)> should be <%s> not <%s>.",
					name, input[0], input[1], input[2], actual, expect);
				messages.length++;
				messages[$-1] = msg;
			}
		}

		string report()
		{
			string rep = format("%-10s: %s", name, tests(testCount));
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

		string tests(int n)
		{
			if (n == 1) return " 1 test ";
			else return format("%2d tests", n);
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

