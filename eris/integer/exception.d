// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.integer.exception;

//import std.stdio;

unittest {
	writeln("==========================");
	writeln("integer exceptions...begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.assertion;
}

	/// The base class for integer arithmetic exceptions.
	@safe
	class IntegerException: object.Exception {
		this(string msg = "Integer Exception", string file = __FILE__,
				uint line = cast(uint)__LINE__, Throwable next = null) {
			super(msg, file, line, next);
		}
	};

	@safe
	/// Raised when a result would be undefined or impossible.
	/// General Decimal Arithmetic Specification, p. 15.
	class IntegerOverflowException: IntegerException {
		this(string msg = "Integer Overflow", string file = __FILE__,
		     	uint line = cast(uint)__LINE__, Throwable next = null) {
			super(msg, file, line, next);
		}
	};

	/// Raised when a result would be undefined or impossible.
	/// General Decimal Arithmetic Specification, p. 15.
	@safe
	class InvalidOperationException: IntegerException {
		this(string msg = "Invalid Integer Operation", string file = __FILE__,
		     	uint line = cast(uint)__LINE__, Throwable next = null){
			super(msg, file, line, next);
		}
	};

	/// Raised when a result would be undefined or impossible.
	/// General Decimal Arithmetic Specification, p. 15.
	@safe
	class DivByZeroException: IntegerException {
		this(string msg = "Integer Division by Zero", string file = __FILE__,
		     	uint line = cast(uint)__LINE__, Throwable next = null) {
			super(msg, file, line, next);
		}
	};

unittest {
	writeln("==========================");
	writeln("integer exceptions.....end");
	writeln("==========================");
}

