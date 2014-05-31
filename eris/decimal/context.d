﻿
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

module eris.decimal.context;

//unittest {
//	writeln("==========================");
//	writeln("decimal context......begin");
//	writeln("==========================");
//}
//
//version(unittest) {
//	import std.stdio;
//	import eris.assertions;
//}

/// The available rounding modes. For cumulative operations use the
/// HALF_EVEN mode to prevent accumulation of errors. Otherwise the
/// HALF_UP and HALF_DOWN modes are satisfactory. The UP, DOWN, FLOOR,
/// and CEILING modes are also useful for some operations.
/// General Decimal Arithmetic Specification, p. 13-14.
public enum Rounding {
	NONE,
    HALF_EVEN,
    HALF_DOWN,
    HALF_UP,
    DOWN,
    UP,
    FLOOR,
    CEILING,
}

/// The available flags and trap-enablers.
/// The larger value have higher precedence.
/// If more than one flag is set by an operation and traps are enabled,
/// the flag with higher precedence will throw its exception.
/// General Decimal Arithmetic Specification, p. 15.
public enum : ubyte {
	INVALID_OPERATION  = 0x80,
	DIVISION_BY_ZERO   = 0x40,
	OVERFLOW           = 0x20,
	SUBNORMAL          = 0x10,
	INEXACT            = 0x08,
	ROUNDED            = 0x04,
	UNDERFLOW          = 0x02,
	CLAMPED            = 0x01
}

/// "The exceptional conditions are grouped into signals,
/// which can be controlled individually.
/// The context contains a flag (which is either 0 or 1)
/// and a trap-enabler (which also is either 0 or 1) for each signal.
/// For each of the signals, the corresponding flag is
/// set to 1 when the signal occurs.
/// It is only reset to 0 by explicit user action."
/// General Decimal Arithmetic Specification, p. 15.
public struct ContextFlags {

	private static ubyte flags;
	private static ubyte traps;

	/// Sets or resets the specified context flag(s).
@safe
	void setFlags(const ubyte flags, const bool value = true) {
		if (value) {
			ubyte saved = this.flags;
			this.flags |= flags;
			ubyte changed = saved ^ flags;
			checkFlags(changed);
			// (X)TODO: if this flag is trapped an exception should be thrown.
		} else {
			this.flags &= !flags;
		}
	}

	// Checks the state of the flags. If a flag is set and its
	// trap-enabler is set, an exception is thrown.
@safe
	 void checkFlags(const ubyte flags) {
		if (flags & INVALID_OPERATION && traps & INVALID_OPERATION) {
			throw new InvalidOperationException("INVALID_OPERATION");
		}
		if (flags & DIVISION_BY_ZERO && traps & DIVISION_BY_ZERO) {
			throw new DivByZeroException("DIVISION_BY_ZERO");
		}
		if (flags & OVERFLOW && traps & OVERFLOW) {
			throw new OverflowException("OVERFLOW");
		}
		if (flags & SUBNORMAL && traps & SUBNORMAL) {
			throw new SubnormalException("SUBNORMAL");
		}
		if (flags & INEXACT && traps & INEXACT) {
			throw new InexactException("INEXACT");
		}
		if (flags & ROUNDED && traps & ROUNDED) {
			throw new RoundedException("ROUNDED");
		}
		if (flags & UNDERFLOW && traps & UNDERFLOW) {
			throw new UnderflowException("UNDERFLOW");
		}
		if (flags & CLAMPED && traps & CLAMPED) {
			throw new ClampedException("CLAMPED");
		}
	}

	/// Gets the value of the specified context flag.
	 bool getFlag(const ubyte flag) {
		return (this.flags & flag) == flag;
	}

	/// Returns a snapshot of the context flags.
	 ubyte getFlags() {
		return flags;
	}

	/// Clears all the context flags.
	void clearFlags() {
		flags = 0;
	}

	/// Sets or resets the specified trap(s).
	void setTrap(const ubyte traps, const bool value = true) {
		if (value) {
			this.traps |= traps;
		} else {
			this.traps &= !traps;
		}
	}

	/// Returns the value of the specified trap.
	 bool getTrap(const ubyte trap) {
		return (this.traps & trap) == trap;
	}

	/// Returns a snapshot of traps.
	public ubyte getTraps() {
		return traps;
	}

	/// Clears all the traps.
	void clearTraps() {
		traps = 0;
	}

};

// this is the single instance of the context flags
public ContextFlags contextFlags;


//--------------------------
// Context flags and trap-enablers
//--------------------------

/// The base class for all decimal arithmetic exceptions.
@safe
class DecimalException: object.Exception {
	this(string msg, string file = __FILE__,
		uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when the exponent of a result has been altered or constrained
/// in order to fit the constraints of a specific concrete representation.
/// General Decimal Arithmetic Specification, p. 15.
@safe
class ClampedException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when a non-zero dividend is divided by zero.
/// General Decimal Arithmetic Specification, p. 15.
@safe
class DivByZeroException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when a result is not exact (one or more non-zero coefficient
/// digits were discarded during rounding).
/// General Decimal Arithmetic Specification, p. 15.
@safe
class InexactException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when a result would be undefined or impossible.
/// General Decimal Arithmetic Specification, p. 15.
@safe
class InvalidOperationException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when the exponent of a result is too large to be represented.
/// General Decimal Arithmetic Specification, p. 15.
@safe
class OverflowException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when a result has been rounded (that is, some zero or non-zero
/// coefficient digits were discarded).
/// General Decimal Arithmetic Specification, p. 15.
@safe
class RoundedException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when a result is subnormal (its adjusted exponent is less
/// than the minimum exponent) before any rounding.
/// General Decimal Arithmetic Specification, p. 15.
@safe
class SubnormalException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/// Raised when a result is both subnormal and inexact.
/// General Decimal Arithmetic Specification, p. 15.
@safe
class UnderflowException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null)	{
		super(msg, file, line, next);
	}
};

//unittest {
//	writeln("==========================");
//	writeln("decimal context........end");
//	writeln("==========================");
//}
