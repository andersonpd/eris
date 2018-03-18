// Written in the D programming language

/**
 * Arithmetic context for floating-point decimal arithmetic.
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

module eris.decimal.context;

unittest {
	writeln("      context tests       ");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
}

/// Pre-defined default context for decimal numbers.
public enum Context DefaultContext = Context(99, 9999, HALF_EVEN);

/// Approximates floating-point real numbers.
public enum Context RealContext    = Context(real.dig, real.max_10_exp, HALF_EVEN);
/// Approximates floating-point double numbers.
public enum Context DoubleContext  = Context(double.dig, double.max_10_exp, HALF_EVEN);
/// Approximates floating-point float numbers.
public enum Context FloatContext   = Context(float.dig, float.max_10_exp, HALF_EVEN);

/// Decimal64 BID
public enum Context Bid32Context   = Context(7, 96, HALF_EVEN);
/// Decimal64 BID
public enum Context Bid64Context   = Context(16, 369, HALF_EVEN);

/**
 * The available rounding modes. For cumulative operations use the
 * halfEven mode to prevent accumulation of errors. Otherwise, the
 * halfUp and halfDown modes are satisfactory. The up, down, floor,
 * and ceiling modes are also useful for some operations.
 * General Decimal Arithmetic Specification, p. 13-14.
 *
 */
public enum Round {
	none,
    halfEven,
    halfDown,
    halfUp,
    down,
    up,
    floor,
    ceiling,
}

alias ROUND_NONE = Round.none;
alias HALF_EVEN  = Round.halfEven;
alias HALF_DOWN  = Round.halfDown;
alias HALF_UP    = Round.halfUp;
alias ROUND_DOWN = Round.down;
alias ROUND_UP   = Round.up;
alias FLOOR      = Round.floor;
alias CEILING    = Round.ceiling;

/**
 *  Flags: The available flags and trap-enablers.
 *
 *  The larger value have higher precedence.
 *  If more than one flag is set by an operation and traps are enabled,
 *  the flag with higher precedence will throw its exception.
 *  General Decimal Arithmetic Specification, p. 15.
 */

///
public enum : ubyte
{
	INVALID_OPERATION  = 0x80,
	DIVISION_BY_ZERO   = 0x40,
	OVERFLOW           = 0x20,
	SUBNORMAL          = 0x10,
	INEXACT            = 0x08,
	ROUNDED            = 0x04,
	UNDERFLOW          = 0x02,
	CLAMPED            = 0x01
}


/**
 *	Decimal arithmetic operations are governed by their context.
 *  The context specifies the precision (number of decimal digits)
 *  the maximum exponent value and the rounding mode in place for the operation.
 * 	The result of most operations will be rounded to the context precision
 *  using the context rounding mode
 */
public struct Context
{
	public immutable int precision;
	public immutable int maxExpo;
	public immutable Round mode;

	@disable this();

	this(int precision, int maxExpo, Round mode)
	{
		this.precision = precision;
		this.maxExpo = maxExpo;
		this.mode = mode;
	}
}

//public bool enableFlags = false;

version(unittest)
{
	public enum Context TestContext = Context(9, 99, HALF_EVEN);
}

/**
 *  Standards: "The exceptional conditions are grouped into signals,
 *  which can be controlled individually.
 *  The context contains a flag (which is either 0 or 1)
 *  and a trap-enabler (which also is either 0 or 1) for each signal.
 *  For each of the signals, the corresponding flag is
 *  set to 1 when the signal occurs.
 *  It is only reset to 0 by explicit user action."
 *  General Decimal Arithmetic Specification, p. 15.
 */
public struct ContextFlags {

	private static ubyte flags = 0;
	private static ubyte traps = 0;

//	static ContextFlags instance = ContextFlags();

	 ///  Sets or resets the specified context flag(s).
	@safe
	public void set(ubyte flags, bool value = true) {
		if (value) {
			ubyte saved = this.flags;
			this.flags |= flags;
			ubyte changed = saved ^ flags;
			checkFlags(changed);
		} else {
			this.flags &= !flags;
		}
	}

	public void resetFlags(ubyte flags) {
		set(flags, false);
	}

	// Checks the state of the flags. If a flag is set and its
	// trap-enabler is set, an exception is thrown.
	@safe
    public void checkFlags(ubyte flags) {
		if (flags & INVALID_OPERATION && traps & INVALID_OPERATION) {
			throw new InvalidOperationException("InvalidOperation");
		}
		if (flags & DIVISION_BY_ZERO && traps & DIVISION_BY_ZERO) {
			throw new DivByZeroException("DivisionByZero");
		}
		if (flags & OVERFLOW && traps & OVERFLOW) {
			throw new OverflowException("Overflow");
		}
		if (flags & SUBNORMAL && traps & SUBNORMAL) {
			throw new SubnormalException("Subnormal");
		}
		if (flags & INEXACT && traps & INEXACT) {
			throw new InexactException("Inexact");
		}
		if (flags & ROUNDED && traps & ROUNDED) {
			throw new RoundedException("Rounded");
		}
		if (flags & UNDERFLOW && traps & UNDERFLOW) {
			throw new UnderflowException("Underflow");
		}
		if (flags & CLAMPED && traps & CLAMPED) {
			throw new ClampedException("Clamped");
		}
	}

	///  Gets the value of the specified context flag.
	public bool getFlags(ubyte flags) {
		return (this.flags & flags) == flags;
	}

	 ///  Returns a snapshot of the context flags.
	public ubyte getFlags() {
		return flags;
	}

	///  Clears all the context flags.
	public void clearFlags() {
		flags = 0;
	}

	///  Sets or resets the specified trap(s).
	public void setTraps(ubyte traps, bool value = true) {
		if (value) {
			this.traps |= traps;
		} else {
			this.traps &= !traps;
		}
	}

	 ///  Returns the value of the specified trap.
	 bool getTrap(ubyte trap) {
		return (this.traps & trap) == trap;
	}

	///  Returns a snapshot of traps.
	public ubyte getTraps() {
		return traps;
	}

	 ///  Clears all the traps.
	public void clearTraps() {
		traps = 0;
	}

};

// this is the single instance of the context flags.
public enum ContextFlags contextFlags = ContextFlags();

//--------------------------
// Context flags and trap-enablers
//--------------------------
/**
 *  The base class for all decimal arithmetic exceptions.
 */
@safe
class DecimalException: object.Exception {
	this(string msg, string file = __FILE__,
		uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when the exponent of a result has been altered or constrained
 *  in order to fit the constraints of a specific concrete representation.
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class ClampedException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when a non-zero dividend is divided by zero.
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class DivByZeroException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when a result is not exact (one or more non-zero coefficient
 *  digits were discarded during rounding).
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class InexactException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when a result would be undefined or impossible.
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class InvalidOperationException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when the exponent of a result is too large to be represented.
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class OverflowException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when a result has been rounded (that is, some zero or non-zero
 *  coefficient digits were discarded).
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class RoundedException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when a result is subnormal (its adjusted exponent is less
 *  than the minimum exponent) before any rounding.
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class SubnormalException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null) {
		super(msg, file, line, next);
	}
};

/**
 *  Raised when a result is both subnormal and inexact.
 *  General Decimal Arithmetic Specification, p. 15.
 */
@safe
class UnderflowException: DecimalException {
	this(string msg, string file = __FILE__,
	     uint line = cast(uint)__LINE__, Throwable next = null)	{
		super(msg, file, line, next);
	}
};

unittest
{
	writeln("==========================");
}

