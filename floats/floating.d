// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.floating;

import std.math;
/*import std.ascii;
import std.conv;
import std.stdio;
import std.string;
import std.traits;*/

version(unittest) {
	import std.stdio;
	import eris.assertions;
}

unittest {
	writeln("========================");
	writeln("floating module......begin");
	writeln("========================");
}

public struct Floating {

//--------------------------------
// structure
//--------------------------------

	private double value = 0;

//--------------------------------
// construction
//--------------------------------

	public this(const double value) {
		this.value = value;
	}

	unittest {	// construction
		write("-- this(double)........");
		Floating a = Floating(27);
		assertEqual(a.value, 27);
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const Floating that) {
		this.value = that.value;
	}

	/// Returns a copy of a Floating.
	public const Floating dup() {
		return Floating(this);
	}

	unittest {	// copy
		write("-- copying..........");
		Floating a = Floating(217);
		Floating b = a.dup;
		assertEqual(b.value, 217);
		Floating c = Floating(b);
		assertEqual(b.value, 217);
		writeln("passed");
	}

//--------------------------------
// bit manipulation
//--------------------------------

	/// Sets a single bit in a Floating.
	public void setBit(double n, bool value = true) {
		if (value) {
			this |= shl(ONE, n);
		}
		else {
			this &= (shl(ONE, n).complement);
		}
	}

	/// Tests a single bit in a Floating.
	public const bool testBit(double n) {
		Floating value = this & shl(ONE, n);
		return !value.isZero;
	}

	unittest {	// bit manipulation
		write("-- bit manipulation...");
		writeln("test missing");
	}

//--------------------------------
// constants
//--------------------------------

	public enum Floating ZERO = Floating(0);
	public enum Floating ONE  = Floating(1);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the Floating is zero.
	public const bool isZero() {
		return this.value == 0;
	}

	/// Returns true if the value of the Floating is less than zero.
	/// For unsigned extended integers the return value is always false.
	public const bool isNegative() {
		return value < 0;
	}

	/// Returns true if the value of the Floating is odd.
	public const bool isOdd() {
		return value & 1;
	}

	/// Returns true if the value of the Floating is even.
	public const bool isEven() {
		return !isOdd();
	}

	unittest {	// classification
		write("-- classification...");
		writeln("test missing");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial value for Floating types.
	@property
	public static Floating init() {
		return ZERO;
	}

	// min/max properties

   	/// Returns the maximum value for this type.
	@property
	public static Floating max() {
		return Floating(double.max);
	}

	/// Returns the minimum value for this type.
	@property
	public static Floating min() {
		return Floating(double.min);
	}

	unittest
	{	// min/max values
		write("-- min/max values...");
		writeln("test missing");
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string str) {
		// FIXTHIS: convert string to double
	}


	unittest
	{
		write("-- this(string)...");
		writeln("test missing");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		// FIXTHIS: Implement toString
		string str = "";
		return str;
	}
	unittest { // toString
		write("-- toString...");
		writeln("test missing");
	}

//--------------------------------
// conversion to/from integral
//--------------------------------

/*	///
    T opCast(T:bool)() pure
    {
        return !isZero();
    }*/

	/// Converts the Floating to an unsigned integer.
	public const double toInt() {
		return value;
	}

	unittest {	// conversion
		write("-- to integer...");
		writeln("test missing");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this Floating is equal to the argument.
	private const bool opEquals(T:Floating)(const T that) {
		return this.value == that.value;
	}

	 /// Returns true if this Floating is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(Floating(that));
	}

	unittest { // equality
		write("-- equality...");
		writeln("test missing");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this Floating is, respectively,
	/// less than, equal to or greater than the argument.
	private const double opCmp(T:Floating)(const T that) {
		if (this < that) return -1;
		if (this > that) return  1;
		return 0;
	}

	/// Returns -1, 0, or 1, if this Floating is, respectively,
	/// less than, equal to or greater than the argument.
	private const double opCmp(T)(const T that) {
		return opCmp(Floating(that));
	}

	unittest { // comparison
		write("-- comparison...");
		writeln("test missing");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a Floating value to this.
	private void opAssign(T:Floating)(const T that) {
		this.value = that.value;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(Floating(that));
	}

	unittest {	// assignment
		write("-- opAssign...");
		writeln("test missing");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref Floating opOpAssign(string op, T:Floating)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref Floating opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- opOpAssign...");
		writeln("test missing");
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private Floating opUnary(string op)() {
		static if (op == "+") {
			return plus();
		} else static if (op == "-") {
			return negate();
		} else static if (op == "~") {
			return complement();
		}else static if (op == "++") {
			this = add(this, ONE);
			return this;
		} else static if (op == "--") {
			this = sub(this, ONE);
			return this;
		}
	}

	/// Returns a copy of this Floating
	public const Floating plus() {
		return Floating(this.value);
	}

	/// Returns the ones complement of this Floating
	public const Floating complement() {
		return Floating(~this.value);
	}

	/// Returns the twos complement of this Floating
	public const Floating negate() {
		return Floating(-this.value);
	}

	unittest {	// opUnary
		write("-- unary operations...");
		writeln("test missing");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const Floating opBinary(string op, T:Floating)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {
			return sub(this, that);
		} else static if (op == "*") {
			return mul(this, that);
		} else static if (op == "/") {
			return div(this, that);
		} else static if (op == "%") {
			return mod(this, that);
		} else static if (op == "^^") {
			return pow(this, that);
		} else static if (op == "&") {
			return and(this, that);
		} else static if (op == "|") {
			return or(this, that);
		} else static if (op == "^") {
			return xor(this, that);
		} else static if (op == "<<") {
			return shl(this, that);
		} else static if (op == ">>") {
			return lshr(this, that);
		} else static if (op == ">>>") {
			return lshr(this, that);
		}
	}

	private const Floating opBinary(string op, T)(const T that) {
		return opBinary!(op, Floating)(Floating(that));
	}

	unittest {	// opBinary
		write("-- opBinary...");
		writeln("test missing");
	}

	/// Adds two Numbers and returns the sum.
	public const Floating add(const Floating x, const Floating y) {
		return Floating(x.value + y.value);
	}

	unittest {
		write("-- addition...");
		writeln("test missing");
	}

	/// Subtracts one Floating from another and returns the difference.
	public const Floating sub(const Floating x, const Floating y) {
		return Floating(x.value - y.value);
	}

	unittest {
		write("-- subtraction...");
		writeln("test missing");
	}

	/// Multiplies two unsigned extended integers and returns the product.
	public const Floating mul(const Floating x, const Floating y) {
		return Floating(x.value * y.value);
	}

	unittest {
		write("-- mul...");
		writeln("test missing");
	}

	/// Divides one Floating by another
	/// and returns the quotient.
	public const Floating div(const Floating x, const Floating y)
	{
		return Floating(x.value / y.value);
	}

	unittest {
		write("-- div...");
		writeln("test missing");
	}

	/// Divides one Floating by another
	/// and returns the remainder.
	public const Floating mod(const Floating x, const Floating y)
	{
		return Floating(x.value % y.value);
	}

	unittest {
		write("-- mod...");
		writeln("test missing");
	}

	/// Raises a Floating to an integer power
	/// and returns the result. Tests the result for overflow.
	public const Floating pow(const Floating x, const Floating y) {
		return Floating(x.value ^^ y.value);
	}

	/// Raises a Floating to an integer power
	/// and returns the result. Tests the result for overflow.
	public const Floating pow(const Floating x, const double n) {
		return Floating(x.value ^^ n);
	}

	unittest {
		write("-- power...");
		writeln("test missing");
	}

	/// Returns the logical AND of two unsigned extended integers
	public const Floating and(const Floating x, const Floating y) {
		return Floating(x.value & y.value);
	}

	/// Returns the logical OR of two unsigned extended integers
	public const Floating or(const Floating x, const Floating y) {
		return Floating(x.value | y.value);
	}

	/// Returns the logical XOR of two unsigned extended integers
	public const Floating xor(const Floating x, const Floating y) {
		return Floating(x.value ^ y.value);
	}

	unittest {
		write("-- logical ops...");
		writeln("test missing");
	}

	/// Shifts a Floating left by an integral value.
	/// No check for overflow is made.
	public const Floating shl(const Floating x, const Floating y) {
		return Floating(x.value << y.value);
	}

	/// Shifts a Floating left by an integral value.
	public const Floating shl(const Floating x, const uint n) {
		return Floating(x.value << n);
	}

	/// Shifts a Floating right by an integral value.
	public static Floating shr(const Floating x, const Floating y) {
		return Floating(x.value >> y.value);
	}

	/// Shifts a Floating right by an integral value.
	public static Floating shr(const Floating x, const uint n) {
		return Floating(x.value >> n);
	}

	/// Shifts a Floating right by an integral value.
	public const Floating lshr(const Floating x, const Floating y) {
		return Floating(x.value >>> y.value);
	}

	/// Shifts a Floating right by an integral value.
	public const Floating lshr(const Floating x, const uint n) {
		return Floating(x.value >>> n);
	}

	unittest {
		write("-- shift ops...");
		writeln("test missing");
	}

	/// Returns the absolute value of the value of the Floating.
	/// No effect on unsigned extended integers -- returns a copy.
	public const Floating abs() {
		Floating copy = this.dup;
		if (copy.value < 0) copy.value = -copy.value;
		return copy;
	}

	unittest {
		write("-- abs...");
		writeln("test missing");
	}

	public const Floating sqr() {
		return Floating(this.value * this.value);
	}

	unittest {
		write("-- sqr...");
		writeln("test missing");
	}

}	// end Floating

unittest {
	writeln("========================");
	writeln("floating module........end");
	writeln("========================");
}

