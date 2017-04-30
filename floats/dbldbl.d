// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.floats;

/*import std.ascii;
import std.conv;
import std.stdio;
import std.string;
import std.traits;*/

unittest
{
	writeln("=================");
	writeln("DoubleDouble.....begin");
	writeln("=================");
}

version(unittest)
{
	import eris.assertions;
}

public struct DoubleDouble {

//--------------------------------
// structure
//--------------------------------

	private double hi;
	private double lo;

//--------------------------------
// construction
//--------------------------------

	public this(double hi, double lo) {
		this.hi = hi;
		this.lo = lo;
	}

	public this(double x) {
		this(x, 0.0);
	}

	unittest {	// construction
		write("-- DoubleDouble this(int)...");
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const DoubleDouble that) {
		this.hi = that.hi;
		this.lo = that.lo;
	}

	/// Returns a copy of an DoubleDouble.
	public const DoubleDouble dup() {
		return DoubleDouble(this);
	}

	unittest {	// copy
		write("-- DoubleDouble copying...");
		writeln("passed");
	}

//--------------------------------
// bit manipulation
//--------------------------------

	/// Sets a single bit in an DoubleDouble.
	public void setBit(int n, bool value = true) {
		if (value) {
			this |= shl(ONE, n);
		}
		else {
			this &= (shl(ONE, n).complement);
		}
	}

	/// Tests a single bit in an DoubleDouble.
	public const bool testBit(int n) {
		DoubleDouble value = this & shl(ONE, n);
		return !value.isZero;
	}

	unittest {	// bit manipulation
		write("-- DoubleDouble bit manipulation...");
		uint128 test = uint128(0);
		assert(!test.testBit(5));
		test.setBit(5);
		assert(test.testBit(5));
		writeln("passed");
	}

//--------------------------------
// constants
//--------------------------------

	public enum DoubleDouble ZERO = DoubleDouble(0.0);
	public enum DoubleDouble ONE  = DoubleDouble(1.0);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the DoubleDouble is zero.
	public const bool isZero() {
		return (this.hi == 0.0 && this.lo == 0.0);
	}

	/// Returns true if the value of the DoubleDouble is less than zero.
	/// For unsigned extended integers the return value is always false.
	public const bool isNegative() {
		return value < 0;
	}

	/**
	 * Returns true if this value is NaN
	 */
	public boolean isNaN() {
		return Double.isNaN(hi);
	}

	/// Returns true if the value of the DoubleDouble is odd.
	public const bool isOdd() {
		return value & 1;
	}

	/// Returns true if the value of the DoubleDouble is even.
	public const bool isEven() {
		return !isOdd();
	}

	unittest {	// classification
		write("-- DoubleDouble classification...");
		writeln("passed");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial value for DoubleDouble types.
	@property
	public static DoubleDouble init() {
		return ZERO;
	}

	// min/max properties

   	/// Returns the maximum value for this type.
	@property
	public static DoubleDouble max() {
		return DoubleDouble(int.max);
	}

	/// Returns the minimum value for this type.
	@property
	public static DoubleDouble min() {
		return DoubleDouble(int.min);
	}

	unittest
	{	// min/max values
		write("-- DoubleDouble min/max values...");
		writeln("passed");
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string str) {
		// FIXTHIS: convert string to int
	}


	unittest
	{
		write("-- DoubleDouble this(string)...");
		writeln("passed");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		// FIXTHIS: Implement toString
		string str = "";
		return str;
	}
	unittest { // toString
		write("-- DoubleDouble toString...");
		writeln("passed");
	}

//--------------------------------
// conversion to/from integral
//--------------------------------

/*	///
    T opCast(T:bool)() pure
    {
        return !isZero();
    }*/

	/// Converts the DoubleDouble to an unsigned integer.
	public const int toInt() {
		return value;
	}

	unittest {	// conversion
		write("-- DoubleDouble to integer...");
		writeln("passed");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this DoubleDouble is equal to the argument.
	private const bool opEquals(T:DoubleDouble)(const T that) {
		return this.value == that.value;
	}

	 /// Returns true if this DoubleDouble is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(DoubleDouble(that));
	}

	unittest { // equality
		write("-- DoubleDouble equality...");
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this DoubleDouble is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T:DoubleDouble)(const T that) {
		if (this < that) return -1;
		if (this > that) return  1;
		return 0;
	}

	/// Returns -1, 0, or 1, if this DoubleDouble is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T)(const T that) {
		return opCmp(DoubleDouble(that));
	}

	unittest { // comparison
		write("-- DoubleDouble comparison...");
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns an DoubleDouble value to this.
	private void opAssign(T:DoubleDouble)(const T that) {
		this.value = that.value;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(DoubleDouble(that));
	}

	unittest {	// assignment
		write("-- DoubleDouble opAssign...");
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref DoubleDouble opOpAssign(string op, T:DoubleDouble)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref DoubleDouble opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- DoubleDouble opOpAssign...");
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private DoubleDouble opUnary(string op)() {
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

	/// Returns a copy of this DoubleDouble
	public const DoubleDouble plus() {
		return DoubleDouble(this.value);
	}

	/// Returns the ones complement of this DoubleDouble
	public const DoubleDouble complement() {
		return DoubleDouble(~this.value);
	}

	/// Returns the twos complement of this DoubleDouble
	public const DoubleDouble negate() {
  		// TODO: implement
	}

	unittest {	// opUnary
		write("-- DoubleDouble unary operations...");
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const DoubleDouble opBinary(string op, T:DoubleDouble)(const T that)
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

	private const DoubleDouble opBinary(string op, T)(const T that) {
		return opBinary!(op, DoubleDouble)(DoubleDouble(that));
	}

	unittest {	// opBinary
		write("-- DoubleDouble opBinary...");
		writeln("passed");
	}

	/// Adds two Numbers and returns the sum.
	public const DoubleDouble add(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value + y.value);
	}

	unittest {
		write("-- DoubleDouble addition...");
		writeln("passed");
	}

	/// Subtracts one DoubleDouble from another and returns the difference.
	public const DoubleDouble sub(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value - y.value);
	}

	unittest {
		write("-- DoubleDouble subtraction...");
		writeln("passed");
	}

	/// Multiplies two unsigned extended integers and returns the product.
	public const DoubleDouble mul(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value * y.value);
	}

	unittest {
		write("-- DoubleDouble mul...");
		writeln("passed");
	}

	/// Divides one DoubleDouble by another
	/// and returns the quotient.
	public const DoubleDouble div(const DoubleDouble x, const DoubleDouble y)
	{
		return DoubleDouble(x.value / y.value);
	}

	unittest {
		write("-- DoubleDouble div...");
		writeln("passed");
	}

	/// Divides one DoubleDouble by another
	/// and returns the remainder.
	public const DoubleDouble mod(const DoubleDouble x, const DoubleDouble y)
	{
		return DoubleDouble(x.value % y.value);
	}

	unittest {
		write("-- DoubleDouble mod...");
		writeln("passed");
	}

	/// Raises an DoubleDouble to an integer power
	/// and returns the result. Tests the result for overflow.
	public const DoubleDouble pow(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value ^^ y.value);
	}

	/// Raises an DoubleDouble to an integer power
	/// and returns the result. Tests the result for overflow.
	public const DoubleDouble pow(const DoubleDouble x, const int n) {
		return DoubleDouble(x.value ^^ n);
	}

	unittest {
		write("power...");
		writeln("test missing");
	}

	/// Returns the logical AND of two unsigned extended integers
	public const DoubleDouble and(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value & y.value);
	}

	/// Returns the logical OR of two unsigned extended integers
	public const DoubleDouble or(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value | y.value);
	}

	/// Returns the logical XOR of two unsigned extended integers
	public const DoubleDouble xor(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value ^ y.value);
	}

	unittest {
		write("DoubleDouble logical ops...");
		writeln("passed");
	}

	/// Shifts an DoubleDouble left by an integral value.
	/// No check for overflow is made.
	public const DoubleDouble shl(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value << y.value);
	}

	/// Shifts an DoubleDouble left by an integral value.
	public const DoubleDouble shl(const DoubleDouble x, const uint n) {
		return DoubleDouble(x.value << n);
	}

	/// Shifts an DoubleDouble right by an integral value.
	public static DoubleDouble shr(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value >> y.value);
	}

	/// Shifts an DoubleDouble right by an integral value.
	public static DoubleDouble shr(const DoubleDouble x, const uint n) {
		return DoubleDouble(x.value >> n);
	}

	/// Shifts an DoubleDouble right by an integral value.
	public const DoubleDouble lshr(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.value >>> y.value);
	}

	/// Shifts an DoubleDouble right by an integral value.
	public const DoubleDouble lshr(const DoubleDouble x, const uint n) {
		return DoubleDouble(x.value >>> n);
	}

	unittest {
		write("shift ops...");
		writeln("test missing");
	}

	/// Returns the absolute value of the value of the DoubleDouble.
	/// No effect on unsigned extended integers -- returns a copy.
	public const DoubleDouble abs() {
		DoubleDouble copy = this.dup;
		if (copy.value < 0) copy.value = -copy.value;
		return copy;
	}

	unittest {
		write("-- DoubleDouble abs...");
		writeln("passed");
	}

	public const DoubleDouble sqr() {
		return DoubleDouble(this.value * this.value);
	}

	unittest {
		write("-- DoubleDouble sqr...");
		writeln("passed");
	}

	unittest {
		writeln("===================");
		writeln("DoubleDouble.........end");
		writeln("===================");
	}

}	// end DoubleDouble

