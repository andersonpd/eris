// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.floating;

unittest
{
	writeln("========================");
	writeln("Double.....begin");
	writeln("========================");
}

version(unittest)
{
	import eris.assertions;
}

public struct Double {

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
		write("-- Double this(int)...");
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const Double that) {
		this.value = that.value;
	}

	/// Returns a copy of an Double.
	public const Double dup() {
		return Double(this);
	}

	unittest {	// copy
		write("-- Double copying...");
		writeln("passed");
	}

//--------------------------------
// bit manipulation
//--------------------------------

	/// Sets a single bit in an Double.
	public void setBit(int n, bool value = true) {
		if (value) {
			this |= shl(ONE, n);
		}
		else {
			this &= (shl(ONE, n).complement);
		}
	}

	/// Tests a single bit in an Double.
	public const bool testBit(int n) {
		Double value = this & shl(ONE, n);
		return !value.isZero;
	}

	unittest {	// bit manipulation
		write("-- Double bit manipulation...");
		uint128 test = uint128(0);
		assert(!test.testBit(5));
		test.setBit(5);
		assert(test.testBit(5));
		writeln("passed");
	}

//--------------------------------
// constants
//--------------------------------

	public enum Double ZERO = Double(0);
	public enum Double ONE  = Double(1);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the Double is zero.
	public const bool isZero() {
		return numDigits(this.value) == 0;
	}

	/// Returns true if the value of the Double is less than zero.
	/// For unsigned extended integers the return value is always false.
	public const bool isNegative() {
		return value < 0;
	}

	/// Returns true if the value of the Double is odd.
	public const bool isOdd() {
		return value & 1;
	}

	/// Returns true if the value of the Double is even.
	public const bool isEven() {
		return !isOdd();
	}

	unittest {	// classification
		write("-- Double classification...");
		writeln("passed");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial value for Double types.
	@property
	public static Double init() {
		return ZERO;
	}

	// min/max properties

   	/// Returns the maximum value for this type.
	@property
	public static Double max() {
		return Double(int.max);
	}

	/// Returns the minimum value for this type.
	@property
	public static Double min() {
		return Double(int.min);
	}

	unittest
	{	// min/max values
		write("-- Double min/max values...");
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
		write("-- Double this(string)...");
		writeln("passed");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		// FIXTHIS: Implement toString
		string str = "";
		return str;
	}
	unittest { // toString
		write("-- Double toString...");
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

	/// Converts the Double to an unsigned integer.
	public const int toInt() {
		return value;
	}

	unittest {	// conversion
		write("-- Double to integer...");
		writeln("passed");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this Double is equal to the argument.
	private const bool opEquals(T:Double)(const T that) {
		return this.value == that.value;
	}

	 /// Returns true if this Double is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(Double(that));
	}

	unittest { // equality
		write("-- Double equality...");
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this Double is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T:Double)(const T that) {
		if (this < that) return -1;
		if (this > that) return  1;
		return 0;
	}

	/// Returns -1, 0, or 1, if this Double is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T)(const T that) {
		return opCmp(Double(that));
	}

	unittest { // comparison
		write("-- Double comparison...");
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns an Double value to this.
	private void opAssign(T:Double)(const T that) {
		this.value = that.value;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(Double(that));
	}

	unittest {	// assignment
		write("-- Double opAssign...");
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref Double opOpAssign(string op, T:Double)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref Double opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- Double opOpAssign...");
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private Double opUnary(string op)() {
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

	/// Returns a copy of this Double
	public const Double plus() {
		return Double(this.value);
	}

	/// Returns the ones complement of this Double
	public const Double complement() {
		return Double(~this.value);
	}

	/// Returns the twos complement of this Double
	public const Double negate() {
  		// TODO: implement
	}

	unittest {	// opUnary
		write("-- Double unary operations...");
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const Double opBinary(string op, T:Double)(const T that)
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

	private const Double opBinary(string op, T)(const T that) {
		return opBinary!(op, Double)(Double(that));
	}

	unittest {	// opBinary
		write("-- Double opBinary...");
		writeln("passed");
	}

	/// Adds two Numbers and returns the sum.
	public const Double add(const Double x, const Double y) {
		return Double(x.value + y.value);
	}

	unittest {
		write("-- Double addition...");
		writeln("passed");
	}

	/// Subtracts one Double from another and returns the difference.
	public const Double sub(const Double x, const Double y) {
		return Double(x.value - y.value);
	}

	unittest {
		write("-- Double subtraction...");
		writeln("passed");
	}

	/// Multiplies two unsigned extended integers and returns the product.
	public const Double mul(const Double x, const Double y) {
		return Double(x.value * y.value);
	}

	unittest {
		write("-- Double mul...");
		writeln("passed");
	}

	/// Divides one Double by another
	/// and returns the quotient.
	public const Double div(const Double x, const Double y)
	{
		return Double(x.value / y.value);
	}

	unittest {
		write("-- Double div...");
		writeln("passed");
	}

	/// Divides one Double by another
	/// and returns the remainder.
	public const Double mod(const Double x, const Double y)
	{
		return Double(x.value % y.value);
	}

	unittest {
		write("-- Double mod...");
		writeln("passed");
	}

	/// Raises an Double to an integer power
	/// and returns the result. Tests the result for overflow.
	public const Double pow(const Double x, const Double y) {
		return Double(x.value ^^ y.value);
	}

	/// Raises an Double to an integer power
	/// and returns the result. Tests the result for overflow.
	public const Double pow(const Double x, const int n) {
		return Double(x.value ^^ n);
	}

	unittest {
		write("power...");
		writeln("test missing");
	}

	/// Returns the logical AND of two unsigned extended integers
	public const Double and(const Double x, const Double y) {
		return Double(x.value & y.value);
	}

	/// Returns the logical OR of two unsigned extended integers
	public const Double or(const Double x, const Double y) {
		return Double(x.value | y.value);
	}

	/// Returns the logical XOR of two unsigned extended integers
	public const Double xor(const Double x, const Double y) {
		return Double(x.value ^ y.value);
	}

	unittest {
		write("Double logical ops...");
		writeln("passed");
	}

	/// Shifts an Double left by an integral value.
	/// No check for overflow is made.
	public const Double shl(const Double x, const Double y) {
		return Double(x.value << y.value);
	}

	/// Shifts an Double left by an integral value.
	public const Double shl(const Double x, const uint n) {
		return Double(x.value << n);
	}

	/// Shifts an Double right by an integral value.
	public static Double shr(const Double x, const Double y) {
		return Double(x.value >> y.value);
	}

	/// Shifts an Double right by an integral value.
	public static Double shr(const Double x, const uint n) {
		return Double(x.value >> n);
	}

	/// Shifts an Double right by an integral value.
	public const Double lshr(const Double x, const Double y) {
		return Double(x.value >>> y.value);
	}

	/// Shifts an Double right by an integral value.
	public const Double lshr(const Double x, const uint n) {
		return Double(x.value >>> n);
	}

	unittest {
		write("shift ops...");
		writeln("test missing");
	}

	/// Returns the absolute value of the value of the Double.
	/// No effect on unsigned extended integers -- returns a copy.
	public const Double abs() {
		Double copy = this.dup;
		if (copy.value < 0) copy.value = -copy.value;
		return copy;
	}

	unittest {
		write("-- Double abs...");
		writeln("passed");
	}

	public const Double sqr() {
		return Double(this.value * this.value);
	}

	unittest {
		write("-- Double sqr...");
		writeln("passed");
	}

	unittest {
		writeln("===================");
		writeln("Double........end");
		writeln("===================");
	}

}	// end Double

