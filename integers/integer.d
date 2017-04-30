// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.integers.integer;

version(unittest) {
	import std.stdio;
	import eris.test.assertion;
}

unittest {
	writeln("==========================");
	writeln("example integers.....begin");
	writeln("==========================");
}


///
/// An example of an new integer type.
/// Implemented as a simple wrapper around a built-in integer type.
/// Intended as an example only.
///
public struct Integer(Z) {

	alias Int = Integer!Z;

//--------------------------------
// structure
//--------------------------------

	private Z value = 0;

//--------------------------------
// construction
//--------------------------------

	public this(const Z _value) {
		this.value = _value;
	}

	unittest {	// construction
		auto a = Int(27);
		assertNotEqual(a, 28);
		assertEqual(a, 27);
	}

//--------------------------------
// copying
//--------------------------------

	/// copy constructor.
	public this(const Int that) {
		this.value = that.value;
	}

	/// Returns a copy of an integer.
	public const Int copy() {
		return Int(this);
	}

	unittest {	// copy
		Int a = Int(217);
		Int b = a.copy;
		assertEqual(b, a);
		Int c = Int(b);
		assertEqual(c, 217);
	}

//--------------------------------
// constants
//--------------------------------

	public enum Int ZERO = Int(0);
	public enum Int ONE  = Int(1);
	public enum Int MIN  = Int(Z.min);
	public enum Int MAX  = Int(Z.max);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the integer is zero.
	public bool isZero() const {
		return this.value == 0;
	}

	/// Returns true if the value of the integer is less than zero.
	public bool isNegative() const {
		return this.value < 0;
	}

	/// Returns true if the value of the integer is odd.
	public bool isOdd() const {
		return this.value & 1;
	}

	/// Returns true if the value of the integer is even.
	public bool isEven() const {
		return !isOdd();
	}

	unittest {	// classification
		assertTrue(ZERO.isZero);
		Int a = Int(2);
		assertTrue(a.isEven);
		Int b = Int(13);
		assertTrue(b.isOdd);
		Int c = Int(-23);
		assertTrue(c.isNegative);
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial value for Z types.
	@property
	public enum Int init() {
		return ZERO;
	}

   	/// Returns the maximum value for this type.
	@property
	public static Int max() {
		return MAX;
	}

	/// Returns the minimum value for this type.
	@property
	public static Int min() {
		return MIN;
	}

	unittest {	// min/max values
		Int a = Int.max;
		assertEqual(a.value, Z.max);
		Int b = Int.min;
		assertEqual(b.value, Z.min);
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns an integer value to this.
	private void opAssign(T: Int)(const T that) {
		this.value = that.value;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(Int(that));
	}

	unittest {	// assignment
		Int a = 123;
		assertEqual(a.value, 123);
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this integer is equal to the argument.
	private bool opEquals(T:Int)(const T that) const {
		return this.value == that.value;
	}

	 /// Returns true if this integer is equal to the argument.
	private bool opEquals(T)(const T that) const {
		return opEquals(Int(that));
	}

	unittest { // equality
		Int a = Int(123);
		Int b = Int(123);
		assertEqual(a, b);
		assertEqual(a, 123);
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this integer is, respectively,
	/// less than, equal to or greater than the argument.
	private int opCmp(T: Int)(const T that) const {
		if (this.value < that.value) return -1;
		if (this.value > that.value) return  1;
		return 0;
	}

	/// Returns -1, 0, or 1, if this integer is, respectively,
	/// less than, equal to or greater than the argument.
	private int opCmp(T)(const T that) const {
		return opCmp(Int(that));
	}

	unittest { // comparison
		Int a = 5;
		Int b = 6;
		assertLessThan(a, b);
		assertGreaterThan(a, 4);
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private Int opUnary(string op)() {
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

	/// Returns a copy of this integer
	public Int plus() const {
		return Int(this.value);
	}

	/// Returns the one's complement of this integer
	public Int complement() const {
		return Int(~this.value);
	}

	/// Returns the two's complement of this integer
	public Int negate() const {
		return Int(-this.value);
	}

	unittest {	// opUnary
	}

//--------------------------------
// binary operations
//--------------------------------

	private const Int opBinary(string op, T: Int)(const T that)
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

	private const Int opBinary(string op, T)(const T that) {
		return opBinary!(op, Int)(Int(that));
	}

	unittest {	// opBinary
		Int a = 5;
		Int b = 3;
		assertEqual(a + b, 8);
		assertEqual(a - b, 2);
		assertEqual(a * b, 15);
		assertEqual(a / b, 1);
		assertEqual(a % b, 2);
		assertEqual(a ^^ b, 125);
		assertEqual(a & b, 1);
		assertEqual(a | b, 7);
		assertEqual(a ^ b, 6);
		assertEqual(a << b, 40);
		assertEqual(a >> b, 0);
		assertEqual(a >>> b, 0);
	}

	/// Adds two Numbers and returns the sum.
	public const Int add(const Int x, const Int y) {
		return Int(x.value + y.value);
	}

	unittest {
		Int a = 12;
		Int b = 23;
		assertEqual(a + b, 35);
	}

	/// Subtracts one Z from another and returns the difference.
	public const Int sub(const Int x, const Int y) {
		return Int(x.value - y.value);
	}

	unittest {
		Int a = 12;
		Int b = 23;
		assertEqual(a - b, -11);
	}

	/// Multiplies two unsigned extended integers and returns the product.
	public const Int mul(const Int x, const Int y) {
		return Int(x.value * y.value);
	}

	unittest {
		Int a = 12;
		Int b = 23;
		assertEqual(a * b, 276);
	}

	/// Divides one Z by another
	/// and returns the quotient.
	public const Int div(const Int x, const Int y)
	{
		return Int(x.value / y.value);
	}

	unittest {
		Int a = 1205;
		Int b = 23;
		assertEqual(a / b, 52);
	}

	/// Divides one Z by another
	/// and returns the remainder.
	public const Int mod(const Int x, const Int y)
	{
		return Int(x.value % y.value);
	}

	unittest {
		Int a = 1205;
		Int b = 23;
		assertEqual(a % b, 9);
	}

	/// Raises an integer to an integer power
	/// and returns the result. Tests the result for overflow.
	public const Int pow(const Int x, const Int y) {
		return Int(x.value ^^ y.value);
	}

	/// Raises an integer to an integer power
	/// and returns the result. Tests the result for overflow.
	public const Int pow(const Int x, const Z n) {
		return Int(x.value ^^ n);
	}

	unittest {
		Int a = 12;
		Int b = 3;
		assertEqual(a ^^ b, 1728);
		auto n = 4;
		assertEqual(a ^^ n, 20736);
	}

	/// Returns the logical AND of two unsigned extended integers
	public const Int and(const Int x, const Int y) {
		return Int(x.value & y.value);
	}

	unittest {
		Int a = 1290;
		Int b = 4533;
		assertEqual(a & b, 256);
	}

	/// Returns the logical OR of two unsigned extended integers
	public const Int or(const Int x, const Int y) {
		return Int(x.value | y.value);
	}

	unittest {
		Int a = 1208;
		Int b = 324;
		assertEqual(a | b, 1532);
	}

	/// Returns the logical XOR of two unsigned extended integers
	public const Int xor(const Int x, const Int y) {
		return Int(x.value ^ y.value);
	}

	unittest {
		Int a = 0xFFF;
		Int b = 0xFF;
		assertEqual(a ^ b, 3840);
	}


	/// Shifts an integer left by an integral value.
	/// No check for overflow is made.
	public const Int shl(const Int x, const Int y) {
		return Int(x.value << y.value);
	}

	/// Shifts an integer left by an integral value.
	public const Int shl(const Int x, const uint n) {
		return Int(x.value << n);
	}

	/// Shifts an integer right by an integral value.
	public static Int shr(const Int x, const Int y) {
		return Int(x.value >> y.value);
	}

	/// Shifts an integer right by an integral value.
	public static Int shr(const Int x, const uint n) {
		return Int(x.value >> n);
	}

	/// Shifts an integer right by an integral value.
	public const Int lshr(const Int x, const Int y) {
		return Int(x.value >>> y.value);
	}

	/// Shifts an integer right by an integral value.
	public const Int lshr(const Int x, const uint n) {
		return Int(x.value >>> n);
	}

	unittest {
	}

	/// Returns the absolute value of the value of the integer.
	public const Int abs() {
		Int copy = this.copy;
		if (copy.value < 0) copy.value = -copy.value;
		return copy;
	}

	public static Int abs(const Int x) {
		return x.abs;
	}

	unittest {
		Int a = -14;
		Int b = 14;
		assertEqual(a.abs, b);
		assertEqual(abs(a), b);
	}

	public const Int sqr() {
		return Int(this.value * this.value);
	}

	public static Int sqr(const Int x) {
		return x.sqr;
	}

	unittest {
		Int a = 14;
		Int b = 196;
		assertEqual(a.sqr, b);
		assertEqual(sqr(a), b);
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref Int opOpAssign(string op, T: Int)(const Int that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref Int opOpAssign(string op, T)(const T that) {
		this = opBinary!op(Int(that));
		return this;
	}

	unittest {	// opOpAssign
		Int a = 4;
		Int b = 3;
		a += b;
		assertEqual(a, 7);
		a -= b;
		assertEqual(a, 4);
		a *= b;
		assertEqual(a, 12);
		a /= b;
		assertEqual(a, 4);
		a *= 2;
		assertEqual(a, 8);
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	/// Constructs an integer from a string
	public this(const string str) {
		this(std.conv.to!Z(str));
	}

	unittest { // parse string
		Int a = "1465";
		assertEqual(a, 1465);
	}

	/// Converts the integer value to a string.
	public const string toString() {
		return std.conv.to!string(value);
	}

	/// Converts the signed integer value to a string.
	public const string toHexString() {
		return std.string.format("0X%08X", value);
	}

	unittest { // toString
		Int a = 346;
		string str = "346";
		assertEqual(a.toString, str);
		string hex = "0X0000015A";
		assertEqual(a.toHexString, hex);
	}

//--------------------------------
// conversion to/from integral
//--------------------------------

	///
 	bool opCast(T:bool)() const
	{
		return !isZero;
	}

	unittest {
		Int a = 1;
		assertTrue(a);
		assertFalse(!a);
		Int b = 0;
		assertFalse(b);
	}


	/// Converts the integer to a built-in long integer.
	public const long toLong() {
		return cast(long)value;
	}

	unittest {	// conversion
//		long m = 123L;
//		Int a = Int(123);
		//assert(typeid(a.toLong) == typeid(m));
	}

//--------------------------------
// bit manipulation
//--------------------------------

	/// Sets a single bit in an integer.
	public void setBit(Int n, bool set = true) {
		if (set) {
			this |= shl(ONE, n);
		}
		else {
			this &= (shl(ONE, n).complement);
		}
	}

	/// Tests a single bit in an integer.
	public const bool testBit(Int n) {
		Int state = this & shl(ONE, n);
		return !state.isZero;
	}

	unittest {	// bit manipulation
	}

	unittest {
		writeln("==========================");
		writeln("example integers.......end");
		writeln("==========================");
	}
}	// end Integer(Z)

unittest {
	Integer!int i = Integer!int(12);
}


