// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.complex.perplex;

unittest
{
	writeln("========================");
	writeln("perplex............begin");
	writeln("========================");
}

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
	alias perplex = Perplex!double;
}

public struct Perplex(FLOAT) {

//--------------------------------
// structure
//--------------------------------

	public FLOAT r;
	public FLOAT h;

//--------------------------------
// construction
//--------------------------------

	public this(const FLOAT re, const FLOAT hy) {
		this.r = re;
		this.h = hy;
	}

	public this(const FLOAT re) {
		this.r = re;
		this.h = 0.0;
	}

	unittest {	// construction
		write("-- perplex this(int)...");
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const perplex that) {
		this.r = that.r;
		this.h = that.h;
	}

	/// Returns a copy of a perplex number.
	public const perplex dup() {
		return perplex(this);
	}

	unittest {	// copy
		write("-- perplex copying...");
		writeln("passed");
	}

//--------------------------------
// constants
//--------------------------------

	public enum perplex ZERO = perplex(0.0, 0.0);
	public enum perplex ONE  = perplex(1.0, 0.0);
	public enum perplex H    = perplex(0.0, 1.0);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the perplex is zero.
	public const bool isZero() {
		return (r == 0.0 && h == 0.0);
	}

	/// Returns true if the perplex number is not zero and has no hyperbolic part.
	public const bool isReal() {
		return (r != 0.0 && h == 0.0);
	}

	/// Returns true if the perplex number is not zero and has no real part.
	public const bool isHyperbolic() {
		return (r == 0.0 && h != 0.0);
	}

	unittest {	// classification
		write("-- perplex classification...");
		writeln("passed");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns (NaN,NaN) the initial value for perplex numbers.
	@property
	public static perplex init() {
		return perplex();
	}

	/// Returns the real component of this perplex number.
	@property
	public const FLOAT re() {
		return this.r;
	}

	/// Returns (NaN,NaN) the initial value for perplex types.
	@property
	public const FLOAT hy() {
		return this.h;
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string str) {
		// FIXTHIS: convert string to int
	}


	unittest
	{
		write("-- perplex this(string)...");
		writeln("passed");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		return toString(this);
	}

	/// Converts the signed integer value to a string.
	public static string toString(const perplex z) {
		string re = std.conv.to!string(z.r);
		string hy = std.conv.to!string(z.h);
		string sign = (z.h < 0) ? "" : "+";
		return re ~ sign ~ hy ~ "h";
	}

	unittest { // toString
		write("-- perplex toString...");
		perplex z = perplex(1.2,3.4);
		writefln("z = %s", z);
        z = 17.34;
		writefln("z = %s", z);
writefln("H = %s", H);
writefln("ONE = %s", ONE);
writefln("ZERO = %s", ZERO);
		writeln("passed");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this perplex is equal to the argument.
	private const bool opEquals(T:perplex)(const T that) {
		return this.r == that.r && this.h == that.h;
	}

	 /// Returns true if this perplex is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(perplex(that));
	}

	unittest { // equality
		write("-- perplex equality...");
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a perplex number to this perplex number.
	private void opAssign(T:perplex)(const T that) {
		this.r = that.r;
		this.h = that.h;
	}

	/// Assigns a compatible value to this.
	private void opAssign(T)(const T that) {
		opAssign(perplex(that));
	}

	unittest {	// assignment
		write("-- perplex opAssign...");
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref perplex opOpAssign(string op, T:perplex)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref perplex opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- perplex opOpAssign...");
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	/// Returns a copy of the perplex number
	public const perplex plus() {
		return perplex(this);
	}

	/// Returns the additive inverse of the perplex number
	public const perplex negate() {
		return perplex(-this.r, -this.h);
	}

	/// Returns the additive inverse of the perplex number
	public const perplex negate(const perplex z) {
		return this.negate;
	}

	/// Returns the conjugate of a perplex number.
	public static perplex conj(const perplex z) {
		return perplex(z.r, -z.h);
	}

	/// Returns the conjugate of this perplex number.
	public const perplex conj() {
		return conj(this);
	}

	unittest {
		write("-- perplex conj........");
		perplex z = perplex( 2.0, 4 );
		assertEqual(conj(z), perplex(2,-4));
		assertEqual(z.conj, perplex(2,-4));
		writeln("passed");
	}

	/// Returns the square of the absolute value of the perplex number.
	public static FLOAT sqrAbs(perplex z) {
        return z.abs^^2;
	}

	/// Returns the square of the absolute value of this perplex number.
	public const FLOAT sqrAbs() {
		return sqrAbs(this);
	}

	/// Returns the absolute value of the perplex number.
	/// Note that the absolute value is not positive-definite!
	public static FLOAT abs(perplex z) {
		return std.math.abs(z.r*z.r - z.h*z.h);
	}

	/// Returns the absolute value of this perplex number.
	/// Note that the absolute value is not positive-definite!
	public const FLOAT abs() {
		return abs(this);
	}

	unittest {
		write("-- perplex abs.........");
		perplex z = perplex( 2, 4 );
		assertEqual(abs(z), 12.0);
		assertEqual(z.abs,  12.0);
		assertEqual(sqrAbs(z), 144.0);
		assertEqual(z.sqrAbs,  144.0);
		writeln("passed");
	}

	/// Returns the argument of a perplex number.
	public static FLOAT arg(perplex z) {
		FLOAT x = (z.r);
		FLOAT y = (z.h);
		if (x == 0.0 && y == 0.0) {	// undefined at origin
			return 1.0;
		}
		if (x == y) {	// asympote w/ slope == 1
			return 1.0;
		}
		if (x == -y) {	// asymptote w/ slope = -1
			return -1.0;
		}
 		if (std.math.abs(x) > std.math.abs(y)) {	// |x| > |y|
			// hyperbolic quadrant I, III
			return std.math.atanh(y/x);
		}
 		if (std.math.abs(x) < std.math.abs(y)) {	// |y| < |x|
			// hyperbolic quadrant II, IV
			return std.math.atanh(x/y);
		}
 		else {	// should not occur
			return FLOAT.nan;
		}
	}

	/// Returns the argument of this perplex number.
	public const FLOAT arg() {
		return arg(this);
	}

	unittest {
		write("-- perplex arg.........");
		perplex z;
		z = perplex(2.0, 4.0);		// Q I
		assertEqual(arg(z), std.math.atanh(0.5));
		z = perplex(-7.0, 3.0);		// Q II
		assertEqual(z.arg,  std.math.atanh(-3.0/7.0));
		z = perplex(-6.0, -5.0);	// Q III
		assertEqual(arg(z), std.math.atanh(5.0/6.0));
		z = perplex(-0.75, -2.5);	// Q IV
		assertEqual(z.arg,  std.math.atanh(0.75/2.5));
		z = perplex(3.0, 3.0);		// asymptote
		assertEqual(z.arg,  1);
		writeln("passed");
	}

	// implements +, -, ~, ++, --
	private const perplex opUnary(string op)() {
		static if (op == "+") {
			return plus();
		} else static if (op == "-") {
			return negate();
		} else static if (op == "~") {
			return conj();
		}
	}

	unittest {	// opUnary
		write("-- perplex unary ops...");
		perplex z = perplex(5, 2);
		assertEqual(+z, perplex(5, 2));
		assertEqual(-z, perplex(-5, -2));
		assertEqual(~z, perplex(5, -2));
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const perplex opBinary(string op, T:perplex)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {
			return sub(this, that);
		} else static if (op == "*") {
			return mul(this, that);
		} else static if (op == "/") {
			return div(this, that);
		} else static if (op == "^^") {
			return pow(this, that);
		}
	}

	private const perplex opBinary(string op, T:FLOAT)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {
			return sub(this, that);
		} else static if (op == "*") {
			return mul(this, that);
		} else static if (op == "/") {
			return div(this, that);
		} else static if (op == "^^") {
			return pow(this, that);
		}
	}

	private const perplex opBinary(string op, T)(const T that) {
		return opBinary!(op, perplex)(perplex(that));
	}

	unittest {	// opBinary
		writeln("-- perplex opBinary...");
		perplex z = perplex(2,3);
writefln("z = %s", z);
		perplex w = perplex(-3,1);
writefln("w = %s", w);
writefln("z+w = %s", z+w);
writefln("z-w = %s", z-w);
writefln("z*w = %s", z*w);
writefln("z/w = %s", z/w);
		writeln("passed");
	}

	/// Adds two perplex numbers and returns the sum.
	public static perplex add(const perplex z, const perplex w) {
		return perplex(z.r+w.r, z.h+w.h);
	}

	/// Adds a perplex number and a real number and returns the sum.
	public static perplex add(const perplex z, const FLOAT x) {
		return perplex(z.r+x, z.h);
	}

	/// Subtracts a real number from a perplex number
	/// and returns the difference.
	public static perplex sub(const perplex z, const FLOAT x) {
		return perplex(z.r-x, z.h);
	}

	/// Subtracts one perplex number from another
	public static perplex sub(const perplex z, const perplex w) {
		return perplex(z.r-w.r, z.h-w.h);
	}

	unittest {
		write("-- perplex add...");
		perplex z = perplex(94.5, -218.0);
		perplex w = perplex(-4.2, -18.3);
		FLOAT x = 3.5;
		assertEqual(z + w, perplex(90.3, -236.3));
		assertEqual(z + x, perplex(98.0, -218.0));
		assertEqual(z - w, perplex(98.7, -199.7));
		assertEqual(z - x, perplex(91.0, -218.0));
		writeln("passed");
	}

	/// Multiplies two perplex numbers and returns the product.
	public static perplex mul(const perplex z, const perplex w) {
		return perplex(( z.r * w.r + z.h * w.h ), ( z.h * w.r + z.r * w.h ));
	}

	/// Multiplies a perplex number by a real numbers and returns the product.
	public static perplex mul(const perplex z, const FLOAT x) {
		return perplex( z.r * x, z.h * x );
	}

	unittest {
		write("-- perplex mul...");
		perplex z = perplex(3, 2);
		perplex w = perplex(2, 4);
//		FLOAT x = 14;
		assertEqual(z * w, perplex(14, 16));
		assertEqual(z * 14, perplex(42, 28));
		writeln("passed");
	}

	/// Divides one perplex number by another
	/// and returns the quotient.
	public static perplex div(const perplex z, const perplex w)
	{
		FLOAT denom = w.r * w.r + w.h * w.h;
		FLOAT rterm = z.r * w.r + z.h * w.h;
		FLOAT iterm = z.h * w.r - z.r * w.h;
		return perplex(rterm/denom, iterm/denom);
	}

	/// Divides a perplex number by a real number
	/// and returns the quotient.
	public static perplex div(const perplex z, const FLOAT x) {
//		if (x == 0.0) throw new DivisionByZeroException("can't divide by zero");
		return perplex( z.r/x, z.h/x );
	}

	unittest {
		write("-- perplex div...");
		writeln("passed");
	}


}	// end perplex

