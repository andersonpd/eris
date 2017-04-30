// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.complex.dual;

unittest
{
	writeln("========================");
	writeln("dual............begin");
	writeln("========================");
}

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
	alias dual = Dual!double;
}

public struct Dual(FLOAT) {

//--------------------------------
// structure
//--------------------------------

	public FLOAT r;
	public FLOAT j;

//--------------------------------
// construction
//--------------------------------

	public this(const FLOAT re, const FLOAT du) {
		this.r = re;
		this.j = du;
	}

	public this(const FLOAT re) {
		this.r = re;
		this.j = 0.0;
	}

	unittest {	// construction
		write("-- dual this(int)...");
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const dual that) {
		this.r = that.r;
		this.j = that.j;
	}

	/// Returns a copy of a dual number.
	public const dual dup() {
		return dual(this);
	}

	unittest {	// copy
		write("-- dual copying...");
		writeln("passed");
	}

//--------------------------------
// constants
//--------------------------------

	public enum dual ZERO = dual(0.0, 0.0);
	public enum dual ONE  = dual(1.0, 0.0);
	public enum dual J    = dual(0.0, 1.0);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the dual is zero.
	public const bool isZero() {
		return (r == 0.0 && j == 0.0);
	}

	/// Returns true if the dual number is not zero and has no dual part.
	public const bool isReal() {
		return (r != 0.0 && j == 0.0);
	}

	/// Returns true if the dual number is not zero and has no real part.
	public const bool isDual() {
		return (r == 0.0 && j != 0.0);
	}

	unittest {	// classification
		write("-- dual classification...");
		writeln("passed");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns (NaN,NaN) the initial value for dual numbers.
	@property
	public static dual init() {
		return dual();
	}

	/// Returns the real component of this dual number.
	@property
	public const FLOAT re() {
		return this.r;
	}

	/// Returns (NaN,NaN) the initial value for dual types.
	@property
	public const FLOAT du() {
		return this.j;
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string str) {
		// FIXTHIS: convert string to int
	}


	unittest
	{
		write("-- dual this(string)...");
		writeln("passed");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		return toString(this);
	}

	/// Converts the signed integer value to a string.
	public static string toString(const dual z) {
		string re = std.conv.to!string(z.r);
		string du = std.conv.to!string(z.j);
		string sign = (z.j < 0) ? "" : "+";
		return re ~ sign ~ du ~ "j";
	}

	unittest { // toString
		write("-- dual toString...");
		dual z = dual(1.2,3.4);
		writefln("z = %s", z);
        z = 17.34;
		writefln("z = %s", z);
writefln("J = %s", J);
writefln("ONE = %s", ONE);
writefln("ZERO = %s", ZERO);
		writeln("passed");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this dual is equal to the argument.
	private const bool opEquals(T:dual)(const T that) {
		return this.r == that.r && this.j == that.j;
	}

	 /// Returns true if this dual is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(dual(that));
	}

	unittest { // equality
		write("-- dual equality...");
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a dual number to this dual number.
	private void opAssign(T:dual)(const T that) {
		this.r = that.r;
		this.j = that.j;
	}

	/// Assigns a compatible value to this.
	private void opAssign(T)(const T that) {
		opAssign(dual(that));
	}

	unittest {	// assignment
		write("-- dual opAssign...");
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref dual opOpAssign(string op, T:dual)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref dual opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- dual opOpAssign...");
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	/// Returns a copy of the dual number
	public const dual plus() {
		return dual(this);
	}

	/// Returns the additive inverse of the dual number
	public const dual negate() {
		return dual(-this.r, -this.j);
	}

	/// Returns the additive inverse of the dual number
	public const dual negate(const dual z) {
		return this.negate;
	}

	/// Returns the conjugate of the dual number
	public const dual conj() {
		return dual(this.r, -this.j);
	}

	/// Returns the square of the absolute value of the dual number.
	public static FLOAT sqrAbs(dual z) {
        return z.r*z.r;
	}

	/// Returns the absolute value of the dual number.
	public const FLOAT sqrAbs() {
		return sqrAbs(this);
	}

	/// Returns the absolute value of the dual number.
	public static FLOAT abs(dual z) {
		return std.math.sqrt(z.r*z.r);
	}

	/// Returns the absolute value of the dual number.
	public const FLOAT abs() {
		return abs(this);
	}

	unittest {
		write("-- dual abs...");
		dual z = dual( 2, 4 );
writefln("z = %s", z);
writefln("abs(z) = %s", abs(z));
writefln("z.abs = %s", z.abs);
		writeln("passed");
	}

	// TODO: need return to the correct quadrant
	/// Returns the absolute value (or modulus) of the dual number.
	public static FLOAT arg(dual z) {
		return std.math.atanh(z.j / z.r);
	}

	unittest {
		write("-- dual arg...");
		writeln("passed");
	}

	/// Returns the conjugate of the dual number.
	public const dual conj(dual z) {
		return dual(z.r, -z.j);
	}

	unittest {
		write("-- dual conj...");
		writeln("passed");
	}

	// implements +, -, ~, ++, --
	private const dual opUnary(string op)() {
		static if (op == "+") {
			return plus();
		} else static if (op == "-") {
			return negate();
		} else static if (op == "~") {
			return conj();
		}
	}

	unittest {	// opUnary
		writeln("-- dual unary operations...");
		dual z = dual(5, 2);
writefln("z = %s", z);
writefln("+z = %s", +z);
writefln("-z = %s", -z);
writefln("~z = %s", ~z);
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const dual opBinary(string op, T:dual)(const T that)
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

	private const dual opBinary(string op, T:FLOAT)(const T that)
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

	private const dual opBinary(string op, T)(const T that) {
		return opBinary!(op, dual)(dual(that));
	}

	unittest {	// opBinary
		writeln("-- dual opBinary...");
		dual z = dual(2,3);
writefln("z = %s", z);
		dual w = dual(-3,1);
writefln("w = %s", w);
writefln("z+w = %s", z+w);
writefln("z-w = %s", z-w);
writefln("z*w = %s", z*w);
writefln("z/w = %s", z/w);
		writeln("passed");
	}

	/// Adds two dual numbers and returns the sum.
	public static dual add(const dual z, const dual w) {
		return dual(z.r+w.r, z.j+w.j);
	}

	/// Adds a dual number and a real number and returns the sum.
	public static dual add(const dual z, const FLOAT x) {
		return dual(z.r+x, z.j);
	}

	unittest {
		writeln("-- dual addition...");
		dual z = dual(94.57, -218.0);
		dual w = dual(-4.2, -18.3);
writefln("z = %s", z);
writefln("w = %s", w);
		dual c = z + w;
writefln("c = %s", c);
		writeln("passed");
	}

	/// Subtracts a real number from a dual number
	/// and returns the difference.
	public static dual sub(const dual z, const FLOAT x) {
		return dual(z.r-x, z.j);
	}

	/// Subtracts one dual number from another
	public static dual sub(const dual z, const dual w) {
		return dual(z.r-w.r, z.j-w.j);
	}

	unittest {
		write("-- dual subtraction...");
		writeln("passed");
	}

	/// Multiplies two dual numbers and returns the product.
	public static dual mul(const dual z, const dual w) {
		return dual(( z.r * w.r + z.j * w.j ), ( z.j * w.r ));
	}

	/// Multiplies a dual number by a real numbers and returns the product.
	public static dual mul(const dual z, const FLOAT x) {
		return dual( z.r * x, z.j * x );
	}

	unittest {
		writeln("-- dual mul...");
		dual z = dual(3, 2);
		dual w = dual(2, 4);
writefln("z = %s", z);
writefln("w = %s", w);
		dual c = z * w;
writefln("c = %s", c);
		writeln("passed");
	}

	/// Divides one dual number by another
	/// and returns the quotient.
	public static dual div(const dual z, const dual w)
	{
/*		FLOAT denom = w.r * w.r + w.j * w.j;*/
		FLOAT rterm = z.r; // + z.j * w.j;
		FLOAT iterm = z.j * w.r - z.r * w.j;
		return dual(rterm/w.r, iterm/(w.r*w.r));
	}

	/// Divides a dual number by a real number
	/// and returns the quotient.
	public static dual div(const dual z, const FLOAT x) {
//		if (x == 0.0) throw new DivisionByZeroException("can't divide by zero");
		return dual( z.r/x, z.j/x );
	}

	unittest {
		write("-- dual div...");
		writeln("passed");
	}


}	// end dual

