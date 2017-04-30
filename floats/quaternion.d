// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2012 - 2014.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.floats.quaternion;

unittest {
	writeln("========================");
	writeln("Quaternion.....begin");
	writeln("========================");
}

unittest {
	writeln("abcd");
	Quaternion!double q = (1,2,3,4);
writefln("q.toString = %s", q.toString);
}

version(unittest) {
	import std.stdio;
	import eris.assertions;
	alias qtrn = Quaternion!double;
}

public struct Quaternion(T = double) {

alias quaternion = Quaternion!T;

//--------------------------------
// structure
//--------------------------------

	private T r = T.nan;
	private T i = T.nan;
	private T j = T.nan;
	private T k = T.nan;

//--------------------------------
// construction
//--------------------------------

	public this(const T value) {
		this.r = value;
		this.i = 0.0;
		this.j = 0.0;
		this.k = 0.0;
	}

	public this(const T r, const T i, const T j, const T k) {
		this.r = r;
		this.i  = i;
		this.j  = j;
		this.k  = k;
	}

	unittest {	// construction
		write("-- quaternion this(T)...");
		qtrn q, r;
		q = qtrn(2.345);
		r = qtrn(-123.39E27);
writefln("q = %s", q);
writefln("r = %s", r);
		write("\n-- quaternion this(T,T,T,T)...");
		q = qtrn(23.3,536,-98.2,85.543);
		r = qtrn(129.06, 65482.002, 846.21, -4562800.005);
writefln("q = %s", q);
writefln("r = %s", r);
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const quaternion that) {
		this.r = that.r;
		this.i  = that.i;
		this.j  = that.j;
		this.k  = that.k;
	}

	/// Returns a copy of an Quaternion.
	public const quaternion dup() {
		return quaternion(this);
	}

	unittest {	// copy
		write("-- quaternion copying...");
		writeln("passed");
	}

//--------------------------------
// constants
//--------------------------------

	public enum quaternion ZERO = quaternion(0);
	public enum quaternion ONE  = quaternion(1);

//--------------------------------
/*// classification
//--------------------------------

	/// Returns true if the r of the quaternion is zero.
	public const bool isZero() {
		return numDigits(this.r) == 0;
	}

	/// Returns true if the r of the quaternion is less than zero.
	/// For unsigned extended integers the return r is always false.
	public const bool isNegative() {
		return r < 0;
	}

	/// Returns true if the r of the quaternion is odd.
	public const bool isOdd() {
		return r & 1;
	}

	/// Returns true if the r of the quaternion is even.
	public const bool isEven() {
		return !isOdd();
	}

	unittest {	// classification
		write("-- quaternion classification...");
		writeln("passed");
	}
*/
//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial r for Quaternion types.
	@property
	public static quaternion init() {
		return ZERO;
	}

	// min/max properties

   	/// Returns the maximum r for this type.
	@property
	public static quaternion max() {
		return quaternion(int.max);
	}

	/// Returns the minimum r for this type.
	@property
	public static quaternion min() {
		return quaternion(int.min);
	}

	unittest
	{	// min/max values
		write("-- quaternion min/max values...");
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
		write("-- quaternion this(string)...");
		writeln("passed");
	}

	/// Converts the signed integer r to a string.
	public const string toString() {
		// FIXTHIS: Implement toString
		string str = "";
//			str = std.string.format("%#.1f%+#.1fi%+#.1fj%+#.1fk", r, i, j, k);
//			str = std.string.format("%s+%si+%sj+%sk", r, i, j, k);
			str = std.string.format("(%s, %s, %s, %s)", r, i, j, k);
		return str;
	}
	unittest { // toString
		write("-- quaternion toString...");
writefln("quaternion(12.8,200.7,-34.46,303) = %s", quaternion(12.8,200.7,-34.46,303));
writefln("quaternion(12.8,200.7,34.46,303) = %s", quaternion(-13, 200.7,34.46,303));

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

	/// Converts the quaternion to an unsigned integer.
/*	public const int toInt() {
		return r;
	}*/

	unittest {	// conversion
		write("-- quaternion to integer...");
		writeln("passed");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this quaternion is equal to the argument.
	private const bool opEquals(U:quaternion)(const U that) {
		return this.r == that.r;
	}

	 /// Returns true if this quaternion is equal to the argument.
	private const bool opEquals(U)(const U that){
		return opEquals(quaternion(that));
	}

	unittest { // equality
		write("-- quaternion equality...");
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this quaternion is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(U:quaternion)(const U that) {
		if (this < that) return -1;
		if (this > that) return  1;
		return 0;
	}

	/// Returns -1, 0, or 1, if this quaternion is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(U)(const U that) {
		return opCmp(quaternion(that));
	}

	unittest { // comparison
		write("-- quaternion comparison...");
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns an Quaternion r to this.
	private void opAssign(U:quaternion)(const U that) {
		this.r = that.r;
		this.i = that.i;
		this.j = that.j;
		this.k = that.k;
	}

	/// Assigns an integral r to this.
	private void opAssign(U)(const U that) {
		opAssign(quaternion(that));
	}

	unittest {	// assignment
		write("-- quaternion opAssign...");
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref quaternion opOpAssign(string op, U:quaternion)(const U that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref quaternion opOpAssign(string op, U)(const U that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- quaternion opOpAssign...");
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private quaternion opUnary(string op)() {
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

	/// Returns a copy of this quaternion
	public const quaternion plus(T)() {
		return quaternion(this);
	}

	/// Returns the twos complement of this quaternion
	public const quaternion minus(T)() {
		quaternion s;
		s.r = -r.r;
		s.i = -r.i ;
		s.j = -r.j ;
		s.k = -r.k ;
		return s;
	}

	unittest {	// opUnary
		write("-- quaternion unary operations...");
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const quaternion opBinary(string op, U:quaternion)(const U that)
	{
		static if (op == "+") {
			return add!T(this, that);
		} else static if (op == "-") {
			return sub!T(this, that);
		} else static if (op == "*") {
			return mul!T(this, that);
		} else static if (op == "/") {
			return div!T(this, that);
		} else static if (op == "%") {
			return mod(this, that);
		} else static if (op == "^^") {
			return pow(this, that);
		}
	}

	private const quaternion opBinary(string op, U)(const U that) {
		return opBinary!(op, quaternion)(quaternion(that));
	}

	unittest {	// opBinary
		write("-- quaternion opBinary...");
		qtrn q = qtrn(2.345);
		qtrn r = qtrn(23.3,536,-98.2,85.543);
writefln("q = %s", q);
writefln("r = %s", r);
writefln("q+r = %s", q+r);
writefln("q-r = %s", q-r);
writefln("q*r = %s", q*r);
//writefln("q/r = %s", q/r);
		writeln("passed");
	}

	/// Adds two quaternions and returns the sum.
	public const quaternion add(T)(const quaternion q, const quaternion r) {
		quaternion s;
		s.r = q.r + r.r;
		s.i = q.i + r.i ;
		s.j = q.j + r.j ;
		s.k = q.k + r.k ;
		return s;
	}

	unittest {
		write("-- quaternion addition...");
		writeln("passed");
	}

	/// Subtracts one quaternion from another and returns the difference.
	public const quaternion sub(T)(const quaternion q, const quaternion r) {
		quaternion s;
		s.r = q.r - r.r;
		s.i = q.i - r.i ;
		s.j = q.j - r.j ;
		s.k = q.k - r.k ;
		return s;
	}


	unittest {
		write("-- quaternion subtraction...");
		writeln("passed");
	}

	/// Multiplies two quaternions and returns the product.
	public const quaternion mul(T)(const quaternion q, const quaternion r) {
	    quaternion s;
		s.r = q.r*r.r - q.i*r.i - q.j*r.j - q.k*r.k;
		s.i = q.r*r.i + q.i*r.r + q.j*r.j - q.k*r.j;
		s.j = q.r*r.j + q.j*r.r + q.k*r.k - q.i*r.k;
		s.k = q.r*r.k + q.k*r.r + q.i*r.i - q.j*r.i;
		return s;
	}

	unittest {
		write("-- quaternion mul...");
		writeln("passed");
	}

	/// Divides one Quaternion by another
	/// and returns the quotient.
	public const quaternion div(T)(const quaternion q, const quaternion r) {
		return q * reciprocal!T(r);
	}

	unittest {
		write("-- quaternion div...");
		writeln("passed");
	}

	/// Divides one Quaternion by another
	/// and returns the remainder.
	public const quaternion mod(const quaternion q, const quaternion r) {
		return quaternion(q.r % r.r);
	}

	unittest {
		write("-- quaternion mod...");
		writeln("passed");
	}

	/// Raises an Quaternion to an integer power
	/// and returns the result. Tests the result for overflow.
	public const quaternion pow(const quaternion q, const quaternion r) {
		return quaternion(q.r ^^ r.r);
	}

	/// Raises an Quaternion to an integer power
	/// and returns the result. Tests the result for overflow.
	public const quaternion pow(const quaternion q, const int n) {
		return quaternion(q.r ^^ n);
	}

	unittest {
		write("power...");
		writeln("test missing");
	}

/*	/// Shifts an Quaternion left by an integral r.
	/// No check for overflow is made.
	public const Quaternion shl(const Quaternion q, const Quaternion r) {
		return Quaternion(q.r << r.r);
	}

	/// Shifts an Quaternion left by an integral r.
	public const Quaternion shl(const Quaternion q, const uint n) {
		return Quaternion(q.r << n);
	}

	/// Shifts an Quaternion right by an integral r.
	public static Quaternion shr(const Quaternion q, const Quaternion r) {
		return Quaternion(q.r >> r.r);
	}

	/// Shifts an Quaternion right by an integral r.
	public static Quaternion shr(const Quaternion q, const uint n) {
		return Quaternion(q.r >> n);
	}

	/// Shifts an Quaternion right by an integral r.
	public const Quaternion lshr(const Quaternion q, const Quaternion r) {
		return Quaternion(q.r >>> r.r);
	}

	/// Shifts an Quaternion right by an integral r.
	public const Quaternion lshr(const Quaternion q, const uint n) {
		return Quaternion(q.r >>> n);
	}

	unittest {
		write("shift ops...");
		writeln("test missing");
	}
*/
	/// Returns the multiplicative inverse of the quaternion.
	public const quaternion reciprocal(T)() {
		quaternion q = this;
		T d = norm!T(q);
		q.r /= d;
		q.i = -q.i / d;
		q.j = -q.j / d;
		q.k = -q.k / d;
		return q;
	}

	unittest {
		write("-- quaternion reciprocal...");
		writeln("passed");
	}

	/// Returns the modulus of the quaternion.
	public static T norm(T)(const quaternion q) {
		return q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k;
	}

	unittest {
		write("-- quaternion modulus...");
		writeln("passed");
	}

	/// Returns the modulus of the quaternion.
	public static T modulus(T)(const quaternion q) {
		return std.math.sqrt(q.norm);
	}

	unittest {
		write("-- quaternion modulus...");
		writeln("passed");
	}

	/// Returns the modulus of the quaternion.
	public static quaternion unit(T)(const quaternion q) {
		T d = modulus(q);
		quaternion r = q;
		r.r /= d;
		r.i /= d;
		r.j /= d;
		r.k /= d;
		return r;
	}

	unittest {
		write("-- quaternion modulus...");
		writeln("passed");
	}

	/// Returns the conjugate of the quaternion.
	public const quaternion conj(T)() {
		quaternion q = this;
		q.i = -q.i;
		q.j = -q.j;
		q.k = -q.k;
		return q;
	}

	unittest {
		write("-- quaternion abs...");
		writeln("passed");
	}

	/// Returns the absolute r of the r of the quaternion.
	/// No effect on quaternions -- returns a copy.
	public const quaternion abs() {
		quaternion copy = this.dup;
		if (copy.r < 0) copy.r = -copy.r;
		return copy;
	}

	unittest {
		write("-- quaternion abs...");
		writeln("passed");
	}

	public const quaternion sqr() {
		return quaternion(this.r * this.r);
	}

	unittest {
		write("-- quaternion sqr...");
		writeln("passed");
	}

	unittest {
		writeln("===================");
		writeln("Quaternion........end");
		writeln("===================");
	}

}	// end Quaternion

