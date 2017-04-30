// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.floats.doubledouble;

import std.ascii;
import std.conv;
import std.stdio;
import std.string;
import std.traits;

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
// constants
//--------------------------------

	public enum DoubleDouble NaN = DoubleDouble(double.nan);
	public enum DoubleDouble ZERO = DoubleDouble(0.0);
	public enum DoubleDouble ONE  = DoubleDouble(1.0);

	/**
	 * The value to split a double-precision value on during multiplication
	 */
	private const double SPLIT = 134217729.0; // 2^27+1, for IEEE double

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the DoubleDouble is zero.
	public const bool isZero() {
		return hi == 0.0 && lo == 0.0;
	}

	/// Returns true if the value of the DoubleDouble is less than zero.
	public const bool isNegative() {
		return hi < 0.0 || (hi == 0.0 && lo < 0.0);
	}

	/// Returns true if the value of the DoubleDouble is NaN.
	public bool isNaN() {
		return std.math.isNaN(hi);
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
		return 0; // FIXTHIS: value;
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
		return this.hi == that.hi && this.lo == that.lo;
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
		if (this.hi < that.hi) return -1;
		if (this.hi > that.hi) return  1;
		// this.hi == that.hi
		if (this.lo < that.lo) return -1;
		if (this.lo > that.lo) return  1;
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
		this.hi = that.hi;
		this.lo = that.lo;
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
			return minus();
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
		return this.dup;
	}

	/// Returns the twos complement of this DoubleDouble
	public const DoubleDouble minus() {
  		return DoubleDouble(-hi, -lo);
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

	/// Adds two DoubleDoubles and returns the sum.
	public static DoubleDouble add(const DoubleDouble x, const DoubleDouble y) {

		double S = x.hi + y.hi;
  		double T = x.lo + y.lo;
  		double e = S - x.hi;
  		double f = T - x.lo;
  		double s = S - e;
  		double t = T - f;
  		s = (y.hi - e)+(x.hi - s);
  		t = (y.lo - f)+(x.lo - t);
  		e = s + T;
		double H = S + e;
		double h = e + (S-H);
		e = t + h;

  		double zhi = H + e;
  		double zlo = e + (H - zhi);
  		return DoubleDouble(zhi, zlo);
	}

	unittest {
		write("-- DoubleDouble addition...");
		writeln("passed");
	}

	/// Subtracts one DoubleDouble from another and returns the difference.
	public static DoubleDouble sub(const DoubleDouble x, const DoubleDouble y) {
		return add(x, -y.dup);
	}

	unittest {
		write("-- DoubleDouble subtraction...");
		writeln("passed");
	}

	/// Multiplies two unsigned extended integers and returns the product.
	public static const DoubleDouble mul(const DoubleDouble x, const DoubleDouble y) {
	  double hx, tx, hy, ty, C, c;
	  C = SPLIT * x.hi; hx = C-x.hi; c = SPLIT * y.hi;
	  hx = C-hx; tx = x.hi-hx; hy = c-y.hi;
	  C = x.hi*y.hi; hy = c-hy; ty = y.hi-hy;
	  c = ((((hx*hy-C)+hx*ty)+tx*hy)+tx*ty)+(x.hi*y.lo+x.lo*y.hi);
	  double zhi = C+c; hx = C-zhi;
	  double zlo = c+hx;
	  return DoubleDouble(zhi, zlo);
	}

	unittest {
		write("-- DoubleDouble mul...");
		writeln("passed");
	}

	/// Divides one DoubleDouble by another
	/// and returns the quotient.
	public static DoubleDouble div(const DoubleDouble x, const DoubleDouble y)
	{
	  double hc, tc, hy, ty, C, c, U, u;
	  C = x.hi/y.hi; c = SPLIT*C; hc =c-C;  u = SPLIT*y.hi; hc = c-hc;
	  tc = C-hc; hy = u-y.hi; U = C * y.hi; hy = u-hy; ty = y.hi-hy;
	  u = (((hc*hy-U)+hc*ty)+tc*hy)+tc*ty;
	  c = ((((x.hi-U)-u)+x.lo)-C*y.lo)/y.hi;
	  u = C+c;

	  double zhi = u;
	  double zlo = (C-u)+c;
	  return DoubleDouble(zhi, zlo);
	}

	unittest {
		write("-- DoubleDouble div...");
		writeln("passed");
	}

	/// Divides one DoubleDouble by another
	/// and returns the remainder.
	public const DoubleDouble mod(const DoubleDouble x, const DoubleDouble y)
	{
		return DoubleDouble(x.hi % y.hi);
	}

	unittest {
		write("-- DoubleDouble mod...");
		writeln("passed");
	}

	/// Raises an DoubleDouble to an integer power
	/// and returns the result. Tests the result for overflow.
	public const DoubleDouble pow(const DoubleDouble x, const DoubleDouble y) {
		return DoubleDouble(x.hi ^^ y.hi);
	}

	/// Raises an DoubleDouble to an integer power
	/// and returns the result. Tests the result for overflow.
	public const DoubleDouble pow(const DoubleDouble x, const int n) {
		return DoubleDouble(x.hi ^^ n);
	}

	unittest {
		write("power...");
		writeln("test missing");
	}

	/// Returns the absolute value of the value of the DoubleDouble.
	/// No effect on unsigned extended integers -- returns a copy.
	public const DoubleDouble abs() {
		DoubleDouble copy = this.dup;
		if (copy.hi < 0) copy.hi = -copy.hi;
		return copy;
	}

	unittest {
		write("-- DoubleDouble abs...");
		writeln("passed");
	}

	public const DoubleDouble sqr() {
		return DoubleDouble(this.hi * this.hi);
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

