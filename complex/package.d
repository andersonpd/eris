// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.complex;

unittest
{
	writeln("========================");
	writeln("complex............begin");
	writeln("========================");
}

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
	alias complex = Complex!double;
}

public struct Complex(FLOAT) if(std.traits.isFloatingPoint!FLOAT) {

//--------------------------------
// structure
//--------------------------------

	public FLOAT r;
	public FLOAT i;

//--------------------------------
// construction
//--------------------------------

	public this(const FLOAT re, const FLOAT im) {
		this.r = re;
		this.i = im;
	}

	public this(const FLOAT re) {
		this.r = re;
		this.i = 0.0;
	}

	unittest {	// construction
		write("-- complex this(int)...");
		complex z = complex(1.0,2.0);
		assertEqual(z.re, 1.0);
		assertEqual(z.im, 2.0);
		complex w = complex(1.0);
		assertEqual(w.re, 1.0);
		assertEqual(w.im, 0.0);
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const complex that) {
		this.r = that.r;
		this.i = that.i;
	}

	/// Returns a copy of a complex number.
	public const complex dup() {
		return complex(this);
	}

	unittest {	// copy
		write("-- complex copying...");
		complex z = complex(1.0, -2.0);
		complex w = z.dup;
		assertEqual(w.re, 1.0);
		assertEqual(w.im, -2.0);
		assertEqual(w, complex(z));
		writeln("passed");
	}

//--------------------------------
// constants
//--------------------------------

    /// constant equal to complex(0,0)
	public enum complex ZERO = complex(0.0, 0.0);
    /// constant equal to complex(1,0)
	public enum complex ONE  = complex(1.0, 0.0);
    /// constant equal to complex(0,1)
	public enum complex I    = complex(0.0, 1.0);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the complex is zero.
	public const bool isZero() {
		return (r == 0.0 && i == 0.0);
	}

	/// Returns true if the value of the complex is zero.
	public const bool isReal() {
		return (r != 0.0 && i == 0.0);
	}

	/// Returns true if the value of the complex is zero.
	public const bool isImaginary() {
		return (r == 0.0 && i != 0.0);
	}

	unittest {	// classification
		write("-- complex classification...");
		complex z = complex(1,2);
		assertFalse(z.isZero);
		assertFalse(z.isReal);
		assertFalse(z.isImaginary);
		z = ZERO;
		assertTrue(z.isZero);
		assertFalse(z.isReal);
		assertFalse(z.isImaginary);
		z = ONE;
		assertFalse(z.isZero);
		assertTrue(z.isReal);
		assertFalse(z.isImaginary);
		z = I;
		assertFalse(z.isZero);
		assertFalse(z.isReal);
		assertTrue(z.isImaginary);
		writeln("passed");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns (NaN,NaN) the initial value for complex types.
	@property
	public static complex init() {
		return complex();
	}

	/// Returns the real component of a complex number.
	@property
	public const FLOAT re() {
		return this.r;
	}

	/// Sets the real component of a complex number.
	@property
	public FLOAT re(FLOAT x) {
		return this.r = x;
	}

	/// Returns the imaginary component of a complex number.
	@property
	public const FLOAT im() {
		return this.i;
	}

	/// Sets the real imaginary of a complex number.
	@property
	public FLOAT im(FLOAT y) {
		return this.i = y;
	}

	unittest {
	write("properties...");
	complex z;
	z = complex( -3, 45 );
	assertEqual(z.re, -3);
	assertEqual(z.im, 45);
	writeln("passed");
}


//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string inStr) {
/*		T num;
		bool sign = false;
		// strip and copy
		char[] str = strip(inStr).dup;
		// get sign, if any
		if (startsWith(str, "-")) {
			sign = true;
			str = str[1..$];
		} else if (startsWith(str, "+")) {
			str = str[1..$];
		}*/
		// FIXTHIS: convert string to int
	}


	unittest
	{
		write("-- complex this(string)...");
		writefln("complex(1.0-2.0i) = %s", complex("1.0-2.0i"));

		writeln("passed");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		return toString(this);
	}

	/// Converts the signed integer value to a string.
	public static string toString(const complex z) {
		string re = std.conv.to!string(z.r);
		string im = std.conv.to!string(z.i);
		string sign = (z.i < 0) ? "" : "+";
		return re ~ sign ~ im ~ "i";
	}

	unittest { // toString
		write("-- complex toString...");
		complex z = complex(1.2,3.4);
		writefln("z = %s", z);
        z = 17.34;
		writefln("z = %s", z);
writefln("I = %s", I);
writefln("ONE = %s", ONE);
writefln("ZERO = %s", ZERO);
		writeln("passed");
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this complex is equal to the argument.
	private const bool opEquals(T:complex)(const T that) {
		return this.r == that.r && this.i == that.i;
	}

	 /// Returns true if this complex is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(complex(that));
	}

	unittest { // equality
		write("-- complex equality....");
		complex z = complex(2,5);
		complex w = complex(2,5);
		assertEqual(z,w);
		z.im = 0.0;
		assertEqual(z,2);
		assertNotEqual(z,w);
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a complex number to a complex number.
	private void opAssign(T:complex)(const T that) {
		this.r = that.r;
		this.i = that.i;
	}

	/// Assigns a compatible value to a complex number.
	private void opAssign(T)(const T that) {
		opAssign(complex(that));
	}

	unittest {	// assignment
		write("-- complex assignment..");
		complex z;
		complex w = complex(-4, 32);
		z = w;
		assertEqual(z,w);
		z = 5;
		assertEqual(z, complex(5, 0));
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref complex opOpAssign(string op, T:complex)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref complex opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- complex opOpAssign..");
		complex z = complex(12, -83);
		complex w = complex(3, -7);
		z += w;
		assertEqual(z, complex(15, -90));
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	/// Returns a copy of a complex number without changing its sign
	public static complex plus(const complex z) {
		return complex(z.r, z.i);
	}

	/// Returns a copy of the complex number without changing its sign
	public const complex plus() {
		return plus(this);
	}

	/// Returns the additive inverse of a complex number
	public static complex negate(const complex z) {
		return complex(-z.r, -z.i);
	}

	/// Returns the additive inverse of the complex number
	public const complex negate() {
		return negate(this);
	}

	/// Returns the conjugate of a complex number.
	public static complex conj(const complex z) {
		return complex(z.r, -z.i);
	}

	/// Returns the conjugate of the complex number.
	public const complex conj() {
		return conj(this);
	}

	unittest {
		write("-- complex +, -, ~.....");
		complex z = complex( 2, 4 );
		assertEqual(conj(z), complex(2,-4));
		assertEqual(z.conj, complex(2,-4));
		assertEqual(negate(z), complex(-2,-4));
		assertEqual(z.negate, complex(-2,-4));
		assertEqual(plus(z), complex(2,4));
		assertEqual(z.plus, complex(2,4));
		writeln("passed");
	}

	// implements +, -, ~
	private const complex opUnary(string op)() {
		static if (op == "+") {
			return plus();
		} else static if (op == "-") {
			return negate();
		} else static if (op == "~") {
			return conj();
		}
	}

	unittest {	// opUnary
		write("-- complex unary ops...");
		complex z = complex(5, 2);
		assertEqual(+z, complex(5, 2));
		assertEqual(-z, complex(-5, -2));
		assertEqual(~z, complex(5, -2));
		writeln("passed");
	}

	/// Returns the square of the absolute value of a complex number.
	public static FLOAT absSqr(complex z) {
        return z.r*z.r + z.i*z.i;
	}

	/// Returns the absolute value of the complex number.
	public const FLOAT absSqr() {
		return absSqr(this);
	}

	/// Returns the absolute value of a complex number.
	public static FLOAT abs(complex z) {
		return std.math.sqrt(z.r*z.r + z.i*z.i);
	}

	/// Returns the absolute value of the complex number.
	public const FLOAT abs() {
		return abs(this);
	}

	unittest {
		write("-- complex abs.........");
		complex z = complex( 2, 4 );
		assertEqual(abs(z), 2.0 * std.math.sqrt(5.0));
		assertEqual(z.abs,  2.0 * std.math.sqrt(5.0));
		writeln("passed");
	}

	/// Returns the argument of a complex number.
	public static FLOAT arg(complex z) {
		return std.math.atan2(z.i, z.r);
	}

	/// Returns the argument of the complex number.
	public const FLOAT arg() {
		return arg(this);
	}

	unittest {
		write("-- complex arg.........");
		complex z = complex( 2, 4 );
		assertEqual(arg(z), std.math.atan(2.0));
		assertEqual(z.arg,  std.math.atan(2.0));
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	// implements +, -, *, /, ^^
	// binary operations between two complex numbers
	private const complex opBinary(string op, T:complex)(const T that)
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

	// implements +, -, *, /, ^^
	// binary operations between a complex number and a real
	private const complex opBinary(string op, T:FLOAT)(const T that)
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

	// implements +, -, *, /, ^^
	// binary operations between a complex number and a real
	private const complex opBinaryRight(string op, T:FLOAT)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {
			return sub(that, this);
		} else static if (op == "*") {
			return mul(this, that);
		} else static if (op == "/") {
			return div(that, this);
		}
	}

	private const complex opBinary(string op, T)(const T that) {
		return opBinary!(op, complex)(complex(that));
	}

	unittest {	// opBinary
		write("-- complex binary ops..");
		complex z = complex(2,3);
		complex w = complex(-3,1);
		assertEqual(z + w, complex(-1, 4));
		assertEqual(z - w, complex(5, 2));
		assertEqual(z * w, complex(-9, -7));
		assertEqual(z / w, complex(-0.3, -1.1));
		writeln("passed");
	}

	/// Adds two complex numbers and returns the sum.
	public static complex add(const complex z, const complex w) {
		return complex(z.r+w.r, z.i+w.i);
	}

	/// Adds a complex number and a real number and returns the sum.
	public static complex add(const complex z, const FLOAT x) {
		return complex(z.r+x, z.i);
	}

	/// Subtracts one complex number from another
	public static complex sub(const complex z, const complex w) {
		return complex(z.r-w.r, z.i-w.i);
	}

	/// Subtracts a real number from a complex number
	/// and returns the difference.
	public static complex sub(const complex z, const FLOAT x) {
		return complex(z.r-x, z.i);
	}

	/// Subtracts a complex number from a real number
	/// and returns the difference.
	public static complex sub(const FLOAT x, const complex z) {
		return complex(x-z.r, -z.i);
	}

	unittest {
		write("-- complex add.........");
		complex z = complex( 94.0, -218.0);
		complex w = complex(-4.0,   -18.0);
		complex s = complex( 90.0, -236.0);
		assertEqual(z+w, s);
		assertEqual(z + 5, complex(99, -218));
		complex d = complex( 98.0, -200.0);
		assertEqual(z -5, complex(89, -218));
		assertEqual(5.0 - z, complex(-89, 218));
		assertEqual(5.0 + z, complex(99,-218));
		writeln("passed");
	}

	/// Multiplies two complex numbers and returns the product.
	public static complex mul(const complex z, const complex w) {
		return complex(( z.r * w.r - z.i * w.i ), ( z.i * w.r + z.r * w.i ));
	}

	/// Multiplies a complex number by a real number and returns the product.
	public static complex mul(const complex z, const FLOAT x) {
		return complex( z.r * x, z.i * x );
	}

	unittest {
		write("-- complex mul.........");
		complex z = complex(3, 2);
		complex w = complex(2, 4);
		complex c = complex(-2, 16);
		assertEqual(z * w, c);
		assertEqual(2*w, complex(2)*w);
		assertEqual(w*2, complex(2)*w);
		writeln("passed");
	}

	/// Divides one complex number by another
	/// and returns the quotient.
	public static complex div(const complex z, const complex w)
	{
		FLOAT denom = w.r * w.r + w.i * w.i;
		FLOAT rterm = z.r * w.r + z.i * w.i;
		FLOAT iterm = z.i * w.r - z.r * w.i;
		return complex(rterm/denom, iterm/denom);
	}

	/// Divides a complex number by a real number
	/// and returns the complex quotient.
	public static complex div(const complex z, const FLOAT x) {
		return complex( z.r/x, z.i/x );
	}

	/// Divides a real number by a complex number
	/// and returns the complex quotient.
	public static complex div(const FLOAT x, const complex z) {
		return div(complex(x),z);
	}

	unittest {
		write("-- complex div.........");
		complex z = complex( 2, 3);
		complex w = complex(-3, 4);
		complex c = complex(0.24, -0.68);
		assertEqual(z / w, c);
		assertEqual(z / 2, c);
		assertEqual(2 / z, c);
		writeln("passed");
	}

}	// end complex
