// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.complex.hypermath;

import eris.complex;

unittest
{
	writeln("========================");
	writeln("complex.math.......begin");
	writeln("========================");
}

version(unittest)
{
	import std.stdio;
	import eris.test.assertion;
	alias complex = Complex!double;
}

//alias R = double;
//alias complex = Complex!R;
/*alias I = Complex!R.I;
alias ONE = Complex!R.ONE;
alias abs = Complex!R.abs;
alias arg = Complex!R.arg;*/

//--------------------------------
// unary operations
//--------------------------------

	/// Returns the square of the Complex!R number
	public Complex!R sqr(R)(const Complex!R z) {
		return Complex!R( z.r*z.r - z.i*z.i, 2.0 * z.r*z.i );
	}

	unittest {
		write("-- complex sqr...");
		complex z = complex(1, 0);
		assertEqual(sqr(z), z);
		z = complex(0, 1);
		assertEqual(sqr(z), -1);
		z = complex(2, 3);
		assertEqual(sqr(z), complex(-5,12));
		writeln("passed");
	}


	/// Returns the complex exponential of the complex number.
	public static Complex!R exp(R)(const Complex!R z) {
		R ez = std.math.exp(z.r);
		return Complex!R(ez * std.math.cos(z.im), ez * std.math.cos(z.im));
	}

	unittest {
		writeln("exp...");
//		complex z = I;
//		complex w = I;
writefln("exp(I) = %s", exp(complex.I));
writefln("exp(complex(2),0.0) = %s", exp(complex(2)));

		writeln("test missing");
	}

	/// Returns the principal natural logarithm of a complex number
	public static Complex!R log(R)(const Complex!R z) {
		return Complex!R(std.math.log(Complex!R.abs(z)), Complex!R.arg(z));
	}

	unittest {
		write("log...");
//		complex z = I;
//		complex w = I;
		writeln("test missing");
	}

	/// Returns the principal square root of a complex number.
	public static Complex!R sqrt(R)(const Complex!R z) {
		R rad = std.math.sqrt(Complex!R.abs(z));
		R ang = 0.5 * Complex!R.arg(z);
		return Complex!R(rad * std.math.cos(ang), rad * std.math.sin(ang));
	}

//--------------------------------
// binary operations
//--------------------------------

	/// Returns the inner product of two complex numbers.
	public static R inner(R)(const Complex!R z, const Complex!R w) {
		return z.r*w.r + z.i*w.i;
	}

	/// Returns the outer product of two complex numbers.
	public static R outer(R)(const Complex!R z, const Complex!R w) {
		return z.r*w.i - z.i*w.r;
	}

	/// Returns the conjugate product of two complex numbers.
	public static Complex!R cnjp(R)(const Complex!R z, const Complex!R w) {
		return Complex!R( inner(z,w), outer(z,w) );
	}

	unittest {
		write("products...");
		writeln("test missing");
	}

	/// Raises a complex number to a real power
	public static Complex!R pow(R)(const Complex!R z, const R x) {
		R rad = abs(z)^^x;
		R ang = x * arg(z);
		return Complex!R(rad * std.math.cos(ang), rad * std.math.sin(ang));
	}

	/// Raises a complex number to a complex power
	public static Complex!R pow(R)(const Complex!R z, const Complex!R w) {
		R abs = Complex!R.abs(z);
		R arg = Complex!R.arg(z);
		R rad = abs^^w.r * std.math.log(-w.im*arg);
		R ang = w.im*arg + w.im * std.math.log(abs);
		return Complex!R(rad * std.math.cos(ang), rad * std.math.sin(ang));
	}

	unittest {
		write("power...");
		complex z = (1,2);
		writefln("pow(z,z) = %s", pow(z,z));

writeln("something");
		writeln("test missing");
	}

	/// Returns the distance between to numbers on the complex plane
	public static R distance(R)(const Complex!R z, const Complex!R w) {
		return std.math.sqrt((z.re - w.re)^^2 + (z.im - w.im)^^2);
	}

	unittest {
		write("distance...");
		writeln("test missing");
	}

//--------------------------------
// trigonometric functions
//--------------------------------

	/// Returns the cosine of a complex number.
	public static Complex!R cos(R)(const Complex!R z) {
		R r = std.math.cos(z.re)*std.math.cosh(z.i);
		R i = std.math.sin(z.re)*std.math.sinh(z.i);
		return Complex!R( r, -i );
	}

	/// Returns the sine of a complex number.
	public static Complex!R sin(R)(const Complex!R z) {
		R r = std.math.sin(z.re)*std.math.cosh(z.i);
		R i = std.math.cos(z.re)*std.math.sinh(z.i);
		return Complex!R( r, i );
	}

/*	/// Returns the inverse cosine of a complex number.
	public static Complex!R acos(R)(const Complex!R z) {
  		Complex!R w = z + Complex!R.I * sqrt(Complex!R.ONE - sqr(z));
		return -Complex!R.I * log(w);
	}

	unittest {
	write("acos...");
	complex z = (1,2);
	writefln("acos(z) = %s", acos(z));

	writeln("test missing");
}*/


	/// Returns the inverse sine of a complex number.
	public static Complex!R asin(R)(const Complex!R z) {
  		Complex!R w = I * z + sqrt(ONE - sqr(z));
		return -I * log(w);
	}

	/// Returns the inverse tangent of a complex number.
	public static Complex!R atan(R)(const Complex!R z) {
  		Complex!R w = I/2.0 * log((I+z) / (I-z));
		return w;
	}

//--------------------------------
// hyperbolic trigonometric functions
//--------------------------------

	/// Returns the hyperbolic cosine of a complex number.
	public static Complex!R cosh(R)(const Complex!R z) {
		R r = std.math.cosh(z.re)*std.math.cos(z.i);
		R i = std.math.sinh(z.re)*std.math.sin(z.i);
		return Complex!R( r, i );
	}

	/// Returns the hyperbolic sine of a complex number.
	public static Complex!R sinh(R)(const Complex!R z) {
		R r = std.math.sinh(z.re)*std.math.cos(z.i);
		R i = std.math.cosh(z.re)*std.math.sin(z.i);
		return Complex!R( r, i );
	}

	/// Returns the inverse hyperbolic cosine of a complex number.
	public static Complex!R acosh(R)(const Complex!R z) {
  		Complex!R w = log(z + sqrt(sqr(z) - ONE));
		return w;
	}

	/// Returns the inverse hyperbolic cosine of a complex number.
	public static Complex!R asinh(R)(const Complex!R z) {
  		Complex!R w = log(z + sqrt(sqr(z) + ONE));
		return w;
	}

	unittest {
		writeln("===================");
		writeln("complex.math....end");
		writeln("===================");
	}


