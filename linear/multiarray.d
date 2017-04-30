// Written in the DIM programming language

/**
 *	Copyright Paul DIM. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.linear.multiarray;

import std.ascii;
import std.conv;
import std.stdio;
import std.string;
import std.traits;

unittest {
	writeln("=================");
	writeln("vector.....begin");
	writeln("=================");
}

version(unittest) {
	import eris.test.assertion;

	alias vector2d = Vector!(2,double);
	alias vector3d = Vector!(3,double);
	alias vector4d = Vector!(4,double);
}

public struct Vector(int DIM, FLOAT = double) {

alias vector = Vector!(DIM,FLOAT);

//--------------------------------
// structure
//--------------------------------

	private FLOAT[DIM] co; // = [ 0.0, 0.0, 0.0 ];

//--------------------------------
// construction
//--------------------------------

	// TODO: should throw
	public this(const FLOAT[] list ...) {
		if (list.length == DIM) {
			foreach (int i, FLOAT n; list) {
				co[i] = n;
			}
		}
	}

	// Does this constructor differ from the one above?
	public this(const FLOAT[DIM] array) {
		foreach (int i, FLOAT n; array) {
			co[i] = n;
		}
	}

	unittest {	// construction
		write("-- Vector this(double[] ...");
		static if (DIM == 3) {
			auto v = vector3d(1.0, 2.0, 3.0);
			vector3d a = [1.0,2,3];
			assertEqual(v, a);
			v = vector3d([1.0, 2.0, 3.0]);
			assertEqual(v, a);
		}
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const vector that) {
		this.co = that.co;
	}

	/// Returns a copy of an Vector.
	public const vector dup() {
		return vector(this);
	}

	unittest {	// copy
		write("-- Vector copying...");
		writeln("test missing");
	}

//--------------------------------
// constants
//--------------------------------

	static if (DIM == 2) {
		public enum vector ZERO = vector(0.0, 0.0);
		public enum vector X    = vector(1.0, 0.0);
		public enum vector Y    = vector(0.0, 1.0);
		@property
		public const FLOAT x() {
			return co[0];
		}
		@property
		public const FLOAT y() {
			return co[1];
		}
	}

	static if (DIM == 3) {
		public enum vector ZERO = vector(0.0, 0.0, 0.0);
		public enum vector X    = vector(1.0, 0.0, 0.0);
		public enum vector Y    = vector(0.0, 1.0, 0.0);
		public enum vector Z    = vector(0.0, 0.0, 1.0);

		@property
		public const FLOAT x() {
			return co[0];
		}
		@property
		public const FLOAT y() {
			return co[1];
		}
		@property
		public const FLOAT z() {
			return co[2];
		}
	}

	static if (DIM == 4) {
		public enum vector ZERO = vector(0.0, 0.0, 0.0, 0.0);
		public enum vector X    = vector(1.0, 0.0, 0.0, 0.0);
		public enum vector Y    = vector(0.0, 1.0, 0.0, 0.0);
		public enum vector Z    = vector(0.0, 0.0, 1.0, 0.0);
		public enum vector W    = vector(0.0, 0.0, 0.0, 1.0);

		@property
		public const FLOAT x() {
			return co[0];
		}
		@property
		public const FLOAT y() {
			return co[1];
		}
		@property
		public const FLOAT z() {
			return co[2];
		}
		@property
		public const FLOAT w() {
			return co[3];
		}
	}

unittest {
	write("properties...");
	auto v = vector3d(1.0,2.0,3.0);
writefln("v = %s", v);
writefln("v.x = %s", v.x);
writefln("v.y = %s", v.y);
writefln("v.z = %s", v.z);
writefln("v.X = %s", v.X);
//writefln("v.Z = %s", v.Z);


	writeln("test missing");
}
//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the Vector is zero.
	public const bool isZero() {
		foreach (int i, FLOAT n; co) {
			if (n != 0.0) return false;
		}
		return true;
	}

/*	/// Returns true if the value of the Vector is less than zero.
	/// For unsigned extended integers the return value is always false.
	public const bool isNegative() {
		return value < 0;
	}

	/// Returns true if the value of the Vector is odd.
	public const bool isOdd() {
		return value & 1;
	}

	/// Returns true if the value of the Vector is even.
	public const bool isEven() {
		return !isOdd();
	}

	unittest {	// classification
		write("-- Vector classification...");
		writeln("test missing");
	}*/

//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial value for Vector types.
	// TODO: No it's not!
	@property
	public static vector init() {
		return ZERO;
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string str) {
		// FIXTHIS: convert string to int
	}


	unittest
	{
		write("-- Vector this(string)...");
		writeln("test missing");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		return std.string.format("%s", co);
	}

	unittest { // toString
		write("-- Vector toString...");
		static if (DIM == 3) {
			vector3d v = vector3d(1,2,3);
			assertStringEqual(v, "[1, 2, 3]");
			writeln("passed");
		}
		else {
			writeln("test missing");
		}
	}

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this vector is equal to the argument.
	private const bool opEquals(FLOAT:vector)(const FLOAT that) {
		return this.co == that.co;
	}

	/// Returns true if this vector is equal to the argument.
	private const bool opEquals(FLOAT)(const FLOAT that){
		return opEquals(vector(that));
	}

	unittest { // equality
		write("-- vector equality...");
		vector3d v1, v2, v3;
		v1 = vector3d(1,2,3);
		v2 = vector3d(1,2,4);
		v3 = vector3d(1,2,4);
		assertNotEqual(v1,v2);
		assertEqual(v2,v3);
		assertEqual(v1,v1);
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

// vectors are not ordered. Compare norms??

/*
	/// Returns -1, 0, or 1, if this vector is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(FLOAT:vector)(const FLOAT that) {
		return 0;
	}

	/// Returns -1, 0, or 1, if this vector is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(FLOAT)(const FLOAT that) {
		return opCmp(vector(that));
	}

	unittest { // comparison
		write("-- vector comparison...");
		writeln("test missing");
	}
*/

//--------------------------------
// assignment
//--------------------------------

	/// Assigns an vector value to this.
	private void opAssign(T:vector)(const T that) {
		this.co = that.co.dup;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(vector(that));
	}

	unittest {	// assignment
		write("-- vector opAssign...");
//		static if (DIM == 3) {
		vector3d a,b,c,d;
		a = vector3d(1,2,3);
		b = [1.0,2.0,3.0];
		assertEqual(b,a);
//		}
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref vector opOpAssign(string op, FLOAT:vector)(const FLOAT that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref vector opOpAssign(string op, FLOAT)(const FLOAT that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- vector opOpAssign...");
		writeln("test missing");
	}

/+
x Vector Addition/Subtraction
x Scalar Multiplication
x Dot Product
x Wedge/Outer/Cross Product
x Norm/Abs/Length
Normalize (convert to unit vector)
isUnitVector
Angle (between vectors)
x nth Element Set/Get
?? Orthogonalize (between vectors)
Projection
Element-Wise:
x  Addition/Subtraction
??  Multiplication
??  Division
x Sum
?? Divergence
?? Curl
+/

//--------------------------------
// index operations
//--------------------------------

	private const FLOAT opIndex(int n) {
		return co[n];
	}

	private FLOAT opIndexAssign(FLOAT value, int n) {
		return co[n] = value;
	}

	unittest {
		write("index operations...");
		vector3d a, b;
		a = [1.0, 1.5, 1.7];
		assertEqual(a[2], 1.7);
		writeln("test missing");
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private vector opUnary(string op)() {
		static if (op == "+") {
			return plus();
		} else static if (op == "-") {
			return minus();
		}
	}

	/// Returns a copy of this vector
	public const vector plus() {
		return this.dup;
	}

	/// Returns the twos complement of this vector
	public const vector minus() {
		vector v = this.dup;
		for (int i; i < DIM; i++) {
			v.co[i] = -v.co[i];
		}
  		return v;
	}

	unittest {	// opUnary
		write("-- vector unary operations...");
		writeln("test missing");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const double opBinary(string op : "*", T:vector)(const T that) {
		return inner(this, that);
	}

	private const vector opBinary(string op : "*", T)(const T that) {
		return scale(this, that);
	}

	private const vector opBinaryRight(string op : "*", T)(const T that) {
		return scale(this, that);
	}

	private const vector opBinary(string op : "/", T)(const T that) {
		return div(this, that);
	}

	private const vector opBinary(string op, T:vector)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {

			return sub(this, that);
		} else static if (op == "*") {
			return inner(this, that);
		} else static if (op == "^") {
			return cross(this, that);
		}
	}

	private const vector opBinary(string op, FLOAT)(const FLOAT that) {
		return opBinary!(op, vector)(vector(that));
	}

	unittest {	// opBinary
		write("-- vector opBinary...");
		vector3d a, b, c, d;
		a = vector3d(3,5,7);
		b = vector3d(-3,5,-7);
		c = vector3d(0,10,0);
		assertEqual(a + b, c);
		d = vector3d(6,0,14);
		assertEqual(a - b, d);
		writeln("passed");
	}

	/// Returns the vector sum of two vectors.
	private static vector add(const vector x, const vector y) {
		vector v;
		v.co[] = x.co[] + y.co[];
		return v;
	}

	unittest {
		write("-- vector addition...");
		vector3d v1, v2, v3;
		v1 = vector3d(1,2,4);
		v2 = vector3d(2,5,6);
		v3 = vector3d(3,7,10);
		assertEqual(v1 + v2, v3);
		writeln("passed");
	}

	/// Subtracts one vector from another and returns the difference.
	public static vector sub(const vector x, const vector y) {
		vector v = x.co;
		v.co[] -= y.co[];
		return v;
	}

	unittest {
		write("-- vector subtraction...");
		vector3d v1, v2, v3;
		v1 = vector3d(1,2,4);
		v2 = vector3d(2,5,6);
		v3 = vector3d(-1, -3, -2);
		assertEqual(v1 - v2, v3);
		writeln("passed");
	}

	/// Returns the inner product of two vectors.
	private static FLOAT inner(const vector x, const vector y) {
		FLOAT product = 0.0;
		for (int i = 0; i < DIM; i++) {
			product += x.co[i] * y.co[i];
		}
		return product;
	}

	unittest {
		write("-- vector inner product...");
		vector3d v1, v2;
		v1 = vector3d(1,2,4);
		v2 = vector3d(2,5,6);
		assertEqual(v1*v2, 36.0);
		writeln("passed");
	}

	/// Multiplies a vector by a scalar.
	public static vector scale(const vector x, const FLOAT y) {
		vector v;
		v.co[] = y * x.co[];
		return v;
	}

	unittest {
		write("vector scaling...");
		vector3d v = [1.0, 3.0, 4.7];
		v = 3.1 * v;
		assertEqual(v, [3.1, 9.3, 14.57]);
		v = v * 0.12;
		assertEqual(v, [0.372, 1.116, 1.7484]);
		v *= 2.0;
		assertEqual(v, [0.744, 2.232, 3.4968]);
		writeln("passed");
	}

	/// Divides a vector by a scalar.
	public const vector div(const vector x, const FLOAT y) {
		vector v;
		v.co[] = x.co[] / y;
		return v;
	}

	/// Multiplies this vector by a scalar.
	// TODO: standardize the constance of this
	public const vector scale(const FLOAT y) {
		return scale(this, y);
	}

	unittest {
		write("-- vector mul...");
		vector3d a,b,c,d;
		a = vector3d(1, 2, 4);
		b = vector3d(2, 4, 3);
		c = a * 10.0;
		d = vector3d(10, 20, 40);
		assertEqual(c, d);
		c = 10.0 * b;
		d = vector3d(20, 40, 30);
		assertEqual(c, d);
		c = a.scale(12);
		d = vector3d(12, 24, 48);
		assertEqual(c, d);
  		c = a / 4.0;
        d = vector3d(0.25, 0.5, 1);
		assertEqual(c, d);
		writeln("passed");
	}

	static if (DIM == 3) {
		/// Returns the cross product of two vectors
		public const vector cross(const vector x, const vector y) {
			vector v;
			v[0] = x[1]*y[2] - x[2]*y[1];
			v[1] = x[2]*y[0] - x[0]*y[2];
			v[2] = x[0]*y[1] - x[1]*y[0];
			return v;
		}

		unittest {
			write("vector cross product...");
			vector3d  a,b,c;
			a = [2.0, 3.0, 4.0];
			b = [5.0, 6.0, 7.0];
			c = [-3.0, 6.0, -3.0];
			assertEqual(a^b, c);
			writeln("test missing");
		}
	}

	/// Returns the absolute value of the value of the vector.
	/// No effect on unsigned extended integers -- returns a copy.
	@property
	public const FLOAT norm() {
		return std.math.sqrt(sqr());
	}

	unittest {
		write("-- vector norm...");
		auto v = vector2d(3,4);
writefln("v = %s", v);
writefln("v.norm = %s", v.norm);
// TODO: UFCS?
//writefln("norm(v) = %s", norm(v));

		writeln("test missing");
	}

	public const FLOAT sqr() {
		return this * this;
	}

	unittest {
		write("-- vector sqr...");
		auto v = vector4d(1,2,3,4);
		assertEqual(30.0, v.sqr);
writefln("v = %s", v);
writefln("v.sqr = %s", v.sqr);
writefln("v.norm = %s", v.norm);
		assertEqual(std.math.sqrt(30.0), v.norm);
        assertEqual(5.47723, v.norm);
		writeln("test missing");
	}

	public const vector unit() {
		auto v = this.dup;
		v /= v.norm;
		return v;
	}

	public const vector project(const vector v) {
		auto a = this.dup;
		auto b = v.unit;
		auto scale = a * b;
		return(scale*b);
	}

unittest {
	write("unit vectors...");
	vector3d a,b,c;
	a = [1,2,3.0];
writefln("a = %s", a);
writefln("a.norm = %s", a.norm);
writefln("a.unit = %s", a.unit);
	b = [3.5,3.4,1.7];
writefln("b = %s", b);
writefln("b.norm = %s", b.norm);
writefln("b.unit = %s", b.unit);
writefln("a.project(b) = %s", a.project(b));
writefln("b.project(a) = %s", b.project(a));


	writeln("test missing");
}

	unittest {
		writeln("===================");
		writeln("vector.........end");
		writeln("===================");
	}

}	// end Vector

