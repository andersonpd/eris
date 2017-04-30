// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.number.rational;

import std.math;
import std.conv;
import std.string;

version(unittest) {
	import std.stdio;
	import eris.rational.assertion;
}

unittest {
	writeln("==========================");
	writeln("rational numbers.....begin");
	writeln("==========================");
}


alias rational = Rational!N;
alias rational32 = Rational!int;
alias rational64 = Rational!long;

public struct Rational(N = long) {

//--------------------------------
// structure
//--------------------------------

	private N nm = 0;
	private N dn = 1;

//--------------------------------
// construction
//--------------------------------

	public this(const N n) {
		this(n, 1);
	}

	public this(const N n, const N d) {
		this.nm = n;
		this.dn = d;
	}

	unittest {	// construction
		write("-- construction.....");
		rational r = rational(3);
		assertStringEqual(r, "3");
		r = rational(2,3);
		assertStringEqual(r, "2/3");
		rational64 x = rational64(7,-213);
		assertStringEqual(x, "7/-213");
		writeln("passed");
	}

//--------------------------------
// copying
//--------------------------------

	/// Copy constructor.
	public this(const rational that) {
		this.nm = that.nm;
		this.dn = that.dn;
	}

	/// Returns a copy of a Rational.
	public const rational dup() {
		return rational(this);
	}

	unittest {	// copy
		write("-- copying..........");
		rational r;
		r = rational(25,5);
		assertEqual(r.dup, rational(r));
		r = r.reduce;
		assertEqual(r.nm, 5);
		assertEqual(r.dn, 1);
        r = rational(12, -30);
		assertEqual(r.nm,  12);
		assertEqual(r.dn, -30);
        r = r.normalize;
		assertEqual(r.nm, -12);
		assertEqual(r.dn,  30);
        r = r.reduce;
		assertEqual(r.nm, -2);
		assertEqual(r.dn,  5);
		writeln("passed");
	}

	public static rational normalize(const rational r) {
		auto a = r.dup;
		if (a.dn < 0) {
			a.nm = -a.nm;
			a.dn = -a.dn;
		}
		return a;
	}

	public const rational normalize() {
		return normalize(this);
	}

	public static rational reduce(const rational r) {
		auto z = r.dup;
		auto gcd = gcd!N(z.nm, z.dn);
		if (gcd == 0) return ZERO;
		if (gcd != 1)
			return rational(r.nm/gcd, r.dn/gcd);
		return z;
	}

	public const rational reduce() {
		return reduce(this);
	}

 	public static N gcd(N)(const N a, const N b) {
		N r = a;
		N s = b;
       	while (s) {
           	auto t = s;
           	s = r % s;
           	r = t;
       	}
       	return r < 0 ? -r : r;
   	}

 	public static N lcm(N)(const N a, const N b) {
		if (a == 0 && b == 0) return 0;
       	return (a * b) / gcd(a, b);
   	}

//--------------------------------
// constants
//--------------------------------

	public enum rational NAN = rational(0,0);
	public enum rational INFINITY = rational(1,0);
	public enum rational ZERO = rational(0);
	public enum rational ONE  = rational(1);
	public enum rational HALF  = rational(1);

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the value of the Rational is zero.
	public const bool isNaN() {
		return dn == 0 && nm == 0;
	}

	/// Returns true if the value of the Rational is zero.
	public const bool isInfinite() {
		return dn == 0 && nm != 0;
	}

	/// Returns true if the value of the Rational is zero.
	public const bool isFinite() {
		return dn != 0;
	}

	/// Returns true if the value of the Rational is zero.
	public const bool isZero() {
		return !this.isNaN && this.nm == 0;
	}

	/// Returns true if the value of the Rational is less than zero.
	/// For unsigned extended integers the return value is always false.
	public const bool isNegative() {
		if (this.isNaN) return false; // should throw
		if (nm < 0) return dn >= 0;
		else return dn < 0;
	}

	/// Returns true if the value of the Rational is less than zero.
	/// For unsigned extended integers the return value is always false.
	public const bool isPositive() {
		return !this.isNegative();
	}

	unittest {	// classification
		write("-- classification...");
		rational64 r;
		r = rational64(0,0);
		assertTrue(r.isNaN);
		assertFalse(r.isFinite);
		assertFalse(r.isInfinite);
		r = rational64(1,0);
		assertFalse(r.isNaN);
		assertFalse(r.isFinite);
		assertTrue(r.isInfinite);
		r = rational64(0);
		assertFalse(r.isNaN);
		assertFalse(r.isInfinite);
		assertTrue(r.isFinite);
		assertTrue(r.isZero);
		r = rational64(21, 45);
		assertFalse(r.isNaN);
		assertFalse(r.isInfinite);
		assertTrue(r.isFinite);
		assertFalse(r.isZero);
		r = rational64(3,0);
		assertFalse(r.isNaN);
		assertFalse(r.isFinite);
		assertTrue(r.isInfinite);
		writeln("passed");
	}

//--------------------------------
// properties
//--------------------------------

	/// Returns zero, the initial value for Rational types.
	@property
	public static rational init() {
		return ZERO;
	}

	@property
	public const N num(rational n) {
		return n.nm;
	}

	@property
	public const N num() {
		return this.nm;
	}

	@property
	public const N den() {
		return this.dn;
	}

	@property
	public const N den(rational n) {
		return n.dn;
	}

	@property static rational nan() {
		return rational.NAN;
	}

	@property static rational infinity() {
		return rational.INFINITY;
	}

	@property static rational zero() {
		return rational.ZERO;
	}

	@property static rational one() {
		return rational.ONE;
	}

	// min/max properties

   	/// Returns the maximum value for this type.
	@property
	public static rational max() {
		return rational(N.max);
	}

	/// Returns the minimum value for this type.
	@property
	public static rational min() {
		return rational(N.min);
	}

	/// Returns the minimum value for this type.
	@property
	public static rational epsilon() {
		return rational(1, N.max);
	}

	unittest
	{	// min/max values
		write("-- min/max values...");
		Rational!long a;
		assertEqual(a.max, Rational!long(long.max));
		assertEqual(a.min, Rational!long(long.min));
        assertStringEqual(a.epsilon, "1/9223372036854775807");
		writeln("passed");
	}

//--------------------------------
// parsing, formatting
//--------------------------------

	public this(const string str) {
		// strip whitespace
		char[] chars = strip(str.dup);
		// parse numerator
		nm = parse!N(chars);
		if (chars.length == 0) {
			dn = 1;
			return;
		}
		// skip whitespace
		chars = stripLeft(chars);
		// next char must be a slash
		char ch = parse!char(chars);
		if (ch != '/') {
			throw new ConvException("Expected \'/\', not \'" ~ ch ~ "\'");
		}
		// skip whitespace
		chars = stripLeft(chars);
		dn = parse!N(chars);
		if (chars != "") {
			string s = cast(string)chars;
			throw new ConvException("Unexpected extra characters -- [" ~ s ~ "]");
		}
	}

	unittest
	{
		write("-- this(string).....");
		rational64 r = rational64("1/2");
		assertEqual(r, rational64(1,2));
		r = rational64("1 / 2");
		assertEqual(r, rational64(1,2));
		r = rational64("1/-2");
		assertEqual(r, rational64(-1,2));
		r = rational64("-1	/	2");
		assertEqual(r, rational64(-1,2));
		assertThrows!ConvException(rational64("1/m3"));
		assertThrows!ConvException(rational64("1/3."));
		assertThrows!ConvException(rational64("1\3"));
		assertThrows!ConvException(rational64("1,3"));
		r = ("10/71");
		assertEqual(r, rational64(10,71));
		writeln("passed");
	}

	/// Converts the signed integer value to a string.
	public const string toString() {
		// FIXTHIS: Implement toString
		string str = "";
		if (nm == 0) return "0";
		if (dn == 1) return std.string.format("%d", nm);
		return std.string.format("%d/%d", nm, dn);
	}
	unittest { // toString
		write("-- toString.........");
		rational x, y, z;
		assertStringEqual(x, "0");
        y = rational(32,8);
		assertStringEqual(y, "32/8");
		z = rational(-123,234567);
		assertStringEqual(z, "-123/234567");
		writeln("passed");
	}

//--------------------------------
// conversion to/from integral
//--------------------------------

/*	///
    N opCast(N:bool)() pure
    {
        return !isZero();
    }*/

/*	/// Converts the Rational to an unsigned integer.
	public const int toInt() {
		return value;
	}

	unittest {	// conversion
		write("-- to integer...");
		writeln("test missing");
	}*/

//--------------------------------
// equality
//--------------------------------

	 /// Returns true if this Rational is equal to the argument.
	private const bool opEquals(T:Rational)(const T that) {
		if (this.isNaN || that.isNaN) return false;
		return this.nm * that.dn == that.nm * this.dn;
	}

	 /// Returns true if this Rational is equal to the argument.
	private const bool opEquals(T)(const T that){
		return opEquals(rational(that));
	}

	unittest { // equality
		write("-- equality.........");
		rational r1, r2;
		r1 = rational(2,3);
		r2 = rational(8,12);
		assertEqual(r1,r2);
		assert(r1 == r2);
		r2 = rational(8,13);
		assertNotEqual(r1,r2);
		r2 = rational(-2,3);
		assertNotEqual(r1,r2);
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Returns -1, 0, or 1, if this Rational is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T:Rational)(const T that) {
		N ad = this.nm * that.dn;
		N bc = that.nm * this.dn;
		if (ad < bc) return -1;
		if (ad > bc) return  1;
		return 0;
	}

	/// Returns -1, 0, or 1, if this Rational is, respectively,
	/// less than, equal to or greater than the argument.
	private const int opCmp(T)(const T that) {
		return opCmp(rational(that));
	}

	unittest { // comparison
		write("-- comparison.......");
		rational r1, r2;
		r1 = rational(2,3);
		r2 = rational(9,12);
		assertLessThan(r1,r2);
		writeln("passed");
	}

//--------------------------------
// assignment
//--------------------------------

	/// Assigns a Rational value to this.
	private void opAssign(T:Rational)(const T that) {
		this.nm = that.nm;
		this.dn = that.dn;
	}

	/// Assigns an integral value to this.
	private void opAssign(T)(const T that) {
		opAssign(rational(that));
	}

	unittest {	// assignment
		write("-- opAssign.........");
		rational r1, r2;
		r1 = rational(34,19);
		r2 = r1;
		assertEqual(r1,r2);
		r2 = 3;
		assertNotEqual(r1,r2);
		writeln("passed");
	}

//--------------------------------
// operator assignment
//--------------------------------

	/// Performs an operation on this and assigns the result to this.
	private ref rational opOpAssign(string op, T:Rational)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	/// Performs an operation on this and assigns the result to this.
	private ref rational opOpAssign(string op, T)(const T that) {
		this = opBinary!op(that);
		return this;
	}

	unittest {	// opOpAssign
		write("-- opOpAssign.......");
		rational64 a = rational64(3,5);
		a += rational64(8,5);
		assertEqual(a, rational64(11,5));
		writeln("passed");
	}

//--------------------------------
// unary operations
//--------------------------------

	// implements +, -, ~, ++, --
	private rational opUnary(string op)() {
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

	/// Returns a copy of this Rational
	public const rational plus() {
		return rational(this);
	}

	/// Inverts the sign and returns a copy.
	public const rational minus() {
		return rational(-this.nm, this.dn);
	}

	unittest {	// opUnary
		write("-- opUnary..........");
		rational64 a, b;
		a = rational64(3,5);
		b = rational64(3,5);
		assertEqual(+a, b);
		b = rational64(-3,5);
		assertEqual(-a, b);
		b = rational64(8,5);
		a++;
		assertEqual(a, b);
		b = rational64(-2,5);
		a--;
		a--;
		assertEqual(a, b);
		writeln("passed");
	}

//--------------------------------
// binary operations
//--------------------------------

	private const rational opBinary(string op, T:rational)(const T that)
	{
		static if (op == "+") {
			return add(this, that);
		} else static if (op == "-") {
			return sub(this, that);
		} else static if (op == "*") {
			return mul(this, that);
		} else static if (op == "/") {
			return div(this, that);
		}
	}

	private const rational opBinary(string op, T)(const T that) {
		static if (op == "^^") return pow(this,that);
		else return opBinary!(op, Rational)(rational(that));
	}

	unittest {	// opBinary
		write("-- opBinary.........");
		rational64 a, b;
		a = rational64(4,7);
		b = rational64(3,8);
		assertEqual(a + b, rational64(53,56));
		assertEqual(a - b, rational64(11,56));
		assertEqual(a * b, rational64(12,56));
		assertEqual(a / b, rational64(32,21));
		writeln("passed");
	}

	/// Adds two rational numbers and returns the sum.
	public static rational add(const rational a, const rational b) {
		if (a.isNaN || b.isNaN) return NAN;
		if (a.isZero) return b;
		if (b.isZero) return a;

        // Find gcd of numerators and denominators
        N f = gcd(a.nm, b.nm);
        N g = gcd(a.dn, b.dn);

		rational sum = rational();
		sum.nm = (a.nm/f) * (b.dn/g) + (b.nm/f) * (a.dn/g);
		sum.dn = lcm(a.dn, b.dn);
		return sum * f;
	}

	unittest {
		write("-- addition.........");
		rational r1,r2,r3;
		r1 = 12;
		r2 = 5;
		r3 = r1 + r2;
		assertEqual(r3,17);
		r1 = rational(12,13);
		r2 = rational(5,8);
		r3 = r1 + r2;
		assertEqual(r3,rational(161,104));
		writeln("passed");
	}

/+
    // return a + b, staving off overflow
    public Rational plus(Rational b) {
        Rational a = this;

        // special cases
        if (a.compareTo(zero) == 0) return b;
        if (b.compareTo(zero) == 0) return a;

        // Find gcd of numerators and denominators
        int f = gcd(a.num, b.num);
        int g = gcd(a.den, b.den);

        // add cross-product terms for numerator
        Rational s = new rational((a.num / f) * (b.den / g) + (b.num / f) * (a.den / g),
                                  lcm(a.den, b.den));

        // multiply back in
        s.num *= f;
        return s;
    }


+/	/// Subtracts one rational number from another and returns the difference.
	public const rational sub(const rational x, const rational y) {
		return add(x, y.minus);
	}

	unittest {
		write("-- subtraction......");
		rational r1,r2,r3;
		r1 = 12;
		r2 = 5;
		r3 = r1 - r2;
		assertEqual(r3, 7);
		r3 = r2 - r1;
		assertEqual(r3, -7);
		r1 = rational(12,13);
		r2 = rational(5,8);
		r3 = r1 - r2;
		assertEqual(r3, rational(31,104));
		writeln("passed");
	}

	/// Multiplies two unsigned extended integers and returns the product.
	public const rational mul(const rational x, const rational y) {
		rational r1 = reduce(x);
		rational r2 = reduce(y);
		rational prod = rational();
		prod.nm = r1.nm * r2.nm;
		prod.dn = r1.dn * r2.dn;
		return prod;
	}

	unittest {
		write("-- multiplication...");
		rational r1,r2,r3;
		r1 = 12;
		r2 = 5;
		r3 = r1 * r2;
		assertEqual(r3, 60);
		r3 = r2 * r1;
		assertEqual(r3, 60);
		r1 = rational(12,13);
		r2 = rational(5,8);
		r3 = r1 * r2;
		assertEqual(r3, rational(60,104));
		writeln("passed");
	}

	/// Divides one Rational by another
	/// and returns the quotient.
	public const rational div(const rational x, const rational y) {
		rational quot = rational();
		quot.nm = x.nm * y.dn;
		quot.dn = y.nm * x.dn;
		return quot;
	}

	unittest {
		write("-- division.........");
		rational r1,r2,r3;
		r1 = 12;
		r2 = 5;
		r3 = r1 / r2;
		assertEqual(r3, rational(12,5));
		r3 = r2 / r1;
		assertEqual(r3, rational(5,12));
		r1 = rational(12,13);
		r2 = rational(5,8);
		r3 = r1 / r2;
		assertEqual(r3, rational(96, 65));
		writeln("passed");
	}

	/// Raises a Rational to an integer power
	/// and returns the result. Tests the result for overflow.
	public const rational pow(const rational x, const int n) {
		if (this.isFinite) {
			if (n == 1) return this.dup;
			if (n == -1) return rational(dn, nm);	// should throw if nm == 0;
			if (n > 0) return rational(nm ^^ n, dn ^^ n);
			if (n < 0) return rational(dn ^^ -n, nm ^^ -n);
			return ONE;
		}
		if (this.isInfinite) return INFINITY;
		return NAN;
	}

	unittest {
		write("-- power............");
		rational a, b;
		a = "2/3";
		b = "4/9";
		assertEqual(a^^2, b);
		assertEqual(a^^2, a.sqr);
		b = "8/27";
		assertEqual(a^^3, b);
		assertEqual(a^^-3, b.reciprocal);
		a = "-2/3";
		assertEqual(a^^3, -b);
		writeln("passed");
	}

	/// Returns the absolute value of the value of the Rational.
	public static rational reciprocal(const rational a) {
		if (a.isNaN) return NAN;
		if (a.isZero) return INFINITY;
		return rational(a.dn,a.nm);
	}

	/// Returns the absolute value of the value of the Rational.
	public const rational reciprocal() {
		return reciprocal(this);
	}

	unittest {
		write("-- reciprocal.......");
		rational a, b;
		a = "2/5";
		b = "5/2";
		assertEqual(reciprocal(a), b);
		a = 0;
		b = INFINITY;
		assertEqual(reciprocal(a), b);
		writeln("passed");
	}

	/// Returns the absolute value of the value of the Rational.
	public const rational abs(rational a) {
		rational copy = a.dup;
		if (copy.nm < 0) copy.nm = -copy.nm;
		if (copy.dn < 0) copy.dn = -copy.dn;
		return copy;
	}

	/// Returns the absolute value of the value of the Rational.
	public const rational abs() {
		return abs(this);
	}

	unittest {
		write("-- absolute value...");
		rational a, b;
		a = "-2/3";
		b = "4/6";
		assertEqual(a.abs, b);
		a = "2/3";
		assertEqual(a.abs, b);
		a = "2/-3";
		assertEqual(a.abs, b);
		writeln("passed");
	}

	public static rational sqr(const rational a) {
		return rational(a.nm * a.nm, a.dn * a.dn);
	}

	public const rational sqr() {
		return rational(nm * nm, dn * dn);
	}

	unittest {
		write("-- square...........");
		rational a, b;
		a = "-2/3";
		b = "4/9";
		assertEqual(a.sqr, b);
		assertEqual(sqr(a), b);
		writeln("passed");
	}

	public const N ceil(const rational a) {
		if (a.isFinite) {
			N q = a.nm / a.dn;
			if (a.nm % a.dn == 0) return q;
			return a.isPositive ? q + 1 : q;
		}
		return 0;
	}

	public const N ceil() {
		return ceil(this);
	}

	unittest {
		write("-- ceiling..........");
		rational r;
		r = rational(12,5);
		assertEqual(r.ceil, 3);
		r = rational(27,10);
		assertEqual(r.ceil, 3);
		r = rational(2);
		assertEqual(r.ceil, 2);
		r = rational(-27,10);
		assertEqual(r.ceil, -2);
		r = rational(-2);
		assertEqual(r.ceil, -2);
		r = rational(-1,2);
		assertEqual(r.ceil, 0);
		r = rational(1,2);
		assertEqual(r.ceil, 1);
		writeln("passed");
	}

	public static N floor(const rational a) {
		if (a.isFinite) {
			N q = a.nm / a.dn;
			if (a.nm % a.dn == 0) return q;
			return a.isPositive ? q : q - 1;
		}
		return 0;
	}

	public const N floor() {
		return floor(this);
	}

	unittest {
		write("-- floor............");
		rational r = rational(12,5);
		assertEqual(r.floor, 2);
		r = rational(27,10);
		assertEqual(r.floor, 2);
		r = rational(-27,10);
		assertEqual(r.floor, -3);
		r = rational(-2);
		assertEqual(r.floor, -2);
		writeln("passed");
	}

	public static N round(const rational a) {
		rational b;
		if (a.isNegative) {
			b = a - HALF;
			return b.ceil;
		}
		b = a + HALF;
		return b.floor;
	}

	public const N round() {
		return round(this);
	}

	public static N trunc(const rational a) {
		rational b;
		b = a - HALF;
		return  a.isNegative ? a.ceil : a.floor;
	}

	public const N trunc() {
		return trunc(this);
	}

	/// Returns the fractional part of the argument.
	public static rational remainder(const rational a) {
		if (!a.isFinite) return rational,nan;
		return rational(a - trunc(a));
	}

	unittest {
		write("-- rounding.........");
		rational a;
		a = "3/4";
		assertEqual(a.ceil, 1);
		assertEqual(a.floor, 0);
		assertEqual(a.round, 1);
		assertEqual(a.trunc, 0);
		a = "-3/4";
		assertEqual(a.floor, -1);
		assertEqual(a.ceil, 0);
		assertEqual(a.round, -1);
		assertEqual(a.trunc, 0);
		writeln("passed");
		a = "1/2";
		assertEqual(a.ceil, 1);
		assertEqual(a.floor, 0);
		assertEqual(a.round, 1);
		assertEqual(a.trunc, 0);
		a = "3/2";
		assertEqual(a.round, 2);
		a = "-1/2";
		assertEqual(a.round, -1);
		assertEqual(remainder(a), a);
		a = "3/2";
		assertEqual(remainder(a), rational(1,2));
	}

	public const real value(rational a) {
		if (a.isFinite) {
			return cast(real)a.nm / cast(real)a.dn;
		}
		return real.nan;
	}

	public const real value() {
		return value(this);
	}

	unittest {
		write("-- real value.......");
		rational a, b;
		a = "5/2";
		assertEqual(a.value, 2.5);
		writeln("passed");
	}

	/// Returns the mediant of the arguments.
	public static rational mediant(const rational a, const rational b) {
		if (a.isNaN || b.isNaN) return NAN;
		return rational(a.nm + b.nm, a.dn + b.dn);
	}

	unittest {
		write("-- mediant..........");
		rational a, b;
		a = "2/3";
		b = "5/7";
		assertEqual(mediant(a,b), rational(7,10));
		writeln("passed");
	}

}	// end Rational

unittest {
	writeln("==========================");
	writeln("rational numbers.......end");
	writeln("==========================");
}

