// Written in the D programming language

/**
 * Floating-point decimal mathematical functions.
 *
 * An implementation of the
 * General Decimal Arithmetic Specification.
 *
 * Authors: Paul D. Anderson
 *
 * Copyright: Copyright 2009-2016 by Paul D. Anderson.
 *
 * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
 *
 * Standards: Conforms to the
 *	General Decimal Arithmetic Specification,
 *	Version 1.70, (25 March 2009).
 */

module eris.decimal.math;

import std.stdio; // for test only

import eris.decimal;

unittest {
	writeln("==========================");
	writeln("decimal math.........begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
//	import eris.decimal.asserts;
	import eris.decimal.test;
}

/*
	TODO: For each function --
		1. Ensure algorithm is properly implemented.
		2. Ensure context flags are being set properly.
		3. Ensure function passes GDA tests.
		4. Determine additional tests needed and implement them.
		5. Ensure all special cases are enumerated.
		6. Automate all tests, if possible.
		7. Determine what effect the context has on the function and
			if it should be explained.
		8. Ensure all documentation is complete:
			a. Header - description, inputs, return value(s)
			b. Code - variables, control statements, branches, return points.
		9. Move most tests to the test module.
*/

/*
mixin template checkNaN() {
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
}*/

//--------------------------------
// ROUNDING
//--------------------------------

/**
 *  Rounds the argument to the nearest integer. If the argument is exactly
 *  half-way between two integers the even integer is returned.
 */
public D rint(D)(D num)
    if (isDecimal!D)
{
  return round(num, HALF_EVEN);
}

unittest
{  // rint
  static struct S { D9 num; D9 expect; }
  S[] s =
  [
    {  "2.1",   "2" },
    {  "100",   "100" },
    {  "100.0", "100" },
    {  "101.5", "102" },
    {  "-101.5", "-102" },
    {  "10E+5", "1.0E+6" },
    { "7.89E+77", "7.89E+77" },
    { "-Inf", "-Infinity" },
  ];
  auto f = FunctionTest!(S,D9)("rint");
  foreach (t; s) f.test(t, rint(t.num));
  writefln(f.report);
}

unittest
{  // rint
  static struct S { D9 num; D9 expect; }
  S[] s =
  [
    {  "2.1",  2 },
    {  "2.5",  2 },
    {  "3.5",  4 },
    {  "2.9",  3 },
    { "-2.1", -2 },
    { "-2.9", -3 },
    { "-2.5", -2 },
  ];
  auto f = FunctionTest!(S,D9)("rint");
  foreach (t; s) f.test(t, rint(t.num));
  writefln(f.report);
}

/**
 *  Returns the nearest integer less than or equal to the argument.
 *  Rounds toward negative infinity.
 */
public T floor(T)(T x) {
	return round(x, FLOOR);
}

unittest
{	// floor
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{  "2.1",  2 },
		{  "2.5",  2 },
		{  "3.5",  3 },
		{  "2.9",  2 },
		{ "-2.1", -3 },
		{ "-2.9", -3 },
		{ "-2.5", -3 },
	];
	auto f = FunctionTest!(S,TD)("floor");
	foreach (t; s) f.test(t, floor(t.x));
  writefln(f.report);
}

/**
 *  Returns the nearest integer greater than or equal to the argument.
 *  Rounds toward positive infinity.
 */
public T ceil(T)(T x) {
	return round(x, CEILING);
}

unittest
{	// ceil
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{  "2.1",  3 },
		{  "2.5",  3 },
		{  "3.5",  4 },
		{  "2.9",  3 },
		{ "-2.1", -2 },
		{ "-2.9", -2 },
		{ "-2.5", -2 },
	];
	auto f = FunctionTest!(S,TD)("ceil");
	foreach (t; s) f.test(t, ceil(t.x));
  writefln(f.report);
}

/**
 *  Returns the truncated argument.
 *  Rounds toward zero.
 */
public T trunc(T)(T x) {
	return round(x, ROUND_DOWN);
}

unittest
{	// trunc
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{  "2.1",  2 },
		{  "2.5",  2 },
		{  "3.5",  3 },
		{  "2.9",  2 },
		{ "-2.1", -2 },
		{ "-2.9", -2 },
		{ "-2.5", -2 },
	];
	auto f = FunctionTest!(S,TD)("trunc");
	foreach (t; s) f.test(t, trunc(t.x));
  writefln(f.report);
}

/**
 *  Returns the nearest integer value. If the value is greater (less) than
 *  the maximum (minimum) int value the maximum (minimum) value is returned.
 *  The value is rounded based on the specified rounding mode. The default
 *  mode is half-even.
 */
public int toInt(T)(T x, Round mode = HALF_EVEN)
{
	if (x.isNaN)
	{
		throw new InvalidOperationException("NaN cannot be converted to int");
	}
	if (x.isInfinite)
	{
		return x.isNegative ? int.min : int.max;
	}
	return toBigInt(x, mode).toInt;
}

/**
 *  Returns the nearest long value. If the value is greater (less) than
 *  the maximum (minimum) long value the maximum (minimum) value is returned.
 *  The value is rounded based on the specified rounding mode. The default
 *  mode is half-even.
 */
public long toLong(T)(T x,
		Round mode = HALF_EVEN) if (isDecimal!T)
{
	if (x.isNaN)
	{
		throw new InvalidOperationException("NaN cannot be converted to long");
	}
	if (x.isInfinite)
	{
		return x.isNegative ? long.min : long.max;
	}
	return toBigInt(x, mode).toLong;
}

// FIXTHIS: toBigInt doesn't work?
/**
 *  Returns the nearest extended integer value.
 *  The value is rounded based on the specified rounding mode. The default
 *  mode is half-even.
 */
public BigInt toBigInt(T)(T x, Round mode = HALF_EVEN)
{
	if (x.isNaN)
	{
		throw new InvalidOperationException("NaN cannot be converted to BigInt");
	}
	if (x.isInfinite)
	{
		return x.isNegative ? -T.max.coff : T.max.coff;
	}
	if (x.expo != 0)
	{
		// FIXTHIS: what does this actually do?
		return round(x, mode).coff;
	}
	return x.coff;
}

//--------------------------------
//	string mixins
//--------------------------------

/*template Constant(string name)
{
const char[] Constant =
	"/// Returns the value of the constant at the specified precision.
	public T " ~ name ~ "(T)(int precision = T.precision) if (isDecimal!T)
	{
		Context context = Context(precision, T.maxExpo, T.mode);
		static T value;
		static int lastPrecision = 0;
		if (precision == lastPrecision) return value;
		if (precision > lastPrecision) {
			value = eris.decimal.math." ~ name ~ "!T(context);
			lastPrecision = precision;
		}
		return precisionRound(value, precision);
	}";
}*/

/*template UnaryFunction(string name)
{
const char[] UnaryFunction =

	"/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T, U:int)(T x, U precision = T.precision) if (isDecimal!T)
	{
		if (x.isNaN) {
			contextFlags.set(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, T.maxExpo, HALF_EVEN);
		return " ~ name ~ "!T(x, context);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,S:string)(S str, int precision = T.precision) if (isDecimal!T) {
		T x = T(str);
		return " ~ name ~ "!T(x, precision);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,L:long)(L n, int precision = T.precision) if (isDecimal!T) {
		T x = T(n);
		return " ~ name ~ "!T(x, precision);
	}

	/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T,R:real)(R a, int precision = T.precision)  if (isDecimal!T){
		T x = T(a);
		return " ~ name ~ "!T(x, precision);
	}";
}
*/

template UnaryFunction(string name)
{
  const char[] UnaryFunction =

  "/// Returns the value of the function at the specified precision.
  public T " ~ name ~ "(T)(T x, int precision = T.precision) if (isDecimal!T)
  {
    if (x.isNaN)
    {
      contextFlags.set(INVALID_OPERATION);
      return T.nan;
    }
    Context context = Context(precision, T.maxExpo, HALF_EVEN);
    return " ~ name ~ "!T(x, context);
  }

  /// Returns the value of the function at the specified precision.
  public T " ~ name ~ "(T,U)(U num, int precision)
    if (isDecimal!T && isConvertible!U)
  {
    T x = T(num);
    return " ~ name ~ "!T(x, precision);
  }";
}

template BinaryFunction(string name)
{
const char[] BinaryFunction =

  "/// Returns the value of the function at the specified precision.
  public T " ~ name ~ "(T)(T x, T y, int precision = T.precision)
  if (isDecimal!T)
  {
    if (x.isNaN || y.isNaN)
    {
      contextFlags.set(INVALID_OPERATION);
      return T.nan;
    }
    Context context = Context(precision, T.maxExpo, HALF_EVEN);
    return " ~ name ~ "!T(x, y, context);
  }";
}

//--------------------------------
//	CONSTANTS
//--------------------------------

/*/// Returns pi (3.14159265...) at the specified precision.
/// If the precision is less than or equal to a prior precision,
/// the earlier calculated value is returned (rounded if needed).
/// If the specified precision is higher than any previously calculated
/// precision, then the constant is recalculated and the higher value
/// is retained for subsequent use.
/// Repeated calls to the function at the same precision perform no rounding
/// or calculation.
public static D pin(D)(int precision, D estimate = D.PI) if (isDecimal!D)
{
  static D value = D.PI;
  static int last = D.precision;
  static D highValue = D.PI;
  static int high = D.precision;

  // attempt to use previously calculated values
  value = avoidCalculation(precision, last, value, high, highValue);

  // if the returned value is NaN, a recalculation is needed.
  if (value.isNaN)
  {
    Context context = Context(precision, D.maxExpo, D.mode);
    value = eris.decimal.math.pi!D(context);
    high = precision;
    last = high;
  }
  return value;
}

unittest
{  // pi
  static struct S { int n; TD expect; }
  S[] s =
  [
    {  9, "3.14159265" },
    { 10, "3.141592654" },
    { 12, "3.14159265359" },
    { 20, "3.1415926535897932385" },
    { 14, "3.1415926535898" },
    { 16, "3.141592653589793" },
    { 18, "3.14159265358979324" },
    { 22, "3.141592653589793238463" },
    { 23, "3.1415926535897932384626" },
    { 24, "3.14159265358979323846264" },
    { 25, "3.141592653589793238462643" },
    { 26, "3.1415926535897932384626434" },
  ];
  auto f = FunctionTest!(S,TD)("pin");
  foreach (t; s) f.test(t, pin(t.n), t.n);
  writefln(f.report);
}*/

/// Returns pi (3.14159265...) at the specified precision.
/// If the precision is less than or equal to a prior precision,
/// the earlier calculated value is returned (rounded if needed).
/// If the specified precision is higher than any previously calculated
/// precision, then the constant is recalculated and the higher value
/// is retained for subsequent use.
/// Repeated calls to the function at the same precision perform no rounding
/// or calculation.
public D pi(D)(Context context) if (isDecimal!D)
{
	// TODO: (behavior) if only 2 guard digits are used, function doesn't return
	auto guarded = guard(context, 3);
	// AGM algorithm
	long k = 1;
	D a0 = D.one;
	D b0 = sqrt1_2!D(guarded);
	D s0 = D(5,-1);//.HALF;
	D a1, b1, s1;
	// loop until the arithmetic mean equals the geometric mean
	while (!equals(a0, b0, guarded))
	{
		// arithmetic mean: a1 = (a0+bo)/2))
		a1 = mul(D.HALF, add(a0, b0, guarded), guarded);
		// geometric mean: b1 = sqrt(a0*b0)
		b1 = sqrt(mul(a0, b0, guarded), guarded);
		k *= 2;
		s1 = sub(s0, mul(sub(sqr(a1, guarded), sqr(b1, guarded), guarded), k, guarded), guarded);
		a0 = a1;
		b0 = b1;
		s0 = s1;
	}
	D pi = mul(div(sqr(a1, guarded), s1, guarded), 2, guarded);
	return precisionRound(pi, context);
}

//mixin (Constant!("pi_2"));
package T pi_2(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T halfPi = mul(pi!T(context), T.HALF, context);
	return precisionRound(halfPi, inContext);
}

/*unittest
{	// pi_2
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "1.57079633" },
		{ 25, "1.570796326794896619231322" },
		{  5, "1.5708234" },	// note extra incorrect digits
	];
	auto f = FunctionTest!(S,TD)("pi/2");
	foreach (t; s) f.test(t, TD.pi_2(t.n), t.n);
  writefln(f.report);
}*/

//mixin (Constant!("invPi"));
// TODO: (efficiency) Need to ensure that previous version of pi isn't reset.
// TODO: shouldn't this be a calculation without a division?
/// Calculates the value of 1/pi in the specified context.
package T invPi(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 4);
	T alpha =  div(T.one, pi!T(context), context);
	return precisionRound(alpha, inContext);
}

/*unittest
{	// invPi
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "0.318309886" },
		{ 25, "0.3183098861837906715377675" },
		{  5, "0.3183144449" },	// note extra incorrect digits
	];
	auto f = FunctionTest!(S,TD)("1/pi");
	foreach (t; s) f.test(t, TD.invPi(t.n), t.n);
  writefln(f.report);
}*/

//mixin (Constant!("twoInvPi"));
// TODO: (efficiency) Need to ensure that previous version of pi isn't reset.
// TODO: shouldn't this be a calculation without a division?
/// Calculates the value of 1/pi in the specified context.
package T twoInvPi(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 4);
	T alpha =  div(T.TWO, pi!T(context), context);
	return precisionRound(alpha, inContext);
}

/*unittest
{	// twoInvPi
	static struct S { int n; TD expect; }
	S[] s =
	[
		{  9, "0.636619772" },
		{ 25, "0.6366197723675813430755351" },
		{  5, "0.63662" },
	];
	auto f = FunctionTest!(S,TD)("2/pi");
	foreach (t; s) f.test(t, TD.twoInvPi(t.n), t.n);
  writefln(f.report);
}*/

/// If a constant has already been calculated, this function attempts
/// to use an existing value rather than recalculate.
/// If the constant has not yet been calculated to the desired precision,
/// returns null.
package D avoidCalculation(D)(int precision,
  ref int last, ref D value,
  ref int high, ref D highValue)
{
  // initialize the high value, if needed.
  if (highValue.isNaN) highValue = value;

  // if the input precision <= last precision used
  if (precision == last)
  {
   return value;
  }
  if (precision < last)
  {
    last = precision;
    value = precisionRound(value, precision);
    return value;
  }

  // if the input precision <= highest precision used
  if (precision == high)
  {
    last = high;
    value = highValue;
    return value;
  }
  if (precision < high)
  {
    last = precision;
    value = precisionRound(highValue, precision);
    return value;
  }
  return D.nan;
}

public D e(D)(int precision = D.precision) if (isDecimal!D)
{
  enum D precValue = roundString(
    "2.71828182845904523536028747135266" ~
    "249775724709369995957496696762772" ~
    "407663035354759457138217852516643",
    D.precision);
  static D value = precValue;
  static int last = D.precision;
  static D highValue = precValue;
  static int high = D.precision;

  if (precision == D.precision) return precValue;
  // attempt to use previously calculated values
  value = avoidCalculation!D(precision, last, value, high, highValue);

  // if the returned value is NaN, a recalculation is needed.
  if (value.isNaN)
  {
    Context context = Context(precision, D.maxExpo, D.mode);
    value = eris.decimal.math.e!D(context);
    high = precision;
    last = high;
  }
  return value;
}

/// Returns the value of e in the specified context.
package D e(D)(Context context) if (isDecimal!D)
{
  auto guarded = guard(context);
  // initialize Taylor series.
  long n = 2;
  D term = D.one;
  D sum  = D.one;
  // loop until the term is too small to affect the sum.
  while (term > D.epsilon(guarded)) {
    sum  = add(sum, term, guarded);
    term = div!D(term, n, guarded);
    n++;
  }
  return precisionRound(sum, context);
}

unittest
{  // e
  static struct S { int n; TD expect; }
  S[] s =
  [
    {  9, "2.71828183" },
    { 25, "2.7182818284590452353602874713526625" },
    {  5, "2.7183" },
  ];
  auto f = FunctionTest!(S,TD)("e");
  foreach (t; s) f.test(t, e!TD(t.n), t.n);
  writefln(f.report);
}

/*//mixin (Constant!("ln10"));
package enum T ln10(T)(Context context) if (isDecimal!T)
{
	return log(T.TEN, context, false);
}*/

//mixin (Constant!("ln2"));
package enum T ln2(T)(Context context) if (isDecimal!T)
{
	return log(T.TWO, context, false);
}

//mixin (Constant!("log2_e"));
package enum T log2_e(T)(Context context) if (isDecimal!T)
{
	return div(T.one, log(T.TWO, context, false), context);
}

//mixin (Constant!("log2_10"));
package enum T log2_10(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T log2T = div(log(T.TEN, context, false), log(T.TWO, context, false), context);
	return precisionRound(log2T, inContext);
//	return precisionRound(TD("18690473486004564289165545643685440097"), inContext);
//	return precisionRound(TD("18690473486004564245643685440097"), inContext);
}

//mixin (Constant!("log10_e"));
package enum T log10_e(T)(Context context) {
	return T.one/log(T.TEN, context, false);
}

//mixin (Constant!("log10_2"));
package enum T log10_2(T)(Context context) {
	return log(T.TWO, context)/log(T.TWO, context, false);
}

//mixin (Constant!("sqrt2"));
package enum T sqrt2(T)(Context context) if (isDecimal!T)
{
	return sqrt(T.TWO, context);
}

//mixin (Constant!("sqrt1_2"));
package enum T sqrt1_2(T)(Context context) if (isDecimal!T)
{
	return sqrt(T.HALF, context);
}

//mixin (Constant!("phi"));
package enum T phi(T)(Context context) if (isDecimal!T)
{
	return mul(add(T(1) , sqrt(T(5), context), context), T.half, context);
}

//mixin (Constant!("invSqrtPi"));
package enum T invSqrtPi(T)(Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 4);
	T alpha =  div(T.one, sqrt(pi!T(context), context), context);
	return precisionRound(alpha, inContext);
}

/*unittest
{	// constants
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ TD.ln10,    9,  "2.30258509" },
		{ TD.ln2,     9,  "0.693147181" },
		{ TD.log2_e,  9,  "1.44269504" },
		{ TD.log2_10, 9,  "3.32192809" },
		{ TD.sqrt2,   9,  "1.41421356" },
		{ TD.sqrt1_2, 9,  "0.707106781" },
		{ TD.phi,     9,  "1.61803399" },
		{ TD.ln10(16),    16, "2.302585092994046" },
		{ TD.ln2(16),     16, "0.6931471805599453" },
		{ TD.log2_e(16),  16, "1.442695040888963" },
		{ TD.log2_10(16), 16, "3.21928094887362" }, // FIXTHIS: returns 3.321457567817785
		{ TD.sqrt2(16),   16, "1.414213562373095" },
		{ TD.sqrt1_2(16), 16, "0.7071067811865475" },
		{ TD.phi(16),     16, "1.618033988749895" },
	];
	auto f = FunctionTest!(S,TD)("constants");
	foreach (t; s) f.test(t, t.x, t.p);
  writefln(f.report);
}*/

//--------------------------------
//	ALGEBRAIC FUNCTIONS
//--------------------------------

/// Returns true if n is odd, false otherwise.
private bool isOdd(int n) {
	return n & 1;
}

unittest {	// isOdd
	assertTrue(isOdd(3));
	assertTrue(!isOdd(8));
	assertTrue(isOdd(-1));
}

// TODO: (behavior) add bitshift function?

//mixin (UnaryFunction!("reciprocal"));
mixin (UnaryFunction!("invSqrt"));
mixin (UnaryFunction!("sqrt"));

/// Returns the value of the function at the specified precision.
public T reciprocal(T, U:int)(T x, U precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	Context context = Context(precision, T.maxExpo, HALF_EVEN);
	return reciprocal!T(x, context);
}

/*/// Returns the value of the function at the specified precision.
public T reciprocal(T, U)(in U u, int precision)
	if (isDecimal!T && isConvertible!U)
{
	T x = T(u);
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	Context context = Context(precision, T.maxExpo, HALF_EVEN);
	return reciprocal!T(T(x), context);
}*/

// TODO: what happens if x is very close to zero or one?
// Does it try to work with the numbers anyway??
public T reciprocal(T)(in T x, Context inContext = T.context) if (isDecimal!T)
{
	// special values
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.copyAbs.isOne) return x.copy;
	if (x.isInfinite) return T.zero(x.sign);

	// extend working precision
	auto context = guard(inContext);

	// initial estimate
	T a = x.reduce;
	T r1 = T(2, -ilogb(a)-1);

	// Newton's method
	while (true) {
		T r0 = r1;
		r1 = mul(r0, sub(T.TWO, mul(a, r0, context), context), context);
		if (equals(r0, r1 ,context)) break;
	}
	// round to the original precision
	return precisionRound(r1, context);
}

unittest
{	// reciprocal
	static struct S { TD x; TD expect; }
	S[] s =
	[
//		{ "1234567890123456789", TD(1)/TD("1234567890123456789") },
//		{ "1234567890678900000", TD(1)/TD("1234567890123456789") },
		{ "125", "0.008" },
		{ "0.008", "125" },
	];
	auto f = FunctionTest!(S,TD)("reciprocal");
	foreach (t; s) f.test(t, reciprocal(t.x));
  writefln(f.report);
}

// TODO: see note at reciprocal
public T invSqrt(T)(T x, Context inContext) if (isDecimal!T)
{
	// special values
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity(x.sign);
	}
	if (x.isOne) return x;
	if (x.isInfinite) return T.zero(x.sign);

	// increase the working precision
	auto context = guard(inContext);

	// initial estimate
	T a;
	int k = ilogb(x);
	if (isOdd(k)) {
		a = T(2, -1);
	}
	else {
		a = T(5, -1);
		k++;
	}
	const t = T(3);
	// reduce the exponent
	x.expo = x.expo - k - 1;
	// Newton's method
	while (true) {
		T b = a;
		a = mul(b, mul(T.HALF, sub(t, mul(x, sqr(b, context), context), context), context), context);
		if (equals(b, a, context)) break;
	}
	// restore the exponent
	a.expo = a.expo - k/2 - 1;
	// round to the original precision
	return precisionRound(a, inContext);
}

/*unittest
{	// invSqrt
	static struct S { TD x; int n; TD expect; }
	S[] s =
	[
		{ "2", 6, "0.707107" },
		{ "20", 9, "0.223606798" },
		{ "300", 9, "0.0577350269" },
		{ "98763", 9, "0.00318201969" },
		{ "98763098", 9, "0.000100624248" },
		{ "0.008", 9, "1.11803399" },
		{ "98763098", 9, "0.000100624248" },
		{ "9876387982347", 9, "3.18200552E-7" },
		{ "2", 14, "0.70710678118655" },
		{ "4000", 11, "0.015811388301" },
	];
	auto f = FunctionTest!(S,TD)("invSqt");
	foreach (t; s) f.test(t, invSqrt(t.x,t.n));
  writefln(f.report);
}*/

/// Returns the square root of the argument to the type precision.
/// Uses Newton's method.
public D sqrt(D)(D arg, Context context) if (isDecimal!D)
{
//  auto context = guard(inContext, 3);
  // special values
  if (arg.isNegative) {
    contextFlags.set(INVALID_OPERATION);
    return D.nan;
  }
  // TODO: what if arg is very close to one or zero??
  if (arg.isOne) return D.one;
  if (arg.isZero) return D.zero;
  if (arg.isInfinite) return D.infinity;

  // reduce the exponent and estimate the result
  D value;
  int k = ilogb(arg);
  if (isOdd(k)) {
    value = D(6, -1);
  }
  else {
    value = D(2, -1);
    k++;
  }
  arg.expo = arg.expo - k - 1;

  // Newton's method
  while (true) {
    D prev = value;
    value = mul(D.HALF, add(prev, div(arg, prev, context), context), context);
    if (equals(value, prev, context)) break;
  }
  // restore the exponent
  value.expo = value.expo + (k+1)/2;
  // round the result
  return precisionRound(value, context);
}

unittest
{  // sqrt
  static struct S { TD x; int p; TD expect; }
  S[] s =
  [
    { "2", 9, "1.41421356" },
    { "0.707106781187", 12, "0.840896415254" },
    { "200", 21, "14.142135623730950488000" },
    { "25", 8, "5.0" },
    { "2E-5", 10, "0.00447213595500" },
    { "1E-15", 9, "3.16227766E-8" },
    { "1E-16", 9, "1.00000000E-8" },
  ];
  auto f = FunctionTest!(S,TD)("sqrt");
  foreach (t; s) f.test(t, sqrt(t.x, t.p), t.p);
  writefln(f.report);
}

//--------------------------------
// EXPONENTIAL AND LOGARITHMIC FUNCTIONS
//--------------------------------

/+mixin (UnaryFunction!("exp"));

	"/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T)(T x, int precision = T.precision) if (isDecimal!T)
	{
		if (x.isNaN) {
			contextFlags.set(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, T.maxExpo, HALF_EVEN);
		return " ~ name ~ "!T(x, context);
	}
+/

//--------------------------------
// exponentials and logarithms
//--------------------------------

/// Adds guard digits to the context precision and sets rounding to HALF_EVEN.
/// This is useful for extended calculation to minimize errors.
/// Returns a new context with the precision increased by the guard digits value.
/// Note that this does not create a new Decimal type with this context.
public Context guard(Context context, int guardDigits = 2)
{
  return Context(context.precision + guardDigits, context.maxExpo, HALF_EVEN);
}

/// Returns the exponent of the argument at the specified precision.
public D exp(D)(in D arg, int precision)
    if (isDecimal!D || isConvertible(D))
  {
    D num = D(arg);

    // check for nan
    if (num.isNaN)
    {
      contextFlags.set(INVALID_OPERATION);
      return D.nan;
    }
    // create new context at given precision
    Context context = Context(precision, D.maxExpo, HALF_EVEN);
    // call the function
    return exp!D(num, context);
  }

/// Returns the exponent of the argument at the current precision.
/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
public D exp(D)(D num, Context context = D.context, int guardDigits = 2)
    if (isDecimal!D)
{
  if (num.isInfinite)
  {
    return num.isNegative ? D.zero : num;
  }
  bool negative = num.isNegative;
  if (negative) num = num.copyAbs;
  auto guarded = guard(context, guardDigits);
  D sqrx = sqr(num, guarded);
  long n = 1;
  D fact = 1;
  D t1   = D.one;
  D t2   = num;
  D term = add(t1, t2, guarded);
  D sum  = term;
  while (term > D.epsilon(guarded))
  {
    n   += 2;
    t1   = mul(t2, mul(num, n, guarded), guarded);
    t2   = mul(t2, sqrx, guarded);
    fact = mul(fact, n*(n-1), guarded);
    term = div(add(t1, t2, guarded), fact, guarded);
    sum  = add(sum, term, guarded);
  }
  if (negative) sum = div(D.one, sum, guarded);
  return precisionRound(sum, context);
}

unittest
{  // exp
  static struct S { TD x; int p; TD expect; }
  S[] s =
  [
    {  1, 9, "2.71828183" },
    {  2, 9, "7.3890560989306502272" },
    { -2, 9, "0.13533528324" },
    {  1, 9, "2.71828183" },
    {  1, 11, "2.7182818285" },
    {  1, 15, "2.71828182845905" },
    {  2, 15, "7.3890560989306502272" },
    {  2, 11, "7.3890560989306502272" },
    { -2, 11, "0.13533528324" },
  ];
  auto f = FunctionTest!(S,TD)("exp");
  foreach (t; s) f.test(t, exp(t.x, t.p), t.p);
  writefln(f.report);
}

public enum D ln10(D)(Context context) if (isDecimal!D)
{
  return log(D.TEN, context, false);
}

/// Returns the logarithm of the argument at the specified precision.
public D log(D)(in D arg, int precision = D.precision)
    if (isDecimal!D || isConvertible(D))
{
  D num = D(arg);
   // check for nan
  if (num.isNaN)
  {
    contextFlags.set(INVALID_OPERATION);
    return D.nan;
  }
  // create new context at input precision
  Context context = Context(precision, D.maxExpo, HALF_EVEN);
  // call the function
  return log!D(num, context);
}

public D log(D)(D num, Context context,
    bool reduceArg = true) if (isDecimal!D)
{
  if (num.isZero)
  {
    contextFlags.set(DIVISION_BY_ZERO);
    return -D.infinity;
  }
  if (num.isNegative) return D.nan;
  if (num.isInfinite) return D.infinity;

  auto guarded = guard(context);
  int k;
  if (reduceArg)
  {
    k = ilogb(num) + 1;
    num.expo = num.expo - k;
  }
  D a = div(sub(num, 1, guarded), add(num, 1, guarded), guarded);
  D b = sqr(a, guarded);
  D c = a;
  long n = 3;
  while (true)
  {
    c = mul(c, b, guarded);
    D d = add(a, div(c, n, guarded), guarded);
    if (equals(a, d, guarded))
    {
      D ln = mul(a, 2, guarded);
      if (reduceArg)
      {
        ln = add(ln, mul(ln10!D(guarded), k, guarded), guarded);
      }
      return precisionRound(ln, context);
    }
    a = d;
    n += 2;
  }
}

unittest
{	// log
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{  "2.71828183",  9, "1.0" },
		{  "10.00",      12, "2.30258509299" },
		{  "10.00",       9, "2.30258509" },
		{ "123.45",       9, "4.81583622" },
		{  "99.999E+8",   9, "23.0258409" },
		{  "99.999E+8",   7, "23.0258409" },
		{  "99.999E+8",  15, "23.0258409298905" },
		{  "99.999E+8",   9, "23.0258409" },
	];
	auto f = FunctionTest!(S,TD)("log");
	foreach (t; s) f.test(t, log(t.x, t.p), t.p);
  writefln(f.report);
}


/*/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
package D exp(D)(D x, Context inContext) if (isDecimal!D)
{
  if (x.isInfinite)
  {
    return x.isNegative ? D.zero : x;
  }
  bool negative = x.isNegative;
  if (negative) x = x.copyAbs;
  auto context = guard(inContext);
  D sqrx = sqr(x, context);
  long n = 1;
  D fact = 1;
  D t1   = D.one;
  D t2   = x;
  D term = add(t1, t2, context);
  D sum  = term;
  while (term > D.epsilon(context))
  {
    n   += 2;
    t1   = mul(t2, mul(x, n, context), context);
    t2   = mul(t2, sqrx, context);
    fact = mul(fact, n*(n-1), context);
    term = div(add(t1, t2, context), fact, context);
    sum  = add(sum, term, context);
  }
  if (negative) sum = div(D.one, sum, context);
  return precisionRound(sum, inContext);
}

unittest
{  // exp
  static struct S { TD x; int p; TD expect; }
  S[] s =
  [
    {  1, 9, "2.71828183" },
    {  2, 9, "7.3890560989306502272" },
    { -2, 9, "0.13533528324" },
    {  1, 9, "2.71828183" },
    {  1, 11, "2.7182818285" },
    {  1, 15, "2.71828182845905" },
    {  2, 15, "7.3890560989306502272" },
    {  2, 11, "7.3890560989306502272" },
    { -2, 11, "0.13533528324" },
  ];
  auto f = FunctionTest!(S,TD)("exp");
  foreach (t; s) f.test(t, exp(t.x, t.p), t.p);
  writefln(f.report);
}
*/
/+
/**
 * Decimal version of std.math function.
 * 2^x
 */
public Decimal exp2T)( arg) {
	Decimal result;
	return result;
}

unittest {
	write("exp2...........");
	writeln("test missing");
}
+/

mixin (UnaryFunction!("expm1"));

/// expm1(x) will be more accurate than exp(x) - 1 for x << 1.
/// Decimal version of std.math function.
/// Reference: Beebe, Nelson H. F., "Computation of expm1(x) = exp(x) - 1".
public T expm1(T)(T x, Context inContext) if (isDecimal!T)
{
	// special values
	if (x.isZero) return x;
	if (x.isInfinite) return x.isNegative ? -T.one : T.infinity;

	auto context = guard(inContext);
	T sum = T.zero;

	// if too large return exp(x) - 1.
	const T lower = T("-0.7");
	const T upper = T("0.5");
	// ??? what is this --> if (x.copyAbs < lower || x.copyAbs > upper) {
	if (x < lower || x > upper) {
		sum = sub(exp(x, context), 1, context);
		return precisionRound(sum, inContext);
	}

	bool negative = x.isNegative;
	if (negative) x = x.copyAbs;
/*	// if too large return exp(x) - 1.
	if (x < lower || x > upper) {
		sum = sub(exp(x, context), 1, context);
		return precisionRound(sum, inContext);
	}*/

	// otherwise return expm1(x)
	T term = x;
	long n = 1;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n++;
		term = mul(term, div(x, n, context), context);
	}
	if (negative) sum = div(T.one, sum, context);
	return precisionRound(sum, inContext);
}

unittest
{	// expm1
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{  "0.1", 9, "0.105170918" },
	// FIXTHIS: incorrect result for negative numbers.
		{ "-0.4", 9, "-0.329679954" },
		{ "-2",   9, "-0.864664717" },
	];
	auto f = FunctionTest!(S,TD)("expm1");
	foreach (t; s) f.test(t, expm1(t.x, t.p), t.p);
  writefln(f.report);
}

//mixin (UnaryFunction!("log"));
mixin (UnaryFunction!("log1p"));
mixin (UnaryFunction!("log10"));
mixin (UnaryFunction!("log2"));

/+
	"/// Returns the value of the function at the specified precision.
	public T " ~ name ~ "(T)(T x, int precision = T.precision) if (isDecimal!T)
	{
		if (x.isNaN) {
			contextFlags.set(INVALID_OPERATION);
			return T.nan;
		}
		Context context = Context(precision, T.maxExpo, HALF_EVEN);
		return " ~ name ~ "!T(x, context);
	}
+/

// TODO: (behavior) add log(number, base) function (will have to have a different name -- logBase or something
/// Decimal version of std.math function.
/// Required by General Decimal Arithmetic Specification
// TODO: efficiency) see Natural Logarithm, Wikipedia.
/**
 * log1p (== log(1 + x)).
 * Decimal version of std.math function.
 */
public T log1p(T)(T x, Context inContext) if (isDecimal!T)
{
	// special cases
	if (x.isNaN || x < T.NEG_ONE) 	// use compare(x, T.NEG_ONE) == -1?
	{
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	// check for infinite argument
	if (x.isInfinite) return T.infinity;

	if (x.isZero) return T.zero(x.sign);

	if (equals(x, T.NEG_ONE, inContext)) return T.infinity(true);

	// TODO: There's probably a better breakeven point
	if (x.copyAbs >= T.one) return log(add(T.one, x, inContext), inContext);

	T term = x;
	T pwr  = x;
	T sum  = T.zero;
	T n    = T.one;
	auto context = guard(inContext);
	while (term.copyAbs >= T.epsilon(context))
	{
		sum = add(term, sum, context);
		pwr = mul(-pwr, x, context);// * x;
		n++;
		term = div(pwr, n, context);
	}
	sum = add(term, sum, context);
//if (!__ctfe) writeln;
//if (!__ctfe) writefln("sum = %s", abstractForm(sum));
//if (!__ctfe) writefln("sum.digits = %s", sum.digits);
//if (!__ctfe) writefln("inContext.precision = %s", inContext.precision);

	sum = precisionRound(sum, inContext);
//if (!__ctfe) writefln("sum = %s", abstractForm(sum));
//if (!__ctfe) writefln("sum.digits = %s", sum.digits);
	return sum;
//	return precisionRound(sum, inContext);
}

// TODO: (testing) unittest this.
unittest {
	write("-- log1p............");
	TD x = "0.1";
	assertEqual(log1p(x), "0.09531017980432486");
	writeln("passed");
}

/// Decimal version of std.math.log10.
/// Required by General Decimal Arithmetic Specification
public T log10(T)(T x, Context inContext) if (isDecimal!T)
{
	if (x.isZero) {
		contextFlags.set(DIVISION_BY_ZERO);
		return T.infinity;
	}
	if (x.isNegative) {
		return T.nan;
	}
	auto context = guard(inContext);
	int k = ilogb(x) + 1;
	x.expo = x.expo - k;
//	x.expo -= k;
	T lg10 = add(div(log(x, context), ln10!T(context)), k);
	return precisionRound(lg10, inContext);
}

unittest {
	write("-- log10............");
	TD x = TD("2.55");
	assertEqual(log10(x), TD("0.4065401804339552"));
	x = 123.456;
	assertEqual(log10(x), TD("2.091512201627772"));
	x = 10.0;
	assertEqual(log10(x), 1);
	writeln("passed");
}

/**
 * Decimal version of std.math.log2.
 * Required by General Decimal Arithmetic Specification
 */
public T log2(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T lg2 = div(log(x, context), ln2!T(context), context);
	return precisionRound(lg2, inContext);
}

unittest {
	write("-- log2.............");
	assertEqual(log2(TD(10)), TD("3.321928094887362"));
	assertEqual(log2(TD.E), TD("1.442695040888963"));
	writeln("passed");
}

/**
 * Decimal version of std.math.pow.
 * Required by General Decimal Arithmetic Specification
 */
 // TODO: (behavior) add context
public T pow(T)(T x, T y) if (isDecimal!T)
{
	return exp(x*log(y));
}

unittest {
	write("pow..........");
	writeln("test missing");
}

mixin (BinaryFunction!("hypot"));

/// Returns the square root of the sum of the squares in the specified context.
/// Decimal version of std.math.hypot.
public T hypot(T)(T x, T y, Context context) if (isDecimal!T)
{
	// special values
	if (x.isInfinite || y.isInfinite) return T.infinity();
    if (x.isZero) return y;
	if (y.isZero) return x;
	if (x.isNaN || y.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}

	T a = x.copyAbs;
    T b = y.copyAbs;
	if (a < b) {
		//swap operands
		T t = a;
		a = b;
		b = t;
	}
    b = div(b, a, context);
    return mul(a, sqrt(add(T.one, sqr(b, context), context), context));
}

// TODO: (testing) Need to test operation near precisions where this operation is really useful.
unittest
{	// hypot
	static struct S { TD x; TD y; TD expect; }
	S[] s =
	[
		{  "3", "4", "5" },
	];
	auto f = FunctionTest!(S,TD)("hypot");
	foreach (t; s) f.test(t, hypot(t.x, t.y));
  writefln(f.report);
}

//--------------------------------
// TRIGONOMETRIC FUNCTIONS
//--------------------------------

//mixin (UnaryFunction!("sin"));
//mixin (UnaryFunction!("cos"));


// Returns the reduced argument and the quadrant.
// Reduced argument |x| <= pi/4.
private T reduceAngle(T)(in T x,
	out int n, in Context inContext = T.context) if (isDecimal!T)
{
	auto context = guard(inContext);
	T twoInvPi = twoInvPi!T(context);
	T pi_2 = pi_2!T(context);
	T y = mul(x, twoInvPi, context);
	int k = rint(y).toInt;
	n = k % 4;
	T f = sub(y, k, context);
	T r = mul(f, pi_2, context);
	return r;
}

/// Decimal version of std.math function.
public T sin(T)(in T x, int precision = T.precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}

	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int k;
	T red = reduceAngle(x, k, context);
	switch (k) {
		case 0: return( sin( red, context));
		case 1: return( cos( red, context));
		case 2: return(-sin( red, context));
		case 3: return(-cos( red, context));
		default: return T.nan;
	}
//	return T.nan;
}

// Decimal version of std.math function.
// Precondition: x is in 1st quadrant.
package T sin(T)(in T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T sum = 0;
	int n = 1;
	T powx = x.copy;
	T sqrx = sqr(x, context);
	T fact = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n += 2;
		powx = mul(powx.copyNegate, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(powx, fact, context);
	}
	return precisionRound(sum, inContext);
}

unittest
{	// sin
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ "0.1",   9, "0.0998334166" },
		{ "0.1",   2, "0.10" },
		{ "0.333", 9, "0.326879693" },
		{ "5.0",   9, "-0.958924275" },
		{ "1.0",   9, "0.8414709848" },
		{ "1.0",  16, "0.8414709848078965" },
		{ "2.0",  16, "0.9092974268256817" },
	];
	auto f = FunctionTest!(S,TD)("sin");
	foreach (t; s) f.test(t, sin(t.x, t.p), t.p);
  writefln(f.report);
}

unittest {
	write("-- sin..............");
//	TD difficult = TD(5678900000);
	TD difficult = TD(5);
writefln("difficult = %s", difficult);
// FIXTHIS: throws div by zero exception...
writefln("sin(difficult) = %s", sin(difficult));
//	assertEqual(sin(difficult), TD(0));
	// TODO: (testing) one value from each quadrant, reduced value.
	// TODO: (behavior) this is a notoriously difficult value "sin(10^^22)"
	writeln("passed");
}

/// Decimal version of std.math function.
public T cos(T)(in T x, int precision = T.precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int quadrant;
	T red = reduceAngle(x, quadrant, context);
	switch (quadrant) {
		case 0: return( cos(red, context));
		case 1: return(-sin(red, context));
		case 2: return(-cos(red, context));
		case 3: return( sin(red, context));
		default: return T.nan;
	}
}

/// Decimal version of std.math function.
/// Precondition: x is in 1st quadrant.
package T cos(T)(in T x, Context inContext) {
	auto context = guard(inContext);
	T sum = 0;
	int n = 0;
	T powx = 1;
	T sqrx = sqr(x, context);
	T fact = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		n += 2;
		powx = mul(powx.copyNegate, sqrx, context);
		fact = mul(fact, n*(n-1), context);
		term = div(powx, fact, context);
	}
	return precisionRound(sum, inContext);
}

unittest
{	// cos
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ "1.0",   9, "0.5403023058681397174009" },
		{ "0.333", 9, "0.945065959" },
		{ "5.0",   9, "0.283662185" },
		{ "1.0",  23, "0.540302306" },
		{ "2.0",  16, "-0.416146837" },
	];
	auto f = FunctionTest!(S,TD)("cos");
	foreach (t; s) f.test(t, cos(t.x, t.p), t.p);
  writefln(f.report);
}

public void sincos(T)(T x, out T sine, out T cosine, int precision = T.precision) {
	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int quadrant;
	T red = reduceAngle(x, quadrant, context);
	sincos(red, sine, cosine, context);
/*	switch (quadrant) {
//sin:
		case 0: break;
		case 1:
			sine = cosine;
			cosine = -sine;
			break;
		case 2:
			sine = -sine;
			cosine = -cosine;
			break;
		case 3:
			sine = -cosine;
			cosine = sine;
			break;
		default:
			sine = T.nan;
			cosine = T.nan;
	}*/
}
/**
 * Replaces std.math function expi
 *
 */
// TODO: (behavior) context, angle reduction
public void sincos(T)(const T x, out T sine, out T cosine,
		Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T csum, cterm, cx;
	T ssum, sterm, sx;
	T sqrx = sqr(x, context);
	long n = 2;
	T fact = 1;
	cx = 1;	cterm = cx;	csum = cterm;
	sx = x;	sterm = sx;	ssum = sterm;
	while (sterm.copyAbs > T.epsilon) {
		cx = mul(cx.copyNegate, sqrx, context);
		fact = mul(fact, n++, context);
		cterm = div(cx, fact, context);
		csum = add(csum, cterm, context);
		sx = mul(sx.copyNegate, sqrx, context);
		fact = mul(fact, n++, context);
		sterm = div(sx, fact, context);
		ssum = add(ssum, sterm, context);
	}
    sine   = precisionRound(ssum, inContext);
	cosine = precisionRound(csum, inContext);
}

unittest {
	write("sincos.......");
	TD sine;
	TD cosine;
	sincos(TD("1.0"), sine, cosine);
if (!__ctfe) writefln("sine = %s", sine);
if (!__ctfe) writefln("cosine = %s", cosine);
	writeln("..failed");
}

/**
 * Decimal version of std.math function.
 *
 */
// TODO: (efficiency) compare divTan with tan.
public T divTan(T)(T x) if (isDecimal!T)
{
	T sine;
	T cosine;
	sincos(x, sine, cosine);
	if (sine == T.zero) return T.infinity;
	return sine/cosine;
}

public T tan(T)(T x, int precision = T.precision) if (isDecimal!T)
{
	if (x.isNaN) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	auto context = Context(precision, T.maxExpo, HALF_EVEN);
	int quadrant;
	T red = reduceAngle(x, quadrant, context);
	T sine;
	T cosine;
	sincos(red, sine, cosine, context);
	switch (quadrant) {
		case 0: return( sine/cosine);
		case 1: return(-cosine/sine);
		case 2: return( sine/cosine);
		case 3: return(-sine/cosine);
		default: return T.nan;
	}
}

unittest
{	// tan
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ "1.0",   9, "1.55740772465490223" },
		{ "1.0",   14, "1.55740772465490223" },
		{ "0.333", 9, "0.345880295" },
	];
	auto f = FunctionTest!(S,TD)("tan");
	foreach (t; s) f.test(t, tan(t.x, t.p), t.p);
  writefln(f.report);
}

mixin (UnaryFunction!("atan"));
mixin (UnaryFunction!("asin"));
mixin (UnaryFunction!("acos"));

/// Returns the arctangent of the argument in the specified context.
/// Algorithm uses Taylor's theorem for arctangent.
public T atan(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext, 3);
	long k = 1;
	// reduce the input angle
	while (x > T(0.5)) {
		T a = sqr(x, context);
		T b = add(a, 1, context);
		T c = sqrt(b, context);
		T d = add(c, 1, context);
		x = div(x, d, context);
		k *= 2;
	}
	T sum = 0;
	T powx = x;
	T sqrx = sqr(x, context);
	long dvsr = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum = add(sum, term, context);
		powx = mul(powx.copyNegate, sqrx, context);
		dvsr = dvsr + 2;
		term = div!T(powx, dvsr, context);
	}
	return precisionRound(mul(sum, k, context), inContext);
}


unittest
{	// atan
	static struct S { TD x; TD expect; }
	S[] s =
	[
//		{ "1.0",   "0.785398163397" },
//		{ "0.5",   "0.463647609" },
//		{ "0.333", "0.32145052439664" },
//		{ "0.1", "0.099668652491162038065120043484057532623410224914551" },
//		{ "0.9", "0.73281510178650655085164089541649445891380310058594" },
	];
	auto f = FunctionTest!(S,TD)("atan");
	foreach (t; s) f.test(t, atan(t.x));
  writefln(f.report);
}

/// Decimal version of std.math function.
// TODO: (behavior) convert to std unary function.
public T asin(T)(T x) if (isDecimal!T)
{
	T result = 2 * atan!T(x/(1+sqrt(1-sqr(x)))); //^^2)));
	return result;
}

unittest
{	// asin
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.0",   "1.570796326794897" },
		{ "0.5",   "0.5235987755982990" },
		{ "0.333", "0.3394833781504900" },
	];
	auto f = FunctionTest!(S,TD)("asin");
	foreach (t; s) f.test(t, asin!TD(t.x));
  writefln(f.report);
}

/// Decimal version of std.math function.
// TODO: (behavior) convert to std unary function.
public T acos(T)(T x) if (isDecimal!T)
{
	T result = 2 * atan!T(sqrt(1-sqr(x))/(1 + x));
	return result;
}

unittest
{	// acos
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.0",   "0" },
//		{ "0.5",   "1.04719755120" },
//		{ "0.333", "1.23131294864" },
	];
	auto f = FunctionTest!(S,TD)("acos");
	foreach (t; s) f.test(t, acos(t.x));
  writefln(f.report);
}

/// Decimal version of std.math function.
public TD atan2(T)(T y, TD x) if (isDecimal!T)
{
	TD result;
	return result;
}

unittest {
	write("atan2........");
	writeln("..failed");
}

//--------------------------------
// HYPERBOLIC TRIGONOMETRIC FUNCTIONS
//--------------------------------

mixin (UnaryFunction!("sinh"));
mixin (UnaryFunction!("cosh"));
mixin (UnaryFunction!("tanh"));
//mixin (UnaryFunction!("atanh"));

/// Decimal version of std.math function.
public T sinh(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	long n = 1;
	T sum = 0;
	T powx = x;
	T sqrx = sqr(x, context);
	T fact = n;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum  = add(sum, term, context);
		n += 2;
		fact = mul(fact, n*(n-1), context);
		powx = mul(powx, sqrx, context);
		term = div(powx, fact, context);
	}
	return precisionRound(sum, inContext);
}


unittest
{	// sinh
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.0",   "1.175201193643801" },
		{ "0.5",   "0.5210953054937474" },
		{ "0.333", "0.3391885521570523" },
	];
	auto f = FunctionTest!(S,TD)("sinh");
	foreach (t; s) f.test(t, sinh(t.x));
  writefln(f.report);
}

/// Decimal version of std.math function.
public T cosh(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	long n = 0;
	T sum = 0;
	T powx = 1;
	T sqrx = sqr(x, context);
	T fact = 1;
	T term = powx;
	while (term.copyAbs > T.epsilon(context)) {
		sum  = add(sum, term, context);
		n += 2;
		fact = mul(fact, n*(n-1), context);
		powx = mul(powx, sqrx, context);
		term = div(powx, fact, context);
	}
	return precisionRound(sum, inContext);
}

unittest
{	// cosh
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.0",   "1.543080634815244" },
		{ "0.5",   "1.127625965206381" },
		{ "0.333", "1.055958746312751" },
	];
	auto f = FunctionTest!(S,TD)("cosh");
	foreach (t; s) f.test(t, cosh(t.x));
  writefln(f.report);
}

/// Decimal version of std.math function.
public T tanh(T)(T x, Context inContext) if (isDecimal!T)
{
	auto context = guard(inContext);
	T tan = div(sinh(x, context), cosh(x, context), context);
	return precisionRound(tan, inContext);
}

unittest
{	// tanh
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.0",   "0.7615941559557649" },
		{ "0.5",   "0.4621171572600098" },
		{ "0.333", "0.3212138289885354" },
	];
	auto f = FunctionTest!(S,TD)("tanh");
	foreach (t; s) f.test(t, tanh(t.x));
  writefln(f.report);
}

mixin (UnaryFunction!("asinh"));
mixin (UnaryFunction!("acosh"));
mixin (UnaryFunction!("atanh"));

/// Decimal version of std.math function.
public T asinh(T)(T x, Context inContext) if (isDecimal!T)
{
	if (x.isZero) return T.zero;	// TODO: (behavior) special values
	auto context = guard(inContext);
	// TODO: fix the cutoff -- is the series expansion needed?
	if (x.copyAbs >= "1.0E-6")
	{
		T arg = add(x, sqrt(add(sqr(x, context), T.one, context), context), context);
		return precisionRound(log(arg, context), inContext);
	}
	else
	{
		T sum = 0;
		T powx = x;
		T sqrx = sqr(x, context);
		T n = 1;
		T term = x;
		T scl = 1;
		int count = 0;
		while (term.copyAbs > T.epsilon(context)) {
			sum = add(sum, term, context);
			powx = mul(-powx, sqrx, context);
			n += 2;
			scl *= (n/(n+1));
			term = mul(T(scl), div(powx, T(n), context), context);
		}
		return precisionRound(sum, inContext);
	}
}

unittest
{	// asinh
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		// TODO: add tests for small and large arguments
		{ "1.0",   9, "0.881373587020" },
		{ "0.5",   9,  "0.481211825060" },
		{ "0.333", 9,  "0.327133906664" },
		{ TD.PI,   9,  "1.86229574331" },
		{ "1.1E-3", 9,    "1.1E-3" },
		// FIXTHIS: These tests fail with precision problems.
//		{ "1.0E-3", 9,    "0.000999999998" },
//		{ "0.9E-3", 9,    "0.000899999878500" },
//		{ "1.0E-5", 9,    "1.0E-5" },
//		{ "1.0E-6", 9,    "1.0E-6" },
//		{ "1.0E-7", 9,    "1.0E-7" },
		{ "1.234567E-7",  9,   "1.234567E-7" },
		{ "0.0",   9,  "0.0" },
	];
	auto f = FunctionTest!(S,TD)("asinh");
	foreach (t; s) f.test(t, asinh(t.x, t.p), t.p);
  writefln(f.report);

}

/// Decimal version of std.math function.
public T acosh(T)(T x, Context inContext) if (isDecimal!T)
{
	if (x.copyAbs < T.one) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	if (x.copyAbs.isOne) {
		return T.zero;
	}
	auto context = guard(inContext);
	// TODO: this is imprecise at small arguments -- Taylor series?
	T arg = add(x, sqrt(sub(sqr(x, context), T.one, context), context), context);
	return precisionRound(log(arg, context), inContext);
}

/*unittest
{	// acosh
	static struct S { TD x; TD expect; }
	S[] s =
	[
		{ "1.0",   "0.0" },
		{ "2.0",   "1.31695790" },
		// FIXTHIS: These tests fail with precision problems.
//		{ "1.0000001",   "0.000447213592" },
//		{ "0.0",   "NaN" },
//		{ "1.5",   "0.962423650119" },
//		{ "1.333", "0.794987388708" },
	];
	auto f = FunctionTest!(S,TD)("acosh");
	foreach (t; s) f.test(t, acosh(t.x));
  writefln(f.report);
}*/

/// Decimal version of std.math function.
public T atanh(T)(T x, Context inContext) if (isDecimal!T)
{
	T abs = x.copyAbs;
	if (abs > T.one) {
		contextFlags.set(INVALID_OPERATION);
		return T.nan;
	}
	if (abs.isOne) {
		return T.infinity(x.sign);
	}

	auto context = guard(inContext, 3);
	T cutoff = "1.0E-6"; // cutoff point for switch to series expansion
	if(abs > cutoff)
	{	// atanh(x) = 1/2 * log((1+x)/(1-x))
		T n = add(T.one, x, context);
		T d = sub(T.one, x, context);
		T q = div(n, d, context);
		T l = log(q, context);
		T a = mul(T.HALF, l, context);
		return precisionRound(a, inContext);
	}
	else
	{	// series expansion
		T sum = 0;
		T powx = x;
		T sqrx = sqr(x, context);
		long dvsr = 1;
		T term = powx;
		int count = 0;
		while (term.copyAbs > T.epsilon(context)) {
			sum = add(sum, term, context);
			powx = mul(powx, sqrx, context);
			dvsr += 2;
			term = div!T(powx, dvsr, context);
		}
		return precisionRound(sum, inContext);
	}
}

unittest
{	// atanh
	static struct S { TD x; int p; TD expect; }
	S[] s =
	[
		{ " 1.0",  9, "Infinity" },
		{ "-1.0",  9, "-Infinity" },
		{ " 1.01", 9, "NaN" },
		{ "-1.01", 9, "NaN" },
		{ "0.9",   9, "1.47221949" },
		{ "0.99999",   9, "6.10303382276" },
		{ "1.0E-7",   9, "1.0E-7" },
		{ "0.5",   12, "0.549306144334" },
		{ "0.333", 11, "0.346198637132" },
		{ "0.333", 12, "0.346198637132" },
	];
	auto f = FunctionTest!(S,TD)("atanh");
	foreach (t; s) f.test(t, atanh!TD(t.x, t.p), t.p);
  writefln(f.report);
}

unittest {
	writeln("==========================");
	writeln("decimal math...........end");
	writeln("==========================");
}
