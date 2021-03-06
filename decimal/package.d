// Written in the D programming language

/**
 * Floating-point decimal number and decimal arithmetic library for D.
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
 *  General Decimal Arithmetic Specification,
 *  Version 1.70, (25 March 2009).
 */


module eris.decimal;

import std.array : replace;
import std.bitmanip : DoubleRep;
import std.conv;
import std.string;
import std.traits;
import std.math;
static import std.path;

public import std.bigint;
public import eris.decimal.context;
public import eris.decimal.rounding;
public import eris.decimal.arithmetic;
public import eris.decimal.conv;
static import eris.decimal.math;

import eris.decimal.logical;

// import std.stdio;  // temporary import for testing

version(unittest)
{
  import std.stdio;
  import eris.decimal.test;
}

version(unittest)
{
  alias D9 = Decimal!(TestContext);
  alias D16 = Decimal!(Bid64);
  alias TD = D16;
}

/**
 * A floating-point decimal number.
 *
 * The number consists of:
 * A boolean sign
 * A integer-type coefficient,
 * An integer exponent
 * A tag for special values (NaN, infinity)
 */
struct Decimal(immutable Context _context = DefaultContext)
{

static if (context == Bid64) {
  unittest {
    writeln("==========================");
    writeln("decimal64............begin");
    writeln("==========================");
  }
}

  public static enum IsDecimal;

  public enum Context context = _context;
  alias decimal = Decimal!(context);
  alias D = Decimal!(context);

  private Tag m_tag     = Tag.QNAN;// special value: default is quiet NaN
  private bool m_sign   = 0;    // true if the value is negative, false otherwise.
  private int m_expo    = 0;    // the exponent of the decimal value
  private BigInt m_coff = 0;    // the coefficient of the decimal value
  private int m_digits  = 0;      // the number of decimal digits in the coefficient.

  // special values for NaN, Inf, etc.
  private enum Tag : byte { NONE=0, INF=1, MINF=-1, QNAN=2, SNAN=3 };

  // decimal special values
  private enum NAN  = decimal(Tag.QNAN);
  private enum SNAN = decimal(Tag.SNAN);
  private enum INF  = decimal(Tag.INF);
  private enum MINF = decimal(Tag.MINF);

  // context-based parameters
  /// the maximum length of the coefficient in decimal .
  public enum precision = context.precision;
  /// maximum value of the exponent.
  public enum maxExpo = context.maxExpo;
  /// maximum value of the adjusted exponent.
  public enum maxAdjustedExpo = maxExpo - (precision - 1);
  /// smallest normalized exponent.
  public enum minExpo = 1 - maxExpo;
  /// smallest non-normalized exponent.
  public enum tinyExpo = 1 - maxExpo - precision;
  /// maximum value of the coefficient.
  public enum maxCoefficient = BigInt(10)^^precision - 1;
  /// rounding mode.
  public enum Round mode = context.mode;

/*  unittest
  {  // special values
    static struct S { TD x; string expect; }
    S[] s =
    [
      { NAN,      "NaN" },
      { SNAN,  "sNaN" },
      { INF, "Infinity" },
      { MINF,  "-Infinity" },
    ];
    auto f = FunctionTest!(S,string)("specials");
    foreach (t; s) f.test(t, t.x.toString);
      writefln(f.report);
  }*/

//--------------------------------
// construction
//--------------------------------

  /// Constructs a decimal number from a tag and an optional sign.
  @safe
  private this(Tag tag)
  {
    this.m_sign = tag < 0;
    this.m_tag  = tag;
  }

  /// Constructs a decimal number from a boolean value.
  /// false == 0, true == 1
  this(bool value)
  {
    if (value) {
      this(BigInt(1));
    }
    else
    {
      this(BigInt(0));
    }
  }

  // Constructs a decimal number from a string representation
  this(string str)
  {
    this = fromString!decimal(str);
  }

  this(U)(U r)
    if (isFloatingPoint!U)
  {
    this = fromBinary!(decimal,U)(r);
  }

  this(U)(U coefficient)
    if (isIntegral!U)
  {
    this(BigInt(coefficient));
  }

  this(U)(U coefficient)
    if (is(U == BigInt))
  {
    this(coefficient, 0);
  }

  this(D)(D that) if (isDecimal!D)
  {
    this.m_sign = that.m_sign;
    this.m_tag   = that.m_tag ;
    this.m_digits = that.m_digits;
    this.m_expo  = that.m_expo;
    this.m_coff  = that.m_coff;
    if (that.context > this.context)
      round(this, that.context);
  }

  // TODO: reduce the number of constructors
  /// Constructs a number from a boolean sign, an integer coefficient and
  /// an optional integer exponent.
  /// The sign of the number is the value of the sign parameter
  /// regardless of the sign of the coefficient.
  /// The intial precision of the number is deduced from the number
  /// of decimal digits in the coefficient.
  //@safe
  this(U)(U coefficient, int exponent)
    if (is(U == BigInt) || isIntegral!U)
  {
    static if (isIntegral!U)
    {
      this(BigInt(coefficient), exponent);
    }
    else
    {
      this = zero();
      this.m_sign = coefficient < 0;
      this.m_coff = sign ? -coefficient : coefficient;
      this.m_expo = exponent;
      this.m_digits = countDigits(this.m_coff);
    }
  }

  this(U)(U coefficient, int exponent, bool sign)
    if (is(U == BigInt) || isIntegral!U)
  {
    static if (isIntegral!U)
    {
      this(BigInt(coefficient), exponent, sign);
    }
    else
    {
    this = zero();
    this.m_sign = sign;
    this.m_coff = coefficient >= 0 ? coefficient : -coefficient;
    this.m_expo = exponent;
    this.m_digits = countDigits(this.m_coff);
    }
  }

  // TODO: reduce the number of constructors
  /// Constructs a number from a boolean sign, an integer coefficient and
  /// an optional integer exponent.
  /// The sign of the number is the value of the sign parameter
  /// regardless of the sign of the coefficient.
  /// The intial precision of the number is deduced from the number
  /// of decimal digits in the coefficient.
  //@safe
/*  this(long coefficient, int exponent, bool sign)
  {
    this(BigInt(coefficient), exponent, sign);
  }*/

static if (context == Bid64)
{
  unittest
  {  // this(u,i,b)
    static struct S { BigInt cf; int exp; bool sign; TD expect; }
    S[] s =
    [
      { 7254, 94, true, "-7.254E+97" },
      // NOTE: new constructions aren't rounded and may be too large for type
      { 1,   194, true, "-1E+194" },
    ];
    auto f = FunctionTest!(S,TD)("this(uib)");
    foreach (t; s) f.test(t, TD(t.cf, t.exp, t.sign));
      writefln(f.report);
  }

}

static if (context == Bid64)
{
  unittest
  {  // this(string)
    // NOTE: this is a chicken and egg sort of test:
    // Tests to and from strings at once
    static struct S { string str; string expect; }
    S[] s =
    [
      { "7254E94",    "7.254E+97" },
      { "7254.005",    "7254.005" },
      { "-2.3456E+14",  "-2.3456E+14" },
      { "-0.1234",    "-0.1234" },
      { "234568901234",  "234568901234" },
      { "123.457E+29",  "1.23457E+31" },
      { "2.71828183",    "2.71828183" },
      { "2147483646",    "2147483646" },
      { "2147483648",    "2147483648" },
      { "-2147483647",  "-2147483647" },
      { "-2147483649",  "-2147483649" },
      { "inf",      "Infinity" },
    ];
    auto f = FunctionTest!(S, string)("this(str)");
    foreach (t; s) f.test(t, TD(t.str).toString);
      writefln(f.report);
  }
}


static if (context == Bid64)
{
  unittest
  {  // this(double)
    double third = 1.0/3.0;
    static struct S { double dbl; TD expect; }
    S[] s =
    [
      { double.infinity,    "Infinity" },
      { 1.0E4,    "1E4" },
      { 1.0/3.0,     "0.333333333333333" },
    ];
    auto f = FunctionTest!(S, TD)("this(dbl)");
    foreach (t; s) f.test(t, TD(t.dbl));
      writefln(f.report);
  }
}

static if (context == Bid64)
{
/*  unittest
  {  // this(flt)
    assert(TD(-float.infinity) == TD("-infinity"));
    assert(TD(1.0E4F) == TD("1E4"));
    assert(TD(1.0f/3.0f) == TD("0.333333"));
  }*/

  unittest
  {  // this(flt)
    static struct S { float flt; decimal expect; }
    S[] s =
    [
      { -float.infinity,  "-Infinity" },
      { 1.0E4f,      "1E4" },
      { 1.0f/3.0f,     "0.333333" },
    ];
    auto f = FunctionTest!(S, decimal)("this(flt)");
    foreach (t; s) f.test(t, decimal(t.flt));
      writefln(f.report);
  }
}

  // TODO: (testing) need to test this with 15-17 digit precision
static if (context == Bid64)
{
  unittest // real construction
  {
    static struct S { real x; TD expect; }

    static S[] s =
    [
      { 0.1L,        "0.1" },
      { 7254E94,      "7.254E+97" },
      { 7254.005,      "7254.005" },
      { -2.3456E+14,    "-2.3456E+14" },
      { -0.1234,      "-0.1234" },

      { 234568901234.0,  "234568901234" },
      { 123.457E+29,    "1.23457E+31" },
      { 2.71828183,    "2.71828183" },
      { 2.718281832,    "2.718281832" },
      { 2147483646.0,    "2147483646" },

      { 2147483648.0,    "2147483648" },
      { -2147483647.0,  "-2147483647" },
      { -2147483649.0,  "-2147483649" },
      { 0.0,        "0" },
      { -0.0,        "-0" },

      { 1E54,        "1E54" },

      { double.max,     "1.79769313486231571E+308" },
      { -real.infinity,   "-Infinity" },
    ];

      auto f = FunctionTest!(S, TD)("this(real)");
      foreach (t; s) f.test(t, TD(t.x));
        writefln(f.report);
  }
}

//--------------------------------
// member properties
//--------------------------------

  /// Returns the exponent of this number
  @property
  @safe
  int expo() const
  {
    if (isSpecial) return 0;
    return this.m_expo;
  }


  // NOTE: (language) What does it take to make this an l-value?
  /// sets the exponent of this number
  @property
  @safe
  int expo(int exp)
  {
    this.m_expo = exp;
    return this.m_expo;
  }

  @property
  @safe
  BigInt coff() const
  {
//    if (isSpecial) return BigInt(0);
    return this.m_coff;
  }

  @property
  @safe
  BigInt coff(BigInt m_coff)
  {
    this.m_coff = m_coff;
    return this.m_coff;
  }

  @property
  @safe
  BigInt coff(long m_coff)
  {
    this.m_coff = m_coff;
    return this.m_coff;
  }

/*  @property
  @safe
  ushort payload() const
  {
    if (this.isNaN) {
      return cast(ushort)(this.m_coff.toLong);
    }
    return 0;
  }

  @property
  @safe
  ushort payload(const ushort value)
  {
    if (this.isNaN) {
      this.m_coff = BigInt(value);
      return value;
    }
    return 0;
  }*/

  /// Returns the adjusted exponent of this number
  @property
  @safe
  int adjExpo() const
  {
    if (isSpecial) return 0;
    return m_expo + m_digits - 1;
  }

  /// Returns the number of decimal digits in the coefficient of this number
  @property
  @safe
  int digits() const
  {
    if (isSpecial) return 0;
    return this.m_digits;
  }

  @property
  @safe
  int digits(in int count)
  {
    if (isSpecial) return 0;
    return this.m_digits = count;
  }

  @property
//  @safe
  bool sign() const
  {
    return cast(bool) m_sign & 1;
  }

  @property
//  @safe
  bool sign(const bool value)
  {
    m_sign = value; // | 1 -- may incorporate nan,inf flags into sign byte
    return m_sign & 1;
  }

//--------------------------------
// floating point properties
//--------------------------------

  /// Returns the default value for this type (NaN)
  @safe
  static D init()
  {
    return NAN;
  }

  /// Returns NaN
  @safe
  static D nan(ushort payload = 0, bool sign = false)
  {
    D dec = NAN;
    dec.m_coff = payload;
    dec.m_sign = sign;
    return dec;
  }

  /// Returns signaling NaN
  @safe
  static D snan(ushort payload = 0, bool sign = false)
  {
    D dec = SNAN;
    dec.m_coff = payload;
    dec.m_sign = sign;
    return dec;
  }

  /// Returns infinity.
  @safe
  static D infinity(bool sign = false)
  {
    return sign ? MINF : INF;
  }

  /// Returns the maximum representable normal value in the current context.
  static D max()
  {
    static initialized = false;
    static D maxVal;
    if (!initialized)
    {
      maxVal = D(maxCoefficient, maxAdjustedExpo);
      initialized = true;
      return maxVal;
    }
    else
    {
      return maxVal;
    }
  }

  /// Returns the minimum representable normal value in this context.
  @safe
  enum D min_normal = D(1L, minExpo);

  /// Returns the minimum representable subnormal value in this context.
  @safe
  enum D min = D(1, tinyExpo);

  /// Returns the smallest available increment to 1.0 in this context
  static enum D epsilon(in Context inContext = context) {
    return D(1, -inContext.precision);}

  /// Returns the radix, which is always ten for D numbers.
  @safe
  enum int radix = 10;

  // TODO: need to determine what constants it makes sense to include
  // common D numbers
  enum D ZERO    = D(Tag.NONE);
  enum D NEG_ZRO = -ZERO; // D(Tag.NONE, true);
  enum D HALF    = D(5, -1);
  enum D ONE     = D(1);
  enum D NEG_ONE = D(-1);
  enum D TWO     = D(2);
//  enum D THREE   = D(3);
  enum D FIVE    = D(5);
  enum D TEN     = D(10);

  /// Returns zero.
  @safe
  static enum D zero(bool sign = false)
  {
    return sign ? NEG_ZRO : ZERO;
  }

  /// Returns 1.
  //@safe
  static D one(bool sign = false) {
    return sign ? NEG_ONE : ONE;
  }

static if (context == Bid64)
{
  unittest
  {  // constants
    static struct S { TD x; string expect; }
    S[] s =
    [
      { TD.HALF,    "0.5" },
      { TD.ONE,    "1"   },
    ];
    auto f = FunctionTest!(S, string)("constants");
    foreach (t; s) f.test(t, t.x.toString);
      writefln(f.report);
  }
}

  // copy constructor
  @safe
  public this(const decimal that)
  {
    this.m_sign = that.m_sign;
    this.m_tag   = that.m_tag ;
    this.m_digits = that.m_digits;
    this.m_expo  = that.m_expo;
    this.m_coff  = that.m_coff;
  };

//--------------------------------
// copy functions
//--------------------------------

  /// dup property
  //@safe
  public decimal dup() const
  {
    return decimal(this);
  }

  // TODO: modify this to use a compareTotal?
  unittest
  {  // copy
    static struct S { TD x; TD expect; }
    S[] s =
    [
      { 1.0, 1.0 },
      { 2.0, 2.0 },
      { 1.0E5, 1.0E5 },
      { 0.1, 0.1 },
      {123.456, 123.456},  // passes! fails because r == r only when exact
      { 32E-27, 32E-27 },
      { double.max, double.max },
      { real.max, real.max },
    ];
    auto f = FunctionTest!(S,TD)("copy");
    foreach (t; s) f.test(t, t.x.copy);
    writefln(f.report);
  }

/*  unittest {  // dup
    write("-- dup..............");
    TD num, copy;
    // TODO: add tests for these values
    num = std.math.LB;
    num = std.math.PI;
    num = std.math.LB;
    copy = TD(num);
//    assertCopy!TD(num, copy);
    assertZero(compareTotal(num, copy));
    num = TD(std.math.PI);
    copy = num.dup;
    assertEqual(num, copy);
    writeln("passed");
  }*/

  /// Returns a copy of the operand.
  /// The copy is unaffected by context and is quiet -- no flags are changed.
  /// Implements the 'copy' function in the specification. (p. 43)
  //@safe
  public decimal copy() const
  {
    return dup;
  }

  /// Returns a copy of the operand with a positive sign.
  /// The copy is unaffected by context and is quiet -- no flags are changed.
  /// Implements the 'copy-abs' function in the specification. (p. 44)
  //@safe
  public decimal copyAbs() const
  {
    decimal copy = dup;
    copy.sign = false;
    return copy;
  }

  /// Returns a copy of the operand with the sign inverted.
  /// The copy is unaffected by context and is quiet -- no flags are changed.
  /// Implements the 'copy-negate' function in the specification. (p. 44)
  //@safe
  public decimal copyNegate() const
  {
    decimal copy = dup;
    copy.sign = !sign;
    return copy;
  }

  /// Returns a copy of the first operand with the sign of the second operand.
  /// The copy is unaffected by context and is quiet -- no flags are changed.
  /// Implements the 'copy-sign' function in the specification. (p. 44)
  //@safe
  public decimal copySign()(in decimal arg) const
  {
    decimal copy = dup;
    copy.sign = arg.sign;
    return copy;
  }

  unittest {  // copy
    write("-- copy.............");
    TD arg, expect;
    arg = TD("2.1");
    expect = TD("2.1");
    assertZero(compareTotal(arg.copy,expect));
    arg = TD("-1.00");
    expect = TD("-1.00");
    assertZero(compareTotal(arg.copy,expect));
    // copyAbs
    arg = 2.1;
    expect = 2.1;
    assertZero(compareTotal(arg.copyAbs,expect));
    arg = TD("-1.00");
    expect = TD("1.00");
    assertZero(compareTotal(arg.copyAbs,expect));
    // copyNegate
    arg  = TD("101.5");
    expect = TD("-101.5");
    assertZero(compareTotal(arg.copyNegate,expect));
    // copySign
    TD arg1, arg2;
    arg1 = 1.50; arg2 = 7.33; expect = 1.50;
    assertZero(compareTotal(arg1.copySign(arg2),expect));
    arg2 = -7.33;
    expect = -1.50;
    assertZero(compareTotal(arg1.copySign(arg2),expect));
    writeln("passed");
  }

//--------------------------------
//  classification properties
//--------------------------------

  /// Returns true if this number's representation is canonical.
  ///
  /// Always returns true. All decimal numbers are canonical,
  /// whether or not they are reduced to their simplest form.
  @safe
  const bool isCanonical()
  {
    return true;
  }

static if (context == Bid64)
{
  unittest
  {  // isCanonical
    static struct S { string str; bool expect; }
    S[] s =
    [
      { "7254E94",    true },
      { "inf",      true },
    ];
    auto f = FunctionTest!(S, bool)("isCanonical");
    foreach (t; s) f.test(t, TD(t.str).isCanonical);
      writefln(f.report);
  }
}

  /// Returns true if this number is exactly one.
  //@safe
  const bool isOne()
  {
    if (isNegative || isZero || isSpecial) {
      return false;
    }
    if (coff == 1 && expo == 0) {
      return true;
    }
    return this.reduce.isSimpleOne;
  }

  /// Returns true if this number is exactly (false, 1, 0).
  @safe
  const bool isSimpleOne()
  {
    return isFinite && !isNegative && coff == 1 && expo == 0;
  }

static if (context == Bid64)
{
  unittest
  {  // isNaN, isQuiet, isSignal
    static struct S { string str; bool expect; }

    S[] s1 =
    [
      { "1",    true },
      { "10E-1",  true },
      { "sNaN",  false },
    ];
    auto f1 = FunctionTest!(S, bool)("isOne");
    foreach (t; s1) f1.test(t, TD(t.str).isOne);
      writefln(f1.report);

    S[] s2 =
    [
      { "1",    true },
      { "10E-1",  false },
      { "sNaN",  false },
    ];
    auto f2 = FunctionTest!(S, bool)("isSimpleOne");
    foreach (t; s2) f2.test(t, TD(t.str).isSimpleOne);
      writefln(f2.report);
  }
}

  /// Returns true if this number is + or - zero.
  @safe
  const bool isZero()
  {
    return isFinite && coff == 0;
  }

static if (context == Bid64)
{
  unittest
  {  // isZero
    static struct S { string str; bool expect; }
    S[] s =
    [
      { "0",    true },
      { "2.50",  false },
      { "-0E2",  true },
    ];
    auto f = FunctionTest!(S, bool)("isZero");
    foreach (t; s) f.test(t, TD(t.str).isZero);
      writefln(f.report);
  }
}


  /// Returns true if this number is a quiet or signaling NaN.
  @safe
  const bool isNaN()
  {
    return this.m_tag  == Tag.QNAN
        || this.m_tag  == Tag.SNAN;
  }

  /// Returns true if this number is a signaling NaN.
  @safe
  const bool isSignal()
  {
    return this.m_tag  == Tag.SNAN;
  }

  /// Returns true if this number is a quiet NaN.
  @safe
  const bool isQuiet()
  {
    return this.m_tag  == Tag.QNAN;
  }

static if (context == Bid64)
{
  unittest
  {  // isNaN, isQuiet, isSignal
    static struct S { string str; bool expect; }

    S[] s1 =
    [
      { "2.50",  false },
      { "NaN",  true },
      { "sNaN",  true },
    ];
    auto f1 = FunctionTest!(S, bool)("isNaN");
    foreach (t; s1) f1.test(t, TD(t.str).isNaN);
      writefln(f1.report);

    S[] s2 =
    [
      { "2.50",  false },
      { "NaN",  true },
      { "sNaN",  false },
    ];
    auto f2 = FunctionTest!(S, bool)("isQuiet");
    foreach (t; s2) f2.test(t, TD(t.str).isQuiet);
      writefln(f2.report);

    S[] s3 =
    [
      { "2.50",  false },
      { "NaN",  false },
      { "sNaN",  true },
    ];
    auto f3 = FunctionTest!(S, bool)("isSignal");
    foreach (t; s3) f3.test(t, TD(t.str).isSignal);
      writefln(f3.report);
  }
}

static if (context == Bid64)
{
  unittest
  {  // isNaN
    static struct S { string str; bool expect; }
    S[] s =
    [
      { "NaN",  true },
      { "2.50",  false },
      { "-sNaN",  true },
    ];
    auto f = FunctionTest!(S, bool)("isNaN");
    foreach (t; s) f.test(t, TD(t.str).isNaN);
      writefln(f.report);
  }
}

  /// Returns true if this number is + or - infinity.
  @safe
  const bool isInfinite()
  {
    return m_tag == Tag.INF
      || m_tag == Tag.MINF;
  }

  /// Returns true if this number is not an infinity or a NaN.
  @safe
  const bool isFinite()
  {
    return m_tag  != Tag.INF
      && m_tag  != Tag.MINF
      && m_tag  != Tag.QNAN
      && m_tag  != Tag.SNAN;
  }

static if (context == Bid64)
{
  unittest
  {  // isFinite
    static struct S { string str; bool expect; }

    S[] s1 =
    [
      { "2.50",  true },
      { "-0.3",  true },
      { "0",    true },
      { "-Inf",  false },
      { "NaN",  false },
    ];
    auto f1 = FunctionTest!(S, bool)("isFinite");
    foreach (t; s1) f1.test(t, TD(t.str).isFinite);
      writefln(f1.report);

    S[] s2 =
    [
      { "2.50",  false },
      { "-0.3",  false },
      { "0",    false },
      { "-Inf",  true },
      { "NaN",  false },
    ];
    auto f2 = FunctionTest!(S, bool)("isInfinite");
    foreach (t; s2) f2.test(t, TD(t.str).isInfinite);
      writefln(f2.report);
  }
}

  /// Returns true if this number is a NaN or infinity.
  @safe
  const bool isSpecial()
  {
    return m_tag  == Tag.INF
      || m_tag  == Tag.MINF
      || m_tag  == Tag.QNAN
      || m_tag  == Tag.SNAN;
  }

static if (context == Bid64)
{
  unittest
  {  // isSpecial
    static struct S { string str; bool expect; }
    S[] s =
    [
      { "-Infinity",  true },
      { "-NaN",    true },
      { "sNan1234",  true },
      { "12378.34",  false },
    ];
    auto f = FunctionTest!(S, bool)("isSpecial");
    foreach (t; s) f.test(t, TD(t.str).isSpecial);
      writefln(f.report);
  }
}

  /// Returns true if this number is negative. (Includes -0)
  @safe
  bool isNegative() const
  {
    return sign;
  }

  /// Returns true if this number is positive. (Excludes -0)
  @safe
  const bool isPositive()
  {
    return !sign;
  }

static if (context == Bid64)
{
  unittest
  {  // isSpecial
    static struct S { string str; bool expect; }
    S[] s =
    [
      { "2.50",  false },
      { "-12",  true },
      { "-0",    true },
      { "-Inf",  true },
      { "Inf",  false },
    ];
    auto f = FunctionTest!(S, bool)("isNegative");
    foreach (t; s) f.test(t, TD(t.str).isNegative);
      writefln(f.report);
  }
}

  /// Returns true if this number is normal.
  @safe
  const bool isNormal(int minExponent = minExpo)
  {
    if (isFinite && !isZero) {
      return adjExpo >= minExponent;
    }
    return false;
  }

  /// Returns true if this number is subnormal.
  @safe
  const bool isSubnormal(int minExponent = minExpo)
  {
//    int subExponent = minExponent - precision;
    if (!isFinite) return false;
    return adjExpo < minExponent;
//      && adjExpo >= subExponent;
  }

static if (context == Bid64)
{
  unittest
  {  // isNormal, isSubnormal
    static struct S { string str; bool expect; }

    S[] s1 =
    [
      { "2.50",    true },
      { "1.0E-368",  true },
      { "0.9E-368",  false },
      { "1.0E-384",  false },
      { "0.9E-384",  false },
      { "0.00",    false },
      { "-Inf",    false },
      { "NaN",    false },
    ];
    auto f1 = FunctionTest!(S, bool)("isNormal");
    foreach (t; s1) f1.test(t, TD(t.str).isNormal);
      writefln(f1.report);

    S[] s2 =
    [
      { "2.50",    false },
      { "1.0E-368",  false },
      { "0.9E-368",  true },
      { "1.0E-384",  true },
      { "0.9E-384",  true },
      { "0.00",    false },
      { "-Inf",    false },
      { "NaN",    false },
    ];
    auto f2 = FunctionTest!(S, bool)("isSubnormal");
    foreach (t; s2) f2.test(t, TD(t.str).isSubnormal);
      writefln(f2.report);
  }
}

  /// Returns true if the number is an integer (the fractional part is zero).
  bool isIntegralValued()
  {
    if (isSpecial) return false;
    if (expo >= 0) return true;
    int exp = -expo;
    if (exp >= context.precision) return false;
    int zeros = trailingZeros(coff, digits);
    if (zeros) {
      exp += zeros;
      if (exp >= 0) return true;
    }
    return false;
  }

static if (context == Bid64)
{
  unittest {  // isIntegralValued
    write("-- isIntegralValued.");
    TD num;
    num = 12345;
    assertTrue(num.isIntegralValued);
//    num = TD("123456098420234978023480");
//    assertTrue(num.isIntegralValued);
    num = 1.5;
    assertTrue(!num.isIntegralValued);
    num = 1.5E+1;
    assertTrue(num.isIntegralValued);
    num = 0;
    assertTrue(num.isIntegralValued);
    num = "2.19000000E+5";
    num = "21900.000E-2";
    assertTrue(num.isIntegralValued);
    writeln("passed");
  }
}

  /// Returns true if this number is a true value.
  /// Non-zero finite numbers are true.
  /// Infinity is true and NaN is false.
  @safe
  const bool isTrue()
  {
    return isFinite && !isZero || isInfinite;
  }

  /// Returns true if this number is a false value.
  /// Finite numbers with zero coefficient are false.
  /// Infinity is true and NaN is false.
  @safe
  @property
  const bool isFalse()
  {
    return isNaN || isZero;
  }

static if (context == Bid64)
{
  unittest {  //isTrue/isFalse
    write("-- isTrue/isFalse...");
//    assertTrue(TD(1));
//    assert(ONE);
//    assertEqual(ONE, true);
//    assertTrue(cast(bool)ONE);
    assertTrue(TD("1").isTrue);
    assertFalse(TD("0").isTrue);
    assertTrue(infinity.isTrue);
    assertFalse(nan.isTrue);
    assertTrue(TD("0").isFalse);
    assertFalse(TD("1").isFalse);
    assertFalse(infinity.isFalse);
    assertTrue(nan.isFalse);
    writeln("passed");
  }
}

/*  @safe
  const bool isZeroCoefficient() {
    return !isSpecial && coefficient == 0;
  }*/

static if (context == Bid64)
{
/*  unittest {  // isZeroCoefficient
    write("-- isZeroCoeff......");
    TD num;
    num = 0;
    assertTrue(num.isZeroCoefficient);
    num = BigInt("-0");
    assertTrue(num.isZeroCoefficient);
    num = TD("0E+4");
    assertTrue(num.isZeroCoefficient);
    num = 12345;
    assertFalse(num.isZeroCoefficient);
    num = 1.5;
    assertFalse(num.isZeroCoefficient);
    num = TD.NaN;
    assertFalse(num.isZeroCoefficient);
    num = TD.Infinity;
    assertFalse(num.isZeroCoefficient);
    writeln("passed");
  }*/
}

//--------------------------------
// assignment
//--------------------------------

  /// Assigns a decimal number (makes a copy)
  void opAssign(T:decimal)(in T that)
  {
    this.m_tag    = that.m_tag ;
    this.m_digits  = that.m_digits;
    this.m_sign  = that.m_sign;
    this.m_expo   = that.m_expo;
    this.m_coff   = that.m_coff;
  }

  ///  Assigns an BigInt value.
  void opAssign(T:BigInt)(T that)
  {
    this = decimal(that);
  }

  ///  Assigns an boolean value.
  void opAssign(T:bool)(T that)
  {
    this = decimal(that);
  }

  /// Assigns an value.
  void opAssign(T)(in T that) if (isIntegral!T)
  {
      this = decimal(BigInt(that));
  }

  /// Assigns a floating point value.
  void opAssign(T:real)(in T that) if (isFloatingPoint!T)
  {
    this = decimal(that);
  }

  ///  Assigns a string value.
  void opAssign(T:string)(in T that)
  {
    this = decimal(that);
  }

static if (context == Bid64)
{
  unittest {  // opAssign
    write("-- opAssign.........");
    TD num;
    string str;
    num = TD(245, 8, true);
    str = "-2.45E+10";
    assertStringEqual(num,str);
    num = long.max;
    str = "9223372036854775807";
    assertStringEqual(num,str);
//    num = (int.max - 12);
//    str = "-13";
    assertStringEqual(num,str);
    num = 237UL;
    str = "237";
    assertStringEqual(num,str);
//if (!__ctfe) writefln("real.max = %.18G", real.max);
//if (!__ctfe) writefln("real.dig = %s", real.dig);
//if (!__ctfe) writefln("real.max_10_exp = %s", real.max_10_exp);
    num = real.max;
//if (!__ctfe) writefln("num = %s", num);
    str = "1.18973149535723176E+4932";
//if (!__ctfe) writefln("str = %s", str);
    assertStringEqual(num, str);
    num = BigInt("123456098420234978023480");
    str = "123456098420234978023480";
    assertStringEqual(num, str);
    num = "123456098420234978023480";
    assertStringEqual(num, str);
    writeln("test missing");
  }
}

//--------------------------------
// toString functions
//--------------------------------

  /// Converts a number to the default string representation.
  public string toString(string fmStr = "%s") const
  {
    return eris.decimal.conv.toString(this, fmStr);
  }

  /// Converts a number to an abstract string representation.
  public string toAbstract() const
  {
    return abstractForm(this);
  }

  /// Converts a number to a full string representation.
  public string toFull() const
  {
    return fullForm(this);
  }

  /// Converts a number to a "scientific notation" string representation.
  public string toScientific() const
  {
    return sciForm(this);
  }

  /// Converts a number to an "engineering notation" string representation.
  public string toEngineering() const
  {
    return engForm(this);
  }

//--------------------------------
// comparison
//--------------------------------

  /// Returns -1, 0 or 1, if this number is less than, equal to,
  /// or greater than the argument, respectively. NOTE: The comparison
  /// is made to the current precision.
  const int opCmp(T:decimal)(T that)
  {
    return compare(this, that);
  }

  /// Returns -1, 0 or 1, if this number is less than, equal to,
  /// or greater than the argument, respectively.
  const int opCmp(T)(T that)
  {
    return opCmp(decimal(that));
  }

  /// Returns true if this number is equal to the argument.
  /// Finite numbers are equal if they are numerically equal
  /// to the current precision.
  /// Infinities are equal if they have the same sign.
  /// Zeros are equal regardless of sign.
  /// A NaN is not equal to any number, not even to another NaN.
  /// A number is not even equal to itself (this != this) if it is a NaN.
  const bool opEquals(T:decimal)(T that)
  {
    return equals(this, that);
  }

  /// Returns true if this number is equal to the argument.
  const bool opEquals(T)(T that)
  {
    return opEquals(decimal(that));
  }

static if (context == Bid64)
{
  unittest {  // comparison
    write("-- comparison.......");
    TD num1, num2;
    num1 = 105;
    num2 = 10.543;
    assert(num1 == 105L);
    assertGreaterThan(num1, num2);
    assertNotEqual(num1, num2);
    assertGreaterThan(num1, num2);
    assertLessThan(num2, num1);
    num1 = 10.543;
    assertNotLessThan(num1, num2);
    assertNotGreaterThan(num2, num1);
    assertEqual(num1, num2);
    writeln("passed");
  }
}

//--------------------------------
// unary arithmetic operators
//--------------------------------

  /// Returns the result of the
  /// unary operation on this number.
  //@safe
  public decimal opUnary(string op)()
  {
    static if (op == "+")
    {
      return plus(this);
    }
    else static if (op == "-")
    {
      return minus(this);
    }
    else static if (op == "++")
    {
      this = add(this, 1);
      return this;
    }
    else static if (op == "--")
    {
      this = sub(this, 1);
      return this;
    }
  }

static if (context == Bid64)
{
  unittest {  // opUnary
    write("-- opUnary..........");
    TD num, actual, expect;
    num = 134;
    expect = num;
    actual = +num;
    assertEqual(actual, expect);
    num = "134.02";
    expect = "-134.02";
    actual = -num;
    assertEqual(actual, expect);
    num = 134;
    expect = 135;
    actual = ++num;
    assertEqual(actual, expect);
    num = 1.00E8;
    expect = num - 1;
    actual = --num;
    assertEqual(actual, expect);
    num = 1.00E8;
    expect = num;
    actual = num--;
    assertEqual(actual, expect);
    num = TD(9999999, 90);
    expect = num;
    actual = num++;
    assertEqual(actual, expect);
    num = 12.35;
    expect = 11.35;
    actual = --num;
    assertEqual(actual, expect);
    writeln("passed");
  }
}

//--------------------------------
//  binary arithmetic operators
//--------------------------------

  /// Returns the result of the specified
  /// binary operation on this number and the argument.
  public decimal opBinary(string op, D:decimal)(in D that) const
  {
    static if (op == "+")
    {
      return add(this, that);
    }
    else static if (op == "-")
    {
      return sub(this, that);
    }
    else static if (op == "*")
    {
      return mul(this, that);
    }
    else static if (op == "/")
    {
      return div(this, that);
    }
    else static if (op == "%")
    {
      return remainder(this, that);
    }
  }

static if (context == Bid64)
{
  unittest
  {  // opBinary
    write("-- opBinary.........");
    struct S { string op; string x; string y; string z; }
    S[] tests =
    [
      { "+", "4", "8", "12" },
      { "-", "4", "8", "-4" },
      { "*", "4", "8", "32" },
      { "/", "5", "2", "2.5" },
      { "/", "2", "5", "0.4" },

      { "%", "10", "3", "1" },
      { "%", "3", "10", "3" },
      { "&", "00011010", "10001110", "1010" },
      { "|", "00011010", "10001110", "10011110" },
      { "^", "00011010", "10001110", "10010100" },
    ];
    foreach (i, s; tests)
    {
      testBinaryOp(s.op, s.x, s.y, s.z);
    }
    writeln("passed");
  }
}

  /// Returns the result of performing the specified
  /// binary operation on this number and the argument.
  decimal opBinary(string op, T)(T x) const
  {
    return opBinary!op(decimal(x));
  }

  /// Returns the result of performing the specified
  /// binary operation on this number and the argument.
  decimal opBinaryRight(string op, T)(in T x) const
  {
    static if (op == "+")
    {
      return add(this, x, decimal.context);
    }
    else static if (op == "-")
    {
      return sub(decimal(x), this, decimal.context);
    }
    else static if (op == "*")
    {
      return mul(this, x, decimal.context);
    }
    else static if (op == "/")
    {
      return div(decimal(x), this, decimal.context);
    }
    else static if (op == "%")
    {
      return remainder(decimal(x), this, decimal.context);
    }
    assert(false);
  }

//-----------------------------
// operator assignment
//-----------------------------

  /// Performs the specified binary operation on this number
  /// and the argument then assigns the result to this number.
  ref decimal opOpAssign(string op, T:decimal) (T x)
  {
    this = opBinary!op(x);
    return this;
  }

  /// Performs the specified binary operation on this number
  /// and the argument then assigns the result to this number.
  ref decimal opOpAssign(string op, T) (T x)
  {
    this = opBinary!op(decimal(x));
    return this;
  }

static if (context == Bid64)
{
  unittest {  // opOpAssign
    write("-- opOpAssign.......");
    TD op1, op2, actual, expect;
    op1 = 23.56;
    op2 = -2.07;
    op1 += op2;
    expect = 21.49;
    actual = op1;
    assertEqual(actual, expect);
    op1 *= op2;
    expect = -44.4843;
    actual = op1;
    assertEqual(actual, expect);
    writeln("passed");
  }
}

//--------------------------------
// mathematical constants
//--------------------------------

/*  enum decimal RealMax = decimal("1.1897314953572317649E+4932");
  enum decimal RealMin = RealMax.copyNegate;
  enum decimal RealMinNorm = decimal("3.362103143112093506E-4932");

  enum decimal DoubleMax = decimal("1.7976931348623157079E+308");
  enum decimal DoubleMin = DoubleMax.copyNegate;
  enum decimal DoubleMinNorm = decimal("2.2250738585072013832E-308");
  enum decimal LongMax = decimal("9223372036854775807");
  enum decimal LongMin = decimal("-9223372036854775808");
  enum decimal IntMax = decimal("2147483647");
  enum decimal IntMin = decimal("-2147483648");*/

//  mixin Constant!("REAL_MAX", "1.1897314953572317649E+4932");

//-----------------------------
// nextUp, nextDown, nextAfter
//-----------------------------

  /// Returns the smallest representable number that is larger than
  /// this number.
  decimal nextUp() const
  {
    return nextPlus(this, decimal.context);
  }

  /// Returns the largest representable number that is smaller than
  /// this number.
  decimal nextDown() const
  {
    return nextMinus(this, decimal.context);
  }

  /// Returns the representable number that is closest to the
  /// this number (but not this number) in the
  /// direction toward the argument.
  decimal nextAfter(decimal x) const
  {
    return nextToward(this, x, decimal.context);
  }

  static if (context == Bid64) {
  unittest {  // nextUp, nextDown, nextAfter
    write("-- next.............");
    TD big = 123.45;
    assertEqual(big.nextUp,   TD("123.4500000000001"));
    assertEqual(big.nextDown, TD("123.4499999999999"));
    assertEqual(big.nextAfter(TD(123.46)), big.nextUp);
    assertEqual(big.nextAfter(TD(123.44)), big.nextDown);
    writeln("passed");
  }}


  static if (context == Bid64) {
  unittest {
    writeln("==========================");
    writeln("decimal64..............end");
    writeln("==========================");
  }}
}

unittest {
  writeln("==========================");
  writeln("decimal..............begin");
  writeln("==========================");
}

package BigInt pow10b(uint n)
{
  enum BigInt[19] tens =
  [
    BigInt(1),
    BigInt(10),
    BigInt(100),
    BigInt(1000),
    BigInt(10000),
    BigInt(100000),
    BigInt(1000000),
    BigInt(10000000),
    BigInt(100000000),
    BigInt(1000000000),
    BigInt(10000000000),
    BigInt(100000000000),
    BigInt(1000000000000),
    BigInt(10000000000000),
    BigInt(100000000000000),
    BigInt(1000000000000000),
    BigInt(10000000000000000),
    BigInt(100000000000000000),
    BigInt(1000000000000000000)
  ];

  if (n < 19) return tens[n];
  return BigInt(10)^^n;
}

unittest
{
  for(uint n = 0; n < 19; n++)
  {
//if (!__ctfe) writefln("pow10b(n) = %s", pow10b(n));
  }
}

//package BigInt pow10b(int n)
//{
//  const BigInt ten = 10;
//  if (n < 19) return btens[n];
//  return ten^^n;
//}

/// Returns true if the parameter is convertible to a decimal number.
public enum bool isConvertible(T) =
  is(T:BigInt) || std.traits.isNumeric!T ||
  is(T:string) || isBoolean!T;

unittest {
  write("-- isConvertible....");
  assertTrue(isConvertible!int);
  assertTrue(isConvertible!bool);
  assertTrue(isConvertible!string);
  assertTrue(isConvertible!BigInt);
  assertTrue(isConvertible!double);
//  assertFalse(isConvertible!creal);
  assertFalse(isConvertible!TD);
  writeln("passed");
}

public enum bool isInteger(T) =
  is(T:BigInt) || isIntegral!T;

unittest {
  write("-- isInteger........");
  assertTrue(isInteger!long);
  assertTrue(isInteger!int);
  assertFalse(isInteger!float);
  assertFalse(isInteger!bool);
  assertTrue(isInteger!BigInt);
  assertTrue(isInteger!byte);
  assertTrue(isInteger!ubyte);
  writeln("passed");
}

/// Returns true if the parameter is a decimal number.
public enum bool isDecimal(D) = hasMember!(D, "IsDecimal");
//public enum bool isDecimal(D) = is(D:decimal);
unittest
{  // constants
  static struct S { bool x; bool expect; }
  S[] s =
  [
    { isDecimal!TD, true },
    { isDecimal!int, true },
  ];
  auto f = FunctionTest!(S,bool)("isDecimal");
  foreach (t; s) f.test(t, t.x);
  writefln(f.report);
}

unittest {
  write("-- isDecimal........");
  TD dummy;
  assertTrue(isDecimal!TD);
  assertFalse(isDecimal!int);
//  assertTrue(isDecimal!Dec64);
  writeln("passed");
}


// TODO: move these to math
//--------------------------------
// decimal constant templates
//--------------------------------

/// mixin template to create a constant at the type precision,
/// with an option to create an arbitrary precision constant.
/*mixin template Constant(string name)
{
  mixin ("public static decimal " ~ name ~ "(int precision = decimal.precision)"
    ~ "{"
      ~ "if (precision != decimal.precision)"
      ~ "{"
        ~ "return eris.decimal.math." ~ name ~ "!decimal(precision);"
      ~ "}"
    ~ "return " ~ name.toUpper ~ ";"
    ~ "}");
}*/

/*mixin template Constant(string name, string value)
{
  mixin
  (
    "static decimal " ~ name ~ "()"
  ~ "{"
    ~ "static bool initialized = false;"
    ~ "static decimal " ~ name ~ ";"
    ~ "if (!initialized)"
    ~ "{"
      ~ name ~ " = decimal(\"" ~ value ~ "\");"
      ~ "initialized = true;"
    ~ "}"
    ~ "return " ~ name ~ ";"
  ~ "}"
  );
}*/

/*
/// mixin template to create a constant at the type precision,
/// with an option to create an arbitrary precision constant.
mixin template Constant(string lcName, string ucName)
{
  mixin ("public static decimal " ~ lcName ~ "(int precision = decimal.precision)"
    ~ "{"
      ~ "if (precision != decimal.precision)"
      ~ "{"
        ~ "return eris.decimal.math." ~ lcName ~ "!decimal(precision);"
      ~ "}"
    ~ "return " ~ ucName ~ ";"
    ~ "}");
}
*/

// TODO: move this to rounding.d

version(unittest)
{

  public enum bool testBinaryOp(D)(string op, D x, D y, D z,
      string file = __FILE__, int line = __LINE__)
    if (isDecimal!D)
  {
    switch (op)
    {
    // infix operators
    case "+":
      return assertBinaryOp("plus", x, y, x + y, z, file, line);
    case "-":
      return assertBinaryOp("minus", x, y, x - y, z, file, line);
    case "*":
      return assertBinaryOp("times", x, y, x * y, z, file, line);
    case "/":
      return assertBinaryOp("divided by", x, y, x / y, z, file, line);
    case "%":
      return assertBinaryOp("mod", x, y, x % y, z, file, line);
    default:
      return false;
    }
  }

  public enum bool testBinaryOp(D)
    (string op, D x, D y, D z, string file = __FILE__, int line = __LINE__)
    if (!isDecimal!D && isConvertible!D)
  {
    return testBinaryOp!TD
      (op, TD(x), TD(y), TD(z), file, line);
  }

  private bool assertBinaryOp(D)
    (string op, D x, D y, D computed, D expected, string file, int line)
  {
    if (computed == expected)
    {
      return true;
    }
    else
    {
      writeln("failed at ", std.path.baseName(file), "(", line, "): \"",
        x , "\" " , op, " \"", y , "\" equals \"",
        computed, "\" not \"", expected, "\".");
      return false;
    }
  }

  // TODO: (efficiency) can this take advantage of small numbers? i.e. < long.max?
  public U pow10(U)(int n) if (is(U!BigInt) || is(U!long))
  {
    auto ten = U(10);
//    if (n < 0) throw new InvalidOperationException();
    if (n == 0) return ten;
    return ten^^n;
  }

/*  public enum double pow10Dbl(int n)
  {
    static double[23] dtens;
    static bool initialized = false;
    if (!initialized)
    {
      dtens[0] = 1.0;
      for (size_t i = 1; i < dtens.length; i++)
      {
        dtens[i] = dtens[i-1] * 10.0;
      }
      initialized = true;
    }
    if (n > 22) return double.nan;
    return dtens[n];
  }*/

// ======================================================== //
// dead code
// ======================================================== //

/+
  /// Converts a decimal number to a real number. The decimal will be
  /// rounded to the RealContext before conversion, if necessary.
  public real toReal() const
  {
    // special values
    if (this.isNaN) return real.nan;
    if (this.isInfinite) return sign ? -real.infinity : real.infinity;
    if (this.isZero) return sign ? -0.0 : 0.0;
    int realMinExpo = 1 - RealContext.maxExpo;
    if (this.isSubnormal(realMinExpo)) return real.nan;

    // if this number is larger than the largest real value,
    // return infinity
    if (this.m_expo >= real.max_10_exp) {
      if (this > RealMax)  return  real.infinity;
      if (this < RealMin) return -real.infinity;
    }

    // if smaller than the smallest value, return zero
    if (this.m_expo <= real.min_10_exp) {
      if (this.copyAbs < RealMinNorm) return this.sign ? -0.0 : 0.0;
    }

    // will the coefficent fit into a long integer?
    if (this.coff.ulongLength <= 2)
    {
      return longToReal(this);
    }

    // NOTE: There are real numbers that will be rounded unnecessarily
    // (i.e. more than 18 digits but less than long.max)
    // the reduced coefficient will fit
    bigd reduced = this.reduce(RealContext);
    if (reduced.coff.ulongLength <= 2)
    {
      return longToReal(reduced);
    }

    return real.nan;  // NOTE: nan or infinity?
  }

  static if (context == TestContext) {
  unittest {  // toReal, toDecimal
    write("-- toReal...........");
    static real[] tests =
    [
      1.0,
      2.0,
      1.0E5,
      0.1,
      123.456,
      32E-27,
      double.max,
      real.max,
    ];

    foreach (i, s; tests)
    {
      bigd d = toDecimal(s);
      real r = d.toReal();
      assertEqual(r, d, i);
    }
    writeln("passed");
  }}


  public double toDouble() const
  {
    // try for an exact conversion...
    if (digits <= 15) {
      int absExpo = std.math.abs(m_expo);
      if (absExpo <= 22) {
        double s = cast(double)(coefficient.toLong);
        if (absExpo == 0) return s;
        double p = pow10Dbl(absExpo);
        if (m_expo > 0) return s * p;
        if (m_expo < 0) return s / p;
      }
    }
    // TODO: (behavior) add method for other values
    return double.nan;
  }

  // TODO: (testing) add unit tests
  unittest {
    write("toDouble...");
    TD x = "3.14159";
    writefln("x.toDouble = %s", x.toDouble);
    writeln("test missing");
  }

//--------------------------------
// casts
//--------------------------------

   bool opCast(T:bool)() const
  {
    return isTrue;
  }

   T opCast(T)() const if (isDecimal!T)
  {
    return T(this);
  }

   T opCast(T)() const if (isFloatingPoint!T)
  {
    return T(this);
  }

  static if (context == TestContext) {
  unittest {
    write("-- opCast...........");
/*    assertFalse(TD.init);
    TD abc = TD(12,4);
    assertTrue(abc);
    dec99 def = cast(dec99)abc;
    assertEqual(abc, def);
    TD def2 = cast(dec99)abc;
    assertEqual(def, def2);
    int n = 7;
    TD bdn = cast(TD)n;
    assertEqual(bdn, TD(7));
    auto tr = cast(TD)12;
    assertEqual(typeid(tr), typeid(bdn));
//    assertTrue(is(tr:bdn));
    dec99 big = 1234567890123;
    TD klm = TD(big);
    assertEqual(klm, big);  // klm has not been rounded.
    assertNotEqual(abs(klm), big);  // klm has been rounded.
    dec99 spcl = dec99.infinity(true);
    klm = TD(spcl);
    assertEqual(klm, TD("-Infinity"));*/
    writeln("test missing");
  }}

+/

// ======================================================== //
// end dead code
// ======================================================== //



}
