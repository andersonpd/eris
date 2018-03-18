// Written in the D programming language

/**
 * Rounding methods for floating-point decimal arithmetic.
 *
 * Authors: Paul D. Anderson
 *
 * Copyright: Copyright 2009-2017 by Paul D. Anderson.
 *
 * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>
 *
 * Standards:
 *  General Decimal Arithmetic Specification,
 *  Version 1.70, (25 March 2009).
 */

module eris.decimal.rounding;

import eris.decimal;

import std.conv;
import std.string;
import std.traits;
import std.stdio;

unittest {
  writeln("    rounding tests    ");
  writeln("==========================");
}

version(unittest)
{
  import std.stdio;
  import eris.decimal.test;
}

/**
 *  Returns the number rounded to its context precision.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
public D precisionRound(D)(in D num) if (isDecimal!D)
{
  return precisionRound(num, D.context);
}

/**
 *  Rounds the number to the precision of the context parameter.
 *  if setFlags is false no context flags will be set by this operation.
 *  Returns the rounded number.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
package D precisionRound(D)(in D num, Context context = D.context, bool setFlags = true)
  if (isDecimal!D)
{
  return precisionRound(num,
    context.precision, context.mode, setFlags);
}

/**
 *  Rounds the number to the specified precision using the specified rounding mode.
 *  if setFlags is false none of the context flags will be set by this operation.
 *  Flags: SUBNORMAL, CLAMPED, OVERFLOW, INEXACT, ROUNDED.
 */
//@safe
package D precisionRound(D)(in D num, int precision, Round mode = D.mode,
    bool setFlags = true)
  if (isDecimal!D)
{
  D copy = num;
  // check for overflow before rounding and copy the input
  copy = overflow(num, mode, setFlags);

  // check rounding mode
  if (mode == ROUND_NONE) return copy;

  // special values aren't rounded
  if (!copy.isFinite) return copy;

  // handle subnormal numbers
  if (!copy.isZero && copy.isSubnormal())
  {
    if (setFlags) contextFlags.set(SUBNORMAL);
//    int diff = copy.adjExpo - copy.tinyExpo;
    int subprecision = copy.adjExpo - copy.tinyExpo + 1;
    // use the subnormal precision and round
    if (precision < subprecision) subprecision = precision;
    if (copy.digits > subprecision) {
      copy = modeRound(copy, subprecision, mode, setFlags);
    }
    // if the result of rounding a subnormal is zero
    // the clamped flag is set. (Spec. p. 51)
    if (copy.adjExpo < copy.tinyExpo)
    {
      copy.expo = copy.tinyExpo;
      if (setFlags) contextFlags.set(CLAMPED);
    }
    return copy;
  }
  // zero values aren't rounded, but they are checked for
  // subnormal exponents.
  if (copy.isZero)
  {
    if (copy.expo < copy.minExpo)
    {
      if (setFlags) contextFlags.set(SUBNORMAL);
      if (copy.expo < copy.tinyExpo)
      {
        copy.expo = copy.tinyExpo;
      }
    }
    return copy;
  }


  // round the number
  return modeRound(copy, precision, mode, setFlags);

} // end precisionRound()

unittest
{  // precisionRound
  static struct S { D64 x; int n; D64 expect; }
  S[] s =
  [
    { "9999", 3, "1.00E+4" },
    { "1234567890", 3, "1.23E+9" },
    { "1234567890", 4, "1.235E+9" },
    { "1234567890", 5, "1.2346E+9" },
    { "1234567890", 6, "1.23457E+9" },
    { "1234567890", 7, "1.234568E+9" },
    { "1234567890", 8, "1.2345679E+9" },
    { "1235",  3, "1.24E+3" },
    { "12359", 3, "1.24E+4" },
    { "1245",  3, "1.24E+3" },
    { "12459", 3, "1.25E+4" },
    // subnormal numbers
    { "1.234567890123456E-370", 16, "1.23456789012346E-370" },
    { "1.234567890123456E-380", 12, "1.2346E-380" },
    { "1.234567890123456E-381", 9,  "1.235E-381" },
    { "1.234567890123456E-381", 3,  "1.23E-381" },
    { "1.234567890123456E-382", 6,  "1.23E-382" },
    { "1.234567899123456E-383", 3,  "1.2E-383" },
    { "1.234567899123456E-384", 6,  "1E-384" },
    { "1E-369", 3, "1E-369" },
    { "1E-384", 3, "1E-384" },
    { "1E-385", 3, "0E-384" },
    { "0.1E-384", 3, "0E-384" },
    { "1234E-385", 3, "1.23E-382" },
    // extreme range zeros
    { "0E+369", 3, "0E-368" },
    { "0E+370", 3, "0E-368" },
    // TODO: I don't think overrange zeros should have tinyExpo
    { "0E-368", 3, "0E-368" },
    { "0E-369", 3, "0E-369" },
    { "0E-384", 3, "0E-384" },
    { "0E-385", 3, "0E-384" },
  ];
  auto f = FunctionTest!(S,D64)("roundPrec");
  foreach (t; s) f.test(t, precisionRound!D64(t.x,t.n));
  writefln(f.report);
}

//--------------------------------
// private methods
//--------------------------------

// TODO: needs unittests
// TODO: return boolean value
/**
 *  Returns true if the number is too large to be represented
 *  and adjusts the number according to the rounding mode.
 *  Implements the 'overflow' processing in the specification. (p. 53)
 *  Flags: OVERFLOW, ROUNDED, INEXACT.
 *  Precondition: number must be finite.
 */
//@safe
private D overflow(D)(in D num, Round mode = D.mode,
    bool setFlags = true)
  if (isDecimal!D)
{
  if (!num.isFinite) return num;
  if (num.adjExpo <= D.maxExpo) return num;

  if (setFlags) contextFlags.set(OVERFLOW | INEXACT | ROUNDED);

  switch (mode)
  {
  case HALF_UP:
  case HALF_EVEN:
  case HALF_DOWN:
  case ROUND_UP:
    return D.infinity(num.sign);
  case ROUND_DOWN:
    return num.sign ? D.max.copyNegate : D.max;
  case CEILING:
    return num.sign ? D.max.copyNegate : D.infinity;
  case FLOOR:
    return num.sign ? D.infinity(true) : D.max;
  default:
    return num;
  }
}

// TODO: these tests aren't very useful
unittest
{  // overflow
  static struct S { D64 num; Round mode; D64 expect; }
  S[] s =
  [
    { "1000",  HALF_EVEN,  "1000" },
    { "1000000", HALF_EVEN,  "10000E+2" },
    { "99999",   HALF_EVEN,  "99999" },
//    { "1234550", HALF_EVEN,  "1.2346E+6" },
//    { "1234550", ROUND_DOWN, "1.2345E+6" },
//    { "1234550", ROUND_UP,   "1.2346E+6" },
  ];
  auto f = FunctionTest!(S,D64)("overflow");
  foreach (t; s) f.test(t, overflow!D64(t.num,t.mode));
  writefln(f.report);
}


// TODO: move this into precisionRound -- it's the only function that calls it.
/**
 *  Rounds the number to the context precision
 *  using the specified rounding mode.
 */
private D modeRound(D)(D num, int precision, Round mode = D.mode,
    bool setFlags = true)
  if (isDecimal!D)
{
  // finite numbers only
  if (!num.isFinite) return num;
  // check rounding mode
  if (mode == ROUND_NONE) return num;
  // if the number is short enough, don't round
  if (num.digits <= precision) return num;

  D copy = overflow(num, mode, setFlags);
  // did it overflow ?
  if (!copy.isFinite) return copy;

  // calculate the remainder
  D remainder = getRemainder(copy, precision);
  // if the number wasn't rounded, return
  if (remainder.isZero) return copy;

  // check for deleted leading zeros in the remainder.
  // makes a difference only in round-half modes.
  if (mode == HALF_EVEN || mode == HALF_UP || mode == HALF_DOWN)
  {
    if (countDigits(remainder.coff) != remainder.digits)
    {
      return copy;
    }
  }

  switch (mode) {
  case ROUND_UP:
    incrementAndRound(copy);
    break;
  case ROUND_DOWN:
    break;
  case CEILING:
    if (!num.sign) incrementAndRound(copy);
    break;
  case FLOOR:
    if (num.sign) incrementAndRound(copy);
    break;
  case HALF_UP:
    if (firstDigit(remainder.coff) >= 5)
      incrementAndRound(copy);
    break;
  case HALF_DOWN:
    if (testFive(remainder.coff) > 0)
      incrementAndRound(copy);
    break;
  case HALF_EVEN:
    switch (testFive(remainder.coff))
    {
    case -1:
      break;
    case 1:
      incrementAndRound(copy);
      break;
    case 0:
      if (lastDigit(copy.coff) & 1) {
        incrementAndRound(copy);
      }
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }  // end switch (mode)
  return overflow(copy, mode, setFlags);
}  // end modeRound()

///
unittest
{
}

unittest
{  // modeRound
  static struct S { D64 x; int p; Round r; D64 expect; }
  S[] s =
  [
    { "1000",  5, HALF_EVEN,  "1000" },
    { "1000000", 5, HALF_EVEN,  "10000E+2" },
    { "99999",   5, HALF_EVEN,  "99999" },
    { "1234550", 5, ROUND_NONE, "1234550" },
    { "1234550", 5, ROUND_UP,   "1.2346E+6" },
    { "1234550", 5, ROUND_DOWN, "1.2345E+6" },
    { "1234550", 5, HALF_UP,  "1.2346E+6" },
    { "1234550", 5, HALF_EVEN,  "1.2346E+6" },
    { "1234550", 5, HALF_DOWN,  "1.2345E+6" },
    { "1234550", 5, FLOOR,    "1.2345E+6" },
    { "1234550", 5, CEILING,  "1.2346E+6" },
  ];
  auto f = FunctionTest!(S,D64)("modeRound");
  foreach (t; s) f.test(t, modeRound!D64(t.x,t.p,t.r));
  writefln(f.report);
}

/**
 *  Shortens the coefficient of the number to the specified precision,
 *  adjusts the exponent, and returns the (unsigned) remainder.
 *  If the number is already less than or equal to the precision, the
 *  number is unchanged and the remainder is zero.
 *  Otherwise the rounded flag is set, and if the remainder is not zero
 *  the inexact flag is also set.
 *  Flags: ROUNDED, INEXACT.
 */
private D getRemainder(D) (ref D num, int precision) if (isDecimal!D)
{
  int diff = num.digits - precision;
  if (diff <= 0) return D.zero;

  D remainder = D.zero;
  contextFlags.set(ROUNDED);
  BigInt divisor = pow10b(diff);
  BigInt dividend = num.coff;
  BigInt quotient = dividend/divisor;
  auto cf = dividend - quotient*divisor;
  if (cf != 0) {
  remainder.digits = diff;
  remainder.expo = num.expo;
  remainder.coff = cf;
  contextFlags.set(INEXACT);
  }
  num.coff = quotient;
  num.digits = countDigits(quotient); //precision;
  num.expo = num.expo + diff;
  return remainder;
}

unittest
{  // getRemainder
  static struct S { D64 x; int p; D64 expect; }
  S[] s =
  [
  { 1234567890123456L, 5, "67890123456" },
  ];
  auto f = FunctionTest!(S,D64)("remainder");
  foreach (t; s) f.test(t, getRemainder!D64(t.x,t.p));
   writefln(f.report);
}

/**
 *  Increments the coefficient by one.
 *  If this causes an overflow the coefficient is adjusted by clipping
 *  the last digit (it will be zero) and incrementing the exponent.
 */
private void incrementAndRound(D)(ref D num) if (isDecimal!D)
{
  // if num is zero
  if (num.digits == 0) {
    num.coff = 1;
    num.digits = 1;
    return;
  }
  num.coff = num.coff + 1;
  // TODO: (efficiency) is there a less expensive test?
  if (lastDigit(num.coff) == 0) {
    if (num.coff / pow10b(num.digits) > 0) {
      num.coff = num.coff / 10;
      num.expo = num.expo + 1;
    }
  }
}

unittest
{  // incrementAndRound
  D64 incr(D64 num)
  {
    incrementAndRound(num);
    return num;
  }

  static struct S { D64 num; D64 expect; }
  S[] s =
  [
    {  10, 11 },
    {  19, 20 },
    { 999, "1.00E+3" },
  ];
  auto f = FunctionTest!(S,D64)("increment");
  foreach (t; s) f.test(t, incr(t.num));
  writefln(f.report);
}

/*unittest {  // increment
  write("-- increment&round..");
  D64 actual, expect;
  actual = 10;
  expect = 11;
  incrementAndRound(actual);
  assertEqual(actual, expect);
  actual = 19;
  expect = 20;
  incrementAndRound(actual);
  assertEqual(actual, expect);
  actual = 999;
  expect = "1.00E+3";
  incrementAndRound(actual);
  assertEqual(actual, expect);
  writeln("passed");
}*/

/**
 *  Returns -1, 1, or 0 if the remainder is less than, more than,
 *  or exactly half the least significant digit of the shortened coefficient.
 *  Exactly half is a five followed by zero or more zero digits.
 */
private int testFive(in BigInt x)
{
  int num = countDigits(x);
  BigInt btens = pow10b(num - 1);
  int first = cast(int)(x / btens);
  if (first < 5) return -1;
  if (first > 5) return +1;
  BigInt zeros = x % btens;
  return zeros ? 1 : 0;
}

unittest
{  // testFive
  static struct S { BigInt x; int expect; }
  S[] s =
  [
    { "5000000000000000000000",  0 },
    { "4999999999999999999999", -1 },
    { "50000000000000000000000000000000000000000000000001", 1 },
  ];
  auto f = FunctionTest!(S,int)("testFive");
  foreach (t; s) f.test(t, testFive(t.x));
  writefln(f.report);
}

//-----------------------------
// useful constants
//-----------------------------

/*private enum BigInt BIG_ZERO = BigInt(0);
private enum BigInt BIG_ONE  = BigInt(1);
private enum BigInt BIG_FIVE = BigInt(5);
private enum BigInt BIG_TEN  = BigInt(10);
private enum BigInt BILLION  = BigInt(1_000_000_000);
private enum BigInt QUINTILLION  = BigInt(1_000_000_000_000_000_000);*/


///  The maximum number of decimal digits that fit in an int value.
//public enum int MAX_INT_DIGITS = 9;
///  The maximum decimal value that fits in an int.
//public enum uint MAX_DECIMAL_INT = 999999999U;
///  The maximum number of decimal digits that fit in a long value.
//public enum int MAX_LONG_DIGITS = 18;
///  The maximum decimal value that fits in a long value.
//public enum ulong MAX_DECIMAL_LONG = 10UL^^MAX_LONG_DIGITS - 1;

//-----------------------------
// BigInt digit functions
//-----------------------------

private enum ten = 10UL;
/**
 *  An array of unsigned long integers with values of
 *  powers of ten from 10^^0 to 10^^19
 */
private enum ulong[20] pow10UL = [ten^^0,
    ten^^1,  ten^^2,  ten^^3,  ten^^4,  ten^^5,  ten^^6,
    ten^^7,  ten^^8,  ten^^9,  ten^^10, ten^^11, ten^^12,
    ten^^13, ten^^14, ten^^15, ten^^16, ten^^17, ten^^18, ten^^19];

/**
 * Returns the number of decimal digits in a non-negative big integer
 */
public int countDigits(T)(in T m)
  if (is(T == BigInt) || isUnsigned!T)
{
  if (m == 0)
  {
    return 0;
  }

  static if (is(T == BigInt))
  {
    if (m.ulongLength == 1)
    {
      return countDigits(m.getDigit(0));
    }

    int count;
    ulong n = clipDigits(m, count);
    return count + countDigits(n);
  }
  else
  {
    auto n = cast(ulong)m;
    // special cases:
    if (n == 0) return 0;
    if (n < 10) return 1;
    if (n >= pow10UL[19]) return 20;
    // use a binary search to count the digits
    int min = 2;
    int max = 19;
    while (min <= max)
    {
      int mid = (min + max)/2;
      if (n < pow10UL[mid])
      {
        max = mid - 1;
      }
      else
      {
        min = mid + 1;
      }
    }
    return min;
  }
}

///
unittest // countDigits
{
  auto big = BigInt(
    "1234567890_1234567890_1234567890_1234567890_1234567890" ~
    "1234567890_1234567890_1234567890_1234567890_1234567890_1");
  assert(countDigits(big) == 101);
}

unittest
{  // countDigits
  static struct S { BigInt n; int expect; }
  S[] s =
  [
    { "123456", 6 },
    { "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901", 101 },
    { "1234567890123456789012345678901234567890123456789012", 52 },
  ];
  auto f = FunctionTest!(S,int)("countDigits");
  foreach (t; s) f.test(t, countDigits(t.n));
  writefln(f.report);
}

unittest
{  // countDigits
  static struct S { ulong n; int expect; }
  S[] s =
  [
    {          7,  1 },
    {         13,  2 },
    {        999,  3 },
    {         9999,  4 },
    {        25978,  5 },
    {      2008617,  7 },
    {     1234567890, 10 },
    {    10000000000, 11 },
    {  123456789012345, 15 },
    {  1234567890123456, 16 },
    {123456789012345678, 18 },
    {       long.max, 19 },
    {      ulong.max, 20 },
    {      ulong.min,  0 },
  ];
  auto f = FunctionTest!(S,int)("countDigits");
  foreach (t; s) f.test(t, countDigits(t.n));
  writefln(f.report);
}


/// Clips decimal digits until a single ulong digit is left.
private ulong clipDigits(in BigInt big, out int count)
{
  if (big <= ulong.max) return cast(ulong)big;

  enum BigInt tenQuint = 10UL^^19;
  count = 0;
  BigInt quotient = big;
  while (quotient > tenQuint) {
    quotient /= tenQuint;
    count += 19;
  }
  return cast(ulong)quotient;
}

unittest // clipDigits
{
  int count;
  BigInt big;
  big = BigInt("123456");
  assert(clipDigits(big, count) == 123456);
  assert(clipDigits(BigInt(ulong.max), count) == ulong.max);
  big = BigInt("1234567890_1234567890_1234567890_1234567890_1234567890");
  assert(clipDigits(big, count) == 123456789012);
}

/// returns the first decimal digit of the number
public uint firstDigit(T)(T n)
  if (is(T == BigInt) || isUnsigned!T)
{
  if (n == 0) return 0;
  if (n < 10) return cast(int) n;
  static if (is(T == BigInt))
  {
    int count;
    return firstDigit(clipDigits(n,count));
  }
  else
  {
    int digits = countDigits(n);
    return cast(uint)(n / pow10UL[digits-1]);
  }
}

///
unittest
{
  assert(firstDigit(BigInt("8234567890123456789012345678901234567890123")) == 8);
  assert(firstDigit(0U) == 0);
  assert(firstDigit(7U) == 7);
  assert(firstDigit(9999UL) == 9);
  assert(firstDigit(uint.max) == 4);
  assert(firstDigit(ulong.max) == 1);
}

unittest  // move to regression testing
{  // firstDigit(BigInt)
  static struct S { BigInt n; uint expect; }
  S[] s =
  [
    { "5000000000000000000000", 5 },
    { "45500000001209854300023400000000000000", 4 },
    { "82345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678905", 8 },
  ];
  auto f = FunctionTest!(S,uint)("1stDigit(b)");
  foreach (t; s) f.test(t, firstDigit(t.n));
  writefln(f.report);
}

unittest
{  // firstDigit(ulong)
  static struct S { ulong n; uint expect; }
  S[] s =
  [
    { 0, 0 },
    { 7, 7 },
    { 13, 1 },
    { 999, 9 },
    { 9999, 9 },
    { 25987, 2 },
    { 5008617, 5 },
    { 3234567890, 3 },
    { 10000000000, 1 },
    { 823456789012345, 8 },
    { 4234567890123456, 4 },
    { 623456789012345678, 6 },
    {long.max, 9 }
  ];
  auto f = FunctionTest!(S,uint)("1stDigit(u)");
  foreach (t; s) f.test(t, firstDigit(t.n));
  writefln(f.report);
}

/**
 *  Decimal shift left.
 *  Shifts the number left by the specified number of decimal digits.
 *  If n == 0 the number is returned unchanged.
 *  If n < 0  the number is shifted right.
 */
public BigInt shiftBig(BigInt num, int n)
{
  if (n > 0) {
    num *= pow10b(n);
  }
  if (n < 0) {
    num /= pow10b(-n);
  }
  return num;
}

///
unittest {  // shiftBig(BigInt)
  BigInt big;
  int n;
  big = 12345;
  n = 2;
  assert(shiftBig(big, n) == 1234500);
  big = 1234567890;
  n = 7;
  assert(shiftBig(big, n) == BigInt(12345678900000000));
  big = 12;
  n = 2;
  assert(shiftBig(big, n) == 1200);
  big = 12;
  n = 4;
  assert(shiftBig(big, n) == 120000);
  BigInt res;
  big = BigInt("9223372036854775807");
  n = -10;
  assert(shiftBig(big, n) == BigInt("922337203"));
  big = BigInt("9223372036854775808");
  n = -10;
  assert(shiftBig(big, n) == BigInt("922337203"));
}

///  Returns the last digit of the argument.
public int lastDigit(in BigInt big)
{
  auto digit = big % BigInt(10);
  if (digit < 0) digit = -digit;
  return digit.toInt;
}

unittest
{  // lastDigit
  static struct S { BigInt n; int expect; }
  S[] s =
  [
    {  7, 7 },
    { -13, 3 },
    {  999, 9 },
    { -9999, 9 },
    {  25987, 7 },
    { -5008615, 5 },
    {  3234567893, 3 },
    { -10000000000, 0 },
    {  823456789012348, 8 },
    {  4234567890123456, 6 },
    {  623456789012345674, 4 },
    {  long.max, 7 },
  ];
  auto f = FunctionTest!(S,int)("lastDigit");
  foreach (t; s) f.test(t, lastDigit(t.n));
  writefln(f.report);
}

///  Returns the number of trailing zeros in the argument.
public uint trailingZeros(BigInt n, uint digits)
{
  // shortcuts for frequent values
  if (n ==  0) return 0;
  if (n %  10) return 0;
  if (n % 100) return 1;
  // find by binary search
  uint min = 3;
  uint max = digits - 1;
  while (min <= max) {
    int mid = (min + max)/2;
    if (n % pow10b(mid) != 0)
    {
      max = mid - 1;
    }
    else
    {
      min = mid + 1;
    }
  }
  return max;
}
///
unittest
{
  auto big = BigInt("1234567890123456789012300000000000");
  auto digits = countDigits(big);
  assert(digits == 34);
  auto zeros = trailingZeros(big, digits);
  assert(zeros == 11);
}


///  Trims any trailing zeros and returns the number of zeros trimmed.
public int clipZeros(ref BigInt n, uint digits)
{
  if (digits <= 0) return 0;
  auto zeros = trailingZeros(n, digits);
  if (zeros == 0) return 0;
  n /= pow10b(zeros);
  return zeros;
}

///
unittest {
  auto big = BigInt("123456789000000");
  auto digits = countDigits(big);
  assert(digits == 15);
  auto zeros = clipZeros(big, digits);
  assert(zeros == 6);
}

unittest {
  writeln("==========================");
}

