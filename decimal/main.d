import std.bitmanip;
import std.math;
import std.stdio;
import std.string;
import std.traits;
//import std.BigInt;

import eris.decimal;
/*import eris.decimal.conv;*/
import eris.decimal.logical;
//import eris.decimal.arithmetic;

version(unittest)
{
  import eris.decimal.test;

  void main()
  {
    writefln("total     : %s tests (%s passed, %s failed)", totalTests, totalPass, totalFail);
  }
}
else
{
void main()
{

  alias D16 = Decimal!Bid64;

  /// Constructs a decimal number from a real value.
  D16 convert(T)(T r) if (isFloatingPoint!T)
  {
    if (!__ctfe) writefln("r = %s", r);

    static if (T.sizeof == 10)  // 80-bit real
    {
      RealRep rep;
    }
    else static if (T.sizeof == 8)  // 64-bit double
    {
      std.bitmanip.DoubleRep rep;
    }
    else static if (T.sizeof == 4)  // 32-bit float
    {
      std.bitmanip.FloatRep rep;
    }
    else // Shouldn't reach here
    {
      // always works but it's slow
      string str = format("%.20G", r);
      D16(str);
    }

    // finite numbers
    if (std.math.isFinite(r))
    {
      if (r == 0.0)
      {
        return D16.zero(r < 0.0);
      }
      else if (std.math.fabs(r) == 1.0)
      {
        return D16.one(r < 0.0);
      }
      const T maxInt = std.math.scalbn(1.0, rep.fractionBits + 1);
      real i;
      T frac = modf(r, i);
      if (frac == 0.0 && i < maxInt)
      {
//        writeln("integer");
        rep.value = r;
        BigInt f = BigInt(1) << rep.fractionBits | rep.fraction;
        int e = rep.exponent - rep.bias;
        BigInt val = f >> (rep.fractionBits - e);
        D16 dec = D16(val, 0, rep.sign);
        return dec; //.reduce;
      }
      else
      {
        writeln("string");
        string str = format("%.20G", r);
        return D16(str);
      }
    }
    // special values
    else if (std.math.isInfinity(r))
    {
      return D16.infinity(r < 0.0);
    }
    else
    {
      return D16.nan;
    }
  }

  writeln(convert!float(10.0f));
  writeln(convert!float(10.1f));
  writeln(convert!float(1234.21E2f));
  writeln(convert!float(1234.0E3f));
  writeln(convert!float(999999f));
  writeln(convert!float(9999999f));
  writeln(convert!float(16777215f));
  writeln(convert!float(16777216));
  writeln(convert!double(16777216));
  writeln(convert!double(1234.0E12));
  D16 dec;
  D16 dec1;
  D16 dec2;
  D16 dec3;

  separator;

  title("package.d");
  header("construction");

  // this(decimal)
  dec = D16(127435,-34,true);
  writefln("dec = %s", dec);
  writefln("D16(dec) = %s", D16(dec));

  // this(coefficient)
  writefln("D16(198) = %s", D16(198));
  writefln("D16(-623) = %s", D16(-623));
  // this(coefficient, expo)
  writefln("D16(3,5) = %s", D16(3,5));
  // this(coefficient, expo, sign)
  writefln("D16(12,-3,true) = %s", D16(12,-3,true));
  // this(real)
  writefln("D16(12.3E6) = %s", D16(12.3E6));
  dec = D16(12.3E6);
  writefln("dec = %s", dec);
  writefln("dec.toAbstract = %s", dec.toAbstract);
  writefln("dec.toFull = %s", dec.toFull);
  writefln("dec.toScientific = %s", dec.toScientific);
  writefln("dec.toEngineering = %s", dec.toEngineering);

  // this(string)
  writefln("D16(\"123_456_789\") = %s", D16("123_456_789"));
//  dec = D16("12.3E6");
  writefln("D16(\"1.23E6\") = %s", D16("1.23E6"));
  dec = D16("12.3E6");
  writefln("dec = %s", dec);
  writefln("dec.toAbstract = %s", dec.toAbstract);
  writefln("dec.toScientific = %s", dec.toScientific);
  writefln("dec.toEngineering = %s", dec.toEngineering);
  // this(bool)
  writefln("D16(true) = %s", D16(true));
  writefln("D16(false) = %s", D16(false));

  header("member properties");
  dec = D16(1259, 12, true);
  writefln("dec = %s", dec);
  writefln("dec.coff = %s", dec.coff);
  writefln("dec.coff(2360) = %s", dec.coff(2360));
  writefln("dec = %s", dec);
  writefln("dec.expo = %s", dec.expo);
  writefln("dec.expo(-98) = %s", dec.expo(-98));
  writefln("dec = %s", dec);
  writefln("dec.sign = %s", dec.sign);
  writefln("dec.sign(false) = %s", dec.sign(false));
  writefln("dec = %s", dec);
  writefln("dec.adjExpo = %s", dec.adjExpo);
//  writefln("dec.digits = %s", dec.digits);

  header("floating point properties");
  // also tests this(special value, sign)
  writefln("D16.init = %s", D16.init);
  writefln("D16.infinity = %s", D16.infinity);
  writefln("D16.nan = %s", D16.nan);
  writefln("D16.snan = %s", D16.snan);
  writefln("D16.precision = %s", D16.precision);
  writefln("D16.epsilon = %s", D16.epsilon);
  writefln("D16.maxExpo = %s", D16.maxExpo);
  writefln("D16.minExpo = %s", D16.minExpo);
  writefln("D16.max = %s", D16.max);
  writefln("D16.min = %s", D16.min);
  writefln("D16.min_normal = %s", D16.min_normal);
    writefln("D16.radix = %s", D16.radix);

  header("constants");
  writefln("D16.ONE = %s", D16.ONE);
  writefln("D16.TWO = %s", D16.TWO);
  writefln("D16.TEN = %s", D16.TEN);
  writefln("D16.HALF = %s", D16.HALF);
  writefln("D16.PI = %s", D16.PI);
  writefln("D16.E = %s", D16.E);
  writefln("D16.LN2 = %s", D16.LN2);
//  comment("note extra precision");
//  writefln("D16.REAL_MAX = %s", D16.REAL_MAX);

  header("copy");
  dec1 = "3098.3235";
  writefln("dec1 = %s", dec1);
  writefln("dec1.copy = %s", dec1.copy);
  writefln("dec1.copyNegate = %s", dec1.copyNegate);
  writefln("dec1.copyAbs = %s", dec1.copyAbs);
  dec2 = -400200;
  writefln("dec2 = %s", dec2);
  writefln("dec1.copySign(dec2) = %s", dec1.copySign(dec2));

  header("assignment");
  writefln("dec = %s", dec);
  dec = D16(3069,25,false);
  code("dec = D16(3069,25,false)");
  writefln("dec = %s", dec);
  dec = -567L;
  code("dec = -567L");
  writefln("dec = %s", dec);
  dec = true;
  code("dec = true");
  writefln("dec = %s", dec);
  dec = 12.34E5;
  code("dec = 12.34E5");
  writefln("dec = %s", dec);
  dec = "1.234";
  code("dec = \"1.234\"");
  writefln("dec = %s", dec);

   // TODO: show equal to precision
  header("equals");
  dec1 = 1.0; // 23.45;
  dec2 = -123.45;
  writefln("dec1 = %s", dec1);
  writefln("dec1 == dec1 = %s", dec1 == dec1);
    writefln("dec2 = %s", dec2);
  writefln("dec1 != dec2 = %s", dec1 != dec2);
  writefln("dec1 == dec2 = %s", dec1 == dec2);

  header("compare");
  dec1 = -1;
  dec2 = 0;
  dec3 = 1;
  writefln("dec1 = %s", dec1);
  writefln("dec2 = %s", dec2);
  writefln("dec3 = %s", dec3);
  writefln("dec1 < dec2 = %s", dec1 < dec2);
    writefln("dec3 < dec2 = %s", dec2 < dec3);
    writefln("dec2 >= dec2 = %s", dec2 >= dec2);

  header("opUnary");
  dec = 3E5;
  writefln("dec = %s", dec);
    writefln("+dec = %s", +dec);
    writefln("-dec = %s", -dec);
  writefln("--dec = %s", --dec);
    writefln("++dec = %s", ++dec);

  header("opBinary");
  dec1 = 119;
  dec2 = -12;
  dec3 = 4;
  writefln("dec1 = %s", dec1);
    writefln("dec2 = %s", dec2);
    writefln("dec3 = %s", dec3);
  writefln("dec1 + dec2 = %s", dec1 + dec2);
    writefln("dec1 - dec2 = %s", dec1 - dec2);
    writefln("dec1 * dec2 = %s", dec1 * dec2);
    writefln("dec1 / dec2 = %s", dec1 / dec2);
    writefln("dec1 mod dec3 = %s", dec1 % dec3);
    writefln("dec1 mod dec2 = %s", dec1 % dec2);
    writefln("dec2 / dec1 = %s", dec2 / dec1);
  comment("logical ops");
  dec1 = "1110";
  dec2 = "10010110";
  writefln("dec1 = %s", dec1);
  writefln("dec2 = %s", dec2);
  writefln("and(dec1, dec2) = %s", and(dec1, dec2));
    writefln("or(dec1, dec2) = %s", or(dec1, dec2));
    writefln("xor(dec1, dec2) = %s", xor(dec1, dec2));

//  writefln("dec1 & dec2 = %s", dec1 & dec2);
//  writefln("dec1 | dec2 = %s", dec1 | dec2);
//  writefln("dec1 ^ dec2 = %s", dec1 ^ dec2);

  header("obBinaryRight");
  header("opOpAssign");

  title("rounding.d");
  header("decimal digits");
  BigInt big = "0xFA2988_18830DD_658889AB_EFDCA45B_20933ABC_54AECCD0";
  writefln("big = %X", big);
  writefln("big = %s", big);
  writefln("firstDigit(big) = %s", firstDigit(big));
  writefln("lastDigit(big) = %s", lastDigit(big));
  writeln;
  //@system because opOpAssign is @system
  auto b = BigInt("0xABCD_5678_FFFF_FFFF_1234");
  auto e = BigInt("0x100000000");

//  b += 12345;
  writefln("%X",b); // BigInt("1_000_012_345")
  auto c = b >> 64;
  c <<= 64;
  writefln("%X",c); // BigInt("1_000_012_345")
  writefln("%X",e); // BigInt("1_000_012_345")
    auto d = b % e;
  writefln("%X",d); // BigInt("1_000_012_345")
  b -= c;

if (!__ctfe) writefln("D16.infinity = %s", D16.infinity);

if (!__ctfe) writefln("infinity(true) = %s", D16.infinity(true));


  separator;

if (!__ctfe) writefln("dec1.sign.sizeof   = %s", dec1.sign.sizeof);
if (!__ctfe) writefln("dec1.expo.sizeof   = %s", dec1.expo.sizeof);
if (!__ctfe) writefln("dec1.coff.sizeof   = %s", dec1.coff.sizeof);
if (!__ctfe) writefln("dec1.digits.sizeof = %s", dec1.digits.sizeof);

if (!__ctfe) writefln("D16.sizeof = %s", D16.sizeof);
if (!__ctfe) writefln("D16.alignof = %s", D16.alignof);

  separator;

}
}

void writeline(string str, int before = 0, int after = 0)
{
  while (before != 0)
  {
    writeln;
    before--;
  }
  writeln(str);
  while (after > 0)
  {
    writeln;
    after--;
  }
}

void writeline(string fmt, string str, int before = 0, int after = 0)
{
  while (before != 0)
  {
    writeln;
    before--;
  }
  writeln(format(fmt, str));
  while (after > 0)
  {
    writeln;
    after--;
  }
}

void header(string str, int before = 1, int after = 0)
{
  writeline("-- %s --", str, before, after);
}

void title(string str, int before = 1, int after = 0)
{
  writeline(format("=== %s ===", str), before, after);
}

void comment(string str, int before = 0, int after = 0)
{
  writeline(format("// %s", str), before, after);
}

void code(string str, int before = 0, int after = 0)
{
  writeline(format("[%s]", str), before, after);
}

void separator(int before = 1, int after = 0)
{
  writeline("-----------------------------------------", before, after);
}


