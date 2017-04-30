import std.bitmanip;
import std.math;
import std.stdio;
import std.string;
import std.traits;

import eris.decimal;
import eris.decimal.conv;
import eris.decimal.rounding;
import eris.decimal.logical;
import eris.decimal.arithmetic;
import eris.test.assertion;
import eris.decimal.test;

version(unittest)
{
	void main()
	{
		writeln("Hello, world!");
	}
}
else
{
/*
	struct FloatRep
	{
    	union
    	{
        	float value;
        	mixin(bitfields!(
                  	uint,  "fraction", 23,
                  	ubyte, "exponent",  8,
                  	bool,  "sign",      1));
    	}
    	enum uint bias = 127, fractionBits = 23, exponentBits = 8, signBits = 1;
	}

	struct DoubleRep
	{
    	union
    	{
        	double value;
        	mixin(bitfields!(
                  	ulong,   "fraction", 52,
                  	ushort,  "exponent", 11,
                  	bool,    "sign",      1));
    	}
    	enum uint bias = 1023, signBits = 1, fractionBits = 52, exponentBits = 11;
	}
*/

	private enum struct RealRep
	{
		union
		{
			real value;
			struct
			{
				ulong fraction;
				mixin(std.bitmanip.bitfields!(
					ushort,  "exponent", 15,
					bool,    "sign",      1));
			}
		}
		enum uint bias = 16383, signBits = 1, exponentBits = 15,
				integerBits = 1, fractionBits = 63;
	}

	void main()
{

	/// Constructs a decimal number from a real value.
	D64 convert(T)(T r) if (isFloatingPoint!T)
	{
		if (!__ctfe) writefln("r = %s", r);

		static if (T.sizeof == 10)	// 80-bit real
		{
			RealRep rep;
		}
		else static if (T.sizeof == 8)	// 64-bit double
		{
			std.bitmanip.DoubleRep rep;
		}
		else static if (T.sizeof == 4)	// 32-bit float
		{
			std.bitmanip.FloatRep rep;
		}
		else // Shouldn't reach here
		{
			// always works but it's slow
			string str = format("%.20G", r);
			D64(str);
		}

		// finite numbers
		if (std.math.isFinite(r))
		{
			if (r == 0.0)
			{
				return D64.zero(r < 0.0);
			}
			else if (std.math.fabs(r) == 1.0)
			{
				return D64.one(r < 0.0);
			}
			const T maxInt = std.math.scalbn(1.0, rep.fractionBits + 1);
			real i;
			T frac = modf(r, i);
			if (frac == 0.0 && i < maxInt)
			{
//				writeln("integer");
				rep.value = r;
				bigint f = bigint(1) << rep.fractionBits | rep.fraction;
				int e = rep.exponent - rep.bias;
				bigint val = f >> (rep.fractionBits - e);
				D64 dec = D64(val, 0, rep.sign);
				return dec; //.reduce;
			}
			else
			{
				writeln("string");
				string str = format("%.20G", r);
				return D64(str);
			}
		}
		// special values
		else if (std.math.isInfinity(r))
		{
			return D64.infinity(r < 0.0);
		}
		else
		{
			return D64.nan;
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
	D64 dec;
	D64 dec1;
	D64 dec2;
	D64 dec3;

	separator;

	title("package.d");
	header("construction");

	// this(decimal)
	dec = D64(127435,-34,true);
	writefln("dec = %s", dec);
	writefln("D64(dec) = %s", D64(dec));

	// this(coefficient)
	writefln("D64(198) = %s", D64(198));
	writefln("D64(-623) = %s", D64(-623));
	// this(coefficient, expo)
	writefln("D64(3,5) = %s", D64(3,5));
	// this(coefficient, expo, sign)
	writefln("D64(12,-3,true) = %s", D64(12,-3,true));
	// this(real)
	writefln("D64(12.3E6) = %s", D64(12.3E6));
	dec = D64(12.3E6);
	writefln("dec = %s", dec);
	writefln("dec.toAbstract = %s", dec.toAbstract);
	writefln("dec.toFull = %s", dec.toFull);
	writefln("dec.toScientific = %s", dec.toScientific);
	writefln("dec.toEngineering = %s", dec.toEngineering);

	// this(string)
	writefln("D64(\"1.23E6\") = %s", D64("1.23E6"));
	dec = D64("12.3E6");
	writefln("dec = %s", dec);
	writefln("dec.toAbstract = %s", dec.toAbstract);
	writefln("dec.toScientific = %s", dec.toScientific);
	writefln("dec.toEngineering = %s", dec.toEngineering);
	// this(bool)
	writefln("D64(true) = %s", D64(true));
	writefln("D64(false) = %s", D64(false));

	header("member properties");
	dec = D64(1259, 12, true);
	writefln("dec = %s", dec);
	writefln("dec.coefficient = %s", dec.coefficient);
	writefln("dec.coefficient(2360) = %s", dec.coefficient(2360));
	writefln("dec = %s", dec);
	writefln("dec.exponent = %s", dec.exponent);
	writefln("dec.exponent(-98) = %s", dec.exponent(-98));
	writefln("dec = %s", dec);
	writefln("dec.sign = %s", dec.sign);
	writefln("dec.sign(false) = %s", dec.sign(false));
	writefln("dec = %s", dec);
	writefln("dec.adjustedExponent = %s", dec.adjustedExponent);
	writefln("dec.getDigits = %s", dec.getDigits);

	header("floating point properties");
	// also tests this(special value, sign)
	writefln("D64.init = %s", D64.init);
	writefln("D64.infinity = %s", D64.infinity);
	writefln("D64.nan = %s", D64.nan);
	writefln("D64.snan = %s", D64.snan);
	writefln("D64.precision = %s", D64.precision);
	writefln("D64.epsilon = %s", D64.epsilon);
	writefln("D64.maxExpo = %s", D64.maxExpo);
	writefln("D64.minExpo = %s", D64.minExpo);
	writefln("D64.max = %s", D64.max);
	writefln("D64.min = %s", D64.min);
	writefln("D64.min_normal = %s", D64.min_normal);
    writefln("D64.radix = %s", D64.radix);

	header("constants");
	writefln("D64.ONE = %s", D64.ONE);
	writefln("D64.TWO = %s", D64.TWO);
	writefln("D64.TEN = %s", D64.TEN);
	writefln("D64.HALF = %s", D64.HALF);
	writefln("D64.PI = %s", D64.PI);
    writefln("D64.E = %s", D64.E);
	writefln("D64.LN2 = %s", D64.LN2);
	comment("note extra precision");
	writefln("D64.REAL_MAX = %s", D64.REAL_MAX);

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
	dec = D64(3069,25,false);
	code("dec = D64(3069,25,false)");
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

//	writefln("dec1 & dec2 = %s", dec1 & dec2);
//	writefln("dec1 | dec2 = %s", dec1 | dec2);
//	writefln("dec1 ^ dec2 = %s", dec1 ^ dec2);

	header("obBinaryRight");
	header("opOpAssign");

	title("rounding.d");
	header("decimal digits");
	bigint big = "0xFA2988_18830DD_658889AB_EFDCA45B_20933ABC_54AECCD0";
	writefln("big = %X", big);
	auto len = big.ulongLength;
	writefln("big.ulongLength = %X", big.ulongLength);
	writefln("ulongDigit(big, 3) = %X", ulongDigit(big, 3));
	writefln("ulongDigit(big, 2) = %X", ulongDigit(big, 2));
	writefln("ulongDigit(big, 1) = %X", ulongDigit(big, 1));
	writefln("ulongDigit(big, 0) = %X", ulongDigit(big, 0));
	writeln;
	writefln("big.uintLength = %X", big.uintLength);
	writefln("uintDigit(big, 3) = %X", uintDigit(big, 3));
	writefln("uintDigit(big, 2) = %X", uintDigit(big, 2));
	writefln("uintDigit(big, 1) = %X", uintDigit(big, 1));
	writefln("uintDigit(big, 0) = %X", uintDigit(big, 0));
	writeln;
	writefln("big = %s", big);
	writefln("truncDigits(big) = %s", truncDigits(big));
	writefln("firstDigit(big) = %s", firstDigit(big));
	writefln("lastDigit(big) = %s", lastDigit(big));
	writeln;
/*	writefln("ulongDigit2(big, 3) = %X", ulongDigit2(big, 3));
	writefln("ulongDigit2(big, 2) = %X", ulongDigit2(big, 2));
	writefln("ulongDigit2(big, 1) = %X", ulongDigit2(big, 1));
	writefln("ulongDigit2(big, 0) = %X", ulongDigit2(big, 0));*/



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


