// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2009 - 2014.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.integer.digits;

import std.stdio;
import std.algorithm: reverse;
import std.random: uniform;

import eris.integer.exception;
//import std.stdio;

unittest {
	writeln("==========================");
	writeln("digits module........begin");
	writeln("==========================");
}

version(unittest) {
	import std.stdio;
	import eris.test.assertion;
}

	public static enum ulong BASE = 1UL << 32;

	alias digit = uint;

	//---------------------------------------------------------
	// NOTES:
	//---------------------------------------------------------
	//
	// 	This module contains functions that operate on arrays of unsigned
	//	32-bit integers (uints). The arrays represent the digits of longer
	//	integers, such as 64-, 128-, or 256-bit unsigned integers.
	//
	//	The digits are stored in increasing index value:
	//		the index of the least significant digit is 0,
	//		the index of the most significant digit is the array length - 1.
	//
	//	For example, the
	//
	//	This is computationally efficient since leading zeros can be ignored,
	//	but because we're used to seeing numbers that read  left-to-right
	//	it is easy to forget, especially when trying to create a number
	//	from a list of digits.
	//
	//---------------------------------------------------------
	//
	//	Most of the arithmetic functions have two signatures, one of which
	//	explicitly requires the number of digits in the operands as input.
	//
	//	If the number of digits in the operands is known the explicit functions
	//	can be called. If not, the implicit functions simply count the digits
	//	of the operand(s) and then call the explicit functions.
	//
	//	Counting the digits is a fast operation and should make the functions
	//	run faster, since operating on unnecessary digits is slower than counting.
	//
	// 	If the operands have many digits there is less time saved,
	//	but in that case the counting function also takes less time.
	//
	//---------------------------------------------------------
	//
	//	Algorithms for many of the arithmetic operations
	//	used here are adapted from:
	//
	//	(1)	Modern Computer Arithmetic,
	//		Richard Brent and Paul Zimmermann,
	//		Cambridge University Press, 2010.
	//
	//	An early version of this work (Version 0.5.9) can be found online at:
	//
	//		http://www.loria.fr/~zimmerma/mca/mca-cup-0.5.9.pdf
	//
	//	and:
	//
	//	(2)	Handbook of Applied Cryptography,
	//		A. Menezes, P. van Oorschot, and S. Vanstone,
	//		 CRC Press, 1996.
	//
	//	An online copy of Chapter 14, Efficient Implementation, from this work
	//  can be found online at:
	//
	//		http://cacr.uwaterloo.ca/hac/about/chap14.pdf
	//
	//	The algorithms for division and modulus are further adapted from:
	//
	//	(3)	P. Brinch Hansen,
	//		Multiple-length division revisited: A tour of the minefield.
	//		Software Practice and Experience 24, (June 1994), 579-601.
	//		Copyright 1994, John Wiley & Sons, Ltd.
	//
	//	An online copy of this document can be found at:
	//
	//		http://brinch-hansen.net/papers/1994b.pdf
	//
	//---------------------------------------------------------

//--------------------------------
// string functions
//--------------------------------

	// Returns a hexadecimal string representation of an array of digits.
	public string toString(in digit[] digits) {
		char[] str;
		if (digits.length == 0) {
			return "0x0";
		}
		else foreach_reverse (i, digit d; digits) {
			str ~= std.string.format("%08X", d);
			if (i) str ~= '_';
		}
		return ("0x" ~ str).idup;
	}

	// Returns a decimal string representation of an array of digits.
	public string toDecimal(in digit[] digits, uint spacing = 0) {
		size_t n = numDigits(digits);
		if (n == 0) return "0";
		char[] buff;
		auto app = std.array.appender(buff);
		app.reserve(100);
		auto from = digits[0..n].dup;
		uint count = 0;
		while (n > 0) {
			uint mod;
			from = divmodDigit(from, n, 10, mod);
			char ch = cast(char)(mod + '0');
			app.put(ch);
			// is this really faster? have to do a lot of shifts??
			size_t m = numDigits(from);
			if (m < n) {
				from = from[0..m];
				n = m;
			}
			if (spacing) {
				count++;
				if (count == spacing) {
					app.put('_');
					count = 0;
				}
			}
		}
		reverse(app.data);
		return app.data.idup;
	}

	unittest {
		write("-- toString.........");
		digit[] digits = [123, 456, 789];
		assertEqual("0x00000315_000001C8_0000007B", toString(digits));
		digits = [0xFFFFFFFF, 0x7FFFFFFF];
		assertEqual("9223372036854775807", toDecimal(digits));
		assertEqual("9_223_372_036_854_775_807", toDecimal(digits, 3));
		digits = [];
		assertEqual("0x0", toString(digits));
		digits = [0];
		assertEqual("0x00000000", toString(digits));
		writeln("passed");
	}

//--------------------------------
// comparison
//--------------------------------

	/// Compares two arrays of digits.
	/// Returns -1, 0, or 1 if the first array is, respectively,
	/// smaller than, equal to or larger than the second.
	@safe
	public int compareDigits(in digit[] a, in digit[] b) {
		return compareDigits(a, numDigits(a), b, numDigits(b));
	}

	/// Compares two arrays of digits.
	/// Returns -1, 0, or 1 if the first array is, respectidvely,
	/// smaller than, equal to or larger than the second.
	@safe
	private int compareDigits(in digit[] a, size_t na, in digit[] b, size_t nb) {

		// if lengths differ just compare lengths
		if (na < nb) return -1;
		if (na > nb) return +1;

		// same length; return the first difference
		for (ptrdiff_t  i = nb-1; i >= 0; i--) {
			if (a[i] < b[i]) return -1;
			if (a[i] > b[i]) return +1;
		}
		// no differences; return 0
		return 0;
	}

	/// Compares an array of digits to a single digit.
	/// Returns -1, 0, or 1 if the first argument is, respectively,
	/// smaller than, equal to or larger than the first.
	@safe
	public int compareDigits(in digit[] a, in digit k) {
		if (numDigits(a) > 1) return +1;
		if (a[0] < k) return -1;
		if (a[0] > k) return +1;
		return 0;
	}

	unittest {	// comparison
		write("-- comparison.......");
		digit[] a, b;
		int c;
		a = [5];
		b = [6];
		assertTrue(compareDigits(a, b) < 0);
		a = [4, 3, 2, 0];
		b = [4, 3, 2];
		assertTrue(compareDigits(a, b) == 0);
		b = [4, 3, 2, 1];
		assertTrue(compareDigits(a, b) < 0);
		a = [5, 3, 2, 1];
		assertTrue(compareDigits(a, b) > 0);
		a = [57];
		c = 59;
		assertTrue(compareDigits(a, c) < 0);
		c = 57;
		assertTrue(compareDigits(a, c) == 0);
		c = 29;
		assertTrue(compareDigits(a, c) > 0);
		writeln("passed");
	}

//--------------------------------
// utilities
//--------------------------------

	/// Returns the number of digits in the array.
	/// Leading zero digits are not counted.
	/// If all digits are zero, returns 0.
	@safe
	public size_t numDigits(in digit[] digits)
	{
        // special cases --
		size_t len = digits.length;
		// check for zero length
		if (len == 0) return 0;
		// check for single digit
		if (len == 1) return digits[0] ? 1 : 0;
		// check for no leading zeros
		if (digits[$-1]) return len;
		// check for one leading zero
		if (digits[$-2]) return len-1;

		// loop
		size_t count;
		for (count = len - 2; count > 0; count--)
		{
			if (digits[count-1]) break;
		}
		return count;
	}

	unittest {	// numDigits
		write("-- numDigits........");
		digit[] array;
		array = [1, 2, 3, 5];
		assertEqual(numDigits(array), 4);
		array = [1, 2, 3, 0];
		assertEqual(numDigits(array), 3);
		array = [1, 0, 0, 0, 0];
		assertEqual(numDigits(array), 1);
		array = [0, 0, 0, 0, 0];
		assertEqual(numDigits(array), 0);
		writeln("passed");
	}

	/// Copies the significant digits of an array to another array.
	/// Returns the number of significant digits in the first array.
	@safe
	public size_t copyDigits(in digit[] x, out digit[] y) {
		size_t nx = numDigits(x);
		y = x[0..nx].dup;
		return nx;
	}

	/// Returns a copy of the array with leading zeros removed.
	@safe
	public digit[] trimDigits(in digit[] x) {
		size_t nx = numDigits(x);
		return x[0..nx].dup;
	}

	unittest {
		write("-- copy, trim.......");
		digit[] array;
		digit[] copy;
		array = [1, 2, 3, 5];
		assertEqual(copyDigits(array, copy), 4);
		assertEqual(copy, array);
		assertEqual(trimDigits(array), array);
		array = [1, 2, 3, 0];
		assertEqual(copyDigits(array, copy), 3);
		assertEqual(copy, [1, 2, 3]);
		assertEqual(trimDigits(array), [1, 2, 3]);
		writeln("passed");
	}

	/// Extends the array to the specified length, padding with zeros.
	/// If the length of the array is already equal to or greater than
	/// the specified length, the array is not modified.
	@safe
	public digit[] extend(ref digit[] digits, int n) {
		if (digits.length < n) {
			digits.length = n;
		}
		return digits;
	}

	/// Extends the array to the specified length.
	/// If the length of the array is already equal to or greater than
	/// the specified length, the array is not modified.
	/// If the first bit of the array is one,
	///	the array is padded with ones, otherwise it is padded with zeros.
	@safe
	public digit[] extendSigned(ref digit[] digits, size_t n) {
		if (digits.length < n) {
			bool signed = digits.isNegative;
			size_t m = digits.length;
			digits.length = n;
			if (signed) {
				for (ptrdiff_t i = m; i < n; i++) {
					digits[i] = uint.max;
				}
			}
		}
		return digits;
	}

	/// Removes leading zeros.
	@safe
	public digit[] reduce(ref digit[] digits) {
		size_t n = numDigits(digits);
		if (n == 0) digits.length = 1;
		else if (n < digits.length) {
			digits.length = n;
		}
		return digits;
	}

	/// Removes leading sign digits.
	/// If the first bit of the array is one,
	/// leading digits equal to 0xFFFFFFFF are removed.
	///	Otherwise, leading zeros are removed.
	@safe
	public digit[] reduceSigned(ref digit[] digits) {
		if (digits.length > 1) {
			if (!digits.isNegative) return reduce(digits);
			int count = 0;
			foreach_reverse(i, digit d; digits) {
				if (i == 0 || d != uint.max) {
					if (d <= int.max) {
						count--;
					}
					break;
				}
				count++;
			}
			digits.length -= count;
		}
		return digits;
	}

	unittest {
		write("-- reduce, extend...");
		digit[] array, copy;
		array = [1, 2, 3, -5];
		extend(array, 6);
		assertEqual(array, [1, 2, 3, -5u, 0, 0]);
		reduce(array);
		assertEqual(array, [1, 2, 3, -5u]);
		assertTrue(isNegative(array));
		extendSigned(array, 6);
		assertEqual(array, [1, 2, 3, -5u, uint.max, uint.max]);
		array = [uint.max, uint.max, uint.max];
		reduceSigned(array);
		assertEqual(array, [uint.max]);
		array = [int.max, uint.max, uint.max];
		reduceSigned(array);
		assertEqual(array, [int.max, uint.max]);
		array = [uint.max, 0, uint.max];
		reduceSigned(array);
		assertEqual(array, [uint.max, 0, uint.max]);
		array = [0, int.max + 1, uint.max];
		reduceSigned(array);
		assertEqual(array, [0, int.max + 1]);
		array = [4, uint.max, uint.max, uint.max];
		reduceSigned(array);
		assertEqual(array, [4, uint.max]);
		writeln("passed");
	}

	/// Extends the shorter of the two arrays.
	@safe
	public void matchLength(ref digit[] x, ref digit[] y) {
		size_t nx = x.length;
		size_t ny = y.length;
		if (nx == ny) return;
		if (nx > ny) y.length = nx;
		else x.length = ny;
	}

	/// Extends the shorter of the two arrays.
	@safe
	public void matchLengthSigned(ref digit[] x, ref digit[] y) {
		size_t nx = x.length;
		size_t ny = y.length;
		if (nx == ny) return;
		if (nx > ny) extendSigned(y, nx);
		else extendSigned(x, ny);
	}

	unittest {
		write("-- matchLength......");
		digit[] x1, x2, y1, y2;
		x1 = [1, 2, 3, 5];
		x2 = x1 ~ [0u, 0u];
		y1 = [5, 8, 1, 2, 3, 5];
		y2 = y1;
		matchLength(x1, y1);
		assertEqual(y1, y2);
		assertEqual(x1, x2);
		x1 = [1, 2, 3, 4, 5, 6, 7];
		x2 = x1;
		y1 = [5, 8, 1, 2, 3, cast(uint)-5];
		y2 = y1 ~ [uint.max];
		matchLengthSigned(x1, y1);
		assertEqual(y1, y2);
		assertEqual(x1, x2);
		writeln("passed");
	}

	/// Returns the ones complement of an array.
	@safe
	public digit[] compDigits(in digit[] digits, int n = -1) {
		if (n < 0) {
			n = cast(int)digits.length;
		}
		auto comp = new digit[n];
		for (int i = 0; i < n; i++) {
			comp[i] = ~digits[i];
		}
		return comp;
	}

	/// Returns the twos complement of an array.
	@safe
	public digit[] negateDigits(in digit[] digits, int n = -1) {
		// zero is its own complement
		if (isZero(digits)) {
			return digits.dup;
		}
		auto com = compDigits(digits, n);
		auto neg = addDigit(com, 1);
		return reduce(neg);
	}

	unittest {
		write("-- complement.......");
		digit[] a = [2, 12, 4];
		assertEqual(compDigits(a), [4294967293, 4294967283, 4294967291]);
		assertEqual(trimDigits(negateDigits(a)), [4294967294, 4294967283, 4294967291]);
		writeln("passed");
	}

//--------------------------------
// digit pack/unpack methods
//--------------------------------

	/// Returns the last 32 bits of a 64-bit unsigned integer
	@safe
	public digit low(ulong nn) {
		return nn & 0xFFFFFFFFUL;
	}

	/// Returns the first 32 bits of a 64-bit unsigned integer
	@safe
	public digit high(ulong nn) {
		return (nn & 0xFFFFFFFF00000000UL) >> 32;
	}

	/// Packs two 32-bit unsigned integers into a 64-bit unsigned integer
	/// and returns the 64-bit integer.
	@safe
	public ulong pack(in digit hi, in digit lo) {
		ulong packed = (cast(ulong) hi) << 32;
		return packed |= lo;
	}

//--------------------------------
// bit manipulation
//--------------------------------

	import std.bitmanip: BitArray;

	// Copies the digit array to a std.bitmanip.BitArray and returns
	// the bit array.
	public BitArray toBitArray(in digit[] a) {
		digit[] copy = a.dup;
        BitArray ba = BitArray(cast(void[])copy, copy.length*32);
		return ba;
	}

	unittest {
		write("-- bit manipulation.");
		digit[] a = [10];
		BitArray ba = toBitArray(a);
		bool[] t = new bool[32];
		t[1] = true;
		t[3] = true;
		BitArray bb = BitArray(t);
		assertEqual(ba, bb);
		writeln("passed");
	}

//--------------------------------
// classification
//--------------------------------

	/// Returns true if the array represents a zero value.
	/// That is, if there are no non-zero digits in the array.
	@safe
	public bool isZero(in digit[] a) {
		return numDigits(a) == 0;
	}

 	/// Returns true if the parity bit
	/// (the last bit of the last digit) is a 1,
	/// false otherwise.
	@safe
	public bool isOdd(in digit[] a) {
		if (a.length == 0) return false;
		return a[0] & 1u;
	}

 	/// Returns true if the parity bit
	/// (the last bit of the last digit) is a 0,
	/// false otherwise.
	@safe
	public bool isEven(in digit[] a) {
		return !isOdd(a);
	}

	/// Returns true if the sign bit of the array
	/// (the first bit of the first digit) is a 1,
	/// false otherwise.
	@safe
	public bool isNegative(in digit[] a) {
		return cast(int)a[$-1] < 0;
	}

	unittest {
		write("-- classification...");
		digit[] a = [7];
		assertTrue(isOdd(a));
		a = [7, 3, 0];
		assertTrue(isOdd(a));
		a = [0, 0, 0];
		assertTrue(isEven(a));
		a = [0, 0, 15];
		assertTrue(isEven(a));
		a = [64, 0, 15];
		assertTrue(isEven(a));
		a = [64, 0, 0];
		assertTrue(isEven(a));
		a = [63, 0, 0];
		assertTrue(isOdd(a));
		a = [1, 1, -1];
		assertTrue(isNegative(a));
		a = [0xFF00FF00];
		assertTrue(isNegative(a));
		writeln("passed");
	}

//--------------------------------
// addition and subtraction
//--------------------------------

	/// Returns the sum of two arrays of digits.
	@safe
	public digit[] addDigits(in digit[] x, in digit[] y) {
		size_t nx = numDigits(x);
		size_t ny = numDigits(y);
		if (nx >= ny)
		{
			return addDigits(x, nx, y, ny);
		}
		else
		{
			return addDigits(y, ny, x, nx);
		}
	}

	/// Returns the sum of two digit arrays with specified lengths.
	@safe
	public digit[] addDigits(
			in digit[] x, size_t nx, in digit[] y, size_t ny) {

		// special cases
		if (nx == 0) return y.dup;
		if (nx == 1) return addDigit(y, ny, x[0]);
		if (ny == 0) return x.dup;
		if (ny == 1) return addDigit(x, nx, y[0]);

		digit[] sum = new digit[nx + 1];
		digit carry = 0;
		ulong temp = 0;
		int i = 0;
		while (i < ny) {
			temp = cast(ulong)x[i] + cast(ulong)y[i] + carry;
			sum[i] = low(temp);
			carry = high(temp);
			i++;
		}
		while (carry && i < nx) {
			temp = cast(ulong)x[i] + carry;
			sum[i] = low(temp);
			carry = high(temp);
			i++;
		}
		sum[i..nx] = x[i..nx];
		if (carry) {
			sum[i] = carry;
		}
		return sum;
	}

	/// Returns the sum of an array of digits and a single digit.
	@safe
	public digit[] addDigit(in digit[] x, in digit y) {
		return addDigit(x, numDigits(x), y);
	}

	/// Returns the sum of an array of digits and a single digit.
	@safe
	public digit[] addDigit(in digit[] x, size_t nx, in digit y) {

		if ( y == 0) return x.dup;
		if (nx == 0) return [y];

		digit[] sum = new digit[nx + 1];
		ulong temp = cast(ulong)x[0] + cast(ulong)y;
		sum[0] = low(temp);
		digit carry = high(temp);
		int i = 1;
		while (carry && i < nx) {
			temp = cast(ulong)x[i] + carry;
			sum[i] = low(temp);
			carry = high(temp);
			i++;
		}
		sum[i..nx] = x[i..nx];
		if (carry == 1) {
			sum[i] = carry;
		}
		return sum;
	}

	unittest {
		write("-- addition.........");
		digit[] x, y, z, sum;
		x = [2];
		y = [3, 4, 5];
		sum = addDigits(x, y);
		reduce(sum);
		assertEqual(sum, [5, 4, 5]);
		x = [1, 2];
		y = [3, 4, 5];
		sum = addDigits(x, y);
		reduce(sum);
		assertEqual(sum, [4, 6, 5]);
		x = [1, 2, 3, 4, 5];
		y = [6, 7, 8, 9, 10];
		sum = addDigits(x, y);
		reduce(sum);
		assertEqual(sum, [7, 9, 11, 13, 15]);
		x.length = 4;
		sum = addDigits(x,y);
		reduce(sum);
		assertEqual(sum, [7, 9, 11, 13, 10]);
		sum = addDigit(x, 7);
		reduce(sum);
		assertEqual(sum, [8, 2, 3, 4]);
		extend(x, 7);
		sum = addDigits(x,y);
		reduce(sum);
		assertEqual(sum, [7, 9, 11, 13, 10]);
		sum = addDigit(x, 7);
		reduce(sum);
		assertEqual(sum, [8, 2, 3, 4]);
		writeln("passed");
	}

	// Returns the difference between two arrays of digits.
	@safe
	public digit[] subDigits(in digit[] x, in digit[] y) {
		size_t nx = numDigits(x);
		size_t ny = numDigits(y);
		return subDigits(x, nx, y, ny);
	}

	// Returns the difference between two digit arrays with specified lengths.
	@safe
	public digit[] subDigits(
		in digit[] xin, size_t nx, in digit[] y, size_t ny) {

		if (ny == 1) return subDigit(xin, nx, y[0]);

		digit[] x = xin.dup;
		if (ny == 0) return x;
		if (ny > nx) {
			x.length = ny;
			nx = ny;
		}

  		digit[] diff = new digit[nx + 1];
		digit borrow = 0;
		int i = 0;
		while (i < ny) {
			diff[i] = x[i] - y[i] - borrow;
			borrow = (x[i] >= y[i] + borrow) ? 0 : 1;
			i++;
		}
		while (borrow && i < nx) {
			if (x[i] >= borrow) {
				diff[i] = x[i] - borrow;
				borrow = 0;
			}
			else {
				diff[i] = x[i] - borrow;
				borrow = 1;
			}
			i++;
		}
		diff[i..nx] = x[i..nx];
		if (borrow == 1) {
			diff[i] = borrow;
		}
		if (diff[nx] != 0) {
			diff[nx] = uint.max;
		}
		return diff;
	}

	/// Returns the difference between an array of digits and a single digit.
	@safe
	public digit[] subDigit(in digit[] x, in digit y) {
		return subDigit(x, numDigits(x), y);
	}

	/// Returns the difference of a digit array with specified length
	/// and a single digit.
	@safe
	public digit[] subDigit(
		in digit[] x, size_t nx, in digit y) {

  		digit[] diff = new digit[nx + 1];
		digit borrow = 0;
		diff[0] = x[0] - y;
		borrow = x[0] >= y ? 0 : 1;
		int i = 1;
		while (borrow && i < nx) {
			diff[i] = x[i] - borrow;
			borrow = (x[i] >= borrow) ? 0 : 1;
			i++;
		}
		diff[i..nx] = x[i..nx];
		if (borrow == 1) {
			diff[i] = borrow;
		}
		if (diff[nx] != 0) {
			diff[nx] = uint.max;
		}
		return diff;
	}

	unittest {
		write("-- subtraction......");
		digit[] x, y, z, diff;
		x = [1, 2];
		y = [6, 7, 8, 9, 10];
		diff = subDigits(y, x);
		reduce(diff);
		assertEqual(diff, [5, 5, 8, 9, 10]);
		x = [1, 2, 3, 4, 5];
		y = [6, 7, 8, 9, 10];
		diff = subDigits(y, x);
		reduce(diff);
		assertEqual(diff, [5, 5, 5, 5, 5]);
		diff = subDigits(x, y);
		reduce(diff);
		assertEqual(diff, [-5u, -6u, -6u, -6u, -6u, -1]);
		x.length = 4;
		diff = subDigits(x, y);
		reduce(diff);
		assertEqual(diff, [-5u, -6u, -6u, -6u, -11u, -1]);
		diff = subDigit(x, 7);
		reduce(x);
		reduce(diff);
		assertEqual(diff, [-6u, 1, 3, 4]);
		extend(x, 7);
		diff = subDigits(y,x);
		reduce(diff);
		assertEqual(diff, [5, 5, 5, 5, 10]);
		x = [0, 10];
		diff = subDigit(x, 7);
		reduce(diff);
		assertEqual(diff, [-7u, 9]);
		writeln("passed");
	}

//--------------------------------
// multiplication, squaring, exponentiation
//--------------------------------

	/// Returns the product of two arrays of digits.
	@safe
	public digit[] mulDigits(in digit[] x, in digit[] y) {
		size_t nx = numDigits(x);
		size_t ny = numDigits(y);
		return mulDigits(x, nx, y, ny);
	}

	/// Returns the product of the two digit arrays of specified lengths.
	@safe
	public digit[] mulDigits(
		in digit[] x, size_t nx, in digit[] y, size_t ny) {
		digit[] p = new digit[nx + ny + 1];
		p[] = 0;
		for (int i = 0; i < ny; i++) {
			ulong carry = 0;
			for (int j = 0; j < nx; j++) {
				ulong temp = carry + p[i+j]
					+ cast(ulong)(x[j]) * cast(ulong)(y[i]);
				carry = high(temp);
				p[i+j] = low(temp);
			}
			p[i+nx] = low(carry);
		}
		return p;
	}

	// TODO: when used for parsing a string, this method repeatedly allocates
	/// Returns the product of an array of digits and a single digit
	@safe
	public digit[] mulDigit(in digit[] x, in digit k) {
		size_t nx = numDigits(x);
		return mulDigit(x, nx, k);
	}

unittest {
	write("mulDigit...");
		digit[] x = [0x0,0x1];
		int nx = 2;
		digit k = 2147483648;
		x = mulDigit(x, nx, k);
	writeln("test missing");
}
	/// Returns the product of a digit array of specified length
	/// and a single digit
	@safe
	public digit[] mulDigit(in digit[] x, size_t nx, in digit k) {
		digit[] p = new digit[nx+1];
		ulong carry = 0;
		for (int i = 0; i < nx; i++) {
			ulong temp = cast(ulong)k * x[i] + carry;
			p[i] = low(temp);
			carry = high(temp);
		}
		p[nx] = low(carry);
		return p;
	}

	unittest {
		write("-- multiplication...");
		digit[] x, y, z, product;
		x = [0xFFFFFFFF, 0xFFFFFFFF];
		y = [0xFFFFFFFF, 0xFFFFFFFF];
		product = mulDigits(x, y);
		reduce(product);
		assertEqual(product, [1, 0, 0xFFFFFFFE, 0xFFFFFFFF]);
		x = [1, 2, 3, 4, 5];
		y = [6, 7, 8];
		product = mulDigits(x, y);
		reduce(product);
		assertEqual(product, [6, 19, 40, 61, 82, 67, 40]);
		x = [1, 2, 3, 4, 5];
		y = [6, 7, 8, 9, 10];
		product = mulDigits(x, y);
		reduce(product);
		assertEqual(product, [6, 19, 40, 70, 110, 114, 106, 85, 50]);
		product = mulDigit(x, 7);
		reduce(product);
		assertEqual(product, [7, 14, 21, 28, 35]);
		writeln("passed");
	}

	/// Returns the ulong product of two uints.
	@safe
	public ulong longMul(in digit x, in digit y) {
		return cast(ulong)x * cast(ulong)y;
	}

	/// Returns the square of an array of digits.
	@safe
	public digit[] sqrDigits(in digit[] x) {
		size_t nx = numDigits(x);
		digit[] sqrx = new digit[2*nx];
		ulong overflow = 0;
		for (int i = 0; i < nx; i++) {
			ulong inner = sqrx[2*i]	+ longMul(x[i], x[i]) + overflow;
			ulong carry = high(inner);
			sqrx[2*i] = low(inner);
			for (int j = i+1; j < nx; j++) {
				ulong temp = longMul(x[j], x[i]);
				overflow = temp & 0x8000_0000_0000_0000 ? 0x010000_0000 : 0;
				inner = carry + sqrx[i+j] + (temp << 1);
				carry = high(inner);
				sqrx[i+j] = low(inner);
			}
			sqrx[i+nx] = low(carry);
		}
		return sqrx;
	}

	unittest {	// squaring
		write("-- squaring.........");
		digit[] array, square;
		array = [0xF4DEF769, 0x941F2754];
		square = sqrDigits(array);
		assertEqual(square, [0x3137C911, 0xDF5C24BA, 0xC7C01ADE, 0x55B40944]);
		array = [0xFFFFFFFF, 0xFFFFFFFF];
		square = sqrDigits(array);
		assertEqual(square, [1, 0, 0xFFFFFFFE, 0xFFFFFFFF]);
		array = [0xFF, 0xFF];
		square = sqrDigits(array);
		assertEqual(square, [0x0000FE01, 0x0001FC02, 0x0000FE01, 0x0]);
		writeln("passed");
	}

	/// Returns an array of digits raised to the specified power.
	@safe
	public digit[] powDigits(in digit[] base, int expo) {
		if (expo == 0) return [1];
		if (expo == 1) return base.dup;
		if (expo == 2) return sqrDigits(base);
		if (expo < 0) return [];
		digit[] a = [1];
		digit[] s = base.dup;
		while (expo > 0) {
			if (expo & 1) a = mulDigits(a, s);
			expo /= 2;
			if (expo > 0) s = sqrDigits(s);
		}
		return a;
	}

	unittest {
		write("-- exponentiation...");
		digit[] x = [0xA];
		int y = 12;
		digit[] z = powDigits(x,y);
		digit[] b = [0xC2BA7913U];
		digit e = 4;
		digit[] p = powDigits(b, e);
		digit[] t = [1];
		for (int i = 0; i < e; i++) {
			t = mulDigits(t, b);
		}
		assertZero(compareDigits(p,t));

		for (int j = 0; j < 10; j++) {
			b = randomDigits(1);
			e = 7;
			p = powDigits(b, e);
			t = [1];
			for (int i = 0; i < e; i++) {
				t = mulDigits(t, b);
			}
		}
		assertZero(compareDigits(p,t));

		digit[] array = [0xF4DEF769, 0x941F2754];
		array = powDigits(array, 2);
		assertEqual(array, [0x3137C911, 0xDF5C24BA, 0xC7C01ADE, 0x55B40944]);
		writeln("passed");
	}


//--------------------------------
// division and modulus
//--------------------------------

	/// Returns the quotient of the first array of digits divided by the second.
	@safe
	public digit[] divmodDigits(
		in digit[] xin, in digit[] yin, out digit[] mod) {
		// mutable copies
		digit[] x, y;
		size_t nx = copyDigits(xin, x);
		size_t ny = copyDigits(yin, y);
        if (ny == 0) throw new Exception("division by zero");
		// special cases
		if (nx == 0)  {
			mod = [0];
			return [0];
		}
		if (ny == 1) {
			return divmodDigit(x, nx, y[0], mod[0]);
		}
		return divmodDigits(x, numDigits(x), y, numDigits(y), mod);
	}

	/// Returns the quotient of the first array of digits of specified length
	/// divided by the second.
	/// Preconditions: y != 0
	@safe
	public digit[] divmodDigits(
		in digit[] xin, size_t nx, in digit[] yin, size_t ny, out digit[] mod) {


		if (ny == 1) {
			digit m;
			digit[] q = divmodDigit(xin, nx, yin[0], m);
			mod = [m];
			return q;
		}
		if (xin == yin || (nx == ny && xin[0..nx-1] == yin[0..ny-1])) {
			mod = [0];
			return [1];
		}
		digit[] x = xin.dup;
		digit[] y = yin.dup;

		// normalize the operands
		digit k = divDigit([0u, 1u], y[ny-1]+1)[0];
		if (k != 1) {
			x = mulDigit(x, nx, k);
			nx = numDigits(x);
			y = mulDigit(y, ny, k);
			ny = numDigits(y);
		}

		digit[] q = new digit[nx-ny+1];
		digit[] ys = shlDigits(y, nx-ny);
		while (compareDigits(x,ys) > 0) {
			x = subDigits(x, ys);
		}
        nx = numDigits(x);
		for (ptrdiff_t i = nx-1; i >= ny; i--) {
			ptrdiff_t ix = i-ny;
			if (x[i] == y[ny-1]) {
				q[ix] = digit.max;
			}
			else {
				q[ix] = divDigit(x[i-1..i+1], 2U, y[ny-1])[0];
			}
			digit[] yq = mulDigit(y[ny-2..ny], 2U, q[ix]);
			while ((compareDigits(yq, x[i-2..i+1])) > 0) {
				q[ix]--;
				yq = subDigits(yq, y[ny-2..ny]);
			}
			digit[] yb = shlDigits(y, i-ny);
			digit[] xs = mulDigit(yb, i, q[ix]);
			if (compareDigits(x, xs) < 0) {
				q[ix]--;
				xs = subDigits(xs, yb);
			}
			x = subDigits(x, xs);
		}
		mod = k == 1 ? x : divDigit(x, k);
		return q;
	}

	/// Returns the quotient of an array of digits divided by a single digit.
	@safe
	public digit[] divmodDigit(
			in digit[] x, size_t nx, digit k, out digit mod) {
		if (nx == 0) {
			mod = 0;
			return [0];
		}
		if (k == 1) {
			mod = 0;
			return x.dup;
		}
		if (k == 0) throw new Exception("division by zero");

		digit[] q = x.dup;
		ulong carry = 0;
		for (ptrdiff_t i = nx-1; i >= 0; i--) {
			ulong temp = carry * BASE + x[i];
			q[i] = low(temp / k);
			carry = temp % k;
		}
		mod = cast(digit)carry;
		return q;
	}

	/// Returns the quotient of the first array of digits divided by the second.
	@safe
	public digit[] divDigits(in digit[] xin, in digit[] yin) {
		// mutable copies
		digit[] x, y, mod;
		size_t nx = copyDigits(xin, x);
		size_t ny = copyDigits(yin, y);
		// special cases
		if (nx == 0) return [0];
        if (ny == 0) throw new Exception("division by zero");
		if (ny == 1) return divDigit(x, nx, y[0]);
		// divide arrays
		return divmodDigits(x, nx, y, ny, mod);
	}

	/// Returns the quotient of the first array of digits of specified length
	/// divided by the second.
	/// Preconditions: y != 0
	@safe
	public digit[] divDigits(ref digit[] x, size_t nx, ref digit[] y, size_t ny) {
		if (nx == 0) return [0];
        if (ny == 0) throw new Exception("division by zero");
		if (ny == 1) return divDigit(x, nx, y[0]);
		digit [] mod;
		return divmodDigits(x, nx, y, ny, mod);
	}

       /// Returns the quotient of an array of digits divided by a single digit
       /// Preconditions: k != 0
       @safe
       public digit[] divDigit(in digit[] x, digit k) {
           return divDigit(x, numDigits(x), k);
       }

	/// Returns the quotient of the first array of digits of specified length
	/// divided by a single digit.
	/// Preconditions: k != 0
	@safe
	public digit[] divDigit(in digit[] x, size_t nx, digit k) {
		if (nx == 0) return [0];
		if (k == 1) return x.dup;
		if (k == 0) throw new Exception("division by zero");
		digit[] q = x.dup;
		ulong carry = 0;
		for (int i = (cast(int)nx) - 1; i >= 0; i--) {
			ulong temp = carry * BASE + x[i];
			q[i] = low(temp / k);
			carry = temp % k;
		}
		return q;
	}

	/// Returns the remainder of the first array of digits divided by the second.
	/// Preconditions: y != 0
	@safe
	public digit[] modDigits(in digit[] xin, in digit[] yin) {
		// mutable copies
		digit[] x, y, mod;

		size_t nx = copyDigits(xin, x);
		if (nx == 0) return [0];

		size_t ny = copyDigits(yin, y);
        if (ny == 0) throw new Exception("division by zero");
		// divide arrays
		divmodDigits(x, nx, y, ny, mod);
		return mod;
	}

	/// Returns the remainder of the first array of digits of specified length
	/// divided by the second.
	/// Preconditions: y != 0
	@safe
	public digit[] modDigits(ref digit[] x, size_t nx, ref digit[] y, size_t ny) {
		digit [] mod;
		if (ny == 1) return [modDigit(x, nx, y[0])];
		divmodDigits(x, nx, y, ny, mod);
		return mod;
	}

	/// Returns the remainder of an array of digits divided by a single digit.
	/// Preconditions: k != 0
	@safe
	public digit modDigit(in digit[] x, digit k) {
		return modDigit(x, numDigits(x), k);
	}

	/// Returns the remainder of the first array of digits of specified length
	/// divided by a single digit.
	/// Preconditions: k != 0
	@safe
	public digit modDigit(in digit[] x, size_t nx, in digit k) {
		ulong carry = 0;
		for (ptrdiff_t i = nx-1; i >= 0; i--) {
			ulong temp = carry * BASE + x[i];
			carry = temp % k;
		}
		return cast(digit)carry;
	}

	unittest {
		write("-- division.........");
		digit[] input, output, divisor;
		digit k;
		input = [28, 20, 48, 76];
		output = divDigit(input, 2);
		assertEqual(output, [14, 10, 24, 38]);
		auto value = modDigit(input, 31);
		assertEqual(value, 5);
		input = [0,12];
		divisor = [0,4];
		output = divDigits(input, divisor);
		input = [1748779, 1677624962, 42872311, 3268450563];
		divisor = [174779, 167764962, 4282311, 326850563];
		output = divDigits(input, divisor);
		k = 0x3310566E;
		output = divDigit(input, k);
		assertEqual(output, [720498403, 2603564298, 3500933789, 3]);
		value = modDigit(input, k);
		assertEqual(value, 515229601);
		input = [2,1];
		divisor = [1,1];
		output = modDigits(input, divisor);
		reduce(output);
		assertEqual(output, [1]);
		writeln("passed");
	}

//--------------------------------
// logical operations
//--------------------------------

	/// Returns the logical and of two arrays of digits.
	/// If the arrays are of unequal lengths, the longer is truncated.
	@safe
	public digit[] andDigits(in digit[] a, in digit[] b) {
		size_t n = a.length;
		if (n > b.length) n = b.length;
		digit[] and = new digit[n];
		for (ptrdiff_t i = 0; i < n; i++) {
			and[i] = a[i] & b[i];
		}
		return and[];
	}

	/// Returns the logical or of two arrays.
	/// If the arrays are of unequal lengths, the longer is truncated.
	@safe
	public digit[] orDigits(in digit[] a, in digit[] b) {
		size_t n = a.length;
		if (n > b.length) n = b.length;
		digit[] or = new digit[n];
		for (ptrdiff_t i = 0; i < n; i++) {
			or[i] = a[i] | b[i];
		}
		return or[];
	}

	/// Returns the logical xor of two arrays.
	/// If the arrays are of unequal lengths, the longer is truncated.
	@safe
	public digit[] xorDigits(in digit[] a, in digit[] b) {
		size_t n = a.length;
		if (n > b.length) n = b.length;
		digit[] xor = new digit[n];
		for (ptrdiff_t i = 0; i < n; i++) {
			xor[i] = a[i] ^ b[i];
		}
		return xor[];
	}

	unittest {
		write("-- logical..........");
		digit[] a, b, c;
		a = [1, 2, 3, 5];
		b = [4, 2, 5, 1];
		c = andDigits(a,b);
		assertEqual(c, [0, 2, 1, 1]);
		c = orDigits(a,b);
		assertEqual(c, [5, 2, 7, 5]);
		c = xorDigits(a,b);
		assertEqual(c, [5, 0, 6, 4]);
		a = [1, 2, 3];
		c = andDigits(a,b);
		assertEqual(c, [0, 2, 1]);
		c = orDigits(a,b);
		assertEqual(c, [5, 2, 7]);
		c = xorDigits(a,b);
		assertEqual(c, [5, 0, 6]);
		writeln("passed");
	}

//--------------------------------
// left and right shift operations
//--------------------------------

	// shifts by whole digits (not bits)
	@safe
	public digit[] shlDigits(in digit[] array, size_t nd) {
		digit[] shifted = array.dup;
		if (nd > 0) {
			shifted = new digit[nd] ~ shifted;
		}
		return shifted;
	}

	// shifts by bits
	@safe
	public digit[] shlBits(in digit[] x, int nb) {
		size_t nx = numDigits(x);
		digit[] shifted = new digit[nx + 1];
		digit shout, shin;
		for (int i = 0; i < nx; i++) {
			ulong temp = x[i];
			temp <<= nb;
			shout = high(temp);
			shifted[i] = low(temp) | shin;
			shin = shout;
		}
		shifted[nx] = shin;
		return shifted;
	}

	// shifts by whole digits (not bits)
	@safe
	public digit[] shrDigits(in digit[] x, int nd) {
		if (nd == 0) return x.dup;
		if (nd >= x.length || nd < 0) {
			throw new InvalidOperationException();
		}
		bool sign = cast(int)x[$-1] < 0;
		if (sign) {
			auto y = x[nd..$].dup;
			for (int i = nd; i < x.length; i++)
				y ~= 0xFFFFFFFF;
			return y;
		}
		return x[nd..$].dup;
	}

	// shifts by bits
	@safe
	public digit[] shrBits(in digit[] x, size_t nb, bool arithmetic = true) {
		size_t nx = numDigits(x);
		digit[] shifted = new digit[nx];

		if (nx == 0) return shifted;

		if (nx == 1) {
			shifted[0] = arithmetic ? x[0] >> nb : x[0] >>> nb;
			return shifted;
		}

		digit shout, shin;
		bool first = true;
		for (ptrdiff_t i = nx - 1; i >= 0; i--) {
			ulong temp = x[i];
			temp <<= 32;
			if (first) {
				if (arithmetic) {
					temp = cast(long)temp >> nb;
				} else {
					temp >>>= nb;
				}
				first = false;
			} else {
				temp >>>= nb;
			}
			shout = low(temp);
			shifted[i] = high(temp) | shin;
			shin = shout;
		}
		return shifted;
	}

unittest {
	write("shiftRight...");
	writeln("test missing");
}

	// shifts by whole digits (not bits)
	@safe
	public digit[] lshrDigits(in digit[] x, size_t nd) {
		if (nd == 0) return x.dup;
		if (nd >= x.length || nd < 0) {
			throw new InvalidOperationException();
		}
		return x[nd..$].dup;
	}

	// shifts by bits
	@safe
	public digit[] lshrBits(in digit[] x, int nb) {
		return shrBits(x, nb, false);
	}

	unittest {	// right and left shifts
		write("-- shifts...........");
		digit[] array, shifted;
		array = [1, 2, 3, 5];
		shifted = shlDigits(array, 1);
		assertEqual(shifted,  [0, 1, 2, 3, 5]);
		assertTrue(compareDigits(array, shifted) < 0);
		shifted = shrDigits(shifted, 1);
		assertTrue(compareDigits(array, shifted) == 0);
		shifted = shlBits(array, 1);
		assertEqual(shifted,  [2, 4, 6, 10, 0]);
		shifted = shrBits(shifted, 1);
		assertEqual(shifted,  array);
		array = [1, 2, 3, 0];
		shifted = shrDigits(array, 1);
		assertEqual(shifted,  [2, 3, 0]);
		assertTrue(compareDigits(array, shifted) > 0);
		array = randomDigits(4);
		shifted = shlBits(array, 4);
		assertEqual(compareDigits(shifted, mulDigit(array, 16)), 0);

		array = [0xAAAAAAAA, 0x55555555, 0x33333333, 0xCCCCCCCC];
		shifted = shlBits(array, 1);
		assertEqual(shifted,
			[0x55555554,0xAAAAAAAB,0x66666666,0x99999998,0x00000001]);
		shifted = shrBits(array, 3);
		assertEqual(shifted, [0xB5555555,0x6AAAAAAA,0x86666666,0xF9999999]);
		shifted = shrBits(array, 3, true);
		assertEqual(shifted, [0xB5555555,0x6AAAAAAA,0x86666666,0xF9999999]);
		shifted = lshrBits(array, 2);
		assertEqual(shifted, [0x6AAAAAAA,0xD5555555,0x0CCCCCCC,0x33333333]);
		shifted = shrBits(array, 2, false);
		assertEqual(shifted, [0x6AAAAAAA,0xD5555555,0x0CCCCCCC,0x33333333]);
		writeln("passed");
	}

//--------------------------------
// greatest common denominator
//--------------------------------

	// greatest common denominator
	@safe
	public digit[] gcdDigits(in digit[] xin, in digit[] yin) {
		digit[] x, y;
		size_t nx = copyDigits(xin, x);
		size_t ny = copyDigits(yin, y);
		if (nx == 0 && ny == 0) return [1];
		if (nx == 0) return y;
		if (ny == 0) return x;
		if (compareDigits(x, nx, y, ny) == 0) return x;
		digit[] g = [1];

		while (isEven(x) && isEven(y)) {
			x = shrBits(x, 1);
			y = shrBits(y, 1);
			g = shlBits(g, 1);
		}
		while (!isZero(x)) {
			while (isEven(x)) {
				x = shrBits(x, 1);
			}
			while (isEven(y)) {
				y = shrBits(y, 1);
			}
			if (compareDigits(x,y) >= 0) {
				digit [] t = subDigits(x,y);
				x = shrBits(t, 1);
			}
			else {
				digit[] t = subDigits(y,x);
				y = shrBits(t, 1);
			}
		}
		return mulDigits(g, y);
	}

	// least common multiple
	@safe
	public digit[] lcmDigits(in digit[] xin, in digit[] yin) {
		digit[] x, y;
		size_t nx = copyDigits(xin, x);
		size_t ny = copyDigits(yin, y);
		if (nx == 0 || ny == 0) return [0];
		if (nx == 1 && x[0] == 1) return [1];
		if (ny == 1 && y[0] == 1) return [1];
		if (compareDigits(x, nx, y, ny) == 0) return x;
		digit[] gcd = gcdDigits(x,y);
		if (nx >= ny) {
			return mulDigits(divDigits(y, gcd), x);
		}
		else {
			return mulDigits(divDigits(x, gcd), y);
		}
	}

	unittest {
		write("-- gcd, lcm.........");
			digit[] x, y, gcd, lcm;
			x = [174];
			y = [36];
			gcd = gcdDigits(x,y);
			assertZero(compareDigits(gcd, [6]));
			lcm = lcmDigits(x,y);
			assertZero(compareDigits(lcm, [1044]));
		writeln("passed");
	}

//--------------------------------
// random digits
//--------------------------------

	public digit[] randomDigits(int n) {
		digit[] digits = new digit[n];
		for (ptrdiff_t i = 0; i <n; i++) {
			digits[i] = uniform!digit;
		}
	    return digits;
	}

/*	public uint randomDigit() {
		return ;
	}*/

	unittest {
		write("-- random digits....");
		digit[] r = randomDigits(4);
		digit[] s = randomDigits(4);
		digit[] t, u;
		if (compareDigits(r,s) > 0) {
			t = subDigits(r,s);
			u = addDigits(t,s);
			reduce(u);
			assertEqual(r, u);
		}
		else {
			t = addDigits(r,s);
			u = subDigits(t,s);
			reduce(u);
			assertEqual(r, u);
		}
   		digit[] x = randomDigits(2);
		digit[] y = randomDigits(2);
		digit[] z = mulDigits(x,y);
		digit[] w = divDigits(z,y);
		reduce(w);
		assertEqual(w,x);

		digit[] rt = randomDigits(1);
		digit[] px = mulDigits(rt,rt);
		rt = sqrDigits(rt);
		writeln("passed");
	}

unittest {
	writeln("==========================");
	writeln("digits module..........end");
	writeln("==========================");
}

