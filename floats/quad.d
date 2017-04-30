// Written in the D programming language

/**
 *	Copyright Paul D. Anderson 2012 - 2013.
 *	Distributed under the Boost Software License, Version 1.0.
 *	(See accompanying file LICENSE_1_0.txt or copy at
 *	http://www.boost.org/LICENSE_1_0.txt)
**/

module eris.floats.quad;

import std.stdio;
import std.string;

version(unittest) {
	import eris.assertions;
}

public struct Quad {

	// structure
	uint[8] array;

}

unittest {
	writeln("===================");
	writeln("quad..........begin");
	writeln("===================");
}

unittest {
	writeln("===================");
	writeln("quad............end");
	writeln("===================");
}


