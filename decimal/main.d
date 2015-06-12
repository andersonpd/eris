import std.stdio;

T add(T)(in T x, in T y)
{
//	T z;
//	z = x + y;		// Error: Can't modify const expression z
	T z = x + y;
	return z;
}

	mixin template ABC(){
		int abc() { return 3; }
	}

	mixin ABC;

//	int abc() { return 4; }

void main()
{
	writefln("abc() = %s", abc());

	const double a = 1.0;
	const double b = 2.0;
	double c;
	c = add(a,b);
	writefln("c = %s", c);	// 3.0
	c = 1.0;
	writefln("c = %s", c);	// 1.0
}
