// -*- mode: c++ -*-
// $Id: ary2.cpp,v 1.2 2003/10/07 00:53:07 lattner Exp $
// http://www.bagley.org/~doug/shootout/

#include <iostream>
#include <vector>

int
main(int argc, char *argv[]) {
    int i, n = ((argc == 2) ? atoi(argv[1]) : 1);
    typedef std::vector<int> ARY;
    ARY x(n);
    ARY y(n);

    for (i=0; i<n;) {
	x[i] = i; ++i;
	x[i] = i; ++i;
	x[i] = i; ++i;
	x[i] = i; ++i;
	x[i] = i; ++i;

	x[i] = i; ++i;
	x[i] = i; ++i;
	x[i] = i; ++i;
	x[i] = i; ++i;
	x[i] = i; ++i;
    }
    for (int i = n - 1; i >= 0;) {
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;

        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
        y[i] = x[i]; --i;
    }

    std::cout << y.back() << std::endl;
}
