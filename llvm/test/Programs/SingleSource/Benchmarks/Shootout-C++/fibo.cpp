// -*- mode: c++ -*-
// $Id: fibo.cpp,v 1.1 2003/10/07 00:55:47 lattner Exp $
// http://www.bagley.org/~doug/shootout/

#include <iostream>
#include <stdlib.h>

using namespace std;

unsigned long fib(unsigned long n) {
    if (n < 2)
	return(1);
    else
	return(fib(n-2) + fib(n-1));
}

int main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);

    cout << fib(n) << endl;
    return(0);
}
