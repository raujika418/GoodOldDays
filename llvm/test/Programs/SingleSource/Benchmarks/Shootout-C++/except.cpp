// -*- mode: c++ -*-
// $Id: except.cpp,v 1.1 2003/10/07 01:11:33 lattner Exp $
// http://www.bagley.org/~doug/shootout/
// from Bill Lear

#include <iostream>
#include <cstdlib>
#include <cstdio>

using namespace std;

size_t HI = 0;
size_t LO = 0;

class Hi_exception {
public:
    explicit Hi_exception(size_t _n) : n(_n) {}
    const char* what() { sprintf(N, "%d", n); return N; }
private:
    size_t n; char N[8];
};

class Lo_exception {
public:
    explicit Lo_exception(size_t _n) : n(_n) {}
    const char* what() { sprintf(N, "%d", n); return N; }
private:
    size_t n; char N[8];
};

void blowup(size_t num) {
    if (num % 2) {
        throw Lo_exception(num);
    }
    throw Hi_exception(num);
}

void lo_function(size_t num) {
    try {
        blowup(num);
    } catch(const Lo_exception& ex) {
        ++LO;
    }
}

void hi_function(size_t num) {
    try {
        lo_function(num);
    } catch(const Hi_exception& ex) {
        ++HI;
    }
}

void some_function(size_t num) {
    try {
        hi_function(num);
    } catch (...) {
        cerr << "We shouldn't get here\n"; exit(1);
    }
}

int
main(int argc, char* argv[]) {
    size_t NUM = (argc == 2 ? (atoi(argv[1]) < 1 ? 1 : atoi(argv[1])): 1);
    while (NUM--) {
        some_function(NUM);
    }
    cout << "Exceptions: HI=" << HI << " / " << "LO=" << LO << endl;
}
