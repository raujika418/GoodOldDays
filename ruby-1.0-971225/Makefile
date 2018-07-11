# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

#### Start of system configuration section. ####

srcdir = .
VPATH = .:./missing

CC = gcc
YACC = bison -y
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
PURIFY =


CFLAGS = -g -O2 -I. -I/usr/i686-w64-mingw32/sys-root/mingw/include
LDFLAGS =  $(CFLAGS) -rdynamic
LIBS = -ldl -lcrypt -lm  $(EXTLIBS)
MISSING =  

program_transform_name = -e s,x,x,
RUBY_INSTALL_NAME = `t='$(program_transform_name)'; echo ruby | sed $$t`

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib/$(RUBY_INSTALL_NAME)

binsuffix = 

#### End of system configuration section. ####


LIBRUBY	      = libruby.a

EXTOBJS	      = dmyext.o

MAINOBJ	      = main.o

OBJS	      = array.o \
		bignum.o \
		class.o \
		compar.o \
		dir.o \
		dln.o \
		enum.o \
		error.o \
		eval.o \
		file.o \
		fnmatch.o \
		gc.o \
		glob.o \
		hash.o \
		inits.o \
		io.o \
		math.o \
		numeric.o \
		object.o \
		pack.o \
		parse.o \
		process.o \
		random.o \
		range.o \
		re.o \
		regex.o \
		ruby.o \
		signal.o \
		sprintf.o \
		st.o \
		string.o \
		struct.o \
		time.o \
		util.o \
		variable.o \
		version.o \
		$(MISSING)

all:		miniruby$(binsuffix) ./ext/Setup 
		@if test -z "$$UNDER_EXTMAKE_RB"; \
		then echo "Compiling ext modules"; \
		UNDER_EXTMAKE_RB=yes; export UNDER_EXTMAKE_RB; \
		cd ext; ../miniruby ./extmk.rb ; fi

miniruby$(binsuffix):	$(OBJS) $(MAINOBJ) $(EXTOBJS) 
		@rm -f $@
		$(PURIFY) $(CC) $(LDFLAGS) $(MAINOBJ) $(OBJS) $(EXTOBJS) $(LIBS) -o miniruby

ruby$(binsuffix):     	$(LIBRUBY) $(MAINOBJ) $(EXTOBJS) 
		@rm -f $@
		$(PURIFY) $(CC) $(LDFLAGS) $(MAINOBJ) $(EXTOBJS) $(LIBRUBY) $(LIBS) -o ruby

$(LIBRUBY):	$(OBJS) dmyext.o
		ar rcu $(LIBRUBY) $(OBJS) dmyext.o
		@-ranlib $(LIBRUBY) 2> /dev/null || true

install:;	$(INSTALL_PROGRAM) ruby$(binsuffix) $(bindir)/$(RUBY_INSTALL_NAME)$(binsuffix)
		@-strip -S -x $(bindir)/$(RUBY_INSTALL_NAME)$(binsuffix)
		@test -d $(libdir) || mkdir $(libdir)
		cd ext; ../miniruby ./extmk.rb install
		@for rb in `grep '^lib/' ./MANIFEST`; do \
		  $(INSTALL_DATA) ./$$rb $(libdir); \
		done

clean:;		@rm -f $(OBJS) $(LIBRUBY) $(MAINOBJ)
		@rm -f ext/extinit.c ext/extinit.o dmyext.o
		cd ext; ../miniruby ./extmk.rb clean

realclean:	clean
		@rm -f Makefile ext/extmk.rb ext/config.cache parse.c
		@rm -f config.cache config.h config.log config.status
		@rm -f core ruby$(binsuffix) miniruby$(binsuffix) parse.c *~ *.core gmon.out

test:;		@-./ruby ./sample/test.rb > ./ruby_test 2>&1; \
		 if grep '^end of test' ./ruby_test > /dev/null; then \
		   echo "test succeeded"; \
		 else \
	           grep '^sample/test.rb' ./ruby_test; \
		   grep '^not' ./ruby_test; \
	   	   echo "test failed";\
		 fi;\
		 rm -f ./ruby_test

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

parse.c: parse.y
	$(YACC) $<
	mv -f y.tab.c parse.c

alloca.o: ./missing/alloca.c
	$(CC) -I. $(CFLAGS) $(CPPFLAGS) -c ./missing/alloca.c

crypt.o: ./missing/crypt.c
	$(CC) -I. $(CFLAGS) $(CPPFLAGS) -c ./missing/crypt.c

dup2.o: ./missing/dup2.c
	$(CC) -I. $(CFLAGS) $(CPPFLAGS) -c ./missing/dup2.c

flock.o: ./missing/flock.c
	$(CC) -I. $(CFLAGS) $(CPPFLAGS) -c ./missing/flock.c

memmove.o: ./missing/memmove.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/memmove.c

mkdir.o: ./missing/mkdir.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/mkdir.c

setenv.o: ./missing/setenv.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/setenv.c

strcasecmp.o: ./missing/strcasecmp.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/strcasecmp.c

strerror.o: ./missing/strerror.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/strerror.c

strdup.o: ./missing/strdup.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/strdup.c

strftime.o: ./missing/strftime.c
	$(CC) -I. $(CFLAGS) $(CPPFLAGS) -c ./missing/strftime.c

strstr.o: ./missing/strstr.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/strstr.c

strtol.o: ./missing/strtol.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/strtol.c

strtoul.o: ./missing/strtoul.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/strtoul.c

nt.o: ./missing/nt.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/nt.c

x68.o: ./missing/x68.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c ./missing/x68.c

# Prevent GNU make v3 from overflowing arg limit on SysV.
.NOEXPORT:
###
parse.o : parse.y ruby.h defines.h config.h env.h node.h st.h regex.h 
###
array.o: array.c ruby.h config.h defines.h
bignum.o: bignum.c ruby.h config.h defines.h
class.o: class.c ruby.h config.h defines.h node.h st.h
compar.o: compar.c ruby.h config.h defines.h
dir.o: dir.c ruby.h config.h defines.h
dln.o: dln.c config.h defines.h dln.h st.h
dmyext.o: dmyext.c
enum.o: enum.c ruby.h config.h defines.h
error.o: error.c ruby.h config.h defines.h env.h
eval.o: eval.c ruby.h config.h defines.h env.h node.h sig.h st.h dln.h
file.o: file.c ruby.h config.h defines.h io.h sig.h
fnmatch.o: fnmatch.c config.h fnmatch.h
gc.o: gc.c ruby.h config.h defines.h env.h sig.h st.h node.h re.h regex.h
glob.o: glob.c config.h fnmatch.h
hash.o: hash.c ruby.h config.h defines.h st.h
inits.o: inits.c ruby.h config.h defines.h
io.o: io.c ruby.h config.h defines.h io.h sig.h
main.o: main.c
math.o: math.c ruby.h config.h defines.h
numeric.o: numeric.c ruby.h config.h defines.h
object.o: object.c ruby.h config.h defines.h st.h
pack.o: pack.c ruby.h config.h defines.h
process.o: process.c ruby.h config.h defines.h sig.h st.h
random.o: random.c ruby.h config.h defines.h
range.o: range.c ruby.h config.h defines.h
re.o: re.c ruby.h config.h defines.h re.h regex.h
regex.o: regex.c config.h defines.h regex.h util.h
ruby.o: ruby.c ruby.h config.h defines.h re.h regex.h dln.h
signal.o: signal.c ruby.h config.h defines.h sig.h
sprintf.o: sprintf.c ruby.h config.h defines.h
st.o: st.c config.h st.h
string.o: string.c ruby.h config.h defines.h re.h regex.h
struct.o: struct.c ruby.h config.h defines.h
time.o: time.c ruby.h config.h defines.h
util.o: util.c defines.h config.h util.h
variable.o: variable.c ruby.h config.h defines.h env.h st.h
version.o: version.c ruby.h config.h defines.h version.h
