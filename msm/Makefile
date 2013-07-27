CC=clang
CXX=clang++
CXXFLAGS=-std=c++11 -stdlib=libc++ $(shell nc-config --cflags)

all: lib sample

lib: libmsm.a

libmsm.a: msm.o netcdf.o
	$(AR) cr $@ $^

sample: test

test: test.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -lc++ $(shell nc-config --libs)

clean:
	rm -f *.o *.a test
