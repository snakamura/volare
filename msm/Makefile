CC=clang
CXX=clang++
CXXFLAGS=-std=c++11 -stdlib=libc++ $(shell nc-config --cflags)

all: lib sample

lib: libmsm.a

libmsm.a: msm.o netcdf.o
	$(AR) cr $@ $^

sample: surface pressure

surface: surface.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -lc++ $(shell nc-config --libs)

pressure: pressure.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -lc++ $(shell nc-config --libs)

clean:
	rm -f *.o *.a surface pressure
