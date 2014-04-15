CC=clang
CXX=clang++
CXXFLAGS=-std=c++11 -stdlib=libstdc++ $(shell nc-config --cflags)

all: lib sample

lib: libmsm.a

libmsm.a: msm.o netcdf.o
	$(AR) cr $@ $^

sample: surface barometric

surface: surface.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -lstdc++ $(shell nc-config --libs)

barometric: barometric.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -lstdc++ $(shell nc-config --libs)

clean:
	rm -f *.o *.a surface barometric
