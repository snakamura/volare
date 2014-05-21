ifeq ($(GLIBCXX),1)
	STDLIB=stdc++
else
	STDLIB=c++
endif

CC=clang
CXX=clang++
CXXFLAGS=-std=c++11 -stdlib=lib$(STDLIB) $(shell nc-config --cflags)

all: lib sample

lib: libmsm.a

libmsm.a: msm.o netcdf.o
	$(AR) cr $@ $^

sample: surface barometric

surface: surface.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -l$(STDLIB) $(shell nc-config --libs)

barometric: barometric.o libmsm.a
	$(CC) -o $@ $< -L. -lmsm -l$(STDLIB) $(shell nc-config --libs)

clean:
	rm -f *.o *.a surface barometric
