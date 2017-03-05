ifeq ($(GLIBCXX),1)
	STDLIB=stdc++
else
	STDLIB=c++
endif

UNAME:=$(shell uname -s)
ifeq ($(UNAME),Linux)
	SO=so
	INSTALL_NAME=
	ORIGIN=$ORIGIN
endif
ifeq ($(UNAME),Darwin)
	SO=dylib
	INSTALL_NAME=-install_name @rpath/$(notdir $@)
	ORIGIN=@executable_path
endif

CC=clang
CXX=clang++
CXXFLAGS=-fPIC -std=c++11 -stdlib=lib$(STDLIB) $(shell nc-config --cflags)
LINKMSM=-Llib -lmsm -Wl,-rpath,$(ORIGIN)/../lib

all: lib sample

lib: lib/libmsm.a lib/libmsm.$(SO)

lib/libmsm.a: msm.o netcdf.o
	mkdir -p lib
	$(AR) cr $@ $^

lib/libmsm.$(SO): msm.o netcdf.o
	mkdir -p lib
	$(CXX) -shared $(INSTALL_NAME) -L/usr/local/lib $(shell nc-config --libs) -o $@ $^

sample: bin/surface bin/barometric

bin/surface: surface.o lib/libmsm.$(SO)
	mkdir -p bin
	$(CC) -o $@ $< $(LINKMSM)

bin/barometric: barometric.o lib/libmsm.$(SO)
	mkdir -p bin
	$(CC) -o $@ $< $(LINKMSM)

clean:
	rm -rf *.o lib bin
