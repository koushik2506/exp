.SUFFIXES: .erl .beam .yrl

CXX = gcc
CXXFLAGS = -g -Wall `Magick++-config --cppflags --cxxflags`  -I/usr/lib/erlang/lib/erl_interface-3.5.5.1/include -I.
COMPILE = $(CXX) $(CXXFLAGS) -c
LD = $(CXX)
LDFLAGS = `Magick++-config --ldflags` --Wl,--stack,67108864
LIBS = `Magick++-config --libs`  -lerl_interface -lei `pkg-config --libs gthread`
OBJS = im.o composite_def.o

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = chp2 util build_cpp build_erl build_server test example

all: im imagelib.beam

im: $(OBJS)
	$(LD) $(LDFLAGS) $^ $(LIBS) -o $@

im.o: im_commands.h im.cpp

composite_def.o: composite_def.cpp

%.o: %.cpp
	$(COMPILE) $< -o $@

#im: im_commands.h im.cpp
#	gcc im.cpp -Wl,--stack,8388608 -o im `Magick++-config --cppflags --cxxflags --ldflags --libs` -I/usr/lib/erlang/lib/erl_interface-3.5.5.1/include -L/usr/lib/erlang/lib/erl_interface-3.5.5.1/lib -lerl_interface -lei `pkg-config --libs gthread`

im_commands.h: compile
	${ERL} -pa '/home/bill/src/imerl' -s build_cpp start -noshell

imagelib.beam: compile imagelib.in
	${ERL} -pa '/home/bill/src/imerl' -s build_erl start -noshell
	erlc -W imagelib.erl

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump *.o im

