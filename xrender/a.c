#include <stdio.h>
#include <X11/Xlib.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {

	Display *display;
	Window window;
	int screen_num;
	GC gc;

	display = XOpenDisplay(getenv("DISPLAY"));

	screen_num = DefaultScreen(display);

	window = XCreateSimpleWindow(display, DefaultRootWindow(display), 0, 0,
				     DisplayWidth(display, screen_num)/3,
				     DisplayHeight(display, screen_num)/3,2,
				     WhitePixel(display, screen_num),
				     WhitePixel(display, screen_num));
