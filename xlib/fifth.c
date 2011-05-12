#include <stdio.h>
#include <X11/Xlib.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>


int main(int argc,char **argv) {

	Display *display;
	Window window;
	int screen_num;
	GC gc;
	XFontStruct *font_info;

	char *font_name = "*-helvetica-*-12-*";

	display = XOpenDisplay(getenv("DISPLAY"));

	if(display == NULL) {
		fprintf(stderr,"%s could not connect to '%s'\n",argv[0],getenv("DISPLAY"));
		exit(1);
	}
	
	font_info = XLoadQueryFont(display,font_name);

	if(!font_info) {
		fprintf(stderr,"XLoadQueryFont: failed to load font:%s\n",font_name);
	}


	screen_num = DefaultScreen(display);

	window = XCreateSimpleWindow(display,DefaultRootWindow(display),0,0,
				     DisplayWidth(display,screen_num)/3,
				     DisplayHeight(display,screen_num)/3,2,
				     WhitePixel(display,screen_num),
				     WhitePixel(display,screen_num));

	XSelectInput(display,window,StructureNotifyMask);

	XMapWindow(display,window);

	for(;;) {
		XEvent ev;
		XNextEvent(display,&ev);
		if(ev.type == MapNotify) break;
	}

	gc = XCreateGC(display,window,0,0);

	XSetForeground(display,gc,BlackPixel(display,screen_num));
	XSetFont(display,gc,font_info->fid);

	XDrawString(display,window,gc,20,20,"hello world",strlen("hello world"));


	XSync(display,False);
	XFlush(display);

	sleep(3);

	return 0;
}
