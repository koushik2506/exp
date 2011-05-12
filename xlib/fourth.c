#include <X11/Xlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


int main(int argc,char **argv) {

	Display *display;
	GC gc,gc_erase;
	Window window;
	int screen_num;

	display = XOpenDisplay(getenv("DISPLAY"));

	if (display == NULL) {
		fprintf(stderr,"%s could not connect to %s\n",argv[0],getenv("DISPLAY"));
		exit(1);
	}

	screen_num = DefaultScreen(display);

	window = XCreateSimpleWindow(display,DefaultRootWindow(display),0,0,DisplayWidth(display,screen_num)/3,
					DisplayHeight(display,screen_num)/3,0,
					BlackPixel(display,screen_num),
					BlackPixel(display,screen_num));

	XSelectInput(display,window,StructureNotifyMask);

	XMapWindow(display,window);

	for(;;) {
		XEvent ev;
		XNextEvent(display,&ev);
		if (ev.type == MapNotify) break;
	}

	gc = XCreateGC(display,window,0,0);

	XSetForeground(display,gc,WhitePixel(display,screen_num));

	XDrawLine(display,window,gc,0,0,200,200);

	XSelectInput(display,window,ExposureMask|ButtonPressMask|ButtonMotionMask);

	gc_erase = XCreateGC(display,window,0,0);
	XSetForeground(display,gc_erase,BlackPixel(display,screen_num));

	while(1) {
		XEvent ev;
		XNextEvent(display,&ev);
		switch (ev.type) {
			case Expose:
				fprintf(stderr,"Received Expose event: %d\n",ev.xexpose.count);
				if (ev.xexpose.count > 0) break;
				XDrawLine(display,window,gc,0,0,200,200);
				break;
			case ButtonPress:
				fprintf(stderr,"Received Button press\n");
				switch (ev.xbutton.button) {
					case Button1:
						fprintf(stderr,"Drawing point at (%d,%d)\n",ev.xbutton.x,ev.xbutton.y);
						XDrawPoint(display,ev.xbutton.window,gc,ev.xbutton.x,ev.xbutton.y);
						break;
					case Button2:
						fprintf(stderr,"Erasing point at (%d,%d)\n",ev.xbutton.x,ev.xbutton.y);
						XDrawPoint(display,ev.xbutton.window,gc_erase,ev.xbutton.x,ev.xbutton.y);
						break;
					default:
						break;
				}
			case MotionNotify:
				fprintf(stderr,"Received motion notify\n");
				if (ev.xbutton.state & Button1Mask) XDrawPoint(display,ev.xbutton.window,gc,ev.xbutton.x,ev.xbutton.y);
				break;
			default:
				break;
		}
	}

	XSync(display,False);

	XFlush(display);

	sleep(3);

	return 0;
}
