#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <pango/pangocairo.h>


int screen_num;
Display *display;
Window win;
Pixmap pxmp1, pxmp2;
GC gc;
XImage *ximg;

int main(void)
{
	XEvent xevent;
	int pxmp_flag = 1;

	cairo_surface_t *surface;
	cairo_t *cr;
	char *data;


	display = XOpenDisplay(getenv("DISPLAY"));

	screen_num = DefaultScreen(display);

	win = XCreateSimpleWindow(display, RootWindow(display, screen_num), 0, 0,
					200, 200, 0, BlackPixel(display, screen_num), WhitePixel(display, screen_num));

	pxmp1 = XCreatePixmap(display, win, 200, 200, DefaultDepth(display,DefaultScreen(display)));
	pxmp2 = XCreatePixmap(display, win, 200, 200, DefaultDepth(display,DefaultScreen(display)));
	
	
	XClearWindow(display,win);

	ximg = XCreateImage(display, DefaultVisual(disp,screen_num), 24, XYBitmap, 0, NULL, 640, 480, 8, 0);
#if 0
#if 1	
	surface = cairo_xlib_surface_create(display, pxmp1, DefaultVisual(display, 0), 200, 200);

	cr = cairo_create(surface);

	cairo_set_source_rgba(cr, 1.0, 1.0, 1.0,  1.0);
	cairo_rectangle(cr, 0, 0, 200, 200);
	cairo_fill(cr);

	cairo_set_source_rgba(cr, 0.0, 1.0, 0.0, 1.0);
	cairo_rectangle(cr, 10, 10, 100, 100);
	cairo_fill(cr);
	cairo_surface_destroy(surface);
#endif
	surface = cairo_xlib_surface_create(display, pxmp2, DefaultVisual(display, 0), 200, 200);

	cr = cairo_create(surface);

	cairo_set_source_rgba(cr, 1.0, 1.0, 1.0,  1.0);
	cairo_rectangle(cr, 0, 0, 200, 200);
	cairo_fill(cr);

	cairo_set_source_rgba(cr, 1.0, 0.0, 0.0, 1.0);
	cairo_rectangle(cr, 10, 10, 100, 100);
	cairo_fill(cr);
	cairo_surface_destroy(surface);
#endif



	XSetWindowBackgroundPixmap(display,win,pxmp1);

	XSelectInput(display, win, ExposureMask|ButtonPressMask);
	XMapWindow(display, win);

	XSync(display, True);

	while ( 1 ) {
		XNextEvent(display, &xevent);

		if ( xevent.type == ButtonPress ) {
			fprintf(stderr,"Switching pixmap\n");
			if ( pxmp_flag == 1 ) {
				XSetWindowBackgroundPixmap(display, win, pxmp2);
				pxmp_flag = 2;
			} else { 
				XSetWindowBackgroundPixmap(display, win, pxmp1);
				pxmp_flag = 1;
			}
			XClearWindow(display, win);
		}

	}

	XFreePixmap(display, pxmp1);
	XFreePixmap(display, pxmp2);

	XCloseDisplay(display);

	return 0;

}
