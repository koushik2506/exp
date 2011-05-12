#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <pimglib.h>
#include <pango/pangocairo.h>

#define FONT "Sans 10"

int screen_num;
Display *display;
Window win;
Pixmap pxmp;
GC gc;

int get_color(char *col)
{
	XColor color;

	XAllocNamedColor(display, DefaultColormap(display, screen_num), col, &color, &color);
	
	return color.pixel;
}

void draw_image(struct Image *img, int x, int y)
{
	XPutImage(display, pxmp, gc, img->img, 0, 0, img->x+x, img->y+y, img->width, img->height);
}

#define WIN_WIDTH	800
#define WIN_HEIGHT	400

int main (int argc, char *argv[])
{
	XEvent xevent;
	cairo_surface_t *surface;
	cairo_t *cr;
	cairo_pattern_t *pat;
	PangoLayout *layout;
	PangoFontDescription *desc;
	struct Image *simg;

	display = XOpenDisplay(getenv("DISPLAY"));
	if(display == NULL) {
		fprintf(stderr, "Could not open display.\n");
		exit(1);
	}
	screen_num = DefaultScreen(display);

	win = XCreateSimpleWindow(display, RootWindow(display, screen_num), 0, 0,
				WIN_WIDTH, WIN_HEIGHT, 0, BlackPixel(display, screen_num), WhitePixel(display, screen_num));
	pxmp = XCreatePixmap(display, win, WIN_WIDTH, WIN_HEIGHT, DefaultDepth(display, DefaultScreen(display)));
	XSetWindowBackgroundPixmap(display, win, pxmp);
	XClearWindow(display, win);

	gc = XCreateGC(display, win, 0, NULL);

	XSetForeground(display, gc, get_color("#EEEEEE"));
//	XFillRectangle(display, pxmp, gc, 0, 0, WIN_WIDTH, WIN_HEIGHT);

	XSelectInput(display, win, ExposureMask);

	XMapWindow(display, win);

	pimg_load("out.png", display, &simg, DITHER);
	draw_image(simg, 0, 0);
	pimg_free(&simg);

	surface = cairo_xlib_surface_create(display, pxmp, DefaultVisual(display, 0), 640, 480);
	cr = cairo_create(surface);

	cairo_save(cr);
#if 1
	// Pattern
	pat = cairo_pattern_create_linear(0.0, 0.0,  0.0, 256.0);
	cairo_pattern_add_color_stop_rgba(pat, 1, 0, 0, 0, 0.6);
	cairo_pattern_add_color_stop_rgba(pat, 0, 1, 1, 1, 0.6);
	cairo_rectangle(cr, 30, 100, 200, 30);
	cairo_set_source(cr, pat);
	cairo_fill(cr);
	cairo_pattern_destroy(pat);
#else
	// Filled Rectangle
//	cairo_set_source_rgba(cr, 0.0, 1.0, 0.0, 0.0);
//	cairo_rectangle(cr, 30, 100, 200, 30);
//	cairo_fill(cr);
#endif

	// Rectangle
/*	cairo_set_source_rgba(cr, 1.0, 0.0, 0.0, 1.0);
	cairo_rectangle(cr, 30, 100, 200, 30);
	cairo_set_line_width(cr, 1.0);
	cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
	cairo_stroke(cr); */
	
	cairo_restore(cr);

	// Text
	layout = pango_cairo_create_layout(cr);
	pango_layout_set_text(layout, "hello ನಮಸ್ಕಾರ  नमस्ते", -1);
	desc = pango_font_description_from_string(FONT);
	pango_layout_set_font_description(layout, desc);
	pango_font_description_free(desc);
	cairo_set_source_rgba(cr, 0.0, 0, 0, 1.0);
	cairo_move_to(cr, 70, 105);
	pango_cairo_show_layout(cr, layout);


	XClearWindow(display, win);
	XSync(display, True);
	
	sleep(2);
	
	pimg_load("t.png", display, &simg, DITHER);
	draw_image(simg, 10, 10);
	pimg_free(&simg);
	fprintf(stderr,"put t.png \n");
	XClearWindow(display, win);

	while(1) {
		XNextEvent(display, &xevent);
	}

	cairo_surface_destroy(surface);
	XCloseDisplay(display);

	return 0;
}
