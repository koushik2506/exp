#include <X11/Xlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


Window create_simple_window(Display *display,int width,int height,int x,int y) {

	int screen_num = DefaultScreen(display);
	int win_border_width = 2;

	Window win;

	win = XCreateSimpleWindow(display,RootWindow(display,screen_num),x,y,width,height,win_border_width,
				  WhitePixel(display,screen_num),WhitePixel(display,screen_num));

	XSelectInput(display,win,StructureNotifyMask);

	XMapWindow(display,win);

	XFlush(display);

	return win;
}

GC create_gc(Display *display,Window win,int reverse_video) {

	GC gc;

	unsigned long valuemask = 0;

	XGCValues values;
	unsigned int line_width = 2;
	int line_style = LineSolid;
	int cap_style = CapButt;
	int join_style = JoinBevel;
	int screen_num = DefaultScreen(display);

	gc = XCreateGC(display,win,valuemask,&values);

	 if (reverse_video) {
		XSetForeground(display,gc,WhitePixel(display,screen_num));
		XSetBackground(display,gc,BlackPixel(display,screen_num));
	} else {
		XSetForeground(display,gc,BlackPixel(display,screen_num));
		XSetBackground(display,gc,WhitePixel(display,screen_num));
	} 

	//XSetForeground(display,gc,WhitePixel(display,screen_num));

	XSetLineAttributes(display,gc,line_width,line_style,cap_style,join_style);

	XSetFillStyle(display,gc,FillSolid);

	return gc;
}

int main(int argc,char **argv) {

	Display *display;
	int screen_num;
	Window win;
	
	unsigned int display_width,display_height;
	unsigned int width,height;
	
	char *display_name = getenv("DISPLAY");

	GC gc;

	display = XOpenDisplay(display_name);
	if(display == NULL ){
		fprintf(stderr,"%s couldn't connect to '%s'\n",argv[0],display_name);
		exit(1);
	}

	screen_num = DefaultScreen(display);
	display_width = DisplayWidth(display,screen_num);
	display_height = DisplayHeight(display,screen_num);

	width = display_width/3;
	height = display_height/3;

	win = create_simple_window(display,width,height,0,0);

	gc = create_gc(display,win,0);

	XSync(display,False);

	for(;;) {
		XEvent e;
		XNextEvent(display,&e);
		if(e.type == MapNotify) break;
	}  


	XDrawPoint(display,win,gc,5,5);
	XDrawPoint(display,win,gc,5,height-5);
	XDrawPoint(display,win,gc,width-5,5);
	XDrawPoint(display,win,gc,width-5,height-5);

	XDrawLine(display,win,gc,50,0,50,200);
	XDrawLine(display,win,gc,0,100,200,100);

	XDrawArc(display,win,gc,50-(30/2),100-(30/2),30,30,0,360*64);

	{
		XPoint points[] = {
			{0,0},
			{15,15},
			{0,15},
			{0,0}
		};

		int npoints = sizeof(points)/sizeof(XPoint);

		XDrawLines(display,win,gc,points,npoints,CoordModeOrigin);

	}

	XDrawRectangle(display,win,gc,120,150,50,60);

	XFillRectangle(display,win,gc,60,150,50,60);


	XFlush(display);
	XSync(display,False);

	sleep(30);

	return 0;
}
