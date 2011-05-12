#include <X11/Xlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


int main(int argc,char **argv) {

	Display *display;

	int screen_num;

	Window win;

	unsigned int display_width,display_height;

	unsigned int width,height;

	unsigned int win_x,win_y;

	unsigned int win_border_width;

	char *display_name = getenv("DISPLAY");


	display = XOpenDisplay(display_name);

	if(display == NULL) {
		fprintf(stderr,"%s: cannont connect to display '%s'\n",argv[0],display_name);
		exit(1);
	}

	printf("display: %d\n", sizeof(display));

	screen_num = DefaultScreen(display);
	display_width = DisplayWidth(display,screen_num);
	display_height = DisplayHeight(display,screen_num);

	width = (display_width/3);
	height = (display_height/3);

	win_x = 0;
	win_y = 0;

	win_border_width = 2;

	win = XCreateSimpleWindow(display,RootWindow(display,screen_num),
				  win_x,win_y,width,height,win_border_width,
				  BlackPixel(display,screen_num),WhitePixel(display,screen_num));

	XMapWindow(display,win);

	XSync(display,False);

	sleep(4);

	XCloseDisplay(display);

	return 0;
}
