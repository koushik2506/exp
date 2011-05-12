/*
 *
 *  Copyright (c) 2007 Steve Slaven, All Rights Reserved.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of
 *  the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *  MA 02111-1307 USA
 *
*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int main(int argc,char **argv) {
    Display *display;
    Window root, root_return, child_return, relative;
    char *display_name;
    int screen;
    int root_x_return, root_y_return;
    int win_x_return, win_y_return;
    unsigned int mask_return;
    unsigned int parse_window;

    if( argc > 1 && strcmp( argv[ 1 ], "-h" ) == 0 ) {
        printf( "xmousepos v" VERSION "\n"
            "Gets and prints the current mouse position\n"
            "\n"
            "usage: %s [windowid]\n"
            "\n"
            "output:\n"
            "  X Y U V\n"
            "\n"
            "  X - root x position\n"
            "  Y - root y position\n"
            "  U - mouse relative x position to the specified window\n"
            "  V - mouse relative y position to the specified window\n"
            "\n"
            "If no window is specified, then it will use the window\n"
            "that the pointer is in (as defined by XQueryPointer).\n"
            "\n"
            "The windowid should be specified in hex (0xNNNNNNN), you can\n"
            "obtain a windowid using xwininfo.\n"
            "\n"
            , argv[ 0 ] );
        return( 0 );
    }

    if (! (display_name = getenv("DISPLAY")) ){
        fprintf(stderr,"environment variable DISPLAY must be set\n");
        exit(-1);
    }
    if (! (display = XOpenDisplay(display_name)) ){
        fprintf(stderr,"%s: Cannot open display %s\n", argv[0], 
            display_name);
        exit(-1);
    }

    screen = DefaultScreen( display );
    root = RootWindow( display, screen );

    if( argc > 1 ) {
        // they passed the window
        sscanf( argv[ 1 ], "%x", &parse_window );
        relative = (Window)parse_window;
    }else{
        // call the query to get the current window
        XQueryPointer( display, root, &root_return, &child_return,
              &root_x_return, &root_y_return,
              &win_x_return, &win_y_return,
              &mask_return );
        relative = child_return;
    }

    XQueryPointer( display, relative, &root_return, &child_return,
              &root_x_return, &root_y_return,
              &win_x_return, &win_y_return,
              &mask_return );

    printf( "%d %d %d %d\n",
        root_x_return, root_y_return,
        win_x_return, win_y_return );

    return( 0 );
}
