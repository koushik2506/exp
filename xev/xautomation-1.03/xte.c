/*
 *  
 *  Copyright (c) 2002 Steve Slaven, All Rights Reserved.
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
#include <stdio.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/extensions/XTest.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <wchar.h>
#include <locale.h>
#include <stdint.h>

#include "debug.h"

#define IS_CMD( x, y ) strncmp( x, y, strlen( y ) ) == 0
#define CMD_STRING_MAXLEN 256

#include "kbd.h"
#include "keysym_map.h"
#include "multikey_map.h"

void send_key( Display *d, char *thing );

/* All key events need to go through here because it handles the lookup of
 * keysyms like Num_Lock, etc.  Thing should be something that represents a
 * single key on the keybaord, like KP_PLUS or Num_Lock or just A */
KeyCode thing_to_keycode( Display *d, char *thing ) {
  KeyCode kc;
  KeySym ks;
  
  ks = XStringToKeysym( thing );
  if( ks == NoSymbol ){
    fprintf( stderr, "Unable to resolve keysym for '%s'\n", thing );
    return( thing_to_keycode( d, "space" ) );
  }

  kc = XKeysymToKeycode( d, ks );
  dmsg( 1, "String '%s' maps to keysym '%lld'\n", thing, (long long int)ks );
  dmsg( 1, "String '%s' maps to keycode '%d'\n", thing, kc );

  return( kc );
}

/* Get KeySym for character c, return 0 if not found */
KeySym get_keysym(wchar_t c) {
  KeySym keysym;
  keysym = 0;
  if ( c <= 0xff ) {
      /* keysym = wchar value for Latin-1 (ISO-8859-1) characters */
      keysym = c;
      dmsg( 2, "Keysym = %d (Latin-1 character)\n", keysym);
  } else {
      if ( c-0x0100 < WCHAR_TO_KEYSYM_MAP_SIZE ) {
          /* Look up keysym in table */
          keysym = wchar_to_keysym_map[ c - 0x0100 ];
      } 
      dmsg( 2, "Keysym = %d (from keysym_map)\n", keysym);
      if (!keysym) {
          /* Not found -> assume that it's a newer unicode character */
          keysym = c + 0x01000000;
          dmsg( 2, "Keysym = %d (newer UTF-8 character)\n", keysym);
      }
  }
  if( keysym >= MAX_KEYSYM ) {
    keysym=0;
  }
  return keysym;
}

/* Simulated pressed key(s) to generate the character represented by keysym */
void send_char( Display *d, KeySym keysym) {
  KeyCode keycode;
  char *wrap_key = NULL;
  int shift;

  /* KeyCode and keyboard modifier lookup */
  keycode  = keysym_to_keycode_map[keysym];
  shift    = keysym_to_modifier_map[ keysym ]%2;
  wrap_key = key_modifiers[ (keysym_to_modifier_map[ keysym ]-shift)/2 ];

  dmsg( 1, "Keycode: %d, Wrap key: %s, Shift: %d\n",
        keycode, wrap_key, shift );

  /* Generate key events */
  if( wrap_key != NULL )
    XTestFakeKeyEvent( d, thing_to_keycode( d, wrap_key ), True, CurrentTime );
  if ( shift )
    XTestFakeKeyEvent( d, thing_to_keycode( d, shift_key ), True, CurrentTime );
  XTestFakeKeyEvent( d, keycode, True, CurrentTime );
  XTestFakeKeyEvent( d, keycode, False, CurrentTime );
  if ( shift )
    XTestFakeKeyEvent( d, thing_to_keycode( d, shift_key ), False, CurrentTime );
  if( wrap_key != NULL )
    XTestFakeKeyEvent( d, thing_to_keycode( d, wrap_key ), False, CurrentTime );

  /* Flushing after every key, thanks thorsten@staerk.de */
  XFlush( d );
}

/* Simulate pressed key(s) to generate 'thing_in' string */
void send_string( Display *d, char *thing_in ) {
  int i,j;

  KeySym keysym[2];

  wchar_t thing[ CMD_STRING_MAXLEN ];
  wchar_t wc_singlechar_str[2];

  wmemset( thing, L'\0', CMD_STRING_MAXLEN );
  mbstowcs( thing, thing_in, CMD_STRING_MAXLEN );
  wc_singlechar_str[ 1 ] = L'\0';
  i = 0;
  while( ( thing[ i ] != L'\0' ) && ( i < CMD_STRING_MAXLEN ) ) {
    wc_singlechar_str[ 0 ] = thing[ i ];
    dmsg( 1, "Sending character '%lc'\n", 
        wc_singlechar_str[ 0 ]);

    /* KeySym lookup */
    keysym[0] = get_keysym(wc_singlechar_str[ 0 ]);
    keysym[1] = 0;
    if ( (keysym[0] == 0) || (keysym_to_keycode_map[keysym[0]] == 0) ) {
      // No keycode found -> try to find a Multi_key combination for this character
      keysym[0] = 0;
      for(j=0;j<MULTIKEY_MAP_SIZE;j++) {
        if (wc_singlechar_str[ 0 ] == multikey_map_char[ j ]) {
          /* Found */
          keysym[0] = get_keysym(multikey_map_first[j]);
          keysym[1] = get_keysym(multikey_map_second[j]);
          if ((keysym_to_keycode_map[keysym[0]] == 0) || (keysym_to_keycode_map[keysym[1]] == 0)) {
            /* Character not supported */
            keysym[0] = 0;
          }
          break;
        }
      }
    }

    if (keysym[0]) {
      if (keysym[1]) {
        /* Multi key sequence */
        dmsg( 1, "Sending Multi_key sequence first_keysym=%d, second_keysym=%d\n", keysym[0], keysym[1]);
        send_key(d,"Multi_key");
        send_char(d,keysym[0]);
        send_char(d,keysym[1]);
      } else {
        /* Single key */
        send_char(d,keysym[0]);
      }
    } else {
      fprintf( stderr, "Character '%ls' is not supported by your keyboard layout.\n", wc_singlechar_str );
    }

    i++;
  }
}

void send_key( Display *d, char *thing ) {
  XTestFakeKeyEvent( d, thing_to_keycode( d, thing ), True, CurrentTime );
  XTestFakeKeyEvent( d, thing_to_keycode( d, thing ), False, CurrentTime );
}

void mouse_click( Display *d, int button ) {
  dmsg( 1, "Clicking mouse button %d\n", button );
  XTestFakeButtonEvent( d, button, True, CurrentTime );
  XTestFakeButtonEvent( d, button, False, CurrentTime );
}

void mouse_move( Display *d, int x, int y ) {
  dmsg( 1, "Moving mouse to %d,%d\n", x, y );
  XTestFakeMotionEvent( d, -1, x, y, CurrentTime );
}

void mouse_rel_move( Display *d, int x, int y ) {
  dmsg( 1, "Moving mouse relatively by %d,%d\n", x, y );
  /* This does not match my docs... hrm... */
  XTestFakeRelativeMotionEvent( d, x, y, CurrentTime );
  /* Should include screen?
  XTestFakeRelativeMotionEvent( d, -1, x, y, CurrentTime );
  */
}

void process_command( Display *d, const char *cmd ) {
  /* Process a command */
  int tmpx,tmpy;
  char str[ CMD_STRING_MAXLEN ];

  bzero( str, CMD_STRING_MAXLEN );
  if( IS_CMD( cmd, "mouseclick " ) ) {
    sscanf( cmd, "mouseclick %d", &tmpx );
    mouse_click( d, tmpx );
  }else if( IS_CMD( cmd, "key " ) ) {
    /* This is meant to send a specific keyboard key, not a specific
     * character, use 'str' if you want that instead */
    strncpy( str, &cmd[ 4 ], 128 );
    send_key( d, str );
  }else if( IS_CMD( cmd, "keydown " ) ) {
    strncpy( str, &cmd[ 8 ], CMD_STRING_MAXLEN );
    XTestFakeKeyEvent( d, thing_to_keycode( d, str ), True, CurrentTime );
  }else if( IS_CMD( cmd, "keyup " ) ) {
    strncpy( str, &cmd[ 6 ], CMD_STRING_MAXLEN );
    XTestFakeKeyEvent( d, thing_to_keycode( d, str ), False, CurrentTime );
  }else if( IS_CMD( cmd, "mousemove " ) ) {
    sscanf( cmd, "mousemove %d %d", &tmpx, &tmpy );
    mouse_move( d, tmpx, tmpy );
  }else if( IS_CMD( cmd, "mousermove " ) ) {
    sscanf( cmd, "mousermove %d %d", &tmpx, &tmpy );
    mouse_rel_move( d, tmpx, tmpy );
  }else if( IS_CMD( cmd, "sleep " ) ) {
    sscanf( cmd, "sleep %d", &tmpx );
    dmsg( 1, "sleep %d\n", tmpx );
    sleep( tmpx );
  }else if( IS_CMD( cmd, "usleep " ) ) {
    sscanf( cmd, "usleep %d", &tmpx );
    dmsg( 1, "usleep %d\n", tmpx );
    usleep( tmpx );
  }else if( IS_CMD( cmd, "mousedown " ) ) {
    sscanf( cmd, "mousedown %d", &tmpx );
    XTestFakeButtonEvent( d, tmpx, True, CurrentTime );
  }else if( IS_CMD( cmd, "mouseup " ) ) {
    sscanf( cmd, "mouseup %d", &tmpx );
    XTestFakeButtonEvent( d, tmpx, False, CurrentTime );
  }else if( IS_CMD( cmd, "str " ) ) {
    /* This attempts to decipher what keys need to be pushed to send a
     * specific string to the screen */
    strncpy( str, &cmd[ 4 ], CMD_STRING_MAXLEN );
    send_string( d, str );
  }else{
    fprintf( stderr, "Unknown command '%s'\n", cmd );
  }

  XFlush( d );
}
 
/* Load keycodes and modifiers of current keyboard mapping into arrays,
 * this is needed by the send_string function */
void load_keycodes( Display *d ) {
  int min_keycode,max_keycode,keysyms_per_keycode,keycode_index,wrap_key_index,num_modifiers;
  char *str;
  KeySym *keysyms, keysym;
  KeyCode keycode;

  XDisplayKeycodes( d, &min_keycode, &max_keycode);
  keysyms = XGetKeyboardMapping( d,
    (KeyCode)min_keycode, max_keycode + 1 - min_keycode,
    &keysyms_per_keycode );

  /* Clear tables */
  for( keysym=0; keysym<MAX_KEYSYM; keysym++ ) {
    keysym_to_modifier_map[keysym]=-1;
    keysym_to_keycode_map[keysym]=0;
  }

  if( keysyms_per_keycode < NUM_KEY_MODIFIERS*2 ) {
    num_modifiers = keysyms_per_keycode;
  } else {
    num_modifiers = NUM_KEY_MODIFIERS*2;
  }

  for( keycode_index = 0; keycode_index < ( max_keycode + 1 - min_keycode ); keycode_index++ ) {
    keycode = keycode_index + min_keycode;
    for( wrap_key_index = 0; wrap_key_index < num_modifiers; wrap_key_index++ ) {
      str = XKeysymToString( keysyms[ keycode_index * keysyms_per_keycode + wrap_key_index ] );
      if( str != NULL ) {
        keysym = XStringToKeysym( str );
        dmsg( 2, "keycode=%d + mod=%d => %s (keysym %lld)\n",
            keycode, wrap_key_index,
            str,(long long int)keysym );
  
        if( keysym < MAX_KEYSYM &&
            keysym_to_modifier_map[ keysym ] == -1 ) {
          keysym_to_modifier_map[ keysym ] = wrap_key_index;
          keysym_to_keycode_map[ keysym ] = keycode;
        }
      }
    }
  }
  
  /* Free storage */
  XFree(keysyms);
}

int main( int argc, char *argv[] ) {
  Display *dpy = NULL;
  int cnt, tmp_i;
  char *buf, *display = NULL;
  int opt;
 
  setlocale(LC_ALL,  "" );

  while( ( opt = getopt( argc, argv, "hd:x:" ) ) != EOF ) {
    switch( opt ) {
    case 'h':
      printf( "xte v" VERSION "\n"
	      "Generates fake input using the XTest extension, more reliable than xse\n"
	      "Author: Steve Slaven - http://hoopajoo.net\n"
	      "\n"
	      "usage: %s [-h] [-x display] [arg ..]\n"
	      "\n"
	      "  -h  this help\n"
	      "  -x  send commands to remote X server.  Note that some commands\n"
	      "      may not work correctly unless the display is on the console,\n"
	      "      e.g. the display is currently controlled by the keyboard and\n"
	      "      mouse and not in the background.  This seems to be a limitation\n"
	      "      of the XTest extension.\n"
	      "  arg args instructing the little man on what to do (see below)\n"
	      "      if no args are passed, commands are read from stdin separated\n"
	      "      by newlines, to allow a batch mode\n"
	      "\n"
	      " Commands:\n"
	      "  key k          Press and release key k\n"
	      "  keydown k      Press key k down\n"
	      "  keyup k        Release key k\n"
	      "  str string     Do a bunch of key X events for each char in string\n"
	      "  mouseclick i   Click mouse button i\n"
	      "  mousemove x y  Move mouse to screen position x,y\n"
	      "  mousermove x y Move mouse relative from current location by x,y\n"
	      "  mousedown i    Press mouse button i down\n"
	      "  mouseup i      Release mouse button i\n"
	      "  sleep x        Sleep x seconds\n"
	      "  usleep x       uSleep x microseconds\n"
	      "\n"
	      "Some useful keys (case sensitive)\n"
	      "  Home\n"
	      "  Left\n"
	      "  Up\n"
	      "  Right\n"
	      "  Down\n"
	      "  Page_Up\n"
	      "  Page_Down\n"
	      "  End\n"
	      "  Return\n"
	      "  BackSpace\n"
	      "  Tab\n"
	      "  Escape\n"
	      "  Delete\n"
	      "  Shift_L\n"
	      "  Shift_R\n"
	      "  Control_L\n"
	      "  Control_R\n"
	      "  Meta_L\n"
	      "  Meta_R\n"
	      "  Alt_L\n"
	      "  Alt_R\n"
	      "  Multi_key\n"
	      "\n"
	      "Sample, drag from 100,100 to 200,200 using mouse1:\n"
	      "  xte 'mousemove 100 100' 'mousedown 1' 'mousemove 200 200' 'mouseup 1'\n"
	      "\n"
	      , argv[ 0 ] );
      exit( 0 );
      break;
      
    case 'd':
      sscanf( optarg, "%d", &tmp_i );
      dmsg( 2, "Debug set to %d\n", tmp_i );
      debug_level( tmp_i );
      break;
  
    case 'x':
      display = optarg;
      break;

    case '?':
      fprintf( stderr, "Unknown option '%c'\n", optopt );
      break;
      
    default:
      fprintf( stderr, "Unhandled option '%c'\n", opt );
      break;
    }
  }
  
  dpy = XOpenDisplay( display );
  if( dpy == NULL ) {
    fprintf( stderr, "Unable to open display '%s'\n", display == NULL ? "default" : display );
    exit( 1 );
  }

  load_keycodes(dpy);

  if( argc - optind >= 1 ) {
    /* Arg mode */
    for( cnt = optind; cnt < argc; cnt++ ) {
      process_command( dpy, argv[ cnt ] );
    }
  }else{
    /* STDIN mode */
    buf = (char *)malloc( CMD_STRING_MAXLEN );
    while( fgets( buf, CMD_STRING_MAXLEN, stdin ) ) {
      buf[ strlen( buf ) - 1 ] = 0; /* Chop \n */
      process_command( dpy, buf );
    }
  }
  
  XCloseDisplay( dpy );
  exit( 0 );
}

// vim: set ts=2 sw=2
