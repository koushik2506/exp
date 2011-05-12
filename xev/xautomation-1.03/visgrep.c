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
#include <stdlib.h>
#include <unistd.h>

#include "image.h"
#include "debug.h"

int main( int argc, char *argv[] ) {
  IMAGE *img;
  IMAGE *find;
  int off_x, off_y, tolerance;
  int start_x, start_y;
  int total_match;
  IMAGE **matches;
  
  POINT pt, tmp_pt;

  int cnt, gotmatch, tmp_i, find_next, final_exit;
  int opt;

  off_x = off_y = 0;
  start_x = start_y = 0;
  tolerance = 0;
  final_exit = 1;
  
  while( ( opt = getopt( argc, argv, "hX:Y:x:d:y:t:" ) ) != EOF ) {
    switch( opt ) {
    case 'h':
      printf( "visgrep v" VERSION "\n"
	      "visual grep, greps for images in another image\n"
	      "Author: Steve Slaven - http://hoopajoo.net\n"
	      "\n"
	      "usage: %s [-h] [-x x_off] [-y y_off] [-X start x_off] [-Y start y_off]\n"
	      "          [-d debuglevel] [-t tolerance] image.png detect.pat match.pat ...\n"
	      "\n"
	      "  -h   This help\n"
	      "  -x   Set x offset for detection matching\n"
	      "  -y   Set y offset for detection matching\n"
	      "  -X   Start scanning at X\n"
	      "  -Y   Start scanning at Y\n"
	      "  -t   Set tolerance for 'fuzzy' matches, higher numbers are more tolerant\n"
	      "\n"
	      "All .pat files are created using png2pat or rgb2pat.  The image.png is\n"
	      "scanned for detect.pat starting from X,Y specified above.  When detect.pat\n"
	      "is found, then all the match.pat files are scanned at an offset of x,y as\n"
	      "specified above.  If a match is found, then visgrep prints the x,y and\n"
	      "index of the item.\n"
	      "\n"
	      "image.png must be an 8 bit deep RGB or RGBA png file."
	      "\n"
	      "For example, image.png is a screenshot and match1.pat .. to match5.pat are\n"
	      "images of letters a to e.  Each of these letters is enclosed in a blue box,\n"
	      "so detect.pat is an image of the upper left corner of the box.  This box is\n"
	      "not included in the match*.pat files, so they are actually offset 5 pixels\n"
	      "down and 4 pixels to the left.  You might run it like this then:\n"
	      "\n"
	      "  visgrep -x-4 -y5 image.png match_corner.pat match_a.pat match_b.bat ...\n"
	      "\n"
	      "Etc, with all matches listed.  Now suppose the screen showed 'ace' so\n"
	      "visgrep might output:\n"
	      "\n"
	      "10,10 0\n"
	      "50,10 2\n"
	      "90,10 4\n"
	      "\n"
	      "Showing that match_a.pat (index 0) is at 10,10 on the screen.  If no match\n"
	      "is found even though the detection image is found, the index will be -1.\n"
	      "\n"
	      "Exit status is 0 for successful match, 1 for no match, and 2 for error.\n"
	      , argv[ 0 ] );
      exit( 0 );
      break;
      
    case 'x':
      sscanf( optarg, "%d", &off_x );
      dmsg( 2, "X offset is %d\n", off_x );
      break;
      
    case 'y':
      sscanf( optarg, "%d", &off_y );
      dmsg( 2, "Y offset is %d\n", off_y );
      break;

    case 'X':
      sscanf( optarg, "%d", &start_x );
      dmsg( 2, "Start X offset is %d\n", start_x );
      break;
      
    case 'Y':
      sscanf( optarg, "%d", &start_y );
      dmsg( 2, "Start Y offset is %d\n", start_y );
      break;
      
    case 't':
      sscanf( optarg, "%d", &tolerance );
      dmsg( 2, "Tolerance is %d\n", tolerance );
      break;

    case 'd':
      sscanf( optarg, "%d", &tmp_i );
      dmsg( 2, "Debug set to %d\n", tmp_i );
      debug_level( tmp_i );
      break;
      
    case '?':
      fprintf( stderr, "Unknown option '%c'\n", optopt );
      break;
      
    default:
      fprintf( stderr, "Unhandled option '%c'\n", opt );
      break;
    }
  }

  if( argc - optind < 2 ) {
    fprintf( stderr, "Error, not enough args, try -h\n" );
    exit( 2 );
  }
  
  dmsg( 1, "Loading main PNG image '%s'\n", argv[ optind ] );
  img = img_load_from_png( argv[ optind ] );
  optind++;
  if( img == NULL ) {
    fprintf( stderr, "Error, unable to load png, try -h\n" );
    exit( 2 );
  }
  
  dmsg( 1, "Loading detection image '%s'\n", argv[ optind ] );
  find = img_load( argv[ optind ] );
  dmsg( 1, "Loaded %dx%d\n", find -> width, find -> height );
  optind++;
  
  total_match = argc - optind;
  dmsg( 1, "Now optind=%d : argc=%d : total_match=%d\n", optind, argc, total_match );

  matches = (IMAGE **)malloc( total_match * sizeof( IMAGE * ) );
  
  /* Load actual scoreboxes */
  dmsg( 1, "Loading match data....\n" );
  for( cnt = 0; cnt < total_match; cnt++ ) {
    dmsg( 1, " -- %s\n", argv[ optind ] );
    matches[ cnt ] = img_load( argv[ optind ] );
    if( ! matches[ cnt ] ) {
      printf( "Error loading '%s'\n", argv[ optind ] );
      exit( 2 );
    }
    optind++;
  }

  dmsg( 1, "Detecting offsets...\n" );
  pt.x = start_x;
  pt.y = start_y;
  find_next = 0;
  while( pt.x != -1 ) {
    pt = img_subimage_find( img, find, pt, tolerance, find_next );

    /* Not first time anymore */
    find_next = 1;
    
    if( pt.x != -1 ) {
      dmsg( 1, "  Found match at %d,%d\n", pt.x, pt.y );

      /* Try and identify what thing it is */
      gotmatch = 0;
      tmp_pt.x = pt.x + off_x;
      tmp_pt.y = pt.y + off_y;
      for( cnt = 0; cnt < total_match; cnt++ ) {
	if( matches[ cnt ] != NULL ) {
	  dmsg( 1, " Testing for %d  ", cnt );
	  dmsg( 1, " (%d,%d) ", tmp_pt.x, tmp_pt.y );
	  if( img_subimage_cmp( img, matches[ cnt ], tmp_pt, tolerance ) <= tolerance ) {
	    dmsg( 1, "  YES\n" );
	    
	    printf( "%d,%d %d\n", tmp_pt.x, tmp_pt.y, cnt );
	    gotmatch = 1;
	    final_exit = 0;

	    /* Fall out */
	    cnt = total_match;
	  }else{
	    dmsg( 1, "  NO\n" );
	  }
	}else{
	  dmsg( 1, " No image in slot %d\n", cnt );
	}
      }

      /* Notify of no match */
      if( ! gotmatch ) {
	dmsg( 1, " NO ITEMS MATCHED!\n" );
	printf( "%d,%d %d\n", tmp_pt.x, tmp_pt.y, -1 );
      }
    }
  }

  exit( final_exit );
}
