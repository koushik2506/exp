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

#include <X11/XWDFile.h>
#include <png.h>

#include "image.h"
#include "debug.h"

IMAGE *img_new() {
  return( ( IMAGE * )malloc( sizeof( IMAGE ) ) );
}

IMAGE *img_create( int width, int height ) {
  IMAGE *img = img_new();
  img -> width = width;
  img -> height = height;
  img -> pixels = ( PIXEL * )calloc( width * height, sizeof( PIXEL ) );
  return( img );
}

IMAGE *img_load( const char *fname ) {
  FILE *f;
  IMAGE *ret;
  char sig[ 4 ];
  int total_pixels;

  dmsg( 3, "Loading image '%s'\n", fname );
  
  f = fopen( fname, "r" );
  if( ! f ) {
    dmsg( 1, "Unable to open %s\n", fname );
    return( NULL );
  }

  /* Make sure it's a pattern file */
  bzero( sig, 4 );
  fread( sig, 1, 4, f );
  if( strncmp( sig, "IMg!", 4 ) != 0 ) {
    fclose( f );
    dmsg( 1, "Not an image file %s\n", fname );
    return( NULL );
  }

  /* OKIE! */
  ret = img_new();
  fread( &ret -> width, sizeof( int ), 1, f );
  fread( &ret -> height, sizeof( int ), 1, f );

  dmsg( 3, "Image size: %d,%d\n", ret -> width, ret -> height );

  /* Alloc enough space to load all pixels */
  total_pixels = ret -> width * ret -> height;
  ret -> pixels = ( PIXEL * )calloc( total_pixels, sizeof( PIXEL ) );
  fread( ret -> pixels, sizeof( PIXEL ), total_pixels, f );
  
  fclose( f );

  return( ret );
}

/* assumes 24 bit rgb data, 1 byte each, r/g/b */
IMAGE *img_load_from_rgb( const char *fname, int width, int height ) {
  FILE *f;
  IMAGE *img;
  int total_pels, current_pel, inc_pel;
  unsigned char *buf;
  
  f = fopen( fname, "r" );
  if( ! f ) {
    printf( "Unable to open %s\n", fname );
    return( NULL );
  }

  img = img_new();
  img -> width = width;
  img -> height = height;

  total_pels = width * height;
  img -> pixels = ( PIXEL * )calloc( total_pels, sizeof( PIXEL ) );

  buf = ( unsigned char * )malloc( total_pels * 3 );
  fread( buf, 3, total_pels, f );
  fclose( f );

  inc_pel = 0;
  for( current_pel = 0; current_pel < total_pels; current_pel++ ) {
    img -> pixels[ current_pel ].r = buf[ inc_pel++ ];
    img -> pixels[ current_pel ].g = buf[ inc_pel++ ];
    img -> pixels[ current_pel ].b = buf[ inc_pel++ ];
  }

  free( buf );
  
  return( img );
}

/* This might be sepd into another include later, so that it is not
   dependant on X - NOT COMPLETE */
IMAGE *img_load_from_xwd( const char *fname ) {
  FILE *f;
  XWDFileHeader header;
  int total_pels;
  
  f = fopen( fname, "r" );
  if( ! f ) {
    printf( "Unable to open %s\n", fname );
    return( NULL );
  }

  if( fread( &header, sz_XWDheader, 1, f ) < 1 ) {
    printf( "Error reading header\n" );
    exit( 0 );
  }
  
  /* Change endianness? */

  total_pels = header.header_size;
  printf( "header: %i\n"
	  "file_version: %i\n"
	  "pm_format: %i\n"
	  "pm_depth: %i\n"
	  "pm_width: %i\n"
	  "pm_height: %i\n"
	  "xoffset: %i\n"
	  "byte_order: %i\n"
	  "bits_per_pixel: %i\n",
	  //header.header_size,
	  (int)total_pels,
	  (int)header.file_version,
	  (int)header.pixmap_format,
	  (int)header.pixmap_depth,
	  (int)header.pixmap_width,
	  (int)header.pixmap_height,
	  (int)header.xoffset,
	  (int)header.byte_order,
	  (int)header.bits_per_pixel );

  exit( 0 );
  /*
  img = img_new();
  img -> width = width;
  img -> height = height;

  total_pels = width * height;
  img -> pixels = ( PIXEL * )calloc( total_pels, sizeof( PIXEL ) );

  buf = ( unsigned char * )malloc( total_pels * 3 );
  fread( buf, 3, total_pels, f );
  fclose( f );

  inc_pel = 0;
  for( current_pel = 0; current_pel < total_pels; current_pel++ ) {
    img -> pixels[ current_pel ].r = buf[ inc_pel++ ];
    img -> pixels[ current_pel ].g = buf[ inc_pel++ ];
    img -> pixels[ current_pel ].b = buf[ inc_pel++ ];
  }

  free( buf );
  */
  
  return( NULL );
}

IMAGE *img_load_from_png( const char *fname ) {
  png_infop info;
  png_structp png;
  IMAGE *img;
  int total_pels, x, y, current_pel, pel_length, channels;
  png_byte bit_depth, color_type;
  
  png_bytep *row_pointers;
  
  FILE *in = fopen( fname, "rb" );
  
  if( ! in ) {
    return( NULL );
  }
  png = png_create_read_struct( PNG_LIBPNG_VER_STRING,
				NULL, NULL, NULL );
  if( !png )
    return( NULL );

  info = png_create_info_struct( png );
  
  if( setjmp( png_jmpbuf( png ) ) ) {
    /* Free all of the memory associated with the png_ptr and info_ptr */
    png_destroy_read_struct( &png, &info, (png_infopp)NULL);
    fclose( in );
    /* If we get here, we had a problem reading the file */
    return( NULL );
   }
  
  png_init_io( png, in );
  png_read_png( png, info, PNG_TRANSFORM_EXPAND, NULL);

  /* These don't do proper cleanup... */

  color_type = png_get_color_type( png, info );
  if( color_type != PNG_COLOR_TYPE_RGB &&
      color_type != PNG_COLOR_TYPE_RGB_ALPHA ) {
    dmsg( 2, "Only RGB and RGBA png's are supported\n" );
    return( NULL );
  }

  bit_depth = png_get_bit_depth( png, info );
  if( bit_depth != 8 ) {
    dmsg( 2, "Only png's with a bit depth of 8 are supported\n" );
    return( NULL );
  }

  channels = png_get_channels( png, info );
  if( channels < 3 ) {
    dmsg( 2, "No support for grey png's\n" );
    return( NULL );
  }else if( channels == 3 ) {
    dmsg( 2, "RGB PNG\n" );
    pel_length = channels;
  }else if( channels == 4 ) {
    dmsg( 2, "RGBA PNG\n" );
    pel_length = channels;
  }else{
    dmsg( 1, "Unknown PNG type '%d'\n", channels );
    return( NULL );
  }
  
  img = img_new();
  img -> width = png_get_image_width( png, info );
  img -> height = png_get_image_height( png, info );

  total_pels = img -> width * img -> height;
  img -> pixels = ( PIXEL * )calloc( total_pels, sizeof( PIXEL ) );

  row_pointers = png_get_rows( png, info );
  current_pel = 0;
  for( y = 0; y < img -> height; y++ ) {
    for( x = 0; x < img -> width  * pel_length; x += pel_length ) {
      img -> pixels[ current_pel ].r = row_pointers[ y ][ x ];
      img -> pixels[ current_pel ].g = row_pointers[ y ][ x + 1 ];
      img -> pixels[ current_pel ].b = row_pointers[ y ][ x + 2 ];

      if( channels == 4 ) {
	/* Grab alpha, which measures how opaque something is.  Lower number ==
	 more transparent, because during comparasin all tolerances are scaled by this
	 number (on a scale of 0 .. 255, where 0 == totaly transparent and therefore
	 does not effect tolerance at all, and 255 means no change to tolerance level) */
	img -> pixels[ current_pel ].a = row_pointers[ y ][ x + 3 ];
	dmsg( 4, "alpht %d,%d -- %d\n",
	      x, y,
	      img -> pixels[ current_pel ].a );
      }else{
	img -> pixels[ current_pel ].a = 255;
      }
      current_pel++;
    }
  }

  png_destroy_read_struct( &png, &info, (png_infopp)NULL );
  fclose( in );
  
  return( img );
}

int img_destroy( IMAGE *img ) {
  free( img -> pixels );
  free( img );

  return( 1 );
}

int img_write( IMAGE *img, const char *fname ) {
  FILE *f;
  int ret;
  
  f = fopen( fname, "w" );
  
  if( ! f ) {
    printf( "Error writing to %s\n", fname );
    return( 0 );
  }

  ret = img_write_fd( img, f );
  
  fclose( f );
  
  return( ret );
}

int img_write_fd( IMAGE *img, FILE *f ) {
  /* Write header/width/height */
  fwrite( "IMg!", 1, 4, f );
  fwrite( &img -> width, sizeof( int ), 1, f );
  fwrite( &img -> height, sizeof( int ), 1, f );

  /* Write pixels */
  fwrite( img -> pixels, sizeof( PIXEL ), img -> width * img -> height, f );

  return( 1 );
}

int img_write_ppm( IMAGE *img, const char *fname ) {
  FILE *f;
  int ret;
  
  f = fopen( fname, "w" );
  
  if( ! f ) {
    printf( "Error writing to %s\n", fname );
    return( 0 );
  }

  ret = img_write_ppm_fd( img, f );
  
  fclose( f );
  
  return( ret );
}

int img_write_ppm_fd( IMAGE *img, FILE *f ) {
	int x,y;
	PIXEL p;
	POINT pnt;
	fputs( "P3\n", f );
	fprintf( f, "%d %d\n", img -> width, img -> height );
	fprintf( f, "%d\n", 255 );
	for( y = 0; y < img -> height; y++ ) {
		for( x = 0; x < img -> width; x++ ) {
			pnt.x = x;
			pnt.y = y;
			p = img_pixel_get( img, pnt );
			fprintf( f, "%d %d %d\n", p.r, p.g, p.b );
		}
	}
	return 0;
}

PIXEL img_pixel_get( IMAGE *img, POINT p ) {
  PIXEL foo;
  
  if( p.x < img -> width &&
      p.y < img -> height ) {
    return( img -> pixels[ p.y * img -> width + p.x ] );
  }else{
    foo.r = foo.g = foo.b = foo.a = 0;
    return( foo );
  } 
}

int img_pixel_set( IMAGE *img, POINT pt, PIXEL p ) {
  img -> pixels[ pt.y * img -> width + pt.x ].r = p.r;
  img -> pixels[ pt.y * img -> width + pt.x ].g = p.g;
  img -> pixels[ pt.y * img -> width + pt.x ].b = p.b;
  img -> pixels[ pt.y * img -> width + pt.x ].a = p.a;
  return 0;
}

POINT img_subimage_find( IMAGE *master, IMAGE *find, POINT start_from, int tolerance, int find_next ) {
  POINT px;
  int x_end, y_end;

  x_end = master -> width - find -> width;
  y_end = master -> height - find -> height;

  if( find_next ) {
    start_from.x++;
  }
  
  dmsg( 2, "Starting from %d,%d\n", start_from.x, start_from.y );
  dmsg( 2, "End %d,%d\n", x_end, y_end );

  /* Loop the whole freakin image looking for this sub image, but not past edges */
  for( px.y = start_from.y; px.y <= y_end; px.y++ ) {
    dmsg( 5, "Begin subimg find loop for y %d of %d\n", px.y, y_end );
    for( px.x = start_from.x; px.x <= x_end; px.x++ ) {
      dmsg( 5, "Begin subimg find loop for x: %d,%d\n", px.x, px.y );
      if( img_subimage_cmp( master, find, px, tolerance ) <= tolerance ) {
	dmsg( 2, "Found subimage at %d,%d\n", px.x, px.y );
	return( px );
      }
    }
    /* Start back at left */
    start_from.x = 0;
  }

  /* No match */
  px.x = -1;
  px.y = -1;
      
  return( px );

}

/* o is the compare from pixel, assumed to be from a pattern.  It's transparency
   is the transparency used to modify the tolerance value */
int img_pixel_cmp( PIXEL p, PIXEL o ) {
  /*  return( memcmp( &p, &o, sizeof( PIXEL ) ) ); */
  /* make tolerance mean something */
  int difference = abs( p.r - o.r ) +
    abs( p.g - o.g ) +
    abs( p.b - o.b );
  int transparentness = o.a * 1000;
  difference = ( difference * ( transparentness / 255 ) ) / 1000;
  dmsg( 4, "Difference: %d\n", difference );
  return( difference );
}

/* Returns 0 if subimage is inside master at where, like *cmp usually does for other stuff
   otherwise returns an integer of how different the match is, for each color component
   value off.  tolerance is how high to go before bailing.  set lower to avoid processing
   lots of extra pixels, it will just ret when tolerance is met */
int img_subimage_cmp( IMAGE *master, IMAGE *subimage, POINT where, int tolerance ) {
  PIXEL mpx, spx;
  POINT mpt, spt;
  int badness;

  /* DMSG( "Comparing images where=%d,%d\n", where.x, where.y ); */

  /* Check if subimage even fits in masterimage at POINT */
  if( ( where.x + subimage -> width ) > master -> width ||
      ( where.y + subimage -> height ) > master -> height ) {
    /* Superbad */
    return( 1000 );
  }
  
  badness = 0;
  for( spt.x = 0; spt.x < subimage -> width; spt.x++ ) {
    for( spt.y = 0; spt.y < subimage -> height; spt.y++ ) {
      /* Map U/V to X/Y */
      mpt.x = spt.x + where.x;
      mpt.y = spt.y + where.y;

      /* Grab pels and see if they match */
      mpx = img_pixel_get( master, mpt );
      spx = img_pixel_get( subimage, spt );
      
      badness += abs( img_pixel_cmp( mpx, spx ) );
      /* DMSG( "Badness %d\n", badness ); */
      
      if( badness > tolerance ) {
	/* No match here, bail early */
	return( badness );
      }
    }
  }

  /* Matched all of subimage */
  return( badness );
}

/* Return a subimage from img */
IMAGE *img_extract( IMAGE *img, int x, int y, int width, int height ) {
	IMAGE *subimg;
	int rx, ry;
	POINT pa, pb;

	subimg = img_create( width, height );

	for( rx = 0; rx < width; rx++ ) {
		for( ry = 0; ry < height; ry++ ) {
			pa.x = rx;
			pa.y = ry;
			pb.x = rx + x;
			pb.y = ry + y;
			img_pixel_set( subimg, pa, img_pixel_get( img, pb ) );
		}
	}

	return( subimg );
}

void dump_pixel( const char *label, PIXEL p ) {
  dmsg( 1, "%s", label );
  dmsg( 1, " " );
  dmsg( 1, " %d/%d/%d %d\n", p.r, p.g, p.b, p.a );
}
