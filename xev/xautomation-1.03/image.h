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
#ifndef __IMAGE_H__

#define __IMAGE_H__

#include <stdio.h>

typedef struct PIXEL {
  unsigned char	r;
  unsigned char	g;
  unsigned char	b;
  unsigned char a;
} PIXEL;

typedef struct POINT {
  int x;
  int y;
} POINT;

typedef struct IMAGE {
  int width;
  int height;
  PIXEL *pixels;
} IMAGE;

/* Funckys */
IMAGE *img_new();
IMAGE *img_create( int width, int height );
IMAGE *img_load( const char *fname );
IMAGE *img_load_from_rgb( const char *fname, int width, int height );
IMAGE *img_load_from_xwd( const char *fname );
IMAGE *img_load_from_png( const char *fname );

int img_destroy( IMAGE *img );

int img_write( IMAGE *img, const char *fname );
int img_write_fd( IMAGE *img, FILE *f );

int img_write_png( IMAGE *img, const char *fname );
int img_write_png_fd( IMAGE *img, FILE *f );

int img_write_ppm( IMAGE *img, const char *fname );
int img_write_ppm_fd( IMAGE *img, FILE *f );

PIXEL img_pixel_get( IMAGE *img, POINT p );
int img_pixel_set( IMAGE *img, POINT pt, PIXEL p );

/* Set find_next to true if not first run */
POINT img_subimage_find( IMAGE *master, IMAGE *find, POINT start_from, int tolerance, int find_next );
int img_subimage_cmp( IMAGE *master, IMAGE *subimage, POINT where, int tolerance );

IMAGE *img_extract( IMAGE *img, int x, int y, int width, int height );

void dump_pixel( const char *label, PIXEL p );

#endif
