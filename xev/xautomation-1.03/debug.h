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
/* Handy dandy debug-a-loo */
#ifndef __DEBUG_H__

#define __DEBUG_H__

#ifdef DEBUG_A_LOO

#define dmsg( x, y, args... )	if( x <= _DEBUGLEVEL ) { fprintf( stderr, "DEBUG %d) " y, x, ## args ); }
#define debug_level( x )	_DEBUGLEVEL = x
extern int _DEBUGLEVEL;

#else

#define dmsg( x, y... )		/* */
#define debug_level( x )	fprintf( stderr, "Debugging not enabled, could not set debug level\n" )

#endif /* DEBUG_A_LOO */

#endif /* __DEBUG_H__ */
