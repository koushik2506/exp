########################################################################
#    This file is part of GNU Xnee
#
#    GNU program is free software; 
#    you can redistribute it and/or modify it under the terms of 
#    the GNU General Public License as published by the 
#    Free Software Foundation; either version 3, 
#    or (at your option) any later version.
#
#    GNU program is distributed in the hope that it will be useful, 
#    but WITHOUT ANY WARRANTY; without even the implied warranty of 
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#    See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with program; see the file COPYING. If not, write to the 
#      Free Software Foundation, Inc., 51 Franklin St, 
#      Fifth Floor, Boston, MA 02110-1301 USA. 
########################################################################

########################################################################
#
# This file contains small shell script functions (bourne shell)
# to ease up the use of Xnee sripting primitives.
#
# For more information, please go to http://www.gnu/.org/software/xnee
#
#
# A script would use the functions something like this
# 
# #!/bin/sh
#
# # source the functions
# . /usr/share/Xnee/xnee.sh
#
# xnee_init_file
# echo "hey, I will now move the pointer"
# xnee_move_mouse 12 
# echo "hey, I will now press and release key l"
# xnee_fake_key   l
#
# xnee_close_down
#
########################################################################


########################################################################
#
# Basic settings
#
# MY_FIFO  file/fifo to use as communication between your prog and Xnee
#
# CNEE     Xnee command line program (cnee)
#
# MSEC     Delay (msecs) before ket/motion is faked
#
########################################################################
MY_FIFO=/tmp/xnee_${USER}.$$
CNEE=${CNEE:-cnee}
MSEC=2




########################################################################
#
# Function name:    xnee_init_fifo
#
# Arguments:        none
#
# Return value:     none
#
# Description:      Sets up a fifo to use for communication
#                   with your script (using the fake primitives below
#
########################################################################
xnee_init_fifo()
{
	sleep 0
}

xnee_init_fifo_obsolete()
{
    rm -f $MY_FIFO
    touch $MY_FIFO
    $CNEE --replay --file $MY_FIFO   &
    CNEE_PID=$!
}




########################################################################
#
# Function name:    xnee_init_file
#
# Arguments:        none
#
# Return value:     none
#
# Description:      Sets up a file to use for communication
#                   with your script (using the fake primitives below
#
########################################################################
xnee_init_file()
{
    sleep 0
}

xnee_init_file_obsolete()
{
    rm -f $MY_FIFO
    mkfifo $MY_FIFO
    (tail -f $MY_FIFO | $CNEE --replay --file stdin) &
    CNEE_PID=$!
}




########################################################################
#
# Function name:    xnee_write
#
# Arguments:        string str
#
# Return value:     none
#
# Description:      Writes str to the Xnee instance as invoked
#                   by the xnee_init_file or xnee_init_fifo function
#
# Note:             Internal function
########################################################################
xnee_write()
{
#    echo "$*" >> $MY_FIFO
    echo "$*" | $CNEE --replay --file stdin
}



########################################################################
#
# Function name:    xnee_close_down
#
# Arguments:        none
#
# Return value:     none
#
# Description:      Closes file/fifo and shoots down cnee if still running
#
########################################################################
xnee_close_down()
{
#    rm -f $MY_FIFO 2>/dev/null >/dev/null
#    kill $CNEE_PID 2>/dev/null >/dev/null
    sleep 0
}



########################################################################
#
# Function name:    xnee_move_mouse
#
# Arguments:        int x, int y
#
# Return value:     none
#
# Description:      Moves pointer to position (x,y)
#
########################################################################
xnee_move_mouse()
{
    if [ "$2" = "" ]
	then
	return
    fi
    X_POS=$1
    Y_POS=$2

    xnee_write "fake-motion x=$X_POS y=$Y_POS msec=$MSEC" 
}

########################################################################
#
# Function name:    xnee_fake_key
#
# Arguments:        charachter key
#
# Return value:     none
#
# Description:      Fakes press and release of key 'key' 
#
########################################################################
xnee_fake_key()
{
    if [ "$1" = "" ]
	then
	return
    fi
    KEY=$1

    xnee_write "fake-key key=$KEY msec=$MSEC"
}

########################################################################
#
# Function name:    xnee_fake_key_press
#
# Arguments:        charachter key
#
# Return value:     none
#
# Description:      Fakes press of key 'key' 
#
########################################################################
xnee_fake_key_press()
{
    if [ "$1" = "" ]
	then
	return
    fi
    KEY=$1

    xnee_write "fake-key-press key=$KEY msec=$MSEC"
}

########################################################################
#
# Function name:    xnee_fake_key_release
#
# Arguments:        charachter key
#
# Return value:     none
#
# Description:      Fakes release of key 'key' 
#
########################################################################
xnee_fake_key_release()
{
    if [ "$1" = "" ]
	then
	return
    fi
    KEY=$1

    xnee_write "fake-key-release key=$KEY msec=$MSEC"
}

########################################################################
#
# Function name:    xnee_fake_button_press
#
# Arguments:        integer nr
#
# Return value:     none
#
# Description:      Fakes press of buttton 'nr' 
#
########################################################################
xnee_fake_button_press()
{
    if [ "$1" = "" ]
	then
	return
    fi
    BUTTON=$1

    xnee_write "fake-button-press button=$BUTTON msec=$MSEC"
}


########################################################################
#
# Function name:    xnee_fake_button_release
#
# Arguments:        integer nr
#
# Return value:     none
#
# Description:      Fakes release of buttton 'nr' 
#
########################################################################
xnee_fake_button_release()
{
    if [ "$1" = "" ]
	then
	return
    fi
    BUTTON=$1

    xnee_write "fake-button-release button=$BUTTON msec=$MSEC"
}


########################################################################
#
# Function name:    xnee_fake_button
#
# Arguments:        integer nr
#
# Return value:     none
#
# Description:      Fakes press and release of buttton 'nr' 
#
########################################################################
xnee_fake_button()
{
    if [ "$1" = "" ]
	then
	return
    fi
    BUTTON=$1

    xnee_write "fake-button button=$BUTTON msec=$MSEC"
}





########################################################################
#
# The rest of this script contains 
# automagially generated code
#
########################################################################




########################################################################
#
# Function name:    xnee_version
#
# Arguments:        none
#
# Return value:     none
#
# Description:      Prints the current version of Xnee
#                   
#
########################################################################
xnee_version()
{
	echo "3.06"
}	
