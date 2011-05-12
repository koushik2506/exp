#! /bin/bash
 
 
if [ -f /usr/bin/bin/cnee ]
then
    CNEE=/usr/bin/bin/cnee
fi
 
 
if [ -f /usr/share/xnee/xnee.sh ]
then
    . /usr/share/xnee/xnee.sh
    XNEE_SH=/usr/share/xnee/Xnee.sh
fi

if [ "$CNEE" = "" ]
then
    if  [ -f cnee ]
	then
	export CNEE=cnee
    elif [ -f ../cnee/src/cnee ]
	then
	export CNEE=../cnee/src/cnee
    elif [ -f $(dirname $0)/../../bin/cnee ]
	then
	export CNEE=$(dirname $0)/../../bin/cnee
    else
	echo "Could not find cnee"
	exit 1
    fi
fi

#
# Cleverly trying to find xnee.sh
#

if [ "$XNEE_SH" = "" ]
    then
    if   [ -f xnee.sh ]
	then
	. xnee.sh
    elif [ -f ../share/xnee.sh ]
	then
	. ../share/xnee.sh 
    elif [ -f $(dirname $0)/xnee.sh ]
	then
	. $(dirname $0)/xnee.sh 
    else
	echo "Could not find xnee.sh"
	exit 1
    fi
fi

fake_ls()
{
    echo "   faking press of \"ls -al\" plus additional Enter"
    xnee_fake_key l
    xnee_fake_key s
    xnee_fake_key XK_space
    xnee_fake_key "-"
    xnee_fake_key a
    xnee_fake_key l
    xnee_fake_key XK_Return
}

press_keys_stupidly()
{
    fake_ls
    echo "   faking \"!!\" and return"
    xnee_fake_key_press "Shift"
    xnee_fake_key "1"
    xnee_fake_key "1"
    xnee_fake_key_release "Shift"
    xnee_fake_key XK_Return
}

move_mouse_stupidly()
{
    X_START=100
    X_STOP=300
    Y_START=100
    Y_STOP=300
    

    X=$X_START
    Y=$Y_START

    while [ $X -le $X_STOP ]
    do
      X=$(($X+1))
      xnee_move_mouse $X $Y 
    done

    while [ $Y -le $Y_STOP ]
    do
      Y=$(($Y+1))
      xnee_move_mouse $X $Y 
    done

    while [ $X -gt $X_START ]
    do
      X=$(($X-1))
      xnee_move_mouse $X $Y 
    done

    while [ $Y -gt $Y_START ]
    do
      Y=$(($Y-1))
      xnee_move_mouse $X $Y 
    done
}

#
# main
#
echo "0) Information"
echo "   This is a simple example on how to use Xnee scripting in shell scripts"
echo "   "
echo "   You should see: "
echo "      the mouse moving in a squre"
echo "      ls -l being typed"
echo "      !! being typed"
echo "      ...so you really should see a file listing twice (after this script has returned)"
echo " "
echo " "
sleep 10

xnee_init_file

echo "1) Moving mouse"
echo "      for some stupid reson we move the mouse a bit..."
move_mouse_stupidly
echo " "
echo " "
sleep 4

echo "2) Fake key press and relase"
echo "      and another exmaple, press a few keys"
press_keys_stupidly
echo " "
echo " "
sleep 4

echo "3) Closing down"
echo "      and then we will leave... for the fjords"
sleep 2
xnee_close_down 2>/dev/null  >/dev/null 
echo " "
echo " "

echo "4) Mind the file listings"
echo "      after this scrits dies the key press/releases "
echo "      will be read by your shell and therfore you will not"
echo "      see them until"
sleep 1
echo "      NOW!"
exit 0




