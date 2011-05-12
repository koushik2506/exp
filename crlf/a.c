#include <stdio.h>
#include <string.h>
#include <malloc.h>

int main(int argc, char **argv)
{

	char *str;

	str = (char *)malloc(128*sizeof(char));
	strcpy(str, "AT+CCID\r\r\n89910341210036438007\r\n\r\nOK\r\n");

	char *move;

	move = str;

	while ( *move ) {

		if ( *move == '\r'  ) 
			*move = '\n';

		move++;

	}

	fprintf(stderr,"dest is %s\n", str);

	return 0;
}
