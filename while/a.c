#include <stdio.h>


int main(int argc, char **argv)
{

	int i = 0;
	
	while ( i++ < 3 ) {

		printf("start of while\n");

		if ( 1 ) {

			printf("true\n");

			continue;

		} else {

			printf("never\n");

		}

		if ( 1 ) printf("always\n");

	}

	return 0;
}
