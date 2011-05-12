#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>


int main(int argc, char **argv)
{

	int i;

	for ( i=0; i<100; i++) {

		system("ls");
		sleep(1);

	}

	return 0;
}
