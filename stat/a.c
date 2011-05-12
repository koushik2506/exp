#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>


int main(void)
{

	struct stat empt_stat;

	while ( stat("/tmp/bb", &empt_stat ) < 0 ) {
		printf("..");
		fflush(stdout);
		sleep(2);
	}

	printf("\n");

}
