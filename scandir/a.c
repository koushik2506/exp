#include <stdio.h>
#include <dirent.h>


int main(void)
{
	struct dirent **namelist;

	int n;

	n = scandir("/usr/share/mobile/inbuilt/", &namelist, 0, NULL);

	if ( n < 0 ) perror("scandir");

	else {

		while ( n-- ) {

			printf("%s", namelist[n]->d_name);
			free(namelist[n]);

		}
		free(namelist);
	}
}

