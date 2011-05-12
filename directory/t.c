#include <dirent.h>
#include <stdio.h>
#include <malloc.h>
#include <fnmatch.h>

int filter(const struct dirent *directory) 
{
    if ( fnmatch("messages*",directory->d_name,FNM_NOESCAPE) == 0 )
	    return 1;
	else return 0;
}

int main(void)
{
	struct dirent **namelist;
	int n;

	n = scandir("/var/log/", &namelist, filter, 0);
	if (n < 0)
		perror("scandir");
	else {
		while (n--) {
			printf("%s\n", namelist[n]->d_name);
			free(namelist[n]);
		}
		free(namelist);
	}
}
