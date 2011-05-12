#include <stdio.h>
#include <string.h>
#include <malloc.h>

char *global;

void getstring(char *a)
{
	global = strdup(a);


}

int main(void)
{

	getstring("test");

	printf("%s\n", global);

	free(global);

	return 0;

}
