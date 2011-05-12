#include <stdio.h>
#include <malloc.h>
#include <string.h>

int main(void)
{

	char *a;

	a = (char *)malloc(5*sizeof(char));
	
	memset(a, 0, 5);

	free(a);

	return 0;

}
