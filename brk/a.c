#include <unistd.h>
#include <stdio.h>
#include <malloc.h>

int main(int argc, char **argv)
{
	char *a;
	int start;

	start = (int)sbrk(0);
	
	a = (char *)malloc(100*sizeof(char));
	
	printf("%d\n", ((int)sbrk(0) - start));

	return 0;
}
	
