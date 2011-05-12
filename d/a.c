#include <stdio.h>
#include <malloc.h>
#include <string.h>


int main(int argc, char **argv)
{
	int a = 255;
	char *c = (char *)malloc(4*sizeof(char));

	memcpy(c, &a, (size_t)4);

	printf("%x, %x, %x, %x\n", c[0], c[1], 
					c[2], c[3]);

	free(c);
	return 0;
}
