#include <stdio.h>

int main(int argc, char **argv)
{
	float a = -1.0;
	char *c = (char *)&a;

	printf("%x %x %x %x %x %x %x %x\n",
			c[0],c[1],c[2],c[3],
			c[4],c[5],c[6],c[7]);

	return 0;
}
