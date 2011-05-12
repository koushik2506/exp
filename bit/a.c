#include <stdio.h>

int main(void)
{
	int a,b;
	int c;

	c = 0x3F;
	b = 65;

//	printf("%d",a&b);
	printf("%x", c & ~16);

	return 0;
}
