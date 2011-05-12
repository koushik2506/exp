#include <stdio.h>

int main(int argc, char **argv)
{
	unsigned char a = 0x3;

	a = a << 4;

	printf("0x%x\n", a);

	return 0;
}
