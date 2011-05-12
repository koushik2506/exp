#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	char *st = "2.34g";

	double a = 0.0;
	int b;

	b = atoi(st);
	a = strtod(st, NULL);

	printf("%d, %f\n", b, a);

	return 0;
}
