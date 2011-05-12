#include <stdio.h>
#include <malloc.h>

int main(int argc, char **argv)
{
	char *a;

	a = NULL;

	a = realloc(a, 0);

	printf("%d", a);

	return 0;
}
