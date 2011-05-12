#include <stdio.h>
#include <malloc.h>

int main(void)
{
	char *buff;

	buff = (char *)malloc(128*sizeof(char));

	fscanf(stdin, "%s[^\n]\n", buff);

	fprintf(stdout, "%s\n", buff);

	free(buff);

	return 0;
}
