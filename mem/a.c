#include <stdio.h>
#include <string.h>
#include <malloc.h>

int main(int argc, char **argv)
{
	char *ding = NULL;

	ding = (char *)malloc(3*sizeof(char));

	strcpy(ding, "koushik");


	return 0;
}
