#include <stdio.h>

int main()
{

	FILE *fp;

	fp = fopen("/tmp/tt", "w");

	fprintf(fp, "n");

	fclose(fp);

}
