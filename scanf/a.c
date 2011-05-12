#include <stdio.h>

int main(void)
{
	char a, str[10],b;
	FILE *fp;
	int i = 0;

	fp = fopen("test", "r");

	while ( !feof(fp) && i < 3) {
		if ( i == 0 ) fscanf(fp, "%c", &a);
		if ( i == 1 ) fscanf(fp, "%s\n", str);
		if ( i == 2 ) fscanf(fp, "%c", &b);

		i++;
	}

	fprintf(stderr,"a: %c, str: %s, b: %c\n", a, str, b);

	fclose(fp);

}



