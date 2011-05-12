#include <stdio.h>
#include <string.h>

int main(int argc,char **argv) {

	char *string = "BogoMIPS        : 520.00\n";

	char *a = strchr(string,':');
	char *b = strchr(a,'\n');

	printf("%d",b-a);

	return 0;
}
