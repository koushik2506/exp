#include <stdio.h>
#include <time.h>

int main(void)
{

	struct tm tmp;
	time_t t;
	char str[200];

//	t = time(NULL);
//	tmp = localtime(&t);

//	strftime(str, sizeof(str), "%Y", tmp);

	strptime("1 Jan 2010", "%d %b %Y", &tmp);

	strftime(str, sizeof(str), "%w", &tmp);

	fprintf(stderr, "%s\n", str);

	return 0;
}
