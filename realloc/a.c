#include <stdio.h>
#include <malloc.h>

int main(int argc, char **argv)
{
	char *buff = NULL;;
	int i;
	int count;
	char flag;

	count = 0;
	i = 0;
	flag = 10;

	while ( count < 10 ) {

		while ( i < flag ) {

			buff = (char *)realloc((char *)buff, i+1);
			i++;
		}

		printf("%x\n", buff);

		free(buff);
		buff = NULL;
		i=0;
		count++;

		flag += 10;
	}

	return 0;
}
