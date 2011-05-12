#include <stdio.h>
#include <malloc.h>

int main(void)
{

	int fd;
	int i, c, size;
	char *buff = NULL;

	system("./test.sh > /tmp/out &");

	sleep(3);

	size = i = 0;
	while ( (c = fgetc(fp)) > 0 ) {
		
		buff = (char *)realloc(buff, ++size);

		if ( c != '\n' ) {
	
			*(buff + i) = c;

		} else {

			*(buff + i) = 0;

			printf("%s\n", buff);

			free(buff);

			buff = NULL;

			size = i = 0;

		}
	}

	close(fd);

	if ( buff != NULL ) free(buff);

}
