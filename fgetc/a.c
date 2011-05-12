#include <stdio.h>
#include <malloc.h>


int main(void)
{
	FILE *fp;

	fp = fopen("settings","r");

	if ( fp ) {

		char *buff = NULL;
		char mode;
		int c;
		int size = 0, i = 0;

		mode = fgetc(fp);
		fgetc(fp); // skip \n

		while ( (c = fgetc(fp)) > 0 ) {

			if ( c == '\n' ) break;

			buff = (char *)realloc((char *)buff, ++size);

			*(buff + i) = c;

			i++;

		}
		buff = (char *)realloc((char *)buff, ++size);
		*(buff + i) = 0;

		printf("mode: %c, buff: %s\n", mode, buff);

		fclose(fp);

		free(buff);

	}

	return 0;
}
