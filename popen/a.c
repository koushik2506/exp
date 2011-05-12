#include <stdio.h>
#include <malloc.h>

int main(void)
{
	FILE *fp;
	
	fp = popen("./test.sh", "r");

	if ( fp ) {

		char *buff = NULL;
		int c;
		int size = 0, i = 0;

		while ( (c = fgetc(fp)) > 0 ) {

			buff = (char *)realloc((char *)buff, ++size);

			*(buff + i) = c;
			i++;

		}

		buff = (char *)realloc((char *)buff, ++size);
		*(buff + i ) = 0;

		pclose(fp);

		if ( !strcmp("Process done.\n", buff) ) {

			free(buff);

			fprintf(stderr,"ok\n");

		} else {

			free(buff);
			fprintf(stderr,"not ok\n");

		}

	}

	return 0;
}
