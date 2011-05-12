#include <stdio.h>
#include <string.h>
#include <malloc.h>

#define APN_STR "Init%d = AT+CGDCONT=1,\"IP\",\"ANETAPN\"\n"

int main(int argc, char **argv)
{
	FILE *fp;
	char *buff = NULL;

	fp = fopen("m.conf", "r");

	if ( fp ) {
	
		char *line = NULL;
		int line_num = 0;
		int i = 0;
		int c;
		char apn_set = 0;

		while ( (c = fgetc(fp)) > 0 ) {

			line = (char *)realloc((char *)line, i+1);

			if ( c == '\n' ) {

				*(line + i) = 0;
				i = 0;


				line_num++;

				if ( line_num != 1 && !strstr(line, "Init") && !apn_set) { 
					printf(APN_STR, line_num);
					apn_set = 1;
				}

				if ( !strstr(line, "ANETAPN") ) printf("%s\n",line);

				free(line);

				line = NULL;

			} else {

				*(line + i++) = c;
			}
		}
	
	
		fclose(fp);

	}

	return 0;
}
