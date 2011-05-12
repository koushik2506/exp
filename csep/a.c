#include <stdio.h>
#include <string.h>
#include <malloc.h>

void put_val(FILE *fp, char *key, char *value)
{
	fpos_t fpos;

	fgetpos(fp, &fpos);
	
	fseek(fp, 0L, SEEK_END);

	fprintf(fp, "%s:%s\n", key, value);

	fsetpos(fp, &fpos);
}
	
char *get_val(FILE *fp, char *key) 
{
	fpos_t fpos;
	char *str = NULL;
	int c;
	int size = 0, i = 0;
	char return_this = 0;

	fgetpos(fp, &fpos);
	fseek(fp, 0L, SEEK_SET);

	while  ( (c = fgetc(fp)) > 0 ) {

		fprintf(stderr,"%d\n", c);
			
		str = (char *)realloc((char *)str, ++size);

		if ( c != ':' && c != '\n') {


			*(str+i) = c;

			i++;

		} else {

			*(str + i) = 0;

			if ( return_this ) return str;

			if ( strcmp(str, key) != 0 ) {

				/* skip to next line */

				while ( ( c = fgetc(fp) != '\n' ) ) {  }

			} else { 

				return_this = 1;

			}

			free(str);

			str = NULL;

			size = 0;
			i = 0;
		}
	}

	fsetpos(fp, &fpos);

	return NULL;

}

int main(void)
{
	FILE *fp;
	char *value;

	fp = fopen("/etc/settings/operator_names.db", "a+");

	//put_val(fp, "star","sun");
	//put_val(fp, "planet", "earth");

	value = get_val(fp, "CllOne");

	printf("value: %s\n", value);

	free(value);

	fclose(fp);

}
