#include <string.h>
#include <malloc.h>
#include <stdio.h>


int main()
{
	char *buff = "AT+CREG?\n\n\n+CREG: 0,2\n\n\n\nOK\n\n";
		
	if ( (strcasestr(buff, "+CREG: ")) > 0 ) {

		int len;
		char *start, *end;
		char *output;
		int creg_stat;

		fprintf(stderr, "buff is %s\n", buff);

		start = (char *)strcasestr(buff,",") + 1; 
		end   = (char *)strcasestr(start,"\n") ;

		len = end - start;

		fprintf(stderr, "len is %d\n", len);

		output = (char *)malloc((len+1) * sizeof(char));
		strncpy(output, start, len);
		*(output + len) = 0;

		fprintf(stderr, "output is %s\n", output);

		creg_stat = atoi(output);

		if ( output != NULL ) free(output);

		fprintf(stderr, "creg_stat is %d\n", creg_stat);

	}
}

