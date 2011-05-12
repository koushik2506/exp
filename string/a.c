#include <string.h>
#include <stdio.h>
#include <malloc.h>


int main(void)
{

	char *buff = "AT+CCID\r\r\n8991840022138250568F\r\n\r\nOK\r\n";
	char *start;
	char *end;
	int len;
	char *output;


	start = strcasestr(buff,"\r\n") + 2;
	end = strcasestr(start,"\r\n") ;

	len = end - start;

	output = (char *)malloc((len + 1) * sizeof(char));

	strncpy(output, start, len);

	*(output + len) = 0;

	fprintf(stderr,"output is %s\n", output);

	free(output);

	return 0;
}
