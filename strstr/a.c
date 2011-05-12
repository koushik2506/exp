#include <string.h>
#include <stdio.h>


int main(void)
{
	char b[] = "AT+CSPN?\n\n\n+CSPN: \"CellOne\",0\n\n\nOK";
	char *line, *start, *end;
	char *output;

	line = strcasestr(b, "+CSPN: ");
	start = strcasestr(line, "\"") + 1;
	end = strcasestr(line,",0") - 1;

	output = (char *)malloc((end-start)*sizeof(char));

	strncpy(output,start,(end-start));


	return 0;

}
