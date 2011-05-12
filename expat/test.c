#include <expat.h>
#include <stdio.h>
#include <string.h>

#define STRING "<a attr1=\"value\">hi</a>"

void start(void *data, const char *el, const char **attr) {
	printf("%s",el);
}

int main(int argc,char **argv) {
	XML_Parser p = XML_ParserCreate(NULL);
	XML_SetStartElementHandler(p,start);

	XML_Parse(p,STRING,strlen(STRING),0);

	return 0;
}
