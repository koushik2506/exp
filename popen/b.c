#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <sys/select.h>

int wait_on_select(int fd)
{
	fd_set rfds;
	struct timeval tv;
	int retval;

	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);

	tv.tv_sec = 1;
	tv.tv_usec = 0;

	retval = select(fd+1, &rfds, NULL, NULL, &tv);

	if ( retval == - 1 ) 
		perror("select():");
	else if ( retval < 0 ) 
		printf("Timeout");

	
	return retval;
}

int read_line(FILE *fp, char **buff)
{
	int c;
	int size = 0;
	int i = 0;
	int internal_fd = fileno(fp);
		
	wait_on_select(internal_fd);

	while ( ( c = fgetc(fp)) != 0 && c != '\n' && c != -1 ) {

		*buff = (char *)realloc(*buff, ++size);

		*(*buff + i) = c;

		i++;

	}

	if ( i > 0 ) {
		*buff = (char *)realloc(*buff, ++size);
		*(*buff + i) = 0;
	}

	return size;
}

int main(void)
{
	FILE *fp;
	
	fp = popen("./test2.sh", "r");

	if ( fp ) {

		char *buff;
		int i;

		while ( !feof(fp) ) {

			buff = NULL;

			i = read_line(fp, &buff);

			if ( i > 0 ) printf("%s\n", buff);

			free(buff);

		}

		pclose(fp);

	}

	return 0;
}
