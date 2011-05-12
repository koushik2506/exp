#include <termios.h>
#include <fcntl.h> 
#include <sys/select.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>


static int read_withtimeout_nolen(int fd, char **data, int start_len, int timeout)
{
     fd_set rfds;
     struct timeval tv;
     int retval;


     FD_ZERO(&rfds);
     FD_SET(fd, &rfds);
     tv.tv_sec = timeout; tv.tv_usec = 0;

     retval = select(fd+1, &rfds, NULL, NULL, &tv);
     if(retval == -1) {
          perror("select()");
     } else if(retval) {
          if(FD_ISSET(fd, &rfds)) {

	       int i = 0;

	       /* Allocate and read 1 byte at a time */
	       do {
		       *data = (char *)realloc(*data, (i+1+start_len)*sizeof(char));
		       i++;

		} while ( read(fd, (*data+(i-1)+start_len), 1) > 0 );

		*(*data + (i-1) + start_len) = 0;

		return (i-1);
          }
     } else {
	     	fprintf(stderr, "timeout\n");
		return -2;
     }
     return -3;
}


int __anet_send_at_command_fd(int fd, char *command, char **buff, int timeout)
{
	int ret, tries_over;

	ret = tries_over =  0;

	tcflush(fd, TCIFLUSH);
	tcflush(fd, TCOFLUSH);

	fprintf(stderr, "before write\n");

	ret = write(fd, command, strlen(command));
	tcdrain(fd);

	fprintf(stderr, "write: %d\n", ret);

	sleep(1);

	ret = 0;

	while ( 1 ) {
		
		int l_ret;

	 	l_ret = read_withtimeout_nolen(fd, buff, ret, timeout);

		if ( l_ret < 0 ) {

			ret = -1;
			break;
		}
	
		ret += l_ret;

		if ( strstr(*buff, "OK") || strstr(*buff, "ERROR") ) break;


	}

	return ret;
}

#define MODEM_DEVICE "/dev/ttyUSB0"

int main(int argc, char **argv)
{
	int fd, ret;
	char *buff = NULL;
	int timeout = 5;

	//char *at_cmd = "AT+IPR=0\r\n";


	fd = open(MODEM_DEVICE, O_RDWR|O_NONBLOCK);

	ret = __anet_send_at_command_fd(fd, argv[1], &buff, timeout);
	
	close(fd);

	fprintf(stderr, "%s\n", buff);

	free(buff);

	return ret;

}
