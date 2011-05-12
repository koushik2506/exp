#include <stdio.h>
#include <sys/file.h>


int main(void)
{

	int fd, ret;

	fd = open("/dev/modem_dial", O_RDONLY);

	ret = flock(fd, LOCK_EX);

	if ( ret < 0 ) {
		perror("flock:");
		return 0;
	}

	printf("got lock\n");

	flock(fd, LOCK_UN);

	close(fd);

	return 0;

}
