#include <stdio.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>


int main(int argc, char **argv)
{

	int fd1, fd2, size_co;
	struct stat buf;

	fd1 = open(argv[1], O_RDONLY);

	fd2 = open(argv[2], O_CREAT|O_WRONLY|O_TRUNC, S_IRWXU);

	if ( fd1 < 0 ) {

		close(fd1);

		return -1;

	}

	if ( fd2 < 0 ) {

		close(fd1);

		perror("open");

		return -1;
	}

	stat(argv[0], &buf);

	size_co = sendfile(fd2, fd1, NULL, buf.st_size);

	if ( size_co < 0 ) perror("sendfile");

	printf("copied: %d bytes\n", size_co);

	close(fd1);
	close(fd2);

	return 0;
}
