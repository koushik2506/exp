#include <stdio.h>
#include <sqlite3.h>
#include <malloc.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char **argv)
{
	sqlite3 *db;
	sqlite3_stmt *stmt;
	sqlite3_blob *blob;
	char buff[1] ;
	int rc;
	int fd, i ;

	sqlite3_open("image.db", &db);

	rc = sqlite3_blob_open(db, "main", "image_store", "data", 1, 0, &blob);

	if ( rc != SQLITE_OK ) {
		printf("sqlite3_blob_open: %d\n", rc);
		return 1;
	}

	fd = open("target.jpg", O_RDWR|O_CREAT);

	if ( fd < 0 ) {
		perror("open:");
		sqlite3_blob_close(blob);
		sqlite3_close(db);

	}

	rc = 0;

	printf("blob size: %d\n", sqlite3_blob_bytes(blob));
	
	while ( rc != SQLITE_ERROR )  {
		rc = sqlite3_blob_read(blob, (void *)buff, 1, i++);
		write(fd, buff, 1);
		printf("ding\n");
	}

	sqlite3_blob_close(blob);
	
	close(fd);

	sqlite3_close(db);

	return 0;
}





