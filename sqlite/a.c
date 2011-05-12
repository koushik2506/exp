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
	char *error_mesage = 0;
	char *buff = NULL;
	int rc;
	int fd ;
	struct stat stat_buf;

	rc = sqlite3_open("image.db", &db);

	rc = stat(argv[1], &stat_buf);

	if ( rc < 0 ) {
		perror("stat:");
		return 1;
	}

	buff = (char *)malloc(stat_buf.st_size*sizeof(char));

	fd = open(argv[1], O_RDONLY);

	if ( fd < 0 ) {
		perror("open");
		return 1;
	}

	if ( read(fd, buff, stat_buf.st_size) < 0 ) {
		perror("read:");
		close(fd);
		free(buff);
		return 1;
	}

	close(fd);

	rc = sqlite3_prepare(db, "INSERT INTO image_store VALUES(1,?1);", -1, &stmt, 0);

	if ( rc != SQLITE_OK ) {
		perror("sqlite3_prepare:");
		free(buff);
		return 1;
	}

	rc = sqlite3_bind_blob(stmt, 1, buff, -1, SQLITE_TRANSIENT);

	if ( rc != SQLITE_OK ) printf("error in bind_blob: %d\n", rc);

	rc = 0;

	while ( rc != SQLITE_DONE )
		rc = sqlite3_step(stmt);

	sqlite3_finalize(stmt);

	sqlite3_close(db);

	return 0;
}





