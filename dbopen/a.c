#include <stdio.h>
#include <sys/types.h>
#include <db.h>
#include <fcntl.h>


int main(void)
{
	DB *db;
	DBT key, value;

	db = dbopen("test.db", O_CREAT|O_RDWR, 0, DB_HASH, NULL); 

	key.data = strdup("star");
	key.size = strlen(key.data);

	value.data = strdup("sun");
	value.size = strlen(value.data);

	db->put(db, &key, &value, R_NOOVERWRITE); 

	db->sync(db, 0);

	db->close(db);

}



