#include "anet.h"
#include <stdio.h>
#include <string.h>
#include <curl/curl.h>
#include <expat.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <libgen.h>
#include <unistd.h>
#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include "acctrl.h"
#include "pimglib.h"

/***************************************************************************
*    Macro Declaration                                                     *
****************************************************************************/
#define CHK_BUFFER_SIZE(ptr, sz) { \
    if (ptr == NULL) \
	ptr = alloc_data_buffer(); \
    if ((sz) > (ptr->max_size - ptr->size)) { \
	ptr->max_size += CURL_MAX_WRITE_SIZE; \
	ptr->buffer = (char *)realloc(ptr->buffer, \
	    sizeof(char)*ptr->max_size); \
    } \
}

/***************************************************************************
*    Structure Declaration                                                 *
****************************************************************************/
struct data_buffer {
    char *buffer;
    int max_size, size;
};

/***************************************************************************
*   Global Variables                                                       *
****************************************************************************/
struct data_buffer *dataBuff1=NULL, *dataBuff2=NULL;
CURL *curl;
char net_error[CURL_ERROR_SIZE];

static size_t read_cb(void * ptr, size_t size, size_t nmemb, void *stream)
{
	size_t retcode = fread(ptr, size, nmemb, stream);
	fprintf(stderr, "*** Read %d bytes from file\n", retcode);
	return(retcode);
}


int upload_file_through_curl(char *toUpldFile, char *server_url,
	char *upldFile, char *usr, char *passwd)
{
	short int retval;
	FILE *fd=NULL;
	struct stat fst;
	double upload_sz, tot_time;
//	char usrpwd[]="public:public";
	char usrpwd[64], upldUrl[256];

printf("%s:%s:%s:%s\n", toUpldFile, server_url, upldFile, usr);
	sprintf(usrpwd, "%s:%s", usr, passwd);
	stat(toUpldFile, &fst);
	fd = fopen(toUpldFile, "rb");
	if (fd == NULL) {
		printf("Error opening file for upload...\n");
		return(1);
	}

	curl = curl_easy_init();

	curl_easy_setopt(curl, CURLOPT_USERPWD, usrpwd);
	curl_easy_setopt(curl, CURLOPT_READFUNCTION, read_cb);
	curl_easy_setopt(curl, CURLOPT_UPLOAD, 1L);
	sprintf(upldUrl, "%s%s", server_url, upldFile);
	curl_easy_setopt(curl, CURLOPT_URL, upldUrl);
	curl_easy_setopt(curl, CURLOPT_READDATA, fd);
	curl_easy_setopt(curl, CURLOPT_INFILESIZE_LARGE,
			(curl_off_t)(fst.st_size));
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, net_error);
	curl_easy_setopt(curl, CURLOPT_VERBOSE, 1);

	retval = curl_easy_perform(curl);

	curl_easy_getinfo(curl, CURLINFO_SPEED_UPLOAD, &upload_sz);
	curl_easy_getinfo(curl, CURLINFO_TOTAL_TIME, &tot_time);
	curl_easy_cleanup(curl);

	printf("Speed: %.3f byts/sec during %.3f sec\n", upload_sz, tot_time);

	fclose(fd);
	return(retval);
}

int download_file_through_curl(char *download_path, char *server_url, char *usr, char *passwd)
{
	short int retval;
	struct stat fst;
	FILE *fd = NULL;
	double upload_sz, tot_time;
//	char usrpwd[]="public:public";
	char usrpwd[64], dwnlUrl[256];

	sprintf(usrpwd, "%s:%s", usr, passwd);

	fd = fopen(download_path, "wb");

	if ( !fd ) {
		perror("fopen");
		return -1;
	}

	curl = curl_easy_init();

	curl_easy_setopt(curl, CURLOPT_USERPWD, usrpwd);
	curl_easy_setopt(curl, CURLOPT_READFUNCTION, read_cb);

	curl_easy_setopt(curl, CURLOPT_UPLOAD, 0L);
	sprintf(dwnlUrl, "%s%s", server_url, download_path);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, fd);
	curl_easy_setopt(curl, CURLOPT_URL, dwnlUrl);
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, net_error);
	curl_easy_setopt(curl, CURLOPT_VERBOSE, 1);

	retval = curl_easy_perform(curl);

	curl_easy_getinfo(curl, CURLINFO_SPEED_DOWNLOAD, &upload_sz);
	curl_easy_getinfo(curl, CURLINFO_TOTAL_TIME, &tot_time);
	curl_easy_cleanup(curl);

	printf("Speed: %.3f byts/sec during %.3f sec\n", upload_sz, tot_time);

	fclose(fd);

	return(retval);
}

#define SERVER_URL "ftp://ftpuser1.smartfile.com/"
#define USERNAME "ftpuser1"
#define PASSWORD "ftpuser1"

int main(int argc, char *argv[])
{
	char signOutFileName[256], signOutFilePath[256] ;
	char signOutFlgFileName[256], signOutFlgFilePath[256] ;
	int anetRetVal;
	char toUpload[1024];

	strcpy(toUpload, argv[1]);
	printf("toUpload %s\n", toUpload);

#ifdef __arm__

	if ((anetRetVal = anet_connect()) >= 0) 
		printf("Connected to gprs...\n");
	else {
		printf("anet returned %d: Unable to connect on gprs...\n", anetRetVal);
		return 0;
	}
#endif

	if ( strstr(argv[0], "upload") != 0 ) { /* Upload */

		upload_file_through_curl(toUpload, SERVER_URL, toUpload, USERNAME, PASSWORD);

	} else { /* Download */

		download_file_through_curl(toUpload, SERVER_URL, USERNAME, PASSWORD);

	}

	return 0;
}
