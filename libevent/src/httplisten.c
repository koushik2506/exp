
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>

#include <err.h>
#include <event.h>
#include <evhttp.h>

#define LOGIN_FORM "imserver.xml"
#define FINAL_HTML "final.html"

#define MIME_HTML "text/html"
#define MIME_XML "text/xml"


static void
http_callback(struct evhttp_request *req, void *ign)
{
	struct evbuffer *buf;
	int fd;
	struct stat stat;
	char filename[20];
	char content_type[20];

	fprintf(stderr,"request uri:%s\n",evhttp_request_uri(req));

	if(strcmp(evhttp_request_uri(req),"/?page=1&customer=DEFAULT_CUSTID") == 0){
		strcpy(filename,FINAL_HTML);
		strcpy(content_type,MIME_HTML);
	} else {
		strcpy(filename,LOGIN_FORM);
		strcpy(content_type,MIME_XML);
	}
	
	buf = evbuffer_new();
	if (buf == NULL) {
		err(1,"Error creating buffer\n");
		return;
	}

	fd = open(filename,O_RDONLY);
	
	if (fd == -1) {
		evbuffer_add_printf(buf,"File not found");
	} else {
		fstat(fd,&stat);
		evbuffer_read(buf,fd,stat.st_size);
	}

	evhttp_add_header(req->output_headers,"content-type",content_type);
	evhttp_add_header(req->output_headers,"Server","This is not M2");
	
	evhttp_send_reply(req,HTTP_OK,"OK",buf);
	evbuffer_free(buf);
}


int main(int argc,char **argv)
{
	struct evhttp *httpd;

	event_init();

	httpd = evhttp_new(NULL);
	evhttp_bind_socket(httpd,"0.0.0.0",8000);
	evhttp_set_gencb(httpd,http_callback,NULL);

	event_dispatch();
}
