
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>

#include <err.h>
#include <event.h>
#include <evhttp.h>


static void
http_callback(struct evhttp_request *req, void *ign)
{
	struct evbuffer *buf;
	struct evkeyvalueq keys;

	fprintf(stderr,"request uri:%s\n",evhttp_request_uri(req));

	buf = evbuffer_new();
	if (buf == NULL) {
		err(1,"Error creating buffer\n");
		return;
	}

	evbuffer_add_printf(buf,"TIK");
	evhttp_parse_query(evhttp_request_uri(req),keys);

	evhttp_add_header(req->output_headers,"Server","TIM");
	
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

	return 0;
}
