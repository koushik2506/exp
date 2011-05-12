#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>

#include <strings.h>

#include <openssl/ssl.h>

#define MAX 1000
//#define MSG "GET /index.html HTTP/1.1\r\n\r\n"
#define MSG "GET / HTTP/1.1\r\nHost: gnu.geodesic.net\r\n\r\n"
//#define MSG "GET /?rqty=blind HTTP/1.1\r\n\r\n"
#define MSGLEN 32
#define FILENAME "ab.html"

// #define SERVER "202.86.7.110" //login.yahoo.com
// #define SERVER "61.16.167.26" //s-syncml.geodesic.com
//#define SERVER "209.85.225.83" //gmail.com
#define SERVER "192.168.130.184" //gnu

int main(int argc,char *argv[]) {
	int sockd,read_count,file_d,ret;
	struct sockaddr_in hisaddr;
	char buf[MAX];
	SSL_CTX *ssl_ctx;
	SSL *ssl;

	if((sockd = socket(AF_INET,SOCK_STREAM,0)) < 0) {
		perror("socket");
		exit(1);
	}

	bzero((char *)&hisaddr,sizeof(hisaddr));
	hisaddr.sin_family = AF_INET;
	hisaddr.sin_port = htons(443);

	if(inet_pton(AF_INET,SERVER,&hisaddr.sin_addr) < 0) {
		fprintf(stderr,"inet_pton error for %s",argv[1]);
		exit(1);
	}


	if(connect(sockd,(struct sockaddr *)&hisaddr,sizeof(hisaddr)) < 0) {
		perror("connect");
		exit(1);
	}

	SSL_load_error_strings();
	SSL_library_init();

	ssl_ctx = SSL_CTX_new((SSL_METHOD *)SSLv3_client_method());
	
	if ((ret = SSL_CTX_use_certificate_file(ssl_ctx,"server_cert.pem",SSL_FILETYPE_PEM)) != 1) {
		perror("SSL_CTX_user_certificate_file");
		exit(1);
	}
	
	if ((ret = SSL_CTX_use_PrivateKey_file(ssl_ctx,"server_key.pem",SSL_FILETYPE_PEM)) != 1) {
		perror("SSL_CTX_use_PrivateKey_file");
		exit(1);
	}
	
	ssl = SSL_new(ssl_ctx);

	SSL_set_fd(ssl,sockd);

	printf("yellow\n");

	if((ret = SSL_connect(ssl)) != 1){
		fprintf(stderr,"SSL_connect: %d\n",SSL_get_error(ssl,ret));
		perror("SSL_connect");
		exit(1);
	}
	
	printf("violet\n");

	SSL_write(ssl,MSG,strlen(MSG));
	
	printf("red\n");

	file_d = open(FILENAME,O_RDWR|O_NONBLOCK|O_CREAT,0644);

	while((read_count = SSL_read(ssl,buf,MAX)) > 0) {
		printf("green:%d\n",read_count);
		write(file_d,buf,read_count);
		if(read_count == MAX) break;
	}

	SSL_shutdown(ssl);
	
	close(file_d);
	close(sockd);

	return 0;
}	
