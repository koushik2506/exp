#include "tim.h"
#include "config.h"

char * string_copy(const char *source) {
	int len = strlen(source);
	char *dest; 
	if (!(dest = (char *)malloc(len+1))) {
		fprintf(stderr,"Out of mem\n");
		exit(1);
	}
	strncpy(dest,source,len);
	dest[len] = 0;
	return dest;
}

void start_element(void *data,const char *el, const char **attr) {
	if ( strcmp(el,"server") == 0 ) {
		int i = 0;
		while(attr[i] != 0) {
			if (strcmp(attr[i],"http_port") == 0) {
				config_record.server.http_port = strtol(attr[i+1],NULL,10);
				
			} else if (strcmp(attr[i],"https_port") == 0) {
				config_record.server.https_port = strtol(attr[i+1],NULL,10);

			} else if (strcmp(attr[i], "fqdn") == 0) {
				config_record.server.fqdn = string_copy(attr[i+1]);

			} else if (strcmp(attr[i], "ssl_support") == 0) {
				if (strcmp(attr[i+1],"yes") == 0) config_record.server.ssl_support = 1;
				else config_record.server.ssl_support = 0;

			} else if (strcmp(attr[i],"file_logging") == 0) {
				if (strcmp(attr[i+1],"yes") == 0) config_record.server.file_logging = 1;
				else config_record.server.file_logging = 0;

			} else if (strcmp(attr[i],"dbsupport") == 0) {
				if (strcmp(attr[i+1],"yes") == 0) config_record.server.dbsupport = 1;
				else config_record.server.dbsupport = 0;

			} else if (strcmp(attr[i],"mail_support") == 0) {
				if (strcmp(attr[i+1],"yes") == 0) config_record.server.mail_support = 1;
				else config_record.server.mail_support = 0;

			} else if (strcmp(attr[i],"log_file") == 0) {
				config_record.server.log_file = string_copy(attr[i+1]);
			
			} else if (strcmp(attr[i],"timeout") == 0) {
				config_record.server.timeout = strtol(attr[i+1],NULL,10);

			} else if (strcmp(attr[i],"log_backup_interval") == 0) {
				config_record.server.log_backup_interval = strtol(attr[i+1],NULL,10);

			} else if (strcmp(attr[i],"log_backup_location") == 0) {
				config_record.server.log_backup_location = string_copy(attr[i+1]);
			}
			i+= 2;
		}

	} else if( strcmp(el,"database") == 0) {
		int i = 0;
		while(attr[i] != 0 ){
			if( strcmp(attr[i],"db_host") == 0) {
				config_record.database.db_host = string_copy(attr[i+1]);

			} else if(strcmp(attr[i],"db_port") == 0) {
				config_record.database.db_port = strtol(attr[i+1],NULL,10);

			} else if(strcmp(attr[i],"db_name") == 0) {
				config_record.database.db_name = string_copy(attr[i+1]);

			} else if(strcmp(attr[i],"db_user") == 0) {
				config_record.database.db_user = string_copy(attr[i+1]);


			} else if(strcmp(attr[i],"db_pass") == 0) {
				config_record.database.db_pass = string_copy(attr[i+1]);

			} else if(strcmp(attr[i],"db_type") == 0) {
				config_record.database.db_type = string_copy(attr[i+1]);

			}
		  	i+=2;
		}

	} else if( strcmp(el,"mail") == 0) {
		int i = 0;
		while ( attr[i] != 0 ) {
			if (strcmp(attr[i],"smtp_server") == 0) {
				config_record.mail.smtp_server = string_copy(attr[i+1]);

			} else if(strcmp(attr[i], "smtp_port") == 0) {
				config_record.mail.smtp_port = strtol(attr[i+1],NULL,10);

			} else if(strcmp(attr[i], "user_id") == 0) {
				config_record.mail.userid = string_copy(attr[i+1]);

			} else if(strcmp(attr[i], "password") == 0) {
				config_record.mail.password = string_copy(attr[i+1]);

			} else if(strcmp(attr[i], "subject_file_location") == 0) {
				config_record.mail.subject_file_location = string_copy(attr[i+1]);

			} else if(strcmp(attr[i], "body_file_location") == 0) {
				config_record.mail.body_file_location =	string_copy(attr[i+1]);

			}
			i+= 2;
		}

	} else if(strcmp(el,"security") == 0) {
		int i = 0;
		while(attr[i] != 0) {
			if (strcmp(attr[i],"prv") == 0) {
				config_record.security.prv = string_copy(attr[i+1]);

			} else if(strcmp(attr[i],"mod") == 0) {
				config_record.security.mod = string_copy(attr[i+1]);

			} else if(strcmp(attr[i],"password_encrypted") == 0) {
				if (strcmp(attr[i+1],"yes") == 0) config_record.security.password_encrypted = 1;
				else config_record.security.password_encrypted = 0;

			} else if(strcmp(attr[i],"keyfile") == 0) {
				config_record.security.keyfile = string_copy(attr[i+1]);

			} else if(strcmp(attr[i], "certfile") == 0) {
				config_record.security.certfile = string_copy(attr[i+1]);
			
			} else if(strcmp(attr[i], "cacertfile") == 0) {
				config_record.security.cacertfile = string_copy(attr[i+1]);

			} else if(strcmp(attr[i], "ssl_password") == 0) {
				config_record.security.ssl_password = string_copy(attr[i+1]);

			}
			i+= 2;
		}
	}

}

void parse_conf() {

	FILE *conf_file;
	char *buffer;
	XML_Parser p = XML_ParserCreate(NULL);

	if ( (conf_file = fopen(CONF_FILE,"r")) == NULL ) {
		perror("Error opening config file\n");
		exit(1);
	}

	buffer = (char *)malloc(sizeof(char) * 1024);

	XML_SetStartElementHandler(p,start_element);

	while(1) {
		int len,done;

		len = fread(buffer,1,1024,conf_file);

		if (ferror(conf_file)) {
			perror("Error reading config file\n");
			exit(1);
		}

		done = feof(conf_file);

		if ( !XML_Parse(p,buffer,len,done) ) {
			fprintf(stderr, "Parse error at line %d:\n%s\n", 
					XML_GetCurrentLineNumber(p),
					XML_ErrorString(XML_GetErrorCode(p)));
			exit(1);
		}

		if(done) break;
	}
	fclose(conf_file);
	XML_ParserFree(p);
	free(buffer);

}
		


/* int main(int argc,char **argv) {
	parse_conf();
	printf("config size:%d\n",sizeof(config_record));
	return 0;
} */
