/* Includes for TIM */

#define SERVER_NAME "TIM"
#define MAX 1024
#define CONF_FILE "./im_server.conf"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


struct server_conf {
	int http_port;
	int https_port;
	char *fqdn;
	short int ssl_support;
	short int file_logging;
	short int dbsupport;
	short int mail_support;
	char *log_file;
	int timeout;
	int log_backup_interval;
	char *log_backup_location;
};

struct database_conf {
	char *db_host;
	int db_port;
	char *db_name;
	char *db_user;
	char *db_pass;
	char *db_type;
};

struct mail_conf {
	char *smtp_server;
	int smtp_port;
	char *userid;
	char *password;
	char *subject_file_location;
	char *body_file_location;
};

struct security_conf {
	char *prv;
	char *mod;
	short int password_encrypted;
	char *keyfile;
	char *certfile;
	char *cacertfile;
	char *ssl_password;
};

struct config {
	struct server_conf server;
	struct database_conf database;
	struct mail_conf mail;
	struct security_conf security;
};

struct config config_record;
