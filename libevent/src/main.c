#include "tim.h"
#include "config.h"

int main(int argc,char **argv) {
	parse_conf();

	printf("fqdn : %s\n",config_record.server.fqdn);
	return 0;
}
