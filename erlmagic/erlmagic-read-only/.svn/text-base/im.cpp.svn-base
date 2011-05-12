#include "erl_interface.h"
#include "ei.h"
#define SELF(fd) erl_mk_pid(erl_thisnodename(),fd,0,erl_thiscreation())

#define PORT 7001
#define BUFSIZE 10000

#include "composite_def.h"

#include <Magick++.h>
#include <iostream>
#include <string>
#include <list>
#include <vector>


using namespace std;
using namespace Magick;


int my_listen(int port) {
  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);

  listen(listen_fd, 5);
  return listen_fd;
}

map <int, Image> image_map;
int last_image = 0;

Image &get_image(int param_num, ETERM* msg) 
{
  last_image = ERL_INT_VALUE(erl_element(param_num, msg));
  return image_map[last_image];
}

void del_image(int param_num, ETERM* msg)
{
  int idx = ERL_INT_VALUE(erl_element(param_num, msg));
  map<int, Image>::iterator it = image_map.find(idx); 
  image_map.erase(it);
}

void clear_images()
{
  image_map.erase(image_map.begin(), image_map.end());
}

vector<Image> getImageVector(int param_num, ETERM* msg)
{
  vector<Image> rval;
  ETERM *list = erl_element(param_num, msg);
  if (!ERL_IS_LIST(list)) {
    cout << "list is not a list" << endl;
  }
  for (ETERM *hd = erl_hd(list); hd != NULL; hd = erl_hd(list)) {
    int idx = ERL_INT_VALUE(hd);
    Image& image = image_map[idx];
    rval.push_back(image);
    list = erl_tl(list);
  }

  return rval;
}

MontageFramed get_montage_opts(int param_num, ETERM* msg) {
  MontageFramed rval;

  // defaults
  rval.borderColor( "green" );
  rval.borderWidth( 1 );
  rval.compose( OverCompositeOp );
  rval.fileName( "Montage" );
  //rval.frameGeometry( "6x6+3+3" );
  //rval.geometry("50x50+2+2>");
  rval.gravity( CenterGravity );
  rval.penColor( "yellow" );
  rval.shadow( true );
  rval.texture( "granite:" );
  rval.tile("8x6");

  ETERM *list = erl_element(param_num, msg);
  if (!ERL_IS_LIST(list)) {
    cout << "list is not a list" << endl;
  }
  for (ETERM *hd = erl_hd(list); hd != NULL; hd = erl_hd(list)) {
    string opt((const char*) ERL_ATOM_PTR(erl_element(1, hd)));
    ETERM *param = erl_element(2, hd);
    if (opt == "bordercolor") {
      string color((const char*) erl_iolist_to_string(param));
      rval.borderColor(color);
    }
    else if (opt == "borderWidth") {
      rval.borderWidth(ERL_INT_VALUE(param) );
    }
    else if (opt == "compose") {
      string comp_op((const char*) erl_iolist_to_string(param));
      rval.compose( get_composite(comp_op));
    }
    else if (opt == "fillColor") {
      string color((const char*) erl_iolist_to_string(param));
      rval.fillColor(color);
    }
    else if (opt == "font") {
      string font((const char*) erl_iolist_to_string(param));
      rval.font( font );
    }
    else if (opt == "frameGeometry") {
      string geometry((const char*) erl_iolist_to_string(param));
      rval.frameGeometry( geometry );
    }
    else if (opt == "geometry") {
      string geometry((const char*) erl_iolist_to_string(param));
      rval.geometry( geometry );
    }
    else if (opt == "gravity") {
      string gravity_op((const char*) erl_iolist_to_string(param));
      rval.gravity( get_gravity_type(opt) );
    }
    else if (opt == "penColor") {
      string color((const char*) erl_iolist_to_string(param));
      rval.penColor( color );
    }
    else if (opt == "pointSize") {
      rval.pointSize(ERL_INT_VALUE(param) );
    }
    else if (opt == "shadow") {
      rval.shadow(ERL_INT_VALUE(param) );
    }
    else if (opt == "texture") {
      string texture((const char*) erl_iolist_to_string(param));
      rval.texture( texture );
    }
    else if (opt == "tile") {
      string tile((const char*) erl_iolist_to_string(param));
      rval.tile(tile);
    }
    list = erl_tl(list);
  }
  return rval;
}

int main(int argc,char **argv)
{
  int sockfd;

  ErlConnect erlc;
  int fd;
  int identification_number = 1;
  int creation=1;
  char *cookie="cookie"; /* An example */
  string errcom;

  try {
    erl_init(NULL, 0);

    if (erl_connect_init(identification_number, cookie, creation) == -1)
      erl_err_quit("erl_connect_init");

    //fprintf(stderr, "connected\n");
    if ((sockfd = my_listen(PORT)) <= 0)
      erl_err_quit("error: my_listen");

    // fprintf(stderr, "listening\n");

    if ( erl_publish(PORT) == -1)
      erl_err_quit("error: publish");

    // fprintf(stderr, "published\n");

    if ((fd = erl_accept(sockfd, &erlc)) == ERL_ERROR)
      erl_err_quit("erl_accept");

    fprintf(stderr, "accepting on fd %i\n", fd);
    int image_index = 0;

    ETERM *ok = erl_mk_atom("ok");
    bool exit = false;

    while(!exit) {
      unsigned char buf[BUFSIZE];
      ErlMessage emsg;
      int rec = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
      if (rec == ERL_MSG)
	//cout << "get message" << endl;
	;
      else if (rec == ERL_TICK) {
	//cout << "tick received" << endl;
	continue;
      }
      else if (rec == ERL_ERROR)
	//cout << "msg error " << errno << endl;
	;
      

      ETERM *pid = erl_element(1, emsg.msg);
      ETERM *msg = erl_element(2, emsg.msg);
      if (ERL_IS_TUPLE(msg)) {
	string command((const char*)ERL_ATOM_PTR(erl_element(1, msg)));
	errcom = command;
	if (command == "read") {
	  string file((const char*) erl_iolist_to_string(erl_element(2, msg)));
	  Image image;
	  image.read(file);
	  image_map[image_index] = image;
	  ETERM *reply = erl_mk_int(image_index++);
	  erl_send(fd, pid, reply);
	}
	else if (command == "write") {
	  Image& image = get_image(2, msg);
	  string file((const char*) erl_iolist_to_string(erl_element(3, msg)));
	  cout << "writing " << file << endl;
	  image.write(file);
	  erl_send(fd, pid, ok);
	}
	else if (command == "montageImages") {
	  vector<Image> imageList = getImageVector(2, msg);
	  //string tile((const char*) erl_iolist_to_string(erl_element(3, msg)));
	  MontageFramed montageOpts = get_montage_opts(3, msg);
	  string file((const char*) erl_iolist_to_string(erl_element(4, msg)));
	  montageOpts.fileName(file);
	  vector<Image> montage;
	  montageImages( &montage, imageList.begin(), imageList.end(), montageOpts );
	  writeImages(montage.begin(), montage.end(), file);
	  erl_send(fd, pid, ok);
	}
	else if (command == "clone") {
	  Image& image = get_image(2, msg);
	  Image clone(image);
	  image_map[image_index] = clone;
	  ETERM *reply = erl_mk_int(image_index++);
	  erl_send(fd, pid, reply);
	  
	}
	else if (command == "delete") {
	  del_image(2, msg);
	  erl_send(fd, pid, ok);
	}
	else if (command == "clear") {
	  clear_images();
	  erl_send(fd, pid, ok);
	}
	else if (command == "attribute") {
	  Image& image = get_image(2, msg);
	  string attribute(erl_iolist_to_string(erl_element(3, msg)));
	  string attrib(image.attribute(attribute));
	  ETERM *reply = erl_mk_estring(attrib.c_str(), attrib.size());
	  erl_send(fd, pid, reply);
	}

#include "im_commands.h"
	if (command == "quit") {
	  exit = true;
	  erl_send(fd, pid, ok); 
	}
	else {

	}
      }
    }
  }
  catch( Exception &error_ ) {
    cout << "Caught exception in command " << errcom << ": " << error_.what() << endl;
      return 1;
  }
  return 0;
}

