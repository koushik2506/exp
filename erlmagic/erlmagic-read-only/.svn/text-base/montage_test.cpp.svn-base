#include <Magick++.h>
#include <string>
#include <iostream>
#include <list>
#include <vector>

using namespace std;

using namespace Magick;

int main( int /*argc*/, char **argv)
{

  InitializeMagick("");

  int failures=0;

  try {

    list<Image> imageList;
    readImages( &imageList, "test.jpg" );
    readImages( &imageList, "test2.jpg" );

    vector<Image> montage;
    MontageFramed montageOpts;

    // Default montage
    montageImages( &montage, imageList.begin(), imageList.end(), montageOpts );
    writeImages(montage.begin(), montage.end(), "new1.jpg");

    // Montage with options set
    montage.clear();
    montageOpts.borderColor( "green" );
    montageOpts.borderWidth( 1 );
    montageOpts.compose( OverCompositeOp );
    montageOpts.fileName( "Montage" );
    montageOpts.frameGeometry( "6x6+3+3" );
    montageOpts.geometry("50x50+2+2>");
    montageOpts.gravity( CenterGravity );
    montageOpts.penColor( "yellow" );
    montageOpts.shadow( true );
    montageOpts.texture( "granite:" );
    montageOpts.tile("5x10");
    //montageOpts.label("%s");
    montageImages( &montage, imageList.begin(), imageList.end(), montageOpts );
    writeImages(montage.begin(), montage.end(), "new.jpg");
  }
  catch( Exception &error_ )
    {
      cout << "Caught exception: " << error_.what() << endl;
      return 1;
    }
  catch( exception &error_ )
    {
      cout << "Caught exception: " << error_.what() << endl;
      return 1;
    }

  if ( failures )
    {
      cout << failures << " failures" << endl;
      return 1;
    }

  return 0;
}
