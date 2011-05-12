#include <Magick++.h>
#include <string>

using namespace std;
using namespace Magick;

const CompositeOperator get_composite(string comp);
const NoiseType get_noise_type(string noise); 
const ChannelType get_channel_type(string channel);
const GravityType get_gravity_type(string gravity);
const ImageType get_image_type(string image);
const PaintMethod get_paint_method(string paint_method);
