#include <Magick++.h>
#include <map>
#include <string>
#include <iostream>

using namespace std;
using namespace Magick;


map<string, CompositeOperator> comp_map;

void init_comp(void)
{
  if (comp_map.size() == 0)
    return;

  comp_map["AddCompositeOp"] = AddCompositeOp;
  comp_map["AtopCompositeOp"] = AtopCompositeOp;
  comp_map["BumpmapCompositeOp"] = BumpmapCompositeOp;
  comp_map["ClearCompositeOp"] = ClearCompositeOp;
  comp_map["ColorizeCompositeOp"] = ColorizeCompositeOp;
  comp_map["CopyBlueCompositeOp"] = CopyBlueCompositeOp;
  comp_map["CopyCompositeOp"] = CopyCompositeOp;
  comp_map["CopyGreenCompositeOp"] = CopyGreenCompositeOp;
  comp_map["CopyOpacityCompositeOp"] = CopyOpacityCompositeOp;
  comp_map["CopyRedCompositeOp"] = CopyRedCompositeOp;
  comp_map["DarkenCompositeOp"] = DarkenCompositeOp;
  comp_map["DifferenceCompositeOp"] = DifferenceCompositeOp;
  comp_map["DisplaceCompositeOp"] = DisplaceCompositeOp;
  comp_map["DissolveCompositeOp"] = DissolveCompositeOp;
  comp_map["HueCompositeOp"] = HueCompositeOp;
  comp_map["InCompositeOp"] = InCompositeOp;
  comp_map["LightenCompositeOp"] = LightenCompositeOp;
  comp_map["LuminizeCompositeOp"] = LuminizeCompositeOp;
  comp_map["MinusCompositeOp"] = MinusCompositeOp;
  comp_map["ModulateCompositeOp"] = ModulateCompositeOp;
  comp_map["MultiplyCompositeOp"] = MultiplyCompositeOp;
  comp_map["NoCompositeOp"] = NoCompositeOp;
  comp_map["OutCompositeOp"] = OutCompositeOp;
  comp_map["OverCompositeOp"] = OverCompositeOp;
  comp_map["OverlayCompositeOp"] = OverlayCompositeOp;
  comp_map["PlusCompositeOp"] = PlusCompositeOp;
  comp_map["SaturateCompositeOp"] = SaturateCompositeOp;
  comp_map["ScreenCompositeOp"] = ScreenCompositeOp;
  comp_map["SubtractCompositeOp"] = SubtractCompositeOp;
  comp_map["ThresholdCompositeOp"] = ThresholdCompositeOp;
  comp_map["UndefinedCompositeOp"] = UndefinedCompositeOp;
  comp_map["XorCompositeOp"] = XorCompositeOp;
  comp_map["CopyCyanCompositeOp"] = CopyCyanCompositeOp;
  comp_map["CopyMagentaCompositeOp"] = CopyMagentaCompositeOp;
  comp_map["CopyYellowCompositeOp"] = CopyYellowCompositeOp;
  comp_map["CopyBlackCompositeOp"] = CopyBlackCompositeOp;
}

const CompositeOperator get_composite(string comp) 
{
  init_comp();
  return comp_map[comp];
}

map<string, NoiseType> noise_map;

void init_noise(void)
{
  if (noise_map.size() == 0)
    return;
  noise_map["UniformNoise"] = UniformNoise;
  noise_map["GaussianNoise"] = GaussianNoise;
  noise_map["MultiplicativeGaussianNoise"] = MultiplicativeGaussianNoise;
  noise_map["ImpulseNoise"] = ImpulseNoise;
  noise_map["LaplacianNoise"] = LaplacianNoise;
  noise_map["PoissonNoise"] = PoissonNoise;
}

const NoiseType get_noise_type(string noise) 
{
  init_noise();
  return noise_map[noise];
}

map<string, ChannelType> channel_map;

void init_channel(void)
{
  if (channel_map.size() == 0)
    return;

  channel_map["UndefinedChannel"] = UndefinedChannel;
  channel_map["RedChannel"] = RedChannel;
  channel_map["CyanChannel"] = CyanChannel;
  channel_map["GreenChannel"] = GreenChannel;
  channel_map["MagentaChannel"] = MagentaChannel;
  channel_map["BlueChannel"] = BlueChannel;
  channel_map["YellowChannel"] = YellowChannel;
  channel_map["OpacityChannel"] = OpacityChannel;
  channel_map["BlackChannel"] = BlackChannel;
  channel_map["MatteChannel"] = MatteChannel;

}

const ChannelType get_channel_type(string channel) 
{
  init_channel();
  return channel_map[channel];
}

map<string, GravityType> gravity_map;

void init_gravity(void)
{
  if (gravity_map.size() == 0)
    return;

  gravity_map["ForgetGravity"] = ForgetGravity;
  gravity_map["NorthWestGravity"] = NorthWestGravity;
  gravity_map["NorthGravity"] = NorthGravity;
  gravity_map["NorthEastGravity"] = NorthEastGravity;
  gravity_map["WestGravity"] = WestGravity;
  gravity_map["CenterGravity"] = CenterGravity;
  gravity_map["EastGravity"] = EastGravity;
  gravity_map["SouthWestGravity"] = SouthWestGravity;
  gravity_map["SouthGravity"] = SouthGravity;
  gravity_map["SouthEastGravity"] = SouthEastGravity;
  gravity_map["StaticGravity"] = StaticGravity;
}

const GravityType get_gravity_type(string gravity) 
{
  init_gravity();
  return gravity_map[gravity];
}

map<string, ImageType> image_type_map;

void init_image_type(void)
{
  if (image_type_map.size() == 0)
    return;
  image_type_map["UndefinedType"] = UndefinedType;
  image_type_map["BilevelType"] = BilevelType;
  image_type_map["GrayscaleType"] = GrayscaleType;
  image_type_map["GrayscaleMatteType"] = GrayscaleMatteType;
  image_type_map["PaletteType"] = PaletteType;
  image_type_map["PaletteMatteType"] = PaletteMatteType;
  image_type_map["TrueColorType"] = TrueColorType;
  image_type_map["TrueColorMatteType"] = TrueColorMatteType;
  image_type_map["ColorSeparationType"] = ColorSeparationType;
  image_type_map["ColorSeparationMatteType"] = ColorSeparationMatteType;
  image_type_map["OptimizeType"] = OptimizeType;

}

const ImageType get_image_type(string image) 
{
  init_image_type();
  return image_type_map[image];
}

map<string, PaintMethod> paint_method_map;

void init_paint_method(void)
{
  if (paint_method_map.size() == 0)
    return;
  paint_method_map["PointMethod"] = PointMethod;
  paint_method_map["ReplaceMethod"] = ReplaceMethod;
  paint_method_map["FloodfillMethod"] = FloodfillMethod;
  paint_method_map["FillToBorderMethod"] = FillToBorderMethod;
  paint_method_map["ResetMethod"] = ResetMethod;

}

const PaintMethod get_paint_method(string paint_method) 
{
  init_paint_method();
  return paint_method_map[paint_method];
}
