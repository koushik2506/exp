-module(imagelib).
-compile(export_all).

rpc(Host, Query) ->
    {any, Host} ! {self(), Query},
    receive
	Reply ->
	    Reply
    end.

start() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_atom("c1@" ++ Hostname).

read(Host, File) ->
    rpc(Host, {read, File}).

write(Host, File) ->
    rpc(Host, {write, File}).

delete(Host, Image) ->
    rpc(Host, {delete, Image}).

montageImages(Host, ImageList, Opts, File) ->
    rpc(Host, {montageImages, ImageList, Opts, File}).

clone(Host, Image) ->
    rpc(Host, {clone, Image}).

clear(Host, _) ->
    rpc(Host, {clear}).

attribute(Host, Image, Attribute) ->
    rpc(Host, {attribute, Image, Attribute}).

adaptiveThreshold(Host, Image,Width,Height,Offset) ->
	rpc(Host, {adaptiveThreshold_3,Image,Width,Height,Offset}).

addNoise(Host, Image,NoiseType) ->
	rpc(Host, {addNoise_1,Image,NoiseType}).

%%affineTransform not implemented because of parameter ??DrawableAffine
annotate(Host, Image,Text,Location) ->
	rpc(Host, {annotate_2,Image,Text,Location}).

annotate(Host, Image,Text,BoundingArea,Gravity) ->
	rpc(Host, {annotate_3,Image,Text,BoundingArea,Gravity}).

annotate(Host, Image,Text,BoundingArea,Gravity,Degrees) ->
	rpc(Host, {annotate_4,Image,Text,BoundingArea,Gravity,Degrees}).

%%// not implemented because of parameter not implemented
blur(Host, Image,Radius,Sigma) ->
	rpc(Host, {blur_2,Image,Radius,Sigma}).

border(Host, Image,Geometry) ->
	rpc(Host, {border_1,Image,Geometry}).

channel(Host, Image,Channel) ->
	rpc(Host, {channel_1,Image,Channel}).

channelDepth(Host, Image,Channel,Depth) ->
	rpc(Host, {channelDepth_2,Image,Channel,Depth}).

channelDepth(Host, Image,Channel) ->
	rpc(Host, {channelDepth_1,Image,Channel}).

charcoal(Host, Image,Radius,Sigma) ->
	rpc(Host, {charcoal_2,Image,Radius,Sigma}).

chop(Host, Image,Geometry) ->
	rpc(Host, {chop_1,Image,Geometry}).

colorize(Host, Image,OpacityRed,OpacityGreen,OpacityBlue,PenColor_red,PenColor_green,PenColor_blue) ->
	rpc(Host, {colorize_4,Image,OpacityRed,OpacityGreen,OpacityBlue,PenColor_red,PenColor_green,PenColor_blue}).

colorize(Host, Image,Opacity,PenColor_red,PenColor_green,PenColor_blue) ->
	rpc(Host, {colorize_2,Image,Opacity,PenColor_red,PenColor_green,PenColor_blue}).

comment(Host, Image,Comment) ->
	rpc(Host, {comment_1,Image,Comment}).

%%compose not implemented because of parameter not implemented
%%compose not implemented because of parameter not implemented
compare(Host, Image,Reference) ->
	rpc(Host, {compare_1,Image,Reference}).

%%// not implemented because of parameter not implemented
composite(Host, Image,CompositeImage,Offset,Compose) ->
	rpc(Host, {composite_3,Image,CompositeImage,Offset,Compose}).

%%// not implemented because of parameter not implemented
contrast(Host, Image,Sharpen) ->
	rpc(Host, {contrast_1,Image,Sharpen}).

%%convolve not implemented because of parameter *kernel_
crop(Host, Image,Geometry) ->
	rpc(Host, {crop_1,Image,Geometry}).

cycleColormap(Host, Image,Amount) ->
	rpc(Host, {cycleColormap_1,Image,Amount}).

despeckle(Host, Image) ->
	rpc(Host, {despeckle_0,Image}).

display(Host, Image) ->
	rpc(Host, {display_0,Image}).

%%draw not implemented because of parameter ??Drawable
%%draw not implemented because of parameter ??std::list<Magick::Drawable>
edge(Host, Image,Radius) ->
	rpc(Host, {edge_1,Image,Radius}).

emboss(Host, Image,Radius,Sigma) ->
	rpc(Host, {emboss_2,Image,Radius,Sigma}).

enhance(Host, Image) ->
	rpc(Host, {enhance_0,Image}).

equalize(Host, Image) ->
	rpc(Host, {equalize_0,Image}).

erase(Host, Image) ->
	rpc(Host, {erase_0,Image}).

flip(Host, Image) ->
	rpc(Host, {flip_0,Image}).

%%// not implemented because of parameter not implemented
floodFillColor(Host, Image,Point,FillColor_red,FillColor_green,FillColor_blue) ->
	rpc(Host, {floodFillColor_2,Image,Point,FillColor_red,FillColor_green,FillColor_blue}).

%%// not implemented because of parameter not implemented
floodFillColor(Host, Image,Point,FillColor_red,FillColor_green,FillColor_blue,BorderColor_red,BorderColor_green,BorderColor_blue) ->
	rpc(Host, {floodFillColor_3,Image,Point,FillColor_red,FillColor_green,FillColor_blue,BorderColor_red,BorderColor_green,BorderColor_blue}).

floodFillOpacity(Host, Image,X,Y,Opacity,Method) ->
	rpc(Host, {floodFillOpacity_4,Image,X,Y,Opacity,Method}).

%%// not implemented because of parameter not implemented
%%// not implemented because of parameter not implemented
%%// not implemented because of parameter not implemented
floodFillTexture(Host, Image,Point,Texture,BorderColor_red,BorderColor_green,BorderColor_blue) ->
	rpc(Host, {floodFillTexture_3,Image,Point,Texture,BorderColor_red,BorderColor_green,BorderColor_blue}).

flop(Host, Image) ->
	rpc(Host, {flop_0,Image}).

frame(Host, Image,Geometry) ->
	rpc(Host, {frame_1,Image,Geometry}).

frame(Host, Image,Width,Height,InnerBevel,OuterBevel) ->
	rpc(Host, {frame_4,Image,Width,Height,InnerBevel,OuterBevel}).

gamma(Host, Image,Gamma) ->
	rpc(Host, {gamma_1,Image,Gamma}).

gamma(Host, Image,GammaRed,GammaGreen,GammaBlue) ->
	rpc(Host, {gamma_3,Image,GammaRed,GammaGreen,GammaBlue}).

gaussianBlur(Host, Image,Width,Sigma) ->
	rpc(Host, {gaussianBlur_2,Image,Width,Sigma}).

implode(Host, Image,Factor) ->
	rpc(Host, {implode_1,Image,Factor}).

label(Host, Image,Label) ->
	rpc(Host, {label_1,Image,Label}).

level(Host, Image,Black_point,White_point,Mid_point) ->
	rpc(Host, {level_3,Image,Black_point,White_point,Mid_point}).

levelChannel(Host, Image,Channel,Black_point,White_point,Mid_point) ->
	rpc(Host, {levelChannel_4,Image,Channel,Black_point,White_point,Mid_point}).

magnify(Host, Image) ->
	rpc(Host, {magnify_0,Image}).

map(Host, Image,MapImage,Dither) ->
	rpc(Host, {map_2,Image,MapImage,Dither}).

%%// not implemented because of parameter not implemented
medianFilter(Host, Image,Radius) ->
	rpc(Host, {medianFilter_1,Image,Radius}).

minify(Host, Image) ->
	rpc(Host, {minify_0,Image}).

modulate(Host, Image,Brightness,Saturation,Hue) ->
	rpc(Host, {modulate_3,Image,Brightness,Saturation,Hue}).

negate(Host, Image,Grayscale) ->
	rpc(Host, {negate_1,Image,Grayscale}).

normalize(Host, Image) ->
	rpc(Host, {normalize_0,Image}).

oilPaint(Host, Image,Radius) ->
	rpc(Host, {oilPaint_1,Image,Radius}).

opacity(Host, Image,Opacity) ->
	rpc(Host, {opacity_1,Image,Opacity}).

opaque(Host, Image,OpaqueColor_red,OpaqueColor_green,OpaqueColor_blue,PenColor_red,PenColor_green,PenColor_blue) ->
	rpc(Host, {opaque_2,Image,OpaqueColor_red,OpaqueColor_green,OpaqueColor_blue,PenColor_red,PenColor_green,PenColor_blue}).

ping(Host, Image,ImageSpec) ->
	rpc(Host, {ping_1,Image,ImageSpec}).

%%ping not implemented because of parameter ??Blob
quantize(Host, Image,MeasureError) ->
	rpc(Host, {quantize_1,Image,MeasureError}).

%%quantumOperator not implemented because of parameter ??MagickEvaluateOperator
%%quantumOperator not implemented because of parameter ??MagickEvaluateOperator
%%process not implemented because of parameter **argv_
raise(Host, Image,Geometry,RaisedFlag) ->
	rpc(Host, {raise_2,Image,Geometry,RaisedFlag}).

%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
%%read not implemented because of parameter not implemented
reduceNoise(Host, Image) ->
	rpc(Host, {reduceNoise_0,Image}).

reduceNoise(Host, Image,Order) ->
	rpc(Host, {reduceNoise_1,Image,Order}).

roll(Host, Image,Columns,Rows) ->
	rpc(Host, {roll_2,Image,Columns,Rows}).

rotate(Host, Image,Degrees) ->
	rpc(Host, {rotate_1,Image,Degrees}).

sample(Host, Image,Geometry) ->
	rpc(Host, {sample_1,Image,Geometry}).

scale(Host, Image,Geometry) ->
	rpc(Host, {scale_1,Image,Geometry}).

segment(Host, Image,ClusterThreshold,SmoothingThreshold) ->
	rpc(Host, {segment_2,Image,ClusterThreshold,SmoothingThreshold}).

shade(Host, Image,Azimuth,Elevation,ColorShading) ->
	rpc(Host, {shade_3,Image,Azimuth,Elevation,ColorShading}).

sharpen(Host, Image,Radius,Sigma) ->
	rpc(Host, {sharpen_2,Image,Radius,Sigma}).

shave(Host, Image,Geometry) ->
	rpc(Host, {shave_1,Image,Geometry}).

shear(Host, Image,XShearAngle,YShearAngle) ->
	rpc(Host, {shear_2,Image,XShearAngle,YShearAngle}).

solarize(Host, Image,Factor) ->
	rpc(Host, {solarize_1,Image,Factor}).

spread(Host, Image,Amount) ->
	rpc(Host, {spread_1,Image,Amount}).

stegano(Host, Image,Watermark) ->
	rpc(Host, {stegano_1,Image,Watermark}).

stereo(Host, Image,RightImage) ->
	rpc(Host, {stereo_1,Image,RightImage}).

swirl(Host, Image,Degrees) ->
	rpc(Host, {swirl_1,Image,Degrees}).

texture(Host, Image,Texture) ->
	rpc(Host, {texture_1,Image,Texture}).

threshold(Host, Image,Threshold) ->
	rpc(Host, {threshold_1,Image,Threshold}).

transform(Host, Image,ImageGeometry) ->
	rpc(Host, {transform_1,Image,ImageGeometry}).

transform(Host, Image,ImageGeometry,CropGeometry) ->
	rpc(Host, {transform_2,Image,ImageGeometry,CropGeometry}).

transparent(Host, Image,Color_red,Color_green,Color_blue) ->
	rpc(Host, {transparent_1,Image,Color_red,Color_green,Color_blue}).

trim(Host, Image) ->
	rpc(Host, {trim_0,Image}).

type(Host, Image,Type) ->
	rpc(Host, {type_1,Image,Type}).

unsharpmask(Host, Image,Radius,Sigma,Amount,Threshold) ->
	rpc(Host, {unsharpmask_4,Image,Radius,Sigma,Amount,Threshold}).

wave(Host, Image,Amplitude,Wavelength) ->
	rpc(Host, {wave_2,Image,Amplitude,Wavelength}).

write(Host, Image,ImageSpec) ->
	rpc(Host, {write_1,Image,ImageSpec}).

%%write not implemented because of parameter *blob_
%%write not implemented because of parameter *blob_
%%write not implemented because of parameter *blob_
%%write not implemented because of parameter ??StorageType
zoom(Host, Image,Geometry) ->
	rpc(Host, {zoom_1,Image,Geometry}).

