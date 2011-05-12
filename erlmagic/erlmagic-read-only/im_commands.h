	else if (command == "adaptiveThreshold_3") {
		Image& image = get_image(2, msg);
		int width = ERL_INT_VALUE(erl_element(3, msg));
		int height = ERL_INT_VALUE(erl_element(4, msg));
		int offset = ERL_INT_VALUE(erl_element(5, msg));
		image.adaptiveThreshold(width,height,offset);
		erl_send(fd, pid, ok);
	}
	else if (command == "addNoise_1") {
		Image& image = get_image(2, msg);
		string noiseType = erl_iolist_to_string(erl_element(3, msg));
		image.addNoise(get_noise_type(noiseType));
		erl_send(fd, pid, ok);
	}
		//affineTransform not implemented because of parameter ??DrawableAffine
	else if (command == "annotate_2") {
		Image& image = get_image(2, msg);
		std::string text = erl_iolist_to_string(erl_element(3, msg));
		string location = erl_iolist_to_string(erl_element(4, msg));
		image.annotate(text,Geometry(location));
		erl_send(fd, pid, ok);
	}
	else if (command == "annotate_3") {
		Image& image = get_image(2, msg);
		std::string text = erl_iolist_to_string(erl_element(3, msg));
		string boundingArea = erl_iolist_to_string(erl_element(4, msg));
		string gravity = erl_iolist_to_string(erl_element(5, msg));
		image.annotate(text,Geometry(boundingArea),get_gravity_type(gravity));
		erl_send(fd, pid, ok);
	}
	else if (command == "annotate_4") {
		Image& image = get_image(2, msg);
		std::string text = erl_iolist_to_string(erl_element(3, msg));
		string boundingArea = erl_iolist_to_string(erl_element(4, msg));
		string gravity = erl_iolist_to_string(erl_element(5, msg));
		double degrees = ERL_FLOAT_VALUE(erl_element(6, msg));
		image.annotate(text,Geometry(boundingArea),get_gravity_type(gravity),degrees);
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
	else if (command == "blur_2") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		double sigma = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.blur(radius,sigma);
		erl_send(fd, pid, ok);
	}
	else if (command == "border_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.border(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "channel_1") {
		Image& image = get_image(2, msg);
		string channel = erl_iolist_to_string(erl_element(3, msg));
		image.channel(get_channel_type(channel));
		erl_send(fd, pid, ok);
	}
	else if (command == "channelDepth_2") {
		Image& image = get_image(2, msg);
		string channel = erl_iolist_to_string(erl_element(3, msg));
		int depth = ERL_INT_VALUE(erl_element(4, msg));
		image.channelDepth(get_channel_type(channel),depth);
		erl_send(fd, pid, ok);
	}
	else if (command == "channelDepth_1") {
		Image& image = get_image(2, msg);
		string channel = erl_iolist_to_string(erl_element(3, msg));
		image.channelDepth(get_channel_type(channel));
		erl_send(fd, pid, ok);
	}
	else if (command == "charcoal_2") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		double sigma = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.charcoal(radius,sigma);
		erl_send(fd, pid, ok);
	}
	else if (command == "chop_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.chop(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "colorize_4") {
		Image& image = get_image(2, msg);
		int opacityRed = ERL_INT_VALUE(erl_element(3, msg));
		int opacityGreen = ERL_INT_VALUE(erl_element(4, msg));
		int opacityBlue = ERL_INT_VALUE(erl_element(5, msg));
		double penColor_red = ERL_FLOAT_VALUE(erl_element(6, msg));
		double penColor_green = ERL_FLOAT_VALUE(erl_element(7, msg));
		double penColor_blue = ERL_FLOAT_VALUE(erl_element(8, msg));
		image.colorize(opacityRed,opacityGreen,opacityBlue,ColorRGB(penColor_red,penColor_green,penColor_blue));
		erl_send(fd, pid, ok);
	}
	else if (command == "colorize_2") {
		Image& image = get_image(2, msg);
		int opacity = ERL_INT_VALUE(erl_element(3, msg));
		double penColor_red = ERL_FLOAT_VALUE(erl_element(4, msg));
		double penColor_green = ERL_FLOAT_VALUE(erl_element(5, msg));
		double penColor_blue = ERL_FLOAT_VALUE(erl_element(6, msg));
		image.colorize(opacity,ColorRGB(penColor_red,penColor_green,penColor_blue));
		erl_send(fd, pid, ok);
	}
	else if (command == "comment_1") {
		Image& image = get_image(2, msg);
		std::string comment = erl_iolist_to_string(erl_element(3, msg));
		image.comment(comment);
		erl_send(fd, pid, ok);
	}
		//compose not implemented because of parameter not implemented
		//compose not implemented because of parameter not implemented
	else if (command == "compare_1") {
		Image& image = get_image(2, msg);
		Image& reference = get_image(3, msg);
		image.compare(reference);
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
	else if (command == "composite_3") {
		Image& image = get_image(2, msg);
		Image& compositeImage = get_image(3, msg);
		string offset = erl_iolist_to_string(erl_element(4, msg));
		string compose = erl_iolist_to_string(erl_element(5, msg));
		image.composite(compositeImage,Geometry(offset),get_composite(compose));
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
	else if (command == "contrast_1") {
		Image& image = get_image(2, msg);
		int sharpen = ERL_INT_VALUE(erl_element(3, msg));
		image.contrast(sharpen);
		erl_send(fd, pid, ok);
	}
		//convolve not implemented because of parameter *kernel_
	else if (command == "crop_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.crop(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "cycleColormap_1") {
		Image& image = get_image(2, msg);
		int amount = ERL_INT_VALUE(erl_element(3, msg));
		image.cycleColormap(amount);
		erl_send(fd, pid, ok);
	}
	else if (command == "despeckle_0") {
		Image& image = get_image(2, msg);
		image.despeckle();
		erl_send(fd, pid, ok);
	}
	else if (command == "display_0") {
		Image& image = get_image(2, msg);
		image.display();
		erl_send(fd, pid, ok);
	}
		//draw not implemented because of parameter ??Drawable
		//draw not implemented because of parameter ??std::list<Magick::Drawable>
	else if (command == "edge_1") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.edge(radius);
		erl_send(fd, pid, ok);
	}
	else if (command == "emboss_2") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		double sigma = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.emboss(radius,sigma);
		erl_send(fd, pid, ok);
	}
	else if (command == "enhance_0") {
		Image& image = get_image(2, msg);
		image.enhance();
		erl_send(fd, pid, ok);
	}
	else if (command == "equalize_0") {
		Image& image = get_image(2, msg);
		image.equalize();
		erl_send(fd, pid, ok);
	}
	else if (command == "erase_0") {
		Image& image = get_image(2, msg);
		image.erase();
		erl_send(fd, pid, ok);
	}
	else if (command == "flip_0") {
		Image& image = get_image(2, msg);
		image.flip();
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
	else if (command == "floodFillColor_2") {
		Image& image = get_image(2, msg);
		string point = erl_iolist_to_string(erl_element(3, msg));
		double fillColor_red = ERL_FLOAT_VALUE(erl_element(4, msg));
		double fillColor_green = ERL_FLOAT_VALUE(erl_element(5, msg));
		double fillColor_blue = ERL_FLOAT_VALUE(erl_element(6, msg));
		image.floodFillColor(Geometry(point),ColorRGB(fillColor_red,fillColor_green,fillColor_blue));
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
	else if (command == "floodFillColor_3") {
		Image& image = get_image(2, msg);
		string point = erl_iolist_to_string(erl_element(3, msg));
		double fillColor_red = ERL_FLOAT_VALUE(erl_element(4, msg));
		double fillColor_green = ERL_FLOAT_VALUE(erl_element(5, msg));
		double fillColor_blue = ERL_FLOAT_VALUE(erl_element(6, msg));
		double borderColor_red = ERL_FLOAT_VALUE(erl_element(7, msg));
		double borderColor_green = ERL_FLOAT_VALUE(erl_element(8, msg));
		double borderColor_blue = ERL_FLOAT_VALUE(erl_element(9, msg));
		image.floodFillColor(Geometry(point),ColorRGB(fillColor_red,fillColor_green,fillColor_blue),ColorRGB(borderColor_red,borderColor_green,borderColor_blue));
		erl_send(fd, pid, ok);
	}
	else if (command == "floodFillOpacity_4") {
		Image& image = get_image(2, msg);
		int x = ERL_INT_VALUE(erl_element(3, msg));
		int y = ERL_INT_VALUE(erl_element(4, msg));
		int opacity = ERL_INT_VALUE(erl_element(5, msg));
		string method = erl_iolist_to_string(erl_element(6, msg));
		image.floodFillOpacity(x,y,opacity,get_paint_method(method));
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
		//// not implemented because of parameter not implemented
		//// not implemented because of parameter not implemented
	else if (command == "floodFillTexture_3") {
		Image& image = get_image(2, msg);
		string point = erl_iolist_to_string(erl_element(3, msg));
		Image& texture = get_image(4, msg);
		double borderColor_red = ERL_FLOAT_VALUE(erl_element(5, msg));
		double borderColor_green = ERL_FLOAT_VALUE(erl_element(6, msg));
		double borderColor_blue = ERL_FLOAT_VALUE(erl_element(7, msg));
		image.floodFillTexture(Geometry(point),texture,ColorRGB(borderColor_red,borderColor_green,borderColor_blue));
		erl_send(fd, pid, ok);
	}
	else if (command == "flop_0") {
		Image& image = get_image(2, msg);
		image.flop();
		erl_send(fd, pid, ok);
	}
	else if (command == "frame_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.frame(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "frame_4") {
		Image& image = get_image(2, msg);
		int width = ERL_INT_VALUE(erl_element(3, msg));
		int height = ERL_INT_VALUE(erl_element(4, msg));
		int innerBevel = ERL_INT_VALUE(erl_element(5, msg));
		int outerBevel = ERL_INT_VALUE(erl_element(6, msg));
		image.frame(width,height,innerBevel,outerBevel);
		erl_send(fd, pid, ok);
	}
	else if (command == "gamma_1") {
		Image& image = get_image(2, msg);
		double gamma = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.gamma(gamma);
		erl_send(fd, pid, ok);
	}
	else if (command == "gamma_3") {
		Image& image = get_image(2, msg);
		double gammaRed = ERL_FLOAT_VALUE(erl_element(3, msg));
		double gammaGreen = ERL_FLOAT_VALUE(erl_element(4, msg));
		double gammaBlue = ERL_FLOAT_VALUE(erl_element(5, msg));
		image.gamma(gammaRed,gammaGreen,gammaBlue);
		erl_send(fd, pid, ok);
	}
	else if (command == "gaussianBlur_2") {
		Image& image = get_image(2, msg);
		double width = ERL_FLOAT_VALUE(erl_element(3, msg));
		double sigma = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.gaussianBlur(width,sigma);
		erl_send(fd, pid, ok);
	}
	else if (command == "implode_1") {
		Image& image = get_image(2, msg);
		double factor = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.implode(factor);
		erl_send(fd, pid, ok);
	}
	else if (command == "label_1") {
		Image& image = get_image(2, msg);
		std::string label = erl_iolist_to_string(erl_element(3, msg));
		image.label(label);
		erl_send(fd, pid, ok);
	}
	else if (command == "level_3") {
		Image& image = get_image(2, msg);
		double black_point = ERL_FLOAT_VALUE(erl_element(3, msg));
		double white_point = ERL_FLOAT_VALUE(erl_element(4, msg));
		double mid_point = ERL_FLOAT_VALUE(erl_element(5, msg));
		image.level(black_point,white_point,mid_point);
		erl_send(fd, pid, ok);
	}
	else if (command == "levelChannel_4") {
		Image& image = get_image(2, msg);
		string channel = erl_iolist_to_string(erl_element(3, msg));
		double black_point = ERL_FLOAT_VALUE(erl_element(4, msg));
		double white_point = ERL_FLOAT_VALUE(erl_element(5, msg));
		double mid_point = ERL_FLOAT_VALUE(erl_element(6, msg));
		image.levelChannel(get_channel_type(channel),black_point,white_point,mid_point);
		erl_send(fd, pid, ok);
	}
	else if (command == "magnify_0") {
		Image& image = get_image(2, msg);
		image.magnify();
		erl_send(fd, pid, ok);
	}
	else if (command == "map_2") {
		Image& image = get_image(2, msg);
		Image& mapImage = get_image(3, msg);
		bool dither = ERL_INT_VALUE(erl_element(4, msg));
		image.map(mapImage,dither);
		erl_send(fd, pid, ok);
	}
		//// not implemented because of parameter not implemented
	else if (command == "medianFilter_1") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.medianFilter(radius);
		erl_send(fd, pid, ok);
	}
	else if (command == "minify_0") {
		Image& image = get_image(2, msg);
		image.minify();
		erl_send(fd, pid, ok);
	}
	else if (command == "modulate_3") {
		Image& image = get_image(2, msg);
		double brightness = ERL_FLOAT_VALUE(erl_element(3, msg));
		double saturation = ERL_FLOAT_VALUE(erl_element(4, msg));
		double hue = ERL_FLOAT_VALUE(erl_element(5, msg));
		image.modulate(brightness,saturation,hue);
		erl_send(fd, pid, ok);
	}
	else if (command == "negate_1") {
		Image& image = get_image(2, msg);
		bool grayscale = ERL_INT_VALUE(erl_element(3, msg));
		image.negate(grayscale);
		erl_send(fd, pid, ok);
	}
	else if (command == "normalize_0") {
		Image& image = get_image(2, msg);
		image.normalize();
		erl_send(fd, pid, ok);
	}
	else if (command == "oilPaint_1") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.oilPaint(radius);
		erl_send(fd, pid, ok);
	}
	else if (command == "opacity_1") {
		Image& image = get_image(2, msg);
		int opacity = ERL_INT_VALUE(erl_element(3, msg));
		image.opacity(opacity);
		erl_send(fd, pid, ok);
	}
	else if (command == "opaque_2") {
		Image& image = get_image(2, msg);
		double opaqueColor_red = ERL_FLOAT_VALUE(erl_element(3, msg));
		double opaqueColor_green = ERL_FLOAT_VALUE(erl_element(4, msg));
		double opaqueColor_blue = ERL_FLOAT_VALUE(erl_element(5, msg));
		double penColor_red = ERL_FLOAT_VALUE(erl_element(6, msg));
		double penColor_green = ERL_FLOAT_VALUE(erl_element(7, msg));
		double penColor_blue = ERL_FLOAT_VALUE(erl_element(8, msg));
		image.opaque(ColorRGB(opaqueColor_red,opaqueColor_green,opaqueColor_blue),ColorRGB(penColor_red,penColor_green,penColor_blue));
		erl_send(fd, pid, ok);
	}
	else if (command == "ping_1") {
		Image& image = get_image(2, msg);
		std::string imageSpec = erl_iolist_to_string(erl_element(3, msg));
		image.ping(imageSpec);
		erl_send(fd, pid, ok);
	}
		//ping not implemented because of parameter ??Blob
	else if (command == "quantize_1") {
		Image& image = get_image(2, msg);
		bool measureError = ERL_INT_VALUE(erl_element(3, msg));
		image.quantize(measureError);
		erl_send(fd, pid, ok);
	}
		//quantumOperator not implemented because of parameter ??MagickEvaluateOperator
		//quantumOperator not implemented because of parameter ??MagickEvaluateOperator
		//process not implemented because of parameter **argv_
	else if (command == "raise_2") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		bool raisedFlag = ERL_INT_VALUE(erl_element(4, msg));
		image.raise(Geometry(geometry),raisedFlag);
		erl_send(fd, pid, ok);
	}
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
		//read not implemented because of parameter not implemented
	else if (command == "reduceNoise_0") {
		Image& image = get_image(2, msg);
		image.reduceNoise();
		erl_send(fd, pid, ok);
	}
	else if (command == "reduceNoise_1") {
		Image& image = get_image(2, msg);
		double order = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.reduceNoise(order);
		erl_send(fd, pid, ok);
	}
	else if (command == "roll_2") {
		Image& image = get_image(2, msg);
		int columns = ERL_INT_VALUE(erl_element(3, msg));
		int rows = ERL_INT_VALUE(erl_element(4, msg));
		image.roll(columns,rows);
		erl_send(fd, pid, ok);
	}
	else if (command == "rotate_1") {
		Image& image = get_image(2, msg);
		double degrees = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.rotate(degrees);
		erl_send(fd, pid, ok);
	}
	else if (command == "sample_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.sample(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "scale_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.scale(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "segment_2") {
		Image& image = get_image(2, msg);
		double clusterThreshold = ERL_FLOAT_VALUE(erl_element(3, msg));
		double smoothingThreshold = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.segment(clusterThreshold,smoothingThreshold);
		erl_send(fd, pid, ok);
	}
	else if (command == "shade_3") {
		Image& image = get_image(2, msg);
		double azimuth = ERL_FLOAT_VALUE(erl_element(3, msg));
		double elevation = ERL_FLOAT_VALUE(erl_element(4, msg));
		bool colorShading = ERL_INT_VALUE(erl_element(5, msg));
		image.shade(azimuth,elevation,colorShading);
		erl_send(fd, pid, ok);
	}
	else if (command == "sharpen_2") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		double sigma = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.sharpen(radius,sigma);
		erl_send(fd, pid, ok);
	}
	else if (command == "shave_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.shave(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "shear_2") {
		Image& image = get_image(2, msg);
		double xShearAngle = ERL_FLOAT_VALUE(erl_element(3, msg));
		double yShearAngle = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.shear(xShearAngle,yShearAngle);
		erl_send(fd, pid, ok);
	}
	else if (command == "solarize_1") {
		Image& image = get_image(2, msg);
		double factor = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.solarize(factor);
		erl_send(fd, pid, ok);
	}
	else if (command == "spread_1") {
		Image& image = get_image(2, msg);
		int amount = ERL_INT_VALUE(erl_element(3, msg));
		image.spread(amount);
		erl_send(fd, pid, ok);
	}
	else if (command == "stegano_1") {
		Image& image = get_image(2, msg);
		Image& watermark = get_image(3, msg);
		image.stegano(watermark);
		erl_send(fd, pid, ok);
	}
	else if (command == "stereo_1") {
		Image& image = get_image(2, msg);
		Image& rightImage = get_image(3, msg);
		image.stereo(rightImage);
		erl_send(fd, pid, ok);
	}
	else if (command == "swirl_1") {
		Image& image = get_image(2, msg);
		double degrees = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.swirl(degrees);
		erl_send(fd, pid, ok);
	}
	else if (command == "texture_1") {
		Image& image = get_image(2, msg);
		Image& texture = get_image(3, msg);
		image.texture(texture);
		erl_send(fd, pid, ok);
	}
	else if (command == "threshold_1") {
		Image& image = get_image(2, msg);
		double threshold = ERL_FLOAT_VALUE(erl_element(3, msg));
		image.threshold(threshold);
		erl_send(fd, pid, ok);
	}
	else if (command == "transform_1") {
		Image& image = get_image(2, msg);
		string imageGeometry = erl_iolist_to_string(erl_element(3, msg));
		image.transform(Geometry(imageGeometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "transform_2") {
		Image& image = get_image(2, msg);
		string imageGeometry = erl_iolist_to_string(erl_element(3, msg));
		string cropGeometry = erl_iolist_to_string(erl_element(4, msg));
		image.transform(Geometry(imageGeometry),Geometry(cropGeometry));
		erl_send(fd, pid, ok);
	}
	else if (command == "transparent_1") {
		Image& image = get_image(2, msg);
		double color_red = ERL_FLOAT_VALUE(erl_element(3, msg));
		double color_green = ERL_FLOAT_VALUE(erl_element(4, msg));
		double color_blue = ERL_FLOAT_VALUE(erl_element(5, msg));
		image.transparent(ColorRGB(color_red,color_green,color_blue));
		erl_send(fd, pid, ok);
	}
	else if (command == "trim_0") {
		Image& image = get_image(2, msg);
		image.trim();
		erl_send(fd, pid, ok);
	}
	else if (command == "type_1") {
		Image& image = get_image(2, msg);
		string type = erl_iolist_to_string(erl_element(3, msg));
		image.type(get_image_type(type));
		erl_send(fd, pid, ok);
	}
	else if (command == "unsharpmask_4") {
		Image& image = get_image(2, msg);
		double radius = ERL_FLOAT_VALUE(erl_element(3, msg));
		double sigma = ERL_FLOAT_VALUE(erl_element(4, msg));
		double amount = ERL_FLOAT_VALUE(erl_element(5, msg));
		double threshold = ERL_FLOAT_VALUE(erl_element(6, msg));
		image.unsharpmask(radius,sigma,amount,threshold);
		erl_send(fd, pid, ok);
	}
	else if (command == "wave_2") {
		Image& image = get_image(2, msg);
		double amplitude = ERL_FLOAT_VALUE(erl_element(3, msg));
		double wavelength = ERL_FLOAT_VALUE(erl_element(4, msg));
		image.wave(amplitude,wavelength);
		erl_send(fd, pid, ok);
	}
	else if (command == "write_1") {
		Image& image = get_image(2, msg);
		std::string imageSpec = erl_iolist_to_string(erl_element(3, msg));
		image.write(imageSpec);
		erl_send(fd, pid, ok);
	}
		//write not implemented because of parameter *blob_
		//write not implemented because of parameter *blob_
		//write not implemented because of parameter *blob_
		//write not implemented because of parameter ??StorageType
	else if (command == "zoom_1") {
		Image& image = get_image(2, msg);
		string geometry = erl_iolist_to_string(erl_element(3, msg));
		image.zoom(Geometry(geometry));
		erl_send(fd, pid, ok);
	}
