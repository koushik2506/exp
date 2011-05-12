-module(example).
-compile(export_all).

init() ->	
    Host = imagelib:start(),
    Image = get_test_image(Host),
    {Host, Image}.

get_test_image(Host) ->
    Image = imagelib:read(Host, "test.jpg"),
    io:format("Image = ~p~n", [Image]),
    imagelib:scale(Host, Image, "20%"),
    Image.

test_adaptiveThreshold(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:adaptiveThreshold(Host, Ex, 5, 5, 5),
    imagelib:label(Host, Ex, "adaptiveTheshold"),
    Ex.

test_annotate(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:annotate(Host, Ex,"Erl Magick","+200+10"),
    imagelib:label(Host, Ex, "annotate"),
    Ex.

test_blur(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:blur(Host, Ex,0.0, 10.0),
    imagelib:label(Host, Ex, "blur"),
    Ex.

test_border(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:border(Host, Ex,"6x6"),
    imagelib:label(Host, Ex, "border"),
    Ex.

test_channel(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:channel(Host, Ex, "RedChannel"),
    imagelib:label(Host, Ex, "channel"),
    Ex.
    
test_charcoal(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:charcoal(Host, Ex,0.0,1.0),
    imagelib:label(Host, Ex, "charcoal"),
    Ex.

test_chop(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:chop(Host, Ex,"60x60+20+30"),
    imagelib:label(Host, Ex, "chop"),
    Ex.

test_composite(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    Smiley = imagelib:read(Host, "smiley.jpg"),
    imagelib:composite(Host, Ex,Smiley,"+200+130","OverCompositeOp"),
    imagelib:label(Host, Ex, "composite"),
    Ex.

test_contrast(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:contrast(Host, Ex, 40),
    imagelib:label(Host, Ex, "contrast"),
    Ex.

test_crop(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:crop(Host, Ex,"165x150+180+80"),
    imagelib:label(Host, Ex, "crop"),
    Ex.


test_despeckle(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:despeckle(Host, Ex),
    imagelib:label(Host, Ex, "despeckle"),
    Ex.


test_edge(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:edge(Host, Ex, 0.0),
    imagelib:label(Host, Ex, "edge"),
    Ex.

test_emboss(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:emboss(Host, Ex,0.0,1.0),
    imagelib:label(Host, Ex, "emboss"),
    Ex.

test_enhance(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:enhance(Host, Ex),
    imagelib:label(Host, Ex, "enhance"),
    Ex.

test_equalize(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:equalize(Host, Ex),
    imagelib:label(Host, Ex, "equalize"),
    Ex.

test_erase(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:erase(Host, Ex),
    imagelib:label(Host, Ex, "erase"),
    Ex.

test_flip(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:flip(Host, Ex),
    imagelib:label(Host, Ex, "flip"),
    Ex.


test_flop(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:flop(Host, Ex),
    imagelib:label(Host, Ex, "flop"),
    Ex.

test_frame(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:frame(Host, Ex, "15x15+3+3"),
    imagelib:label(Host, Ex, "frame"),
    Ex.

test_gamma(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:gamma(Host, Ex,1.6),
    imagelib:label(Host, Ex, "gamma"),
    Ex.

test_gaussianBlur(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:gaussianBlur(Host, Ex,0.0,1.5),
    imagelib:label(Host, Ex, "gaussianBlur"),
    Ex.

test_implode(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:implode(Host, Ex,-1.0),
    imagelib:label(Host, Ex, "implode"),
    Ex.

test_level(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:level(Host, Ex,10.0,250.0,1.0),
    imagelib:label(Host, Ex, "level"),
    Ex.


test_medianFilter(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:medianFilter(Host, Ex,0.0),
    imagelib:label(Host, Ex, "medianFilter"),
    Ex.


test_modulate(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:modulate(Host, Ex,110.0,110.0, 110.0),
    imagelib:label(Host, Ex, "modulate"),
    Ex.

test_negate(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:negate(Host, Ex, 0),
    imagelib:label(Host, Ex, "negate"),
    Ex.

test_normalize(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:normalize(Host, Ex),
    imagelib:label(Host, Ex, "normalize"),
    Ex.

test_oilPaint(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:oilPaint(Host, Ex, 3.0),
    imagelib:label(Host, Ex, "oilPaint"),
    Ex.


test_quantize(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:quantize(Host, Ex, 0),
    imagelib:label(Host, Ex, "quantize"),
    Ex.

test_raise(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:raise(Host, Ex, "10x10",0),
    imagelib:label(Host, Ex, "raise"),
    Ex.

test_reduceNoise(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:reduceNoise(Host, Ex),
    imagelib:label(Host, Ex, "reduceNoise"),
    Ex.

test_roll(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:roll(Host, Ex, 10, 10),
    imagelib:label(Host, Ex, "roll"),
    Ex.

test_rotate(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:rotate(Host, Ex,45.0),
    imagelib:label(Host, Ex, "rotate"),
    Ex.

test_sample(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:sample(Host, Ex, "60%"),
    imagelib:label(Host, Ex, "sample"),
    Ex.

test_scale(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:scale(Host, Ex, "60%"),
    imagelib:label(Host, Ex, "scale"),
    Ex.

test_segment(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:segment(Host, Ex, 1.0, 1.5),
    imagelib:label(Host, Ex, "segment"),
    Ex.

test_shade(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:shade(Host, Ex, 30.0, 30.0, 1),
    imagelib:label(Host, Ex, "shade"),
    Ex.

test_sharpen(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:sharpen(Host, Ex, 0.0, 1.0),
    imagelib:label(Host, Ex, "sharpen"),
    Ex.

test_shave(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:shave(Host, Ex,"10x10"),
    imagelib:label(Host, Ex, "shave"),
    Ex.

test_shear(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:shear(Host, Ex, -20.0, 20.0),
    imagelib:label(Host, Ex, "shear"),
    Ex.

test_solarize(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:solarize(Host, Ex, 50.0),
    imagelib:label(Host, Ex, "solarize"),
    Ex.

test_spread(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:spread(Host, Ex, 3),
    imagelib:label(Host, Ex, "spread"),
    Ex.


test_swirl(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:swirl(Host, Ex, 90.0),
    imagelib:label(Host, Ex, "swirl"),
    Ex.


test_unsharpmask(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:unsharpmask(Host, Ex, 3.0, 1.0, 5.0, 1.0),
    imagelib:label(Host, Ex, "unsharpmask"),
    Ex.

test_wave(Host, Image) ->
    Ex = imagelib:clone(Host, Image),
    imagelib:wave(Host, Ex, 25.0, 150.0),
    imagelib:label(Host, Ex, "wave"),
    Ex.

do_funs(L1, Host, Image, File) -> 
    L = [apply(example, element(1, X), [Host, Image]) || X <- L1, 
							 string:substr(atom_to_list(element(1, X)), 1, 5) == "test_"],
    Width = round((length(L)+1) / 5),
    Dim = integer_to_list(Width) ++ "x5",
    imagelib:montageImages(Host, L, [{tile, Dim},
				     {fillColor, "#600"}, 
				     {font, "Helvetica-Oblique"}, 
				     {pointSize, 18}, 
				     {gravity, "Center"}], File),
    Montage = imagelib:read(Host, File),
    imagelib:display(Host, Montage).

start() ->
    {Host, Image} = init(),
    Funs = example:module_info(exports),
    do_funs(Funs, Host, Image, "example1.jpg").
