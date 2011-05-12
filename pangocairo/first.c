#include <stdio.h>
#include <pango/pangocairo.h>

void rendertext(cairo_t *cr) {
	PangoLayout *layout;
	PangoFontDescription *desc;

	cairo_translate(cr,10,20);

	layout = pango_cairo_create_layout(cr);

//	pango_layout_set_text(layout,"கௌஷிக் ",-1);
	pango_layout_set_text(layout,"koushik",-1);

	desc = pango_font_description_from_string("Sans Bold 12");

	pango_layout_set_font_description(layout,desc);

	pango_font_description_free(desc);

	cairo_set_source_rgb(cr,0.0,0.0,1.0);

	pango_cairo_update_layout(cr,layout);

	pango_cairo_show_layout(cr,layout);

	g_object_unref(layout);

}


int main(int argc,char **argv) {

	cairo_t *cr;
	cairo_status_t status;
	cairo_surface_t *surface;

	surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,300,100);

	cr = cairo_create(surface);

	cairo_set_source_rgb(cr,1.0,1.0,1.0);

	cairo_paint(cr);

	rendertext(cr);

	cairo_destroy(cr);

	status = cairo_surface_write_to_png(surface,"out.png");

	cairo_surface_destroy(surface);

	return 0;

}
