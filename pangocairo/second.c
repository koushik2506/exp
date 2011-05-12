#include <stdio.h>
#include <pango/pangocairo.h>

void pangotext(PangoLayout *layout) {

	PangoFontDescription *desc;

	pango_layout_set_text(layout,"Something else",-1);

	desc = pango_font_description_from_string("Sans Bold 12");

	pango_layout_set_font_description(layout,desc);

	pango_font_description_free(desc);
}

void rendertext(cairo_t *cr) {
	PangoLayout *layout;
	layout = pango_cairo_create_layout(cr);

	pangotext(layout);

	cairo_new_path(cr);
	cairo_move_to(cr,0,0);
	cairo_set_line_width(cr,0.5);

	cairo_set_source_rgb(cr,0.0,0.0,1.0);

	pango_cairo_layout_path(cr,layout);

	cairo_stroke_preserve(cr);

	g_object_unref(layout);

}

int main(int argc,char **argv){
	cairo_t *cr;
	cairo_status_t status;
	cairo_surface *surface;

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
