#include <stdio.h>
#include <pango/pangocairo.h>


int main(int argc,char **argv) {

	cairo_t *cr;
	cairo_status_t status;
	cairo_surface_t *surface;
	cairo_pattern_t *pattern;

	int widget_height,widget_width;

	widget_width = 100;
	widget_height = 20;

	surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,300,100);

	cr = cairo_create(surface);

	pattern = cairo_pattern_create_linear(0.0,0.0,0.0,widget_height);

	cairo_pattern_add_color_stop_rgba(pattern,1,0.65,0.65,0.65,1);
	cairo_pattern_add_color_stop_rgba(pattern,0,1.0,1.0,1.0,1);

	//cairo_rectangle(cr,0,0,widget_width,widget_height);

	cairo_set_source(cr,pattern);

	cairo_fill(cr);




	cairo_paint(cr);

	cairo_destroy(cr);

	status = cairo_surface_write_to_png(surface,"out.png");

	cairo_surface_destroy(surface);

	return 0;

}
