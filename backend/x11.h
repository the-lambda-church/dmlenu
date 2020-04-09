#ifndef _DMLENU_X11_H_
#define _DMLENU_X11_H_

#include <X11/Xlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <cairo/cairo-xlib.h>

struct x11_ctx {
  Display* display;
  int32_t screen;
  Drawable drawable;
  XIM xim;
  XIC xic;
  Visual *visual;

  cairo_surface_t* cairo_surface;

  bool bottom;
  uint32_t monitor;

  /* These are the characteristics of the monitor, they do not change through
     the execution */
  uint32_t x0, y0, width, max_height;

  /* This may be modified by the user through the execution when resizing the
     window */
  uint32_t cur_height;
};

struct x11_ctx* dml_x11_create(int bottom, uint32_t monitor);
void dml_x11_destroy(struct x11_ctx* x11);
void dml_x11_resize_height(struct x11_ctx* x11, uint32_t height);
void dml_x11_next_key_event(struct x11_ctx* x11, uint64_t* keysym,
                            uint32_t* mods, uint32_t* unicode);

#endif /* _DMLENU_X11_H_ */
