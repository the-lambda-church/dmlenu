#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <math.h>
#include <X11/Xutil.h>
#include <X11/extensions/Xinerama.h>
#include <cairo/cairo.h>
#include <cairo/cairo-xlib.h>

#include "x11.h"
#include "xkb_unicode.h"

static void die(const char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);

  if(fmt[0] != '\0' && fmt[strlen(fmt)-1] == ':') {
    fputc(' ', stderr);
    perror(NULL);
  }
  exit(EXIT_FAILURE);
}

void create_window(struct x11_ctx* x11)
{
  assert(x11);

  Display* dpy = x11->display;
  Window root = DefaultRootWindow(dpy);
  int screen = DefaultScreen(dpy);

  x11->screen = screen;
  x11->width = x11->cur_height = 1;
  x11->monitor = -1;
  x11->visual = DefaultVisual(dpy, screen);

  XSetWindowAttributes wa =
    {
     .override_redirect = True,
     .event_mask = ExposureMask | KeyPressMask | VisibilityChangeMask
    };

  XVisualInfo vinfo;
  int depth = DefaultDepth(dpy, screen);
  unsigned long valuemask = CWOverrideRedirect | CWEventMask | CWBackPixel;

  if (XMatchVisualInfo(dpy, screen, 32, TrueColor, &vinfo)) {
    depth = vinfo.depth;
    x11->visual = vinfo.visual;
    wa.background_pixmap = None;
    wa.border_pixel = 0;
    wa.colormap = XCreateColormap(dpy, root, x11->visual, AllocNone);
    valuemask = CWOverrideRedirect | CWEventMask | CWBackPixmap | CWColormap | CWBorderPixel;
  }

  x11->drawable =
    XCreateWindow(dpy, root, 0, 0, x11->width, x11->cur_height,
                  0, depth, CopyFromParent, x11->visual, valuemask, &wa);
  XSelectInput(dpy, x11->drawable, ButtonPressMask | KeyPressMask);
  XMapRaised(dpy, x11->drawable);
  x11->xim = XOpenIM(dpy, NULL, NULL, NULL);
  x11->xic =
    XCreateIC(x11->xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
              XNClientWindow, x11->drawable, XNFocusWindow,
              x11->drawable, NULL);
}

uint32_t compute_y (struct x11_ctx *x11) {
  return x11->y0 + (x11->bottom ? x11->max_height - x11->cur_height : 0);
}

void put_window_on_monitor(struct x11_ctx *x11)
{
  Window root = DefaultRootWindow(x11->display);

  {
    /* xinerama logic straight from dmenu */
#define INTERSECT(x,y,w,h,r)  (fmax(0, fmin((x)+(w),(r).x_org+(r).width) - fmax((x),(r).x_org)) * fmax(0, fmin((y)+(h),(r).y_org+(r).height) - fmax((y),(r).y_org)))

    int32_t n;
    XineramaScreenInfo *info;
    if ((info = XineramaQueryScreens(x11->display, &n))) {
      int32_t x, y, a, j, di, i = 0, area = 0;
      uint32_t du;
      Window w, pw, dw, *dws;
      XWindowAttributes wa;

      XGetInputFocus(x11->display, &w, &di);
      if (x11->monitor > 0)
        i = ((int32_t)x11->monitor > n ? n : (int32_t)x11->monitor) - 1;

      if (x11->monitor == 0 && w != root && w != PointerRoot && w != None) {
        /* find top-level window containing current input focus */
        do {
          if (XQueryTree(x11->display, (pw = w), &dw, &w, &dws, &du) && dws)
            XFree(dws);
        } while(w != root && w != pw);

        /* find xinerama screen with which the window intersects most */
        if (XGetWindowAttributes(x11->display, pw, &wa)) {
          for (j = 0; j < n; j++)
            if ((a = INTERSECT(wa.x, wa.y, wa.width, wa.height, info[j])) > area) {
              area = a;
              i = j;
            }
        }
      }

      /* no focused window is on screen, so use pointer location instead */
      if (x11->monitor == 0 && !area
          && XQueryPointer(x11->display, root, &dw, &dw, &x, &y, &di, &di, &du)) {
        for (i = 0; i < n; i++) {
          if (INTERSECT(x, y, 1, 1, info[i]) > 0)
            break;
        }
      }

      x11->x0 = info[i].x_org;
      x11->y0 = info[i].y_org;
      x11->width = info[i].width;
      x11->max_height = info[i].height;
      XFree(info);
    } else {
      x11->x0 = 0;
      x11->y0 = 0;
      x11->width = DisplayWidth(x11->display, x11->screen);
      x11->max_height = DisplayHeight(x11->display, x11->screen);
    }

#undef INTERSECT
  }

  XMoveResizeWindow(x11->display, x11->drawable, x11->x0, compute_y(x11),
                    x11->width, x11->cur_height);
  XFlush(x11->display);
}

static void grab_keyboard(Display* dpy, bool grab)
{
  if (grab) {
    for (uint32_t i = 0; i < 1000; ++i) {
      if (XGrabKeyboard(dpy, DefaultRootWindow(dpy), True,
                        GrabModeAsync, GrabModeAsync, CurrentTime)
          == GrabSuccess)
        return;
      usleep(1000);
    }
    die("x11: cannot grab keyboard\n");
  } else {
    XUngrabKeyboard(dpy, CurrentTime);
  }
}

struct x11_ctx* dml_x11_create(int bottom, uint32_t monitor)
{
  struct x11_ctx* x11;
  if(!(x11 = calloc(1, sizeof(struct x11_ctx))))
    die("Unable to allocate memory for x11_ctx\n");

  if(!(x11->display = XOpenDisplay(NULL)))
    die("Unable to open display\n");

  create_window(x11);

  XSetClassHint(x11->display, x11->drawable,
                (XClassHint[]){{ .res_name = "dmlenu", .res_class = "dmlenu"}});

  x11->bottom = bottom;
  x11->monitor = monitor;
  put_window_on_monitor(x11);

  x11->cairo_surface =
    cairo_xlib_surface_create(x11->display, x11->drawable, x11->visual,
                              x11->width, x11->cur_height);
  if(!x11->cairo_surface) {
    dml_x11_destroy(x11);
    die("Cannot allocate a cairo surface for the window\n");
  }
  cairo_xlib_surface_set_size(x11->cairo_surface, x11->width, x11->cur_height);

  grab_keyboard(x11->display, true);

  return x11;
}

void dml_x11_destroy(struct x11_ctx* x11)
{
  if(!x11 || !x11->display) return;

  grab_keyboard(x11->display, false);

  if(x11->cairo_surface)
    cairo_surface_destroy(x11->cairo_surface);

  if(x11->drawable)
    XDestroyWindow(x11->display, x11->drawable);

  XCloseDisplay(x11->display);
  free(x11);
}

void dml_x11_resize_height(struct x11_ctx* x11, uint32_t height)
{
  if(x11->cur_height == height)
    return;

  uint32_t old_y = compute_y(x11);
  x11->cur_height = height;
  uint32_t new_y = compute_y(x11);

  if(old_y != new_y) {
    XMoveResizeWindow(x11->display, x11->drawable, x11->x0, new_y,
                      x11->width, x11->cur_height);
  } else {
    XResizeWindow(x11->display, x11->drawable, x11->width, x11->cur_height);
  }

  cairo_xlib_surface_set_size(x11->cairo_surface, x11->width, x11->cur_height);
}

void dml_x11_next_key_event(struct x11_ctx* x11,
                            uint64_t* keysym,
                            uint32_t* mods,
                            uint32_t* unicode)
{
  XEvent ev;
  *keysym = NoSymbol;
  *mods = 0;

  for(;;) {
    if (XNextEvent(x11->display, &ev) || XFilterEvent(&ev, x11->drawable))
      continue;

    switch (ev.type) {
    case KeyPress:
      XmbLookupString(x11->xic, &ev.xkey, NULL, 0, keysym, NULL);
      *mods = ev.xkey.state;
      *unicode = bm_x11_key_sym2unicode(*keysym);
      return;

    case SelectionNotify:
      /* ? */
      break;

    case VisibilityNotify:
      if (ev.xvisibility.state != VisibilityUnobscured) {
        XRaiseWindow(x11->display, x11->drawable);
        XFlush(x11->display);
      }
      break;
    }
  }
}
