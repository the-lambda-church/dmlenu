#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <cairo_ocaml.h>

#include "x11.h"

/* ocaml stubs */

static struct x11_ctx* current_x11_ctx = NULL;

value caml_x11_init(value bottom, value monitor)
{
  CAMLparam2(bottom, monitor);
  current_x11_ctx = dml_x11_create(Bool_val(bottom), Int_val(monitor));
  if(current_x11_ctx) {
    CAMLreturn(Val_true);
  } else {
    CAMLreturn(Val_false);
  }
}

value caml_x11_terminate(value unit)
{
  CAMLparam1(unit);
  dml_x11_destroy(current_x11_ctx);
  current_x11_ctx = NULL;
  CAMLreturn(Val_unit);
}

value caml_x11_resize_height(value height)
{
  CAMLparam1(height);
  dml_x11_resize_height(current_x11_ctx, Int_val(height));
  CAMLreturn(Val_unit);
}

value caml_x11_get_cairo_surface(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(vsurf);
  cairo_surface_t* surf = current_x11_ctx->cairo_surface;
  vsurf = alloc_custom(&caml_surface_ops, sizeof(void*), 1, 50);
  SURFACE_VAL(vsurf) = surf;
  CAMLreturn(vsurf);
}

value caml_x11_flush(value unit)
{
  CAMLparam1(unit);
  XFlush(current_x11_ctx->display);
  CAMLreturn(Val_unit);
}

value caml_x11_get_width(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_int(current_x11_ctx->width));
}

value caml_x11_get_height(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_int(current_x11_ctx->cur_height));
}

value caml_x11_next_key_event(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);
  uint64_t keysym;
  uint32_t mods, unicode;
  dml_x11_next_key_event(current_x11_ctx, &keysym, &mods, &unicode);
  ret = caml_alloc_tuple(3);
  Field(ret, 0) = Val_int(keysym);
  Field(ret, 1) = Val_int(mods);
  Field(ret, 2) = Val_int(unicode);
  CAMLreturn(ret);
}
