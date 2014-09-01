#include <memory.h>
#include <string.h>

#include "wrapper.h"

extern "C"
{

/* bitmap/image helpers */
EWXWEXPORT(wxBitmap*,wxBitmap_CreateFromImage)(wxImage* image,int depth)
{
  return new wxBitmap(*image,depth);
}


EWXWEXPORT(wxImage*,wxImage_CreateFromDataEx)(int width,int height,wxUint8* data,bool isStaticData)
{
  return new wxImage(width, height, data, isStaticData);
}


EWXWEXPORT(void,wxImage_Delete)(wxImage* image)
{
  delete image;
}


/* colours */
EWXWEXPORT(wxColour*,wxColour_CreateFromInt)(int rgb)
{
  return new wxColour((rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, rgb & 0xFF);
}

EWXWEXPORT(int,wxColour_GetInt)(wxColour* colour)
{
  int r = colour->Red();
  int g = colour->Green();
  int b = colour->Blue();
  return ((r << 16) | (g << 8) | b);
}

/* basic pixel manipulation */
EWXWEXPORT(void,wxcSetPixelRGB)(wxUint8* buffer,int width,int x,int y,int rgb)
{
  int indexR = 3*(width*y + x);
  buffer[indexR]   = rgb >> 16;
  buffer[indexR+1] = rgb >>  8;
  buffer[indexR+2] = rgb;
}

EWXWEXPORT(int,wxcGetPixelRGB)(wxUint8* buffer,int width,int x,int y)
{
  int indexR = 3*(width*y + x);
  int r,g,b;
  r = buffer[indexR];
  g = buffer[indexR+1];
  b = buffer[indexR+2];
  return ((r << 16) | (g << 8) | b);
}

EWXWEXPORT(void,wxcSetPixelRowRGB)(wxUint8* buffer,int width,int x,int y,int rgb0,int rgb1,int count)
{
  int r0  = ((rgb0 >> 16) & 0xFF);
  int g0  = ((rgb0 >>  8) & 0xFF);
  int b0  = (rgb0 & 0xFF);
  int start = 3*(width*y+x);
  int i;

  if (rgb0 == rgb1) {
    /* same color */
    for( i=0; i < count*3; i +=3) {
      buffer[start+i]   = r0;
      buffer[start+i+1] = g0;
      buffer[start+i+2] = b0;
    }
  }
  else {
    /* do linear interpolation of the color */
    int r1  = ((rgb1 >> 16) & 0xFF);
    int g1  = ((rgb1 >>  8) & 0xFF);
    int b1  = (rgb1 & 0xFF);

    int rd  = ((r1 - r0) << 16) / (count-1);
    int gd  = ((g1 - g0) << 16) / (count-1);
    int bd  = ((b1 - b0) << 16) / (count-1);

    int r   = r0 << 16;
    int g   = g0 << 16;
    int b   = b0 << 16;

    for( i = 0; i < count*3; i += 3 ) {
      buffer[start+i]   = (r >> 16);
      buffer[start+i+1] = (g >> 16);
      buffer[start+i+2] = (b >> 16);
      r += rd;
      g += gd;
      b += bd;
    }
  }
}

EWXWEXPORT(void,wxcInitPixelsRGB)(wxUint8* buffer,int width,int height,int rgb)
{
  int count        = width*height*3;
  wxUint8 r  = ((rgb >> 16) & 0xFF);
  wxUint8 g  = ((rgb >>  8) & 0xFF);
  wxUint8 b  = rgb & 0xFF;
  int i;

  if (r==g && g==b) {
    for( i=0; i < count; i++ ) {
      buffer[i] = r;
    }
  }
  else {
    for( i=0; i < count; i += 3) {
      buffer[i]   = r;
      buffer[i+1] = g;
      buffer[i+2] = b;
    }
  }
}

EWXWEXPORT(wxColour*,wxColour_CreateFromUnsignedInt)(unsigned int rgba)
{
  return new wxColour((rgba >> 24) & 0xFF, (rgba >> 16) & 0xFF, (rgba >> 8) & 0xFF, rgba & 0xFF);
}

EWXWEXPORT(unsigned int,wxColour_GetUnsignedInt)(wxColour* colour)
{
  int r = colour->Red();
  int g = colour->Green();
  int b = colour->Blue();
  int a = colour->Alpha();
  return ((r << 24) | (g << 16) | (b << 8) | a);
}

/* basic pixel manipulation */
EWXWEXPORT(void,wxcSetPixelRGBA)(wxUint8* buffer,int width,int x,int y,unsigned int rgba)
{
  unsigned int indexR = 4*(width*y + x);
  buffer[indexR]   = rgba >> 24;
  buffer[indexR+1] = rgba >> 16;
  buffer[indexR+2] = rgba >>  8;
  buffer[indexR+3] = rgba;
}

EWXWEXPORT(int,wxcGetPixelRGBA)(wxUint8* buffer,int width,int x,int y)
{
  unsigned int indexR = 4*(width*y + x);
  int r,g,b,a;
  r = buffer[indexR];
  g = buffer[indexR+1];
  b = buffer[indexR+2];
  a = buffer[indexR+3];
  return ((r << 24) | (g << 16) | (b << 8) | a);
}

EWXWEXPORT(void,wxcSetPixelRowRGBA)(wxUint8* buffer,int width,int x,int y,unsigned int rgba0,unsigned int rgba1,unsigned int count)
{
  int r0  = ((rgba0 >> 24) & 0xFF);
  int g0  = ((rgba0 >> 16) & 0xFF);
  int b0  = ((rgba0 >>  8) & 0xFF);
  int a0  = (rgba0 & 0xFF);
  unsigned int start = 4*(width*y+x);
  unsigned int i;

  if (rgba0 == rgba1) {
    /* same color */
    for( i=0; i < count*4; i +=4) {
      buffer[start+i]   = r0;
      buffer[start+i+1] = g0;
      buffer[start+i+2] = b0;
      buffer[start+i+3] = a0;
    }
  }
  else {
    /* do linear interpolation of the color */
    int r1  = ((rgba1 >> 24) & 0xFF);
    int g1  = ((rgba1 >> 16) & 0xFF);
    int b1  = ((rgba1 >>  8) & 0xFF);
    int a1  = (rgba1 & 0xFF);

    int rd  = ((r1 - r0) << 24) / (count-1);
    int gd  = ((g1 - g0) << 24) / (count-1);
    int bd  = ((b1 - b0) << 24) / (count-1);
    int ad  = ((a1 - a0) << 24) / (count-1);

    int r   = r0 << 24;
    int g   = g0 << 24;
    int b   = b0 << 24;
    int a   = b0 << 24;

    for( i = 0; i < count*4; i += 4 ) {
      buffer[start+i]   = (r >> 24);
      buffer[start+i+1] = (g >> 24);
      buffer[start+i+2] = (b >> 24);
      buffer[start+i+3] = (a >> 24);
      r += rd;
      g += gd;
      b += bd;
      a += ad;
    }
  }
}

EWXWEXPORT(void,wxcInitPixelsRGBA)(wxUint8* buffer,int width,int height,int rgba)
{
  unsigned int count        = width*height*4;
  wxUint8 r  = ((rgba >> 24) & 0xFF);
  wxUint8 g  = ((rgba >> 16) & 0xFF);
  wxUint8 b  = ((rgba >>  8) & 0xFF);
  wxUint8 a  = rgba & 0xFF;
  unsigned int i;

  if (r==g && g==b && b==a) {
    for( i=0; i < count; i++ ) {
      buffer[i] = r;
    }
  }
  else {
    for( i=0; i < count; i += 4) {
      buffer[i]   = r;
      buffer[i+1] = g;
      buffer[i+2] = b;
      buffer[i+3] = a;
    }
  }
}

}
