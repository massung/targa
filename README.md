# Targa Image Loading for Common Lisp

A Truevision (TGA) image loader for Common Lisp.

This package makes no attempt at trying to put the TGA image loaded into a format that is good for use with any OS. It simply unpacks the image into a 2D array of pixel colors, where each pixel is a 4-element list of red, green, blue, and alpha floats in the range of [0,1]. 

It is assumed that can then be used by the developer to create OpenGL textures, or another image format useful to the user.

## Quickstart

Just use the `tga-load` and `tga-read` functions to load TGA files off disk or to read one from a stream.

	CL-USER > (setf tga (tga-load #p"tests/CTC16.tga"))
	#<TARGA::TGA 16 bit, 128x128, RLE-TRUE-COLOR-IMAGE>
	
If you use `tga-read`, be sure that the stream is an input stream of *element-type* `(unsigned-byte 8)`, otherwise the read will fail.

That's pretty much it. ;-)

Once you have the TGA structure, you can use the reader functions to find out everything about it. Most of the time what will be of most importance are the width, height, and the pixel data.

	CL-USER > (tga-header-width (tga-header tga))
	128
	
	CL-USER > (tga-header-height (tga-header tga))
	128
	
	CL-USER > (tga-get-pixel tga 34 16)
	(1.0 0.0 0.0 0.0)
	
The TGA pixels is actually an array of arrays. The first array are all the scanlines, and each scanline is an array of pixels.

A TGA file can have its screen origin on the y-axis be the bottom or top, the x-axis origin on the left or right. When reading in the TGA, no effort is made to keep a consistent origin for the pixels array. But, as long as you use the `tga-get-pixel` function, it will ensure that you get the "correct" pixel (originating from the top-left).

	(tga-get-pixel tga x y)  ;=> (r g b a)
	
## Getting extension values

When calling `tga-load` or `tga-read`, there is an optional (defaulting to `T`) keyword parameter: *read-ext-and-tags*. If this is non-nil, then, if this is a new TGA file with an extension area and developer tags, they will be read in.

If the TGA has an extension, there are several other optional areas that can then be read from the stream as well:

	(tga-read-color-correction-table stream tga)  ;=> table
	(tga-read-scanline-table stream tga)          ;=> scanlines
	(tga-read-postage-stamp-image stream tga)     ;=> tga

The *table* is an array of 256 lists, where each list is comprised of unsigned, 16-bit red, green, blue, and alpha values that can be used for color remapping. It is up to you to use these values in conjunction with `tga-get-pixel` however you'd like.

The *scanlines* is simply an array of offsets into the stream (1 per row for the image). Generally speaking it isn't very useful since all the scanlines have been pre-read. But it's here if you are interested.

The postage image is probably the most interesting. It will parse a smaller sized version of the TGA and return a new TGA that has the same format as its parent, but a different header and pixels array.

## Future... ?

I might add support in the future for writing TGA files, but that's a low priority unless I happen to get a lot of requests for it.

## Tests

To test this library, I use the images found at [http://www.fileformat.info/format/tga/sample/](http://www.fileformat.info/format/tga/sample/), which have targas saved with the various color formats, RLE compression, and different screen originations.