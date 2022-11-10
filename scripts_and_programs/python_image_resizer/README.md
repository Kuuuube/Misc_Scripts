# Python Image Resizer

Resizes all images in a folder to a specified size.

## Usage

- Place your images into the `resize` folder. (placeholder.png can be deleted)

- Edit the size on line 18 of `python_image_resizer.py`. 

    For example: Change `imResize = im.resize((1920,834), Image.ANTIALIAS)` to `imResize = im.resize((1080,720), Image.ANTIALIAS)` to resize to 1080x720p instead of 1920x834p.

- Run `python_image_reszier.py`

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `PIL/Pillow` module: To install it, enter the following command in cmd or a terminal:

```
pip install pillow
```

## Notes

- No copies of images are made. Copy your files elsewhere before using this if you are worried about losing data.

- `Image.MAX_IMAGE_PIXELS` is set to `99999999999999` to bypass PIL's built in ddos protection and allow for resizing of huge images.