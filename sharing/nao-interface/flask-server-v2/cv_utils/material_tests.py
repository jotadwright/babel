import cv2 as cv
from scipy.ndimage.filters import generic_filter
import numpy as np
import matplotlib.pyplot as plt


def build_filters():
    filters = []
    ksize = 21
    for theta in np.arange(0, np.pi, np.pi / 16):
        kern = cv.getGaborKernel((ksize, ksize), 4.0, theta, 10.0, 0.5, 0, ktype=cv.CV_32F)
        kern /= 1.5 * kern.sum()
        filters.append(kern)
    return filters


def process(img, filters):
    accum = np.zeros_like(img)
    for kern in filters:
        fimg = cv.filter2D(img, cv.CV_8UC3, kern)
        np.maximum(accum, fimg, accum)
    return accum


im = cv.imread('img/cube2.png')
im = cv.cvtColor(im, cv.COLOR_BGR2HSV)
std_filter = generic_filter(im, np.std, size=3)
#show_gray_image(std_filter[:,:,1], "Filtered (rubber)")
max = std_filter[:, :, 2 ].max()
plt.imshow(std_filter[:,:,2], vmin=0, vmax=max)

plt.show()

im = cv.imread('img/shiny_cube.png')
im = cv.cvtColor(im, cv.COLOR_BGR2HSV)
std_filter = generic_filter(im, np.std, size=3)
plt.imshow(std_filter[:,:,2], vmin=0, vmax=max)
plt.show()
