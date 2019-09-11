import os
from datetime import datetime

import numpy as np
import cv2 as cv

import matplotlib.pyplot as plt

DEBUG_IMAGE_PATH = os.path.join(os.getcwd(), 'debug_images')
if not os.path.exists(DEBUG_IMAGE_PATH):
    os.makedirs(DEBUG_IMAGE_PATH)


def _plot_bgr_image(image, title=''):
    rgb = cv.cvtColor(image, cv.COLOR_BGR2RGB)
    plt.imshow(rgb, vmin=0, vmax=255)
    plt.title(title)
    plt.axis("off")


def _save_bgr_image(image, title=''):
    rgb = cv.cvtColor(image, cv.COLOR_BGR2RGB)
    filepath = os.path.join(DEBUG_IMAGE_PATH, '%s.png' % title.lower().replace(" ", "_"))
    plt.imsave(filepath, rgb, vmin=0, vmax=255)


def show_bgr_image(image, title=''):
    _plot_bgr_image(image, title=title)
    plt.show()


def save_bgr_image(image, title=''):
    _save_bgr_image(image, title=title)


def current_time_in_millis():
    return datetime.now().microsecond / 1000


def show_hsv_image(image, title=''):
    bgr = cv.cvtColor(image, cv.COLOR_HSV2BGR)
    _plot_bgr_image(bgr, title=title)


def save_hsv_image(image, title=''):
    bgr = cv.cvtColor(image, cv.COLOR_HSV2BGR)
    _save_bgr_image(bgr, title=title)


def show_gray_image(gray, title=""):
    if np.mean(gray) < 1:
        vmax = 1
    else:
        vmax = 255
    plt.imshow(gray, vmin=0, vmax=vmax, cmap='gray')
    plt.title(title)
    plt.axis('off')
    plt.show()


def save_gray_image(gray, title=''):
    if np.mean(gray) < 1:
        vmax = 1
    else:
        vmax = 255
    filepath = os.path.join(DEBUG_IMAGE_PATH, '%s.png' % title.lower().replace(" ", "_"))
    plt.imsave(filepath, gray, vmin=0, vmax=vmax, cmap='gray')


def show_segmentation(segmentation, title=""):
    plt.imshow(segmentation, cmap='jet')
    plt.title(title)
    plt.axis('off')
    plt.show()


def save_segmentation(segmentation, title=''):
    filepath = os.path.join(DEBUG_IMAGE_PATH, '%s.png' % title.lower().replace(" ", "_"))
    plt.imsave(filepath, segmentation, cmap='jet')
