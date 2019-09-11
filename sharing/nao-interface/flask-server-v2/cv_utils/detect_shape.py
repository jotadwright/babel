import numpy as np
import cv2 as cv
from matplotlib import pyplot as plt
from sklearn.cluster import MeanShift, estimate_bandwidth
from sklearn.datasets.samples_generator import make_blobs
from itertools import cycle
from PIL import Image
import matplotlib.pylab as pylab
from skimage.segmentation import slic
from skimage.segmentation import mark_boundaries
from skimage.util import img_as_float
from skimage import io


def detect_shape(img):
    gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
    ret, binarized = cv.threshold(gray, 0, 255, cv.THRESH_BINARY_INV + cv.THRESH_OTSU)

    contours = cv.findContours(binarized, cv.RETR_TREE, cv.CHAIN_APPROX_SIMPLE)

    n_corners = 0

    cnt = None

    for cnt in contours[0]:
        area = cv.contourArea(cnt)
        if area > 500:
            approx = cv.approxPolyDP(cnt, 0.01 * cv.arcLength(cnt, True), True)
            cv.drawContours(img, [approx], 0, (255, 0, 0), 1)
            for ap in approx:
                cv.drawMarker(img, (ap[0][0], ap[0][1]), (0, 255, 0))
            n_corners = len(approx)
        break

    plt.imshow(img)
    plt.show()


def get_contour(img):
    gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
    ret, binarized = cv.threshold(gray, 0, 255, cv.THRESH_BINARY_INV + cv.THRESH_OTSU)

    contours, _ = cv.findContours(binarized, cv.RETR_TREE, cv.CHAIN_APPROX_SIMPLE)

    for cnt in contours:
        area = cv.contourArea(cnt)
        if area > 500:
            return cnt

    return None


def find_shape(prototypes, img):
    contour = get_contour(img)
    min_dist = 1000
    best_shape = None
    for cnt, shape in prototypes:
        dist = cv.matchShapes(contour, cnt, cv.CONTOURS_MATCH_I1, 0.0)
        if dist < min_dist:
            min_dist = dist
            best_shape = shape
    return best_shape


def find_shape_from_contour(contour, prototypes):
    min_dist = 1000
    best_shape = None
    for cnt, shape in prototypes:
        dist = cv.matchShapes(contour, cnt, cv.CONTOURS_MATCH_I1, 0.0)
        if dist < min_dist:
            min_dist = dist
            best_shape = shape
    return best_shape


def init_prototypes():
    prototypes = []

    contour = get_contour(cv.imread('img/cube.png'))
    if contour is not None:
        prototypes.append((contour, "cube"))

    contour = get_contour(cv.imread('img/sphere.png'))
    if contour is not None:
        prototypes.append((contour, "sphere"))

    contour = get_contour(cv.imread('img/cylinder.png'))
    if contour is not None:
        prototypes.append((contour, "cylinder"))

    return prototypes


if __name__ == '__main__':

    prototypes = init_prototypes()
    best_match = find_shape(prototypes, cv.imread('img/cube3.png'))
    detect_shape(cv.imread('img/cube3.png'))
    print best_match
