import math

from scipy.spatial.distance import euclidean

from mahotas.features import zernike_moments
from shape_context import ShapeContext
import matplotlib.pyplot as plt
import cv2
import numpy as np

from cv_utils.util import show_bgr_image
from cv_utils import preprocess_image

sc = ShapeContext()

im = cv2.imread('img/heart_2.png')
im2 = cv2.imread('img/heart_1.png')


def binarize_image(im):
    im = preprocess_image(im)

    gray = cv2.cvtColor(im, cv2.COLOR_BGR2GRAY)
    ret, binarized = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)
    return binarized


def match_shapes(im1, im2):
    im1 = binarize_image(im1)
    im2 = binarize_image(im2)
    c1, _ = cv2.findContours(im1, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    c2, _ = cv2.findContours(im2, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
    c1 = c1[0]
    c2 = c2[0]

    center1, radius1 = cv2.minEnclosingCircle(c1)
    center2, radius2 = cv2.minEnclosingCircle(c2)

    zernike_moments1 = zernike_moments(im1, int(radius1), degree= 20, cm=(int(center1[0]), int(center1[1])))
    zernike_moments2 = zernike_moments(im2, int(radius2), degree= 20, cm=(int(center2[0]), int(center2[1])))

    print zernike_moments1
    print zernike_moments2

    zernike_match = euclidean(zernike_moments1, zernike_moments2)

    hu_match = cv2.matchShapes(im1, im2, cv2.CONTOURS_MATCH_I2, 0)
    return hu_match, zernike_match


def get_points(im, n_samples=100):

    # denoise
    im = preprocess_image(im)

    gray = cv2.cvtColor(im, cv2.COLOR_BGR2GRAY)
    ret, binarized = cv2.threshold(gray, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)

    plt.imshow(binarized)
    plt.show()

    contours, _ = cv2.findContours(binarized, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)

    c = contours[0]

    points = np.array(c).reshape((-1, 2))
    points = list(points.tolist())
    step = int(math.ceil(len(points) / float(n_samples)))
    points = [points[i] for i in xrange(0, len(points), step)][:n_samples]
    return points

points_a = get_points(im)
points_b = get_points(im2)

# (cx, cy), radius = cv2.minEnclosingCircle(c)
# center = (int(cx),int(cy))
# radius = int(radius)
# circle_im = np.zeros(im.shape[:2], dtype=np.uint8)
# cv2.circle(circle_im, center, radius, color=255, thickness=cv2.FILLED)
#
# contours, _ = cv2.findContours(circle_im, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_NONE)
# c = contours[0]
#
# points = np.array(c).reshape((-1, 2))
# points = list(points.tolist())
# step = int(math.ceil(len(points) / float(n_samples)))
# points = [points[i] for i in xrange(0, len(points), step)][:n_samples]
# points_b = points

test_im = np.zeros(im.shape, dtype=np.uint8)
for point in points_a:
    cv2.drawMarker(test_im, (point[0], point[1]), color=(255, 0,0), markerSize=2)
for point in points_b:
    cv2.drawMarker(test_im, (point[0], point[1]), color=(0,0,255), markerSize=2)



P = sc.compute(points_a)
Q = sc.compute(points_b)

diff, matches = sc.diff(P, Q)
print diff

print match_shapes(im, im2)

for match in matches:
    start_point = points_a[match[0]]
    end_point = points_b[match[1]]
    cv2.line(test_im, (start_point[0], start_point[1]), (end_point[0], end_point[1]), color=(0,255,0))

show_bgr_image(test_im)