import itertools
import os

import cv2 as cv
import numpy as np
from matplotlib import pyplot as plt
from scipy.spatial.distance import euclidean
from skimage.feature import peak_local_max
from sklearn.cluster import MeanShift, estimate_bandwidth
from sklearn.cluster import get_bin_seeds

from cv_utils.color_utils import is_color, get_color_for_mask
import pymeanshift as pms

DIST_THRESH = 10
RANGE_RADIUS = 30


def segment_by_watershed(markers, im, unknown):
    # Add one to all labels so that sure background is not 0 but 1
    markers = markers + 1
    # Now, mark the region of unknown with zero
    markers[unknown == 1] = 0
    markers = markers.astype('int32')
    markers = cv.watershed(im, markers)
    # subtract one so that background is 0
    markers = markers - 1
    markers[markers < 0] = 0
    return markers


def cluster_local_maxima(local_maxima, im):
    y, x = np.where(local_maxima > 0)
    points = np.array(zip(x, y))
    to_merge = []
    im = cv.cvtColor(im, cv.COLOR_BGR2HSV)

    # better clustering algorithm?!
    for p1, p2 in itertools.combinations(points, 2):
        if 1 < euclidean(p1, p2) < 20:
            c1 = im[p1[1], p1[0]]
            c2 = im[p2[1], p2[0]]
            if calculate_hsv_color_distance(h=[c1[0], c2[0]])[0] < 10:
                to_merge.append((p1, p2))

    for p1, p2 in to_merge:
        cv.line(local_maxima, (p1[0], p1[1]), (p2[0], p2[1]), 1, 1)

    return local_maxima


def get_object_markers_from_foreground(foreground, labels, im, dist=10, enhance_markers=True,
                                       show_intermediate_steps=False, save_intermediate_steps=False):
    # Finding markers using a distance transform and local maxima
    dist_transform = np.zeros(labels.shape, dtype=np.float32)
    for l in np.unique(labels):
        if l == 0:
            continue
        l_mask = np.uint8(labels == l)
        l_dist = cv.distanceTransform(l_mask, cv.DIST_L2, maskSize=cv.DIST_MASK_3)
        dist_transform[labels == l] = l_dist[l_mask > 0]
    if show_intermediate_steps:
        show_segmentation(dist_transform, title="Distance transform")
    if save_intermediate_steps:
        save_segmentation(dist_transform, title="Distance transform")

    localMax = peak_local_max(dist_transform, threshold_abs=DIST_THRESH, indices=False, min_distance=dist,
                              labels=labels)
    local_maxima = np.array(localMax, dtype=np.uint8)

    local_maxima = cluster_local_maxima(local_maxima, im)

    if enhance_markers:
        kernel = cv.getStructuringElement(cv.MORPH_CROSS, (5, 5))
        local_maxima = cv.dilate(local_maxima, kernel, iterations=2)

    # Marker labelling
    _, markers = cv.connectedComponents(local_maxima)

    return local_maxima, markers


def get_foreground_from_edges(im, edges):
    closed = cv.morphologyEx(edges, cv.MORPH_CLOSE, cv.getStructuringElement(cv.MORPH_ELLIPSE, (5, 5)), iterations=3)

    border_image = closed.copy()

    border_image[1:-1, 1:-1] = 0

    closed_border = cv.morphologyEx(border_image, cv.MORPH_DILATE, (3, 3), borderValue=0, iterations=30)

    closed[0, :] = closed_border[0, :]
    closed[-1, :] = closed_border[-1, :]
    closed[:, 0] = closed_border[:, 0]
    closed[:, -1] = closed_border[:, -1]

    contours, hierarchy = cv.findContours(closed, cv.RETR_EXTERNAL, cv.CHAIN_APPROX_SIMPLE)
    foreground_contours = []

    for i, contour in enumerate(contours):
        if hierarchy[0][i][3] == -1:
            area = cv.contourArea(contour)
            # dismiss small objects
            if area > 200:
                foreground_contours.append(contour)

    foreground = np.zeros(im.shape[:2], dtype=np.uint8)
    cv.drawContours(foreground, foreground_contours, -1, (1), cv.FILLED)

    kernel = cv.getStructuringElement(cv.MORPH_ELLIPSE, (5, 5))
    # dilate in order to find sure background area
    sure_bg = cv.dilate(foreground, kernel, iterations=2)
    # open in order to find sure foreground area
    foreground = cv.morphologyEx(foreground, cv.MORPH_OPEN, kernel, iterations=1)
    foreground = cv.morphologyEx(foreground, cv.MORPH_ERODE, cv.getStructuringElement(cv.MORPH_CROSS, (3, 3)),
                                 iterations=1)

    return foreground, sure_bg


def detect_edges(img):
    # gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
    # adaptive_threshold_edges = cv.adaptiveThreshold(gray, 255, cv.ADAPTIVE_THRESH_GAUSSIAN_C, cv.THRESH_BINARY_INV, 11,
    #                                                4)
    # v = np.median(cv.cvtColor(img, cv.COLOR_BGR2GRAY))
    # sigma = 0.7
    # ---- apply automatic Canny edge detection using the computed median----
    # lower = int(max(0, (1.0 - sigma) * v))
    # upper = int(min(255, (1.0 + sigma) * v))
    # print lower
    # print upper
    canny_edges = cv.Canny(img, 40, 200)

    # edges = cv.bitwise_or(canny_edges, adaptive_threshold_edges)
    return canny_edges


def remove_shadow(im, mask):
    im = cv.bitwise_and(im, im, mask=mask)
    hsv = cv.cvtColor(im, cv.COLOR_BGR2HSV)

    lower_gray = (0, 0.00 * 255, 0.3 * 255)
    upper_gray = (180, 0.15 * 255, 0.5 * 255)

    shadow = cv.inRange(hsv, lower_gray, upper_gray)

    if np.count_nonzero(shadow) < 0.05 * np.count_nonzero(mask):
        im[shadow > 0] = (0, 0, 0)
        mask[shadow > 0] = 0
    return im, mask


def calculate_hsv_color_distance(h, s=(0, 0), v=(0, 0)):
    h = np.array(h)
    h = h * 2.0
    h = np.deg2rad(h)
    diff_h = np.arctan2(np.sin(h[0] - h[1]), np.cos(h[0] - h[1]))
    diff_v = np.abs(v[0] - v[1])
    diff_s = np.abs(s[0] - s[1])
    return np.abs(np.rad2deg(diff_h) / 2.0), diff_s, diff_v


def merge_clusters(ms):
    cluster_centers = ms.cluster_centers_
    labels = ms.labels_
    unique_labels = np.unique(labels)
    labels_to_merge = []
    for (l1, l2) in itertools.combinations(unique_labels, r=2):
        c1 = cluster_centers[l1]
        c2 = cluster_centers[l2]
        d_h, d_s, d_v = calculate_hsv_color_distance(h=[c1[0], c2[0]])
        if d_h < 10 and d_s < 30 and d_v < 50:
            labels_to_merge.append((l1, l2))
    for l1, l2 in labels_to_merge:
        labels[labels == l2] = l1
    return labels + 1


def should_split(color_mean, color_std):
    # if gray and low std on hue, don't do post segmentation
    # if gray and high std on hue, do post segmentation
    # if gray and high std on s / v don't do post segmentation
    # if color and high std on h or v, do post segmentation
    # else don't do post segmentation
    if is_color(color_mean):
        return color_std[0] > 10 or color_std[2] > 0.4 * 255
    else:
        return color_std[0] > 20 and color_std[1] > 0.4 * 255


def post_segmentation(markers, preprocessed):
    max_label = markers.max()
    segmented = np.zeros(markers.shape, dtype=np.uint8)
    label_i = 0

    preprocessed_hsv = cv.cvtColor(preprocessed, cv.COLOR_BGR2HSV)

    for l in range(0, max_label + 1):
        object = np.array(markers == l, dtype=np.uint8)

        color_mean, color_std = get_color_for_mask(preprocessed, object)

        if should_split(color_mean, color_std):
            h, s, v = cv.split(preprocessed_hsv)
            image_intensities = h[object > 0]
            data = image_intensities.reshape(-1, 1)
            bw = estimate_bandwidth(data, n_samples=100)
            ms = MeanShift(bandwidth=bw, bin_seeding=True)
            try:
                ms.fit(data)
            except ValueError:
                print "MeanShift failed with bandwidth %f, retrying with different seeding." % bw
                seeds = get_bin_seeds(data, 60)
                ms = MeanShift(bandwidth=100, seeds=seeds)
                ms.fit(data)
            labels = merge_clusters(ms)
        else:
            labels = np.ones(len(preprocessed[object > 0]), dtype=np.uint8)
        segmented[object > 0] = label_i + labels
        label_i = label_i + max(labels)

    return segmented


def segment_foreground_by_colors(foreground, preprocessed):
    bgr_foreground = cv.bitwise_and(preprocessed, preprocessed, mask=foreground)
    h_foreground = cv.cvtColor(bgr_foreground, cv.COLOR_BGR2HSV)
    # filter out gray regions
    gray_regions = cv.inRange(h_foreground, (0, 0, int(0.10 * 255)), (180, int(0.2 * 255), int(0.8 * 255)))
    h_foreground[:, :, 1] = foreground * 255
    h_foreground[:, :, 2] = foreground * 255
    h_foreground[gray_regions > 0] = 0

    # segment on hue value with kmeans
    Z = h_foreground[foreground > 0]
    # convert to np.float32
    Z = np.float32(Z)
    # define criteria, number of clusters(K) and apply kmeans()
    criteria = (cv.TERM_CRITERIA_EPS + cv.TERM_CRITERIA_MAX_ITER, 10, 1.0)
    K = 10
    ret, label, center = cv.kmeans(Z, K, None, criteria, 10, cv.KMEANS_PP_CENTERS)

    # post merge if necessary
    for (l1, c1), (l2, c2) in itertools.combinations(enumerate(center), r=2):
        d_h, _, _ = calculate_hsv_color_distance(h=[c1[0], c2[0]])
        if d_h < 10:
            label[label == l2] = l1

    km_labels = np.zeros(foreground.shape, dtype=np.uint8)
    km_labels[foreground > 0] = label.flatten() + 1
    km_labels[gray_regions > 0] = km_labels.max() + 1
    return km_labels


def segment_foreground_meanshift(foreground, preprocessed):
    bgr_foreground = cv.bitwise_and(preprocessed, preprocessed, mask=foreground)

    (segmented_image, labels_image, number_regions) = pms.segment(bgr_foreground, spatial_radius=8,
                                                                  range_radius=RANGE_RADIUS, min_density=50)
    return labels_image
