import numpy as np
import cv2 as cv
import matplotlib.pyplot as plt
from skimage.measure import regionprops
from skimage.segmentation import slic
from sklearn import metrics
from sklearn.cluster import estimate_bandwidth, MeanShift, AgglomerativeClustering
from sklearn.preprocessing import StandardScaler

from cv_utils import util
from cv_utils.segmentation import detect_edges, get_color_for_mask

import pandas as pd

def flood_fill(edges):
    # Copy the thresholded image.
    im_floodfill = edges.copy()

    # Mask used to flood filling.
    # Notice the size needs to be 2 pixels than the image.
    h, w = edges.shape[:2]
    mask = np.zeros((h + 2, w + 2), np.uint8)

    # Floodfill from point (0, 0)
    cv.floodFill(im_floodfill, mask, (0, 0), 255)

    # Invert floodfilled image
    im_floodfill_inv = cv.bitwise_not(im_floodfill)

    # Combine the two images to get the foreground.
    im_out = edges | im_floodfill_inv

    # Display images.
    plt.imshow(im_out)
    plt.show()


def get_superpixels(img, n_segments=100):
    segments = slic(img, n_segments=n_segments, sigma=3, convert2lab=True)
    # plt.imshow(mark_boundaries(img, segments))
    # plt.show()
    return segments


def meanshift(image):
    original_shape = image.shape
    h, w = original_shape[:2]
    # Flatten image.
    data = np.zeros((h * w, 3))
    flat_image = np.reshape(image, [-1, 3])
    data[:, :3] = flat_image
    # for pixel in flat_image
    # Estimate bandwidth
    bandwidth2 = estimate_bandwidth(data,
                                    quantile=.25, n_samples=500)
    ms = MeanShift(bandwidth=bandwidth2, bin_seeding=True)
    ms.fit(data)
    labels = ms.labels_
    im = np.reshape(labels, original_shape[:2])
    plt.imshow(im)
    plt.show()
    return im


def detect_sides(c):
    # initialize the shape name and approximate the contour
    peri = cv.arcLength(c, True)
    approx = cv.approxPolyDP(c, 0.04 * peri, True)
    return len(approx)


def segment_image_saliency(image, type):
    dst = image.copy
    if type == 'spectral_residual':
        saliency = cv.saliency.StaticSaliencySpectralResidual_create()
    else:
        saliency = cv.saliency.StaticSaliencyFineGrained_create()
    (success, saliencyMap) = saliency.computeSaliency(image)
    saliencyMap = (saliencyMap * 255).astype("uint8")
    plt.imshow(saliencyMap)
    plt.show()
    threshMap = cv.threshold(saliencyMap.astype("uint8"), 0, 255,
                             cv.THRESH_BINARY | cv.THRESH_OTSU)[1]
    plt.imshow(threshMap)
    plt.show()


def detect_keypoints(img):
    # Initiate ORB detector
    orb = cv.ORB_create()
    # find the keypoints with ORB
    kp = orb.detect(img, None)
    # compute the descriptors with ORB
    kp, des = orb.compute(img, kp)
    # draw only keypoints location,not size and orientation
    img2 = img.copy()
    for marker in kp:
        img2 = cv.drawMarker(img2, tuple(int(i) for i in marker.pt), color=(0, 255, 0))
    plt.imshow(img2), plt.show()
    return kp


def segment_with_superpixels(image, superpixel_size=15):
    image = cv.cvtColor(image, cv.COLOR_BGR2LAB)
    slic = cv.ximgproc.createSuperpixelSLIC(image, cv.ximgproc.SLICO, superpixel_size)
    slic.iterate(10)
    slic.enforceLabelConnectivity(10)
    num_labels = slic.getNumberOfSuperpixels()
    labels = slic.getLabels()

    segmented = image.copy()

    # calculate means and show segmented img
    for i in range(num_labels):
        sp = image[labels == i]
        sp_mean = [sp[:, 0].mean(), sp[:, 1].mean(), sp[:, 2].mean()]
        segmented[labels == i] = sp_mean

    segmented = cv.cvtColor(segmented, cv.COLOR_LAB2RGB)
    return segmented, slic


def segment_image(image, dst):
    edges = detect_edges(image)
    # rgb = cv.cvtColor(image, cv.COLOR_HSV2RGB)
    gray = cv.cvtColor(image, cv.COLOR_BGR2GRAY)
    h, s, v = cv.split(image)
    retval, h_thresh = cv.threshold(h, 0, 255,
                                    cv.THRESH_BINARY + cv.THRESH_OTSU)
    retval, s_thresh = cv.threshold(s, 0, 255,
                                    cv.THRESH_BINARY_INV + cv.THRESH_OTSU)
    retval, v_thresh = cv.threshold(v, 0, 255,
                                    cv.THRESH_BINARY_INV + cv.THRESH_OTSU)
    retval, gray_thresh = cv.threshold(gray, 0, 255,
                                       cv.THRESH_BINARY_INV + cv.THRESH_OTSU)

    thresh = np.zeros(h.shape, np.uint8)
    thresh[(gray_thresh != 0)] = 1

    # retval, thresh = cv.threshold(gray, 125, 255, cv.THRESH_BINARY_INV)

    # plt.imshow(thresh)
    # plt.show()
    thresh = cv.medianBlur(thresh, 3)

    # more noise removing by delation
    kernel = np.ones((3, 3), np.uint8)
    closing = cv.morphologyEx(thresh, cv.MORPH_CLOSE, kernel, iterations=1)
    opening = cv.morphologyEx(closing, cv.MORPH_OPEN, kernel, iterations=1)

    opening[(edges != 0)] = 0

    plt.imshow(opening)
    plt.axis('off')
    plt.show()
    return find_countours_and_describe(image, dst, opening)


def find_countours_and_describe(image, dst, opening):
    cont_img = opening.copy()
    cnts = cv.findContours(cont_img, cv.RETR_EXTERNAL,
                           cv.CHAIN_APPROX_SIMPLE)
    cnts = cnts[0]
    # loop over the contours and collect data
    data = []

    object_countours = get_instance_contours(cnts, image)

    for i, (M, c) in enumerate(object_countours):
        # Assign a label to the contour
        label = "obj-{}".format(i)

        # compute the center of the contour
        cX = int(M["m10"] / M["m00"])
        cY = int(M["m01"] / M["m00"])

        # detect the shape of the contour and label the color
        sides = detect_sides(c)
        color, std = get_color_for_mask(image, c)
        area = cv.contourArea(c)

        # Get Width and Height using Rotated Bounding Box
        (_, _), (w, h), angle = cv.minAreaRect(c)
        bb_area = w * h
        area_ratio = area / bb_area
        ratio = w / h if w < h else h / w
        if w < 10 or h < 10:
            continue

        # multiply the contour (x, y)-coordinates by the resize ratio,
        # then draw the contours and the name of the shape and labeled
        # color on the image
        c = c.astype("float")
        c = c.astype("int")
        json_data = {
            'label': label,
            'xpos': cX,
            'ypos': cY,
            'width': w,
            'height': h,
            'angle': angle,
            'sides': sides,
            'area': area,
            'bb-area': bb_area,
            'area-ratio': area_ratio,
            'size-ratio': ratio,
            'color': color[::-1],
            'right': [],
            'left': [],
            'front': [],
            'behind': []
        }
        data.append(json_data)
        cv.drawContours(dst, [c], -1, (0, 255, 0), 2)

        cv.circle(dst, (cX, cY), 7, (0, 255, 0), -1)
        cv.putText(dst, label, (cX + 10, cY - 10),
                   cv.FONT_HERSHEY_SIMPLEX,
                   0.5, (0, 255, 0), 2)
    # calculate relations
    for i1, o1 in enumerate(data):
        for i2, o2 in enumerate(data):
            if i2 > i1:
                break
            if i1 != i2:
                if o1['xpos'] > o2['xpos']:
                    o2['right'].append(i1)
                    o1['left'].append(i2)
                else:
                    o1['right'].append(i2)
                    o2['left'].append(i1)
                if o1['ypos'] > o2['ypos']:
                    o2['front'].append(i1)
                    o1['behind'].append(i2)
                else:
                    o1['front'].append(i2)
                    o2['behind'].append(i1)

    return dst, data


def segment_image_with_background(image, background):
    dst = image.copy()

    # TODO transpose background image
    detect_keypoints(background)
    detect_keypoints(image)

    diff = cv.absdiff(image, background)
    gray = cv.cvtColor(diff, cv.COLOR_BGR2GRAY)
    util.show_bgr_image(diff)

    b, g, r = cv.split(diff)

    foreground_mask = np.zeros(image.shape[:2], np.uint8)

    # foreground_mask[(b > 25) | (g > 25) | (r > 25)] = 128
    foreground_mask[(b > 75) | (g > 75) | (r > 75)] = 255
    # foreground_mask[sum([b, g, r]) > 50] = 1
    # foreground_mask[sum([b,g,r])>75] = 255

    thresh = cv.medianBlur(foreground_mask, 5)

    kernel = np.ones((3, 3), np.uint8)
    closing = cv.morphologyEx(thresh, cv.MORPH_CLOSE, kernel, iterations=3)

    util.show_bgr_image(background)
    util.show_bgr_image(image)

    plt.imshow(closing)
    plt.axis('off')
    plt.show()

    foreground = cv.bitwise_and(image, image, mask=closing)
    util.show_bgr_image(foreground)

    fg = cv.cvtColor(foreground, cv.COLOR_BGR2LAB)
    slic = cv.ximgproc.createSuperpixelSLIC(fg, cv.ximgproc.SLICO, 10)
    slic.iterate(10)
    num_labels = slic.getNumberOfSuperpixels()
    labels = slic.getLabels()

    segmented = np.zeros(fg.shape, np.uint8)

    # clustering of superpixels
    # collect superpixels -> we want a list of (cx, cy, b,g,r)

    superpixels = []
    regions = regionprops(labels)
    for props in regions:
        l = props.label
        cx, cy = props.centroid
        coords = tuple(props.coords.T)
        values = foreground[coords]
        mean_intensity = np.mean(values, axis=0)
        b, g, r = mean_intensity
        if b > 30 or g > 30 or r > 30:
            segmented[labels == l] = mean_intensity
            superpixels.append([l, cx, cy, b, g, r])

    superpixels_mat = np.array(superpixels)
    print(superpixels_mat.shape)

    superpixels_df = pd.DataFrame(superpixels_mat[:, 1:], columns=["cx", "cy", "b", "g", "r"],
                                  index=superpixels_mat[:, 0])
    scaler = StandardScaler()
    scaler.fit(superpixels_df)
    X = scaler.transform(superpixels_df.as_matrix())
    for i in range(2, 10):
        clustering = AgglomerativeClustering(n_clusters=i).fit(X)
        print(metrics.davies_bouldin_score(X, clustering.labels_))

    util.show_bgr_image(segmented)

    return find_countours_and_describe(image, dst, closing)


def get_instance_contours(cnts, image):
    instance_contours = []

    split_recursively(instance_contours, image, cnts)

    return instance_contours


def get_segment(image, c):
    segment = []
    height, width = image.shape[:2]

    for y in range(height):
        for x in range(width):
            ret_val = cv.pointPolygonTest(c, (x, y), False)
            if ret_val == 1:
                pixel = image[y][x]
                segment.append((x, y, pixel[0], pixel[1], pixel[2]))

    return segment


def split_contour_to_instances(instance_contours, c, image):
    segment_pixels = get_segment(image, c)
    height, width = image.shape[:2]
    X = [(x / float(width), y / float(height), b / 255.0, g / 255.0, r / 255.0) for (x, y, b, g, r) in
         segment_pixels]
    cluster = AgglomerativeClustering(n_clusters=2, affinity='euclidean', linkage='ward')
    cluster.fit_predict(X)

    clustered_pixels = [(x, y, b, g, r, label) for ((x, y, b, g, r), label) in
                        zip(segment_pixels, cluster.labels_)]

    segment_image = np.zeros(image.shape[:2], np.uint8)
    mask = np.zeros(image.shape[:2], np.uint8)
    for (x, y, b, g, r, l) in clustered_pixels:
        mask[y][x] = 1 if l == 0 else 2
        segment_image[y][x] = 255

    for instance_label in set(cluster.labels_):
        masked_instance = np.zeros(image.shape[:2], np.uint8)
        masked_instance[mask == instance_label + 1] = 1
        plt.imshow(masked_instance)
        plt.show()
        cnts, _ = cv.findContours(masked_instance, cv.RETR_EXTERNAL,
                                  cv.CHAIN_APPROX_SIMPLE)

        split_recursively(instance_contours, image, cnts)

    return instance_contours


def split_recursively(instance_contours, image, cnts):
    for c in cnts:
        # Get the contour's Moments
        M = cv.moments(c)
        if M["m00"] > 0.0:
            color, std = get_color_for_mask(image, c)
            variance = std ** 2
            area = cv.contourArea(c)
            if np.sum(variance) > 2000 and area > 5000:
                print("Should be split: Variance is %f" % np.sum(variance))
                split_contour_to_instances(instance_contours, c, image)
            else:
                instance_contours.append((M, c))
