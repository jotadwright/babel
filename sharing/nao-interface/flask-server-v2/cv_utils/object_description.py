import math

import cv2 as cv
import numpy as np
from scipy.spatial.distance import hamming
from skimage import feature
from sklearn.cluster import KMeans

from cv_utils import color_utils
from cv_utils.color_utils import get_color_for_mask


def get_white_and_black_percentage(image, mask, area):
    hsv = cv.cvtColor(image, cv.COLOR_BGR2HSV)
    h, s, v = cv.split(hsv)
    s = s[mask != 0]
    v = v[mask != 0]
    low_s = s < 0.5 * 255
    high_v = v > 0.7 * 255
    low_v = v < 0.2 * 255
    whites = np.count_nonzero(np.bitwise_and(low_s, high_v))
    blacks = np.count_nonzero(low_v)
    return whites / float(area), blacks / float(area)


def get_value_histogram(im, mask):
    im = cv.cvtColor(im, cv.COLOR_BGR2HSV)
    v_hist = cv.calcHist([im], [2], mask, [50], [0, 256])
    v_hist /= v_hist.sum() + 1e-7
    return v_hist


def get_hamming_distance_to_enclosing_circle(object_mask, c, center, radius):
    obj2 = np.zeros(object_mask.shape, dtype=np.uint8)
    cv.circle(obj2, center, radius, color=255, thickness=cv.FILLED)
    circleCont, _ = cv.findContours(obj2, cv.RETR_EXTERNAL, cv.CHAIN_APPROX_SIMPLE)

    obj1 = np.zeros(object_mask.shape, dtype=np.uint8)
    cv.drawContours(obj1, [c], -1, 1, cv.FILLED)

    x, y, w, h = cv.boundingRect(c)
    obj1 = obj1[y:y + h, x:x + w]
    obj2 = obj2[y:y + h, x:x + w]

    return hamming(obj1.flatten(), obj2.flatten())


def describe_shape(area_ratio, size_ratio, corners, circle_distance):
    factor = 1
    if area_ratio < 0.5:
        factor = 0.5
    if corners < 8*factor:
        shape = "cube"
    elif corners < 10*factor:
        if circle_distance < 0.85:
            shape = "sphere"
        else:
            shape = "cylinder"
    else:
        if circle_distance < 0.85:
            shape = "sphere"
        else:
            shape = "cylinder"

    return shape


def estimate_depth(y, y_max):
    depth = (y_max - y) / float(y_max)
    return depth


def estimate_obj_size_clevr(bb_area, y_pos, y_max):
    depth = estimate_depth(y_pos, y_max)
    normalized_size = bb_area * depth
    if normalized_size > 2000:
        return 'large'
    else:
        return 'small'


def get_color_histogram(im, mask):
    im = cv.cvtColor(im, cv.COLOR_BGR2HSV)
    h_hist = cv.calcHist([im], [0], mask, [25], [0, 256])
    h_hist /= h_hist.sum() + 1e-7
    s_hist = cv.calcHist([im], [1], mask, [25], [0, 256])
    s_hist /= s_hist.sum() + 1e-7
    v_hist = cv.calcHist([im], [2], mask, [25], [0, 256])
    v_hist /= v_hist.sum() + 1e-7
    return h_hist, s_hist, v_hist


def describe_segmented_image(markers, dst, show_itermediate_steps=False):
    im = dst.copy()
    data = []
    image_area = im.shape[0] * im.shape[1]
    x_max = im.shape[1]

    object_i = 1
    for l in range(0, markers.max() + 1):
        object_mask = np.array(markers == l, dtype=np.uint8)

        if np.count_nonzero(object_mask) < 20:
            continue

        contours, _ = cv.findContours(object_mask, cv.RETR_EXTERNAL, cv.CHAIN_APPROX_TC89_KCOS)
        if len(contours) == 0:
            continue

        # find biggest contour (if there is still any noise...)
        area = 0
        c = None
        for cnt in contours:
            _area = cv.contourArea(cnt)
            if _area > area:
                area = _area
                c = cnt

        # Get Width and Height using Rotated Bounding Box
        (_, _), (w, h), angle = cv.minAreaRect(c)
        bb_area = w * h
        if (bb_area == 0) or w < 10 or h < 10:
            continue
        area_ratio = area / bb_area
        ratio = w / h if w < h else h / w

        color, std = get_color_for_mask(im, object_mask)

        approx = cv.approxPolyDP(c, 0.01 * cv.arcLength(c, True), True)
        corners = len(approx)
        x, y, w, h = cv.boundingRect(c)

        # Assign a label to the contour
        label = "obj-{}".format(object_i)
        object_i += 1

        (x, y), radius = cv.minEnclosingCircle(c)
        center = (int(x), int(y))
        radius = int(radius)
        circle_distance = get_hamming_distance_to_enclosing_circle(object_mask, c, center, radius)
        p_whites, p_blacks = get_white_and_black_percentage(im, object_mask, area)

        json_data = {label: {
            'xpos': center[0],
            'ypos': center[1],
            'width': w,
            'height': h,
            'angle': angle,
            'corners': corners,
            'area': area,
            'relative-area': area / image_area,
            'bb-area': bb_area,
            'area-ratio': area_ratio,
            'size-ratio': ratio,
            'color': color,
            'circle-distance': circle_distance,
            'p-whites': p_whites,
            'p-blacks': p_blacks
        }}
        data.append(json_data)

        cv.drawContours(dst, [c], -1, (0, 255, 0), 2)
        cv.circle(dst, (center[0], center[1]), 3, (0, 255, 0), -1)
        label_x = center[0] + 10 if x_max - center[0] > 50 else center[0] - 50
        cv.putText(dst, label, (label_x, center[1] - 10),
                   cv.FONT_HERSHEY_SIMPLEX,
                   0.5, (0, 255, 0), 2)

    return dst, data


def link_clevr_material(p_whites, p_blacks, v_hist):
    sorted_hist = np.array(list(reversed(sorted(v_hist.flatten()))))
    is_rubber = sorted_hist[:3].sum() > 0.5

    shiny = p_whites > 0.0005 or p_blacks > 0.0005

    if shiny:
        material = "metal"
    else:
        material = "rubber" if is_rubber else "metal"
    return material


def get_clevr_color(color):
    color = color_utils.get_main_color(color)
    if color == 'olive':
        color = 'yellow'
    elif color == 'teal':
        color = 'cyan'
    elif color == 'lime':
        color = 'green'
    elif color == 'magenta':
        color = 'purple'
    elif color == 'navy':
        color = 'blue'
    elif color == 'maroon':
        color = 'brown'
    elif color == 'dark red':
        color = 'red'
    elif color == 'black':
        color = 'gray'
    elif color == 'silver':
        color = 'gray'
    elif color == 'white':
        color = 'gray'
    return color


def get_clevr_shape(area_ratio, size_ratio, corners, circle_distance):
    return describe_shape(area_ratio, size_ratio, corners, circle_distance)


def link_semantic_labels_clevr(clevr_obj, item, y_max):
    clevr_obj["material"] = link_clevr_material(item["p-whites"], item["p-blacks"], item["v-hist"])
    clevr_obj["size"] = estimate_obj_size_clevr(item["bb-area"], item["ypos"], y_max)
    clevr_obj["color"] = get_clevr_color(item["color"])
    clevr_obj["shape"] = get_clevr_shape(item["area-ratio"], item["size-ratio"], item["corners"], item["circle-distance"])


def link_real_material(p_whites, p_blacks, vhist):
    if p_whites > 0.0005:
        return 'plastic'
    else:
        return 'paper'


def estimate_obj_size_real(bb_area, y_pos, y_max):
    depth = estimate_depth(y_pos, y_max)
    normalized_size = bb_area * depth
    if normalized_size > 600:
        return 'large'
    else:
        return 'small'


def get_real_color(color):
    color = color_utils.get_main_color(color)
    if color == 'olive':
        color = 'yellow'
    elif color == 'teal':
        color = 'cyan'
    elif color == 'lime':
        color = 'green'
    elif color == 'navy':
        color = 'blue'
    elif color == 'maroon':
        color = 'brown'
    elif color == 'dark red':
        color = 'red'
    return color


def get_real_shape(area_ratio,size_ratio, corners, circle_distance):
    return describe_shape(area_ratio, size_ratio, corners, circle_distance)


def link_semantic_labels_real(obj, item, y_max):
    obj["material"] = link_real_material(item["p-whites"], item["p-blacks"], item["v-hist"])
    obj["size"] = estimate_obj_size_real(item["bb-area"], item["ypos"], y_max)
    obj["color"] = get_real_color(item["color"])
    obj["shape"] = get_real_shape(item["area-ratio"],item["size-ratio"], item["corners"], item["circle-distance"])


def get_size_by_clustering(data, obj, y_max):
    estimated_sizes = [item['bb-area'] * (y_max - item['ypos']) / float(y_max) for item in data]
    print estimated_sizes

    kmeans = KMeans(n_clusters=2).fit(np.array(estimated_sizes).reshape(-1, 1))
    print kmeans.labels_
    centers = kmeans.cluster_centers_
    if centers[0] > centers[1]:
        labels = ["large", "small"]
    else:
        labels = ["small", "large"]

    if np.abs(centers[0] - centers[1]) < 500:
        if np.mean(estimated_sizes) > 2000:
            size = "large"
        else:
            size = "small"
        for i, s_obj in enumerate(obj["objects"]):
            s_obj["size"] = size
    else:
        for i, s_obj in enumerate(obj["objects"]):
            s_obj["size"] = labels[kmeans.labels_[i]]


def build_semantic_data(data, y_max, for_experiment="clevr"):
    obj = {
        'objects': [],
        'relationships': {
            "right": [],
            "left": [],
            "behind": [],
            "front": []
        }
    }

    for o in data:
        o['right'] = []
        o['left'] = []
        o['front'] = []
        o['behind'] = []
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

    for i, item in enumerate(data):
        semantic_obj = {}
        semantic_obj['id'] = item['label']
        semantic_obj['pixel_coords'] = [item["xpos"], item["ypos"]]
        semantic_obj['color_hsv'] = item['color']
        obj["objects"].append(semantic_obj)
        obj["relationships"]["right"].append(item["right"])
        obj["relationships"]["left"].append(item["left"])
        obj["relationships"]["front"].append(item["front"])
        obj["relationships"]["behind"].append(item["behind"])

        if for_experiment == "clevr":
            link_semantic_labels_clevr(semantic_obj, item, y_max)
        else:
            link_semantic_labels_real(semantic_obj,item, y_max)

    #get_size_by_clustering(data, obj, y_max)


    return obj
