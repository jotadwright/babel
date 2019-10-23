import numpy as np
import cv2 as cv
import matplotlib.pyplot as plt
from scipy.stats import circstd, circmean


COLOR_RANGES = {
    "black": {
        "prototype": (0, 0, 0),
        "d_lower": (0, 0, 0),
        "d_upper": (360, 100, 15)
    },
    "white": {
        "prototype": (0, 0, 100),
        "d_lower": (0, 0, 5),
        "d_upper": (360, 5, 0)
    },
    "red": {
        "prototype": (0, 80, 100),
        "d_lower": (30, 60, 50),
        "d_upper": (30, 30, 0)
    },
    "dark red": {
        "prototype": (0, 80, 50),
        "d_lower": (30, 60, 25),
        "d_upper": (20, 20, 50)
    },
    "lime": {
        "prototype": (120, 100, 100),
        "d_lower": (30, 80, 50),
        "d_upper": (30, 0, 0)
    },
    "blue": {
        "prototype": (220, 100, 100),
        "d_lower": (30, 80, 50),
        "d_upper": (30, 0, 0)
    },
    "yellow": {
        "prototype": (70, 100, 100),
        "d_lower": (20, 80, 50),
        "d_upper": (40, 0, 0)
    },
    "cyan": {
        "prototype": (180, 100, 100),
        "d_lower": (30, 80, 50),
        "d_upper": (30, 0, 0)
    },
    "magenta": {
        "prototype": (300, 100, 100),
        "d_lower": (30, 80, 50),
        "d_upper": (30, 0, 0)
    },
    "silver": {
        "prototype": (0, 0, 75),
        "d_lower": (0, 0, 22.5),
        "d_upper": (360, 20, 12.5)
    },
    "gray": {
        "prototype": (0, 0, 40),
        "d_lower": (0, 0, 25),
        "d_upper": (360, 20, 22.5)
    },
    "maroon": {
        "prototype": (10, 100, 40),
        "d_lower": (10, 80, 30),
        "d_upper": (30, 0, 30)
    },
    "green": {
        "prototype": (120, 100, 50),
        "d_lower": (30, 80, 25),
        "d_upper": (30, 0, 50)
    },
    "navy": {
        "prototype": (240, 100, 50),
        "d_lower": (30, 80, 25),
        "d_upper": (30, 0, 50)
    },
    "olive": {
        "prototype": (70, 100, 50),
        "d_lower": (30, 80, 25),
        "d_upper": (20, 0, 50)
    },
    "teal": {
        "prototype": (180, 100, 50),
        "d_lower": (30, 80, 25),
        "d_upper": (30, 0, 50)
    },
    "purple": {
        "prototype": (300, 100, 50),
        "d_lower": (30, 80, 25),
        "d_upper": (30, 0, 50)
    },
}


def get_color_for_mask(im, mask):
    hsv = cv.cvtColor(im, cv.COLOR_BGR2HSV)
    h, s, v = cv.split(hsv)
    #h_mean = circular_mean(h[mask != 0] * 2.0) / 2.0
    h_mean = np.mean(h[mask != 0])
    s_mean = np.mean(s[mask != 0])
    v_mean = np.mean(v[mask != 0])
    mean = (h_mean, s_mean, v_mean)
    _, std = cv.meanStdDev(hsv, mask=mask)
    h_std, s_std, v_std = std
    #h_std = circular_std(h[mask != 0] * 2.0) / 2.0
    return mean, (h_std[0], s_std[0], v_std[0])


def is_color(color_mean):
    return color_mean[1] > 0.2 * 255 and color_mean[2] > 0.2 * 255


def get_main_color(hsv_color):
    m = get_color_memberships(hsv_color)
    return max(m, key=m.get)


def get_color_memberships(hsv_color):
    return {name: get_membership_for_color(hsv_color, name) for name in COLOR_RANGES.keys()}


def filter_color(im, name):
    lower, upper = get_boundaries(name)
    if len(upper) == 1:
        return cv.inRange(im, lower[0], upper[0])
    else:
        return cv.bitwise_or(cv.inRange(im, lower[0], upper[0]), cv.inRange(im, lower[1], upper[1]))


def get_boundaries(name):
    color = COLOR_RANGES[name]
    prototype = np.array(color['prototype'])
    lower_b = to_opencv(prototype - np.array(color['d_lower']) % 360)
    upper_b = to_opencv(prototype + np.array(color['d_upper']))
    if name == 'red':
        return [lower_b, to_opencv([0, 20, 50])], [to_opencv([359, 100, 100]), upper_b]
    else:
        return [lower_b], [upper_b]


def get_membership_for_color(hsv_color, name):
    color = COLOR_RANGES[name]
    hsv_color = np.int32(hsv_color)
    if (name == 'red' or name == 'dark red') and hsv_color[0] > 90:
        hsv_color[0] = hsv_color[0] - 180
    lower_b = to_opencv(np.array(color["d_lower"]))
    upper_b = to_opencv(np.array(color["d_upper"]))
    prototype = to_opencv(color["prototype"])
    dist = hsv_color - prototype

    # hue doesn't matter for grey,silver, black and white
    # saturation doesn't matter for the colors
    if name in ["black", "white", "silver", "gray"]:
        s_dist = dist[1]
        v_dist = dist[2]
        if s_dist == 0:
            s_dist_norm = 0
        elif s_dist > 0:
            s_dist_norm = s_dist / float(upper_b[1])
        else:
            s_dist_norm = np.abs(s_dist) / float(lower_b[1])
        if v_dist == 0:
            v_dist_norm = 0
        elif v_dist > 0:
            v_dist_norm = v_dist / float(upper_b[2])
        else:
            v_dist_norm = np.abs(v_dist) / float(lower_b[2])
        s_membership = max((1.5 - s_dist_norm) / 1.5, 0.0)
        v_membership = max((1.5 - v_dist_norm) / 1.5, 0.0)
        return s_membership * v_membership
    else:
        h_dist = dist[0]
        v_dist = dist[2]
        if h_dist == 0:
            h_dist_norm = 0
        elif h_dist > 0:
            h_dist_norm = h_dist / float(upper_b[0])
        else:
            h_dist_norm = np.abs(h_dist) / float(lower_b[0])
        if v_dist == 0:
            v_dist_norm = 0
        elif v_dist > 0:
            v_dist_norm = v_dist / float(upper_b[2])
        else:
            v_dist_norm = np.abs(v_dist) / float(lower_b[2])
        h_membership = max(1.0- h_dist_norm, 0.0)
        v_membership = max(1.0 - v_dist_norm, 0.0)
        return h_membership * v_membership


def to_opencv(hsv):
    h, s, v = hsv
    return ((h % 360) / 2.0, (s / 100.0) * 255.0, (v / 100.0) * 255.0)


def circular_std(values):
    values = np.deg2rad(values)
    std = circstd(values)
    return np.rad2deg(std)


def circular_mean(values):
    values = np.deg2rad(values)
    mean = circmean(values)
    return np.rad2deg(mean)


def plot_color_range(name):
    color = COLOR_RANGES[name]

    prototype = np.array(color["prototype"])

    lower_b = to_opencv(prototype - np.array(color["d_lower"]))
    upper_b = to_opencv(prototype + np.array(color["d_upper"]))

    s_gradient = np.ones((500, 1), dtype=np.uint8) * np.linspace(lower_b[1], upper_b[1], 500, dtype=np.uint8)
    v_gradient = np.rot90(np.ones((500, 1), dtype=np.uint8) * np.linspace(lower_b[2], upper_b[2], 500, dtype=np.uint8))
    h_array = np.array([lower_b[0], to_opencv(prototype)[0], upper_b[0]])

    fig = plt.figure(name)

    for i, hue in enumerate(h_array):
        h = int(hue) * np.ones((500, 500), dtype=np.uint8)
        hsv_color = cv.merge((h, s_gradient, v_gradient))
        rgb_color = cv.cvtColor(hsv_color, cv.COLOR_HSV2RGB)
        plt.subplot(130 + i + 1)
        plt.axis('off')
        plt.imshow(rgb_color)

    fig.suptitle(name)
    plt.show()
