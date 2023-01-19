#!python2.7
#!/usr/bin/env python

from naoqi import ALProxy
import time
import random
import math
import numpy as np
import skimage.io
import skimage.color
import matplotlib.pyplot as plt
from pycocotools import mask as pycocomask
import colorsys
import cv2 as cv
import matplotlib.patches as patches


class NaoVision(object):
    '''The class NaoVision controls everything that has to do
    with object detection, segmentation and feature extraction.'''

    def __init__(self, cfg):
        self.cfg = cfg
        self.photoCaptureProxy = ALProxy("ALPhotoCapture", cfg.ROBOT_IP,
                                         cfg.ROBOT_PORT)
        # Set camera parameters
        self.photoCaptureProxy.setResolution(cfg.CAMERA_RESOLUTION)
        self.photoCaptureProxy.setPictureFormat(cfg.PICTURE_FORMAT)

    def capture(self):
        '''Capture an image and store it in the image dir.
        Return the complete path to the image'''
        img_name = str(int(time.time()))
        img_directory = "/home/nao/recordings/cameras/"
        img_paths = self.photoCaptureProxy.takePicture(img_directory, img_name)
        # img_path = img_directory + img_name + '.' + self.cfg.PICTURE_FORMAT
        return img_paths[0]


    def _add_bbox_to_image(self, image, bboxes, orig_path):
        '''Add the bboxes to the image and export this'''
        N = bboxes.shape[0]
        height, width = image.shape[:2]
        fig = plt.figure(frameon=False)
        fig.set_size_inches(9, 7)
        ax = plt.Axes(fig, [0., 0., 1., 1.])
        #ax.set_ylim(height + 10, -10)
        #ax.set_xlim(-10, width + 10)
        ax.set_axis_off()
        fig.add_axes(ax)
        ax.imshow(image, aspect='auto')
        for i in range(N):
            y1, x1, y2, x2 = bboxes[i]
            p = patches.Rectangle((x1, y1), x2 - x1, y2 - y1, linewidth=2,
                                  alpha=0.7, linestyle="dashed",
                                  edgecolor='w', facecolor='none')
            ax.add_patch(p)
            object_id = 'obj-' + str(i)
            ax.text(x1, y1-5, object_id, color='w',
                    fontsize='x-large', family='monospace',
                    backgroundcolor="none")
        img_name_and_type = orig_path.split('/')[-1]
        img_name = img_name_and_type.split('.')[0]
        bbox_img_path = self.cfg.IMAGE_DIR + img_name + '_bbox.' + self.cfg.PICTURE_FORMAT
        plt.savefig(bbox_img_path, bbox_inches='tight', pad_inches=0)
        plt.close()
        return bbox_img_path

    def _extract_image_features(self, image, results):
        data = {}
        num_objects = len(results['rois'])
        for i in range(num_objects):
            object_id = 'obj-' + str(i)
            object = {}
            # Get data from results
            bbox = results['rois'][i]
            mask = results['masks'][:, :, i]
            mask = np.array(mask.astype(np.uint8), order='F')
            class_id = results['class_ids'][i]
            score = results['scores'][i]
            RLE_mask = pycocomask.encode(mask)
            # colors
            lab = self._extract_lab_from_mask(image, mask)
            hsv = self._extract_hsv_from_mask(image, mask)
            rgb = self._extract_rgb_from_mask(image, mask)
            object['lab'] = lab
            object['hsv'] = hsv
            object['rgb'] = rgb
            # position
            x_mid = int((bbox[1] + bbox[3]) / 2)
            y_mid = int((bbox[0] + bbox[2]) / 2)
            object['xpos'] = x_mid
            object['ypos'] = y_mid
            # width, height, area, etc
            bbox_width = float(bbox[3] - bbox[1])  # bbox[2]
            bbox_height = float(bbox[2] - bbox[0])  # bbox[3]
            bbox_wh_ratio = float(bbox_width/bbox_height)
            bbox_area = float(bbox_width * bbox_height)
            area = float(pycocomask.area(RLE_mask))
            bbox_area_ratio = float(area/bbox_area)
            circle_area_ratio = self._enclosing_circle_ratio(image, mask, area)
            object['bbox_width'] = bbox_width
            object['bbox_height'] = bbox_height
            object['bbox_wh_ratio'] = bbox_wh_ratio
            object['bbox_area'] = bbox_area
            object['area'] = area
            object['bbox_area_ratio'] = bbox_area_ratio
            object['circle_area_ratio'] = circle_area_ratio
            # sides and corners
            corners = self._detect_and_count_corners(image, mask)
            object['corners'] = corners
            # whites and blacks
            p_whites, p_blacks = self._white_and_black_percentage(image,
                                                                  mask,
                                                                  area)
            object['p_whites'] = p_whites
            object['p_blacks'] = p_blacks
            # add the object to data
            data[object_id] = object
        return data

    def _random_colors(self, N, bright=True):
        '''Generate random colors. To get visually distinct colors,
        generate them in HSV space then convert to RGB.'''
        brightness = 1.0 if bright else 0.7
        hsv = [(float(i) / N, 1, brightness) for i in range(N)]
        colors = list(map(lambda c: colorsys.hsv_to_rgb(*c), hsv))
        random.shuffle(colors)
        return colors

    def _extract_rgb_from_mask(self, image, mask):
        '''Extract RGB in range [0,255]'''
        r, g, b = cv.split(image)
        mean_r = np.mean(r[mask != 0])
        mean_g = np.mean(g[mask != 0])
        mean_b = np.mean(b[mask != 0])
        return [mean_r, mean_g, mean_b]

    def _extract_hsv_from_mask(self, image, mask):
        '''Extract HSV with range H [0, 360],
        and S,V [0,100]'''
        hsv_image = cv.cvtColor(image, cv.COLOR_RGB2HSV)
        h, s, v = cv.split(hsv_image)
        mean_h = np.mean(h[mask != 0]) * 2.0
        mean_s = (np.mean(s[mask != 0]) / 255.0) * 100.0
        mean_v = (np.mean(v[mask != 0]) / 255.0) * 100.0
        return [mean_h, mean_s, mean_v]

    def _extract_lab_from_mask(self, image, mask):
        '''Extract LAB with range L [0,100]
        and A, B [-127,127]'''
        lab_image = cv.cvtColor(image, cv.COLOR_RGB2Lab)
        l, a, b = cv.split(lab_image)
        mean_l = (np.mean(l[mask != 0]) * 100.0) / 255.0
        mean_a = np.mean(a[mask != 0]) - 128.0
        mean_b = np.mean(b[mask != 0]) - 128.0
        return [mean_l, mean_a, mean_b]

    def _white_and_black_percentage(self, image, mask, area):
        '''Compute the percentage of bright and dark
        pixels in the HSV color space.'''
        hsv = cv.cvtColor(image, cv.COLOR_RGB2HSV)
        h, s, v = cv.split(hsv)
        s = s[mask != 0]
        v = v[mask != 0]
        low_s = s < 0.5 * 255
        high_v = v > 0.7 * 255
        low_v = v < 0.2 * 255
        whites = np.count_nonzero(np.bitwise_and(low_s, high_v))
        blacks = np.count_nonzero(low_v)
        return whites / float(area), blacks / float(area)

    def _detect_and_count_corners(self, image, mask):
        '''After applying the mask to the image,
        find contours and use them to count corners'''
        contours, _ = cv.findContours(mask, cv.RETR_TREE,
                                      cv.CHAIN_APPROX_SIMPLE)
        contour = max(contours, key=cv.contourArea)
        approx = cv.approxPolyDP(contour, 0.01 * cv.arcLength(contour, True),
                                 True)
        corners = len(approx)
        return corners

    def _enclosing_circle_ratio(self, image, mask, area):
        '''After applying the mask to the image,
        find contours and use them to get the
        minimum enclosing circle. Determine the ratio
        between the masks area and the circle area.'''
        contours, _ = cv.findContours(mask, cv.RETR_TREE,
                                      cv.CHAIN_APPROX_SIMPLE)
        contour = max(contours, key=cv.contourArea)
        (x, y), r = cv.minEnclosingCircle(contour)
        circle_area = float(r*r*math.pi)
        ratio = float(area/circle_area)
        return ratio
