#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import numpy as np
import math
import cv2
from pycocotools import mask as pycocomask
from detectron2.utils.visualizer import Visualizer

from detectron2.data import MetadataCatalog


class Vision(object):
    '''The class Vision controls everything that has to do
    with object detection, segmentation and feature extraction.'''
    def __init__(self, cfg):
        self.cfg = cfg
        self.model_cfg = cfg.MODEL_cfg
        # Get the Mask R-CNN model
        self.model = cfg.MODEL
    
    def analyze(self, img_path):
        '''Analyze the given image. Create a new image with the found
        bounding boxes and masks ad use the masks to do feature extraction,
        returning the features'''
        im = cv2.imread(img_path)
        outputs = self.model(im)
        bbox_img_path = self.visualise_bboxes(im, outputs, img_path)
        data = self._extract_image_features(im, outputs)
        return bbox_img_path, data
    
    def visualise_bboxes(self, im, outputs, orig_path):
        '''Add the bboxes to the image and export this'''
        v = Visualizer(im[:, :, ::-1], MetadataCatalog.get(self.model_cfg.DATASETS.TRAIN[0]), scale=1.2)
        out = v.draw_instance_predictions(outputs["instances"].to("cpu"))
        img_name_and_type = orig_path.split('/')[-1]
        img_name = img_name_and_type.split('.')[0]
        bbox_img_path = self.cfg.IMAGE_DIR + img_name + '_bbox.' + self.cfg.PICTURE_FORMAT
        out.save(bbox_img_path)
        return bbox_img_path
    
    def _extract_image_features(self, image, results):
        data = {}
        num_objects = len(results["instances"])
        for i in range(num_objects):
            object_id = 'obj-' + str(i)
            object = {}
            bboxes = results["instances"].get("pred_boxes").tensor.numpy()
            # Get data from results
            bbox = bboxes[i]
            mask = results["instances"].get("pred_masks")[i]
            mask = np.array(mask.numpy().astype(np.uint8), order='F')
            RLE_mask = pycocomask.encode(mask)
            # colors
            lab = self._extract_lab_from_mask(image, mask)
            hsv = self._extract_hsv_from_mask(image, mask)
            rgb = self._extract_rgb_from_mask(image, mask)
            object['lab'] = lab
            object['hsv'] = hsv
            object['rgb'] = rgb
            # position
            x_mid = int((bbox[0] + bbox[3]) / 2)
            print(x_mid)
            y_mid = int((bbox[0] + bbox[1]) / 2)
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
            p_whites, p_blacks = self._white_and_black_percentage(image, mask, area)
            object['p_whites'] = p_whites
            object['p_blacks'] = p_blacks
            # add the object to data
            data[object_id] = object
        return data










    
    def _extract_rgb_from_mask(self, image, mask):
        '''Extract RGB in range [0,255]'''
        r, g, b = cv2.split(image)
        mean_r = np.mean(r[mask != 0])
        mean_g = np.mean(g[mask != 0])
        mean_b = np.mean(b[mask != 0])
        return [mean_r, mean_g, mean_b]

    def _extract_hsv_from_mask(self, image, mask):
        '''Extract HSV with range H [0, 360],
        and S,V [0,100]'''
        hsv_image = cv2.cvtColor(image, cv2.COLOR_RGB2HSV)
        h, s, v = cv2.split(hsv_image)
        mean_h = np.mean(h[mask != 0]) * 2.0
        mean_s = (np.mean(s[mask != 0]) / 255.0) * 100.0
        mean_v = (np.mean(v[mask != 0]) / 255.0) * 100.0
        return [mean_h, mean_s, mean_v]

    def _extract_lab_from_mask(self, image, mask):
        '''Extract LAB with range L [0,100]
        and A, B [-127,127]'''
        lab_image = cv2.cvtColor(image, cv2.COLOR_RGB2Lab)
        l, a, b = cv2.split(lab_image)
        mean_l = (np.mean(l[mask != 0]) * 100.0) / 255.0
        mean_a = np.mean(a[mask != 0]) - 128.0
        mean_b = np.mean(b[mask != 0]) - 128.0
        return [mean_l, mean_a, mean_b]

    def _white_and_black_percentage(self, image, mask, area):
        '''Compute the percentage of bright and dark
        pixels in the HSV color space.'''
        hsv = cv2.cvtColor(image, cv2.COLOR_RGB2HSV)
        h, s, v = cv2.split(hsv)
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
        contours, _ = cv2.findContours(mask, cv2.RETR_TREE,
                                      cv2.CHAIN_APPROX_SIMPLE)
        contour = max(contours, key=cv2.contourArea)
        approx = cv2.approxPolyDP(contour, 0.01 * cv2.arcLength(contour, True),
                                 True)
        corners = len(approx)
        return corners

    def _enclosing_circle_ratio(self, image, mask, area):
        '''After applying the mask to the image,
        find contours and use them to get the
        minimum enclosing circle. Determine the ratio
        between the masks area and the circle area.'''
        contours, _ = cv2.findContours(mask, cv2.RETR_TREE,
                                      cv2.CHAIN_APPROX_SIMPLE)
        contour = max(contours, key=cv2.contourArea)
        (x, y), r = cv2.minEnclosingCircle(contour)
        circle_area = float(r*r*math.pi)
        ratio = float(area/circle_area)
        return ratio
