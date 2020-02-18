#!python2.7
#!/usr/bin/env python

import naoqi
from naoqi import ALProxy
import os
import sys
import time
import random
import math
import numpy as np
import skimage.io
import skimage.color
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import patches,  lines
from matplotlib.patches import Polygon
from pycocotools import mask
import colorsys

sys.path.append('./Mask_RCNN/')
sys.path.append('./Mask_RCNN/samples/clevr')
from mrcnn import utils
import mrcnn.model as modellib
from mrcnn import visualize
import clevr


class InferenceConfig(clevr.ClevrConfig):
    GPU_COUNT = 1
    IMAGES_PER_GPU = 1
    NUM_CLASSES = 4


class NaoVision(object):
	'''The class NaoVision controls everything that has to do
	with object detection, segmentation and feature extraction.'''

	def __init__(self, cfg):
		self.cfg = cfg
		self.photoCaptureProxy = ALProxy("ALPhotoCapture", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		# Set camera parameters
        self.photoCaptureProxy.setResolution(cfg.CAMERA_RESOLUTION)
        self.photoCaptureProxy.setPictureFormat(cfg.PICTURE_FORMAT)
        # Mask R-CNN model
        self.config = InferenceConfig()
        self.model = modellib.MaskRCNN(mode="inference", model_dir=cfg.MODEL_DIR, config=self.config)
        model.load_weights(cfg.CLEVR_MODEL_PATH, by_name=True)


    def capture(self):
    	'''Capture an image and store it in the image dir.
    	Return the complete path to the image'''
        img_name = str(int(time.time()))
        img_directory = "/home/nao/recordings/cameras/"
        self.photoCaptureProxy.takePicture(img_path, img_name)
        img_path = img_directory + img_name + '.' + self.cfg.PICTURE_FORMAT
        return img_path


    def analyze(self, img_path):
    	'''Analyze the given image. Create a new image with the found
    	bounding boxes and use the masks to do feature extraction,
    	returning the features'''
    	image = skimage.io.imread(img_path)
    	results = self.model.detect([image], verbose=1)[0]
    	bboxes = results['rois']
    	bbox_img_path = self._add_bbox_to_image(image, bboxes, img_path)
    	masks  results['masks']
    	data = self._extract_image_features(image, masks)
    	return bbox_img_path, data


    def capture_and_analyze(self):
    	'''Capture an image and immediately analyze it.
    	Returns both the original image path, the image with
    	bboxes and the feature extracted data.'''
    	pass


	def _add_bbox_to_image(self, image, bboxes, orig_path):
		'''Add the bboxes to the image and export this'''
		N = bboxes.shape[0]
		colors = self._random_colors(N)
		height, width = image.shape[:2]
		fig, ax = plt.subplots(1, figsize=figsize)
    	ax.set_ylim(height + 10, -10)
    	ax.set_xlim(-10, width + 10)
    	ax.axis('off')
		masked_image = image.astype(np.uint32).copy()
    	for i in range(N):
        	color = colors[i]
        	y1, x1, y2, x2 = bboxes[i]
        	p = patches.Rectangle((x1, y1), x2 - x1, y2 - y1, linewidth=2,
                                  alpha=0.7, linestyle="dashed",
                                  edgecolor=color, facecolor='none')
        	ax.add_patch(p)
        img_name_and_type = orig_path.split('/')[-1]
        img_name = img_name_and_type.split('.')[0]
        img_type = img_name_and_type.split('.')[1]
        bbox_img_path = self.cfg.IMAGE_DIR + img_name + '_bbox.' + self.cfg.PICTURE_FORMAT
        plt.imsave(bbox_img_path, masked_image.astype(np.uint8))
        return bbox_img_path


	def _extract_image_features(self, image, masks):
		binary_mask = np.array(masks.astype(np.uint8), order='F')
		RLE_masks = mask.encode(binary_mask)
		# Apply each mask in turn to the image
		# Extract features such as x-pos, y-pos, color, width, height, area, etc.
		data = {}
		for mask in RLE_masks:
			pass
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
    	r, g, b = cv.split(image)
    	binary_mask = mask.decode(mask)
    	mean_r = np.mean(r[binary_mask != 0])
    	mean_g = np.mean(g[binary_mask != 0])
    	mean_b = np.mean(b[binary_mask != 0])
    	return (mean_r, mean_g, mean_b)


    def _extract_hsv_from_mask(self, image, mask):
    	hsv_image = skimage.color.rgb2hsv(image)
    	h, s, v = cv.split(image)
    	binary_mask = mask.decode(mask)
    	# Maybe use other way to compute the mean here?
    	# See Bjorn's code
    	mean_h = np.mean(h[binary_mask != 0])
    	mean_s = np.mean(s[binary_mask != 0])
    	mean_v = np.mean(v[binary_mask != 0])
    	return (mean_h, mean_s, mean_v)


    def _extract_lab_from_mask(self, image, mask):
    	lab_image = skimage.color.rgb2lab(image)
    	l, a, b = cv.split(image)
    	binary_mask = mask.decode(mask)
    	mean_l = np.mean(l[binary_mask != 0])
    	mean_a = np.mean(a[binary_mask != 0])
    	mean_b = np.mean(b[binary_mask != 0])
    	return (mean_l, mean_a, mean_b)