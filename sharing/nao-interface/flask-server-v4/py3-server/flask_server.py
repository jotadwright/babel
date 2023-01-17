#!/usr/bin/env python

# Import packages
from __future__ import print_function

import json
import os
from glob import glob
import argparse
import matplotlib.pyplot as plt
from flask import Flask, request

from gluoncv import model_zoo, data, utils

home = os.path.expanduser('~')
babel = glob(os.path.join(home, '**/babel'))[0]


class VisionConfig(object):
    '''The NaoConfig holds all configurations related to the vision server'''
    ROBOT_IP = "192.168.2.4"
    ROBOT_PORT = 7850
    # Camera parameters
    CAMERA_RESOLUTION = 2
    PICTURE_FORMAT = "jpg"
    IMAGE_DIR = os.path.join(babel, ".tmp/nao-img/")
    MODEL = model_zoo.get_model('mask_rcnn_resnet50_v1b_coco', pretrained=True)

vision_config = VisionConfig()

class Vision(object):
    '''The class Vision controls everything that has to do
    with object detection, segmentation and feature extraction.'''
    def __init__(self, cfg):
        self.cfg = cfg
        # Get the Mask R-CNN model
        self.model = cfg.MODEL
    
    def analyze(self, img_path):
        '''Analyze the given image. Create a new image with the found
        bounding boxes and masks ad use the masks to do feature extraction,
        returning the features'''
        x, img = data.transforms.presets.rcnn.load_test(img_path)
        ids, scores, bboxes, masks = [xx[0].asnumpy() for xx in self.model(x)]
        print('Found %d objects' % len(bboxes))
        bbox_img_path = self.visualise_bboxes(img, masks, bboxes, scores, ids, self.model.classes, img_path)
        data = self._extract_image_features(img, bboxes, masks, ids, scores)
        return bbox_img_path, data
    
    def visualise_bboxes(self, img, masks, bboxes, scores, ids, classes, ax, orig_path):
        '''Add the bboxes to the image and export this'''
        width, height = img.shape[1], img.shape[0]
        masks, _ = utils.viz.expand_mask(masks, bboxes, (width, height), scores)
        img = utils.viz.plot_mask(img, masks)
        fig = plt.figure(figsize=(10, 10))
        ax = fig.add_subplot(1, 1, 1)
        ax = utils.viz.plot_bbox(img, bboxes, scores, ids, class_names=classes, ax=ax)
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
            p_whites, p_blacks = self._white_and_black_percentage(image, mask, area)
            object['p_whites'] = p_whites
            object['p_blacks'] = p_blacks
            # add the object to data
            data[object_id] = object
        return data

# Flask server
py3_server = Flask(__name__)

# checking data retrieved from lisp
def check_request_data(request_data, keys):
    '''Helper function that checks if certain key(s) are present
    in the POST data and if they are, returns the data.'''
    errors = []
    for key in keys:
        if key not in request_data:
            errors.append('No data provided using the "{}" key.'.format(key))
    return errors


@py3_server.route("/vision/analyse", methods=["POST"])
def analyze_image():
    '''Analyze the image at the given pathname.
    Returns both the data of the analysis and the pathname
    of the image + bboxes'''
    # decodes json into request_data
    request_data = request.get_json(force=True)
    # if request_data does not have a filename, then an error is thrown
    errors = check_request_data(request_data, ['filename'])
    # if there are errors, analysis is not performed
    if errors:
        return json.dumps({'errors' : errors}), 400
    # if there are no errors, the image located at filename is analyzed
    # the data from analysis is encoded to json and returned by the function
    else:
        vision = Vision(vision_config)
        pathname, data = vision.analyze(request_data['filename'])
        return json.dumps({'pathname': pathname,
                           'data': data}), 200

# Running the py-3 server

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--robot-ip',
                        action="store",
                        dest="robot_ip",
                        default="192.168.2.4",
                        help="The robot's IP address")
    parser.add_argument('--robot-port',
                        action="store",
                        dest="robot_port",
                        default=7850,
                        type=int,
                        help="The robot's port number")
    parser.add_argument('--server-host',
                        action='store',
                        dest='server_host',
                        default='127.0.0.1',
                        help='The server host address')
    parser.add_argument('--server-port',
                        action='store',
                        dest='server_port',
                        default=7851,
                        help='The server port number')
    cmd = parser.parse_args()
    
    if cmd.robot_ip is not None:
        vision_config.ROBOT_IP = cmd.robot_ip
    if cmd.robot_port is not None:
        vision_config.ROBOT_PORT = cmd.robot_port
    py3_server.run(host=cmd.server_host, port=cmd.server_port)