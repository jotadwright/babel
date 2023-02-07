#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
from glob import glob
# import some common detectron2 utilities
from detectron2 import model_zoo
from detectron2.engine import DefaultPredictor
from detectron2.config import get_cfg

# Set the home directory
home = os.path.expanduser('~')
babel = glob(os.path.join(home, 'babel'))[0]


def load_detectron2(detectron_cfg):
    '''Loads in the detectron2-predictor'''
    detectron_cfg.merge_from_file(model_zoo.get_config_file("COCO-InstanceSegmentation/mask_rcnn_R_50_FPN_3x.yaml"))
    detectron_cfg.MODEL.WEIGHTS = model_zoo.get_checkpoint_url("COCO-InstanceSegmentation/mask_rcnn_R_50_FPN_3x.yaml")
    return DefaultPredictor(detectron_cfg)

def set_model_cfg(tresh = 0.5, device = 'cpu', mask_format = 'bitmask'):
    cfg = get_cfg()
    cfg.MODEL.ROI_HEADS.SCORE_THRESH_TEST = tresh
    cfg.MODEL.DEVICE = device
    cfg.INPUT.MASK_FORMAT = mask_format
    cfg.merge_from_file(model_zoo.get_config_file("COCO-InstanceSegmentation/mask_rcnn_R_50_FPN_3x.yaml"))
    cfg.MODEL.WEIGHTS = model_zoo.get_checkpoint_url("COCO-InstanceSegmentation/mask_rcnn_R_50_FPN_3x.yaml")
    return cfg

class VisionConfig(object):
    '''The VisionConfig holds all configurations related to the vision server'''
    ROBOT_IP = "192.168.0.101"
    ROBOT_PORT = 7850
    # Camera parameters
    CAMERA_RESOLUTION = 2
    PICTURE_FORMAT = "jpg"
    IMAGE_DIR = os.path.join(babel, ".tmp/nao-img/")
    MODEL_cfg = set_model_cfg()
    MODEL = load_detectron2(MODEL_cfg)
    

   

