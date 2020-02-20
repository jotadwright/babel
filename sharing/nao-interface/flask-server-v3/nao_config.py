#!python2.7
#!/usr/bin/env python

import sys
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


class NaoConfig(object):
    '''The NaoConfig holds all configurations for the robot'''
    # Robot IP and port
    ROBOT_IP = "192.168.1.4"
    ROBOT_PORT = 9559
    # Camera parameters
    CAMERA_RESOLUTION = 2
    PICTURE_FORMAT = "jpg"
    # Mask R-CNN parameters
    MODEL_DIR = './Mask_RCNN/logs/'
    CLEVR_MODEL_PATH = "./Mask_RCNN/mask_rcnn_clevr.h5"
    IMAGE_DIR = "./robot_images/"
    MODEL_CLASS_NAMES = ['BG', 'cube', 'sphere', 'cylinder']
    # Mask R-CNN model
    # We put the model already in the NaoConfig, since it's
    # quite slow to load. So it will only need to load once,
    # when the Flask server is starting
    MODEL_CONFIG = InferenceConfig()
    MODEL = modellib.MaskRCNN(mode="inference", model_dir=MODEL_DIR, config=MODEL_CONFIG)
    MODEL.load_weights(CLEVR_MODEL_PATH, by_name=True)