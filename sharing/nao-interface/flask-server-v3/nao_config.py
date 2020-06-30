#!python2.7
#!/usr/bin/env python

import os
import sys
from glob import glob
home = os.path.expanduser('~')
# We assume the directory is called Babel3!!
babel = glob(os.path.join(home, '**/Babel3'))[0]
flask_server = os.path.join(babel, 'sharing/nao-interface/flask-server-v3')
sys.path.append(os.path.join(flask_server, 'Mask_RCNN/'))
sys.path.append(os.path.join(flask_server, 'Mask_RCNN/samples/clevr'))
import mrcnn.model as modellib
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
    MODEL_DIR = os.path.join(flask_server, 'Mask_RCNN/logs/')
    CLEVR_MODEL_PATH = os.path.join(flask_server,
                                    "Mask_RCNN/mask_rcnn_clevr.h5")
    IMAGE_DIR = os.path.join(babel, ".tmp/nao-img/")
    MODEL_CLASS_NAMES = ['BG', 'cube', 'sphere', 'cylinder']
    # Mask R-CNN model
    # We put the model already in the NaoConfig, since it's
    # quite slow to load. So it will only need to load once,
    # when the Flask server is starting
    MODEL_CONFIG = InferenceConfig()
    MODEL = modellib.MaskRCNN(mode="inference", model_dir=MODEL_DIR,
                              config=MODEL_CONFIG)
    MODEL.load_weights(CLEVR_MODEL_PATH, by_name=True)
