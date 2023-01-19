#!python2.7
#!/usr/bin/env python

import os
from glob import glob
home = os.path.expanduser('~')
# We assume the directory is called Babel3!!
babel = '/Users/ajouglar/babel'
flask_server = os.path.join(babel, 'sharing/nao-interface/flask-server-v3')

class NaoConfig(object):
    '''The NaoConfig holds all configurations for the robot'''
    # Robot IP and port
    ROBOT_IP = "192.168.1.4"
    ROBOT_PORT = 9559
    ROBOT_LANG = "English"
    # Camera parameters
    CAMERA_RESOLUTION = 2
    PICTURE_FORMAT = "jpg"
    IMAGE_DIR = os.path.join(babel, ".tmp/nao-img/")
