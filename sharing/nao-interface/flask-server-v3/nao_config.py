#!python2.7
#!/usr/bin/env python

class NaoConfig(object):
	'''The NaoConfig holds all configurations for the robot'''
	ROBOT_IP = "192.168.1.4"
	ROBOT_PORT = 9559

	CAMERA_RESOLUTION = 2
	PICTURE_FORMAT = "jpg"

	MODEL_DIR = './Mask_RCNN/logs/'
	CLEVR_MODEL_PATH = "./Mask_RCNN/mask_rcnn_clevr.h5"
	IMAGE_DIR = "./robot_images/"
	MODEL_CLASS_NAMES = ['BG', 'cube', 'sphere', 'cylinder']