from __future__ import print_function

import os

import cv2 as cv
import numpy as np
import time

from naoqi import ALProxy
from PIL import Image

from cv_utils.vision import analyze_image


class NaoVision(object):
    resolution = 2
    color_space = 11
    frame_rate = 5

    def __init__(self, ip, port):
        # Init proxies.
        try:
            self.photoCaptureProxy = ALProxy("ALPhotoCapture", ip, port)
            self.videoProxy = ALProxy("ALVideoDevice", ip, port)
        except Exception as e:
            print("Error when creating ALPhotoCapture proxy:")
            print(str(e))
        # Set camera parameters
        self.photoCaptureProxy.setResolution(self.resolution)
        self.photoCaptureProxy.setPictureFormat("jpg")

        self.videoProxy.setParameter(0, 22, 1)

    def _capture_image(self):
        subscription = self.videoProxy.subscribe('python_subscribe', self.resolution, self.color_space, self.frame_rate)
        image = self.videoProxy.getImageRemote(subscription)
        image_width = image[0]
        image_height = image[1]
        i = image[6]
        im = Image.frombytes("RGB", (image_width, image_height), i)
        self.videoProxy.unsubscribe(subscription)
        im = np.array(im, np.uint8)
        im = cv.cvtColor(im, cv.COLOR_RGB2BGR)
        return im

    def capture_and_analyze(self):
        im = self._capture_image()
        img_name = str(int(time.time())) + ".jpg"
        cv.imwrite(os.path.join("img", img_name), im)
        dst, data, clevr_data = analyze_image(im, pre_color_segmentation=True, show_intermediate_steps=True)
        img_dir = '/naoqi/src/img/'
        unix = img_name.split('.')[0]
        outname = unix + '-analysis'
        outfile = outname + '.jpg'
        outpath = img_dir + outfile
        cv.imwrite(outpath, dst)
        return outname, data, clevr_data

    def capture(self, img_path="/home/nao/recordings/cameras/"):
        # Create a unique name for picture -> current timestamp
        img_name = str(int(time.time()))
        # Take the picture
        self.photoCaptureProxy.takePicture(img_path, img_name)
        # We want path, name and ext separately (?)
        # Could also return _, but then need parsing in Lisp
        # to get filename and extension out
        return img_path, img_name, "jpg"

    def analyse(self, filename=""):
        # Get the following data for each object:
        # Position of the center (X,Y), Shape, Color, Width, Height, ID

        # load the image and resize it to a smaller factor so that
        # the shapes can be approximated better

        img_dir = '/naoqi/src/img/'

        path = img_dir + filename
        image = cv.imread(path)
        dst, data = analyze_image(image)

        unix = filename.split('.')[0]
        outname = unix + '-analysis'
        outfile = outname + '.jpg'
        outpath = img_dir + outfile
        cv.imwrite(outpath, dst)

        return outname, data
