#!/usr/bin/env python

from __future__ import print_function

import json
import argparse

from gluoncv import model_zoo, data, utils
from matplotlib import pyplot as plt

vision_net = model_zoo.get_model('yolo3_darknet53_coco', pretrained=True)

from flask import Flask, request
py3_server = Flask(__name__)

@py3_server.route("/vision/analyse")
def analyze_image():
    im_fname = utils.download('https://raw.githubusercontent.com/zhreshold/' +
                          'mxnet-ssd/master/data/demo/dog.jpg',
                          path='dog.jpg')
    x, img = data.transforms.presets.yolo.load_test(im_fname, short=512)
    class_IDs, scores, bounding_boxs = vision_net(x)
    ax = utils.viz.plot_bbox(img, bounding_boxs[0], scores[0], class_IDs[0], class_names=vision_net.classes)
    return ax



if __name__ == "__main__":
    py3_server.debug = True
    py3_server.run()


