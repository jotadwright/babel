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
    
    return "This is working"



if __name__ == "__main__":
    py3_server.debug = True
    py3_server.run()


