#!python2.7
#!/usr/bin/env python

from __future__ import print_function

import json
import argparse

from flask import Flask, request
from nao_config import NaoConfig
from nao_vision import NaoVision
from nao_actions import NaoActions

# The Flask app is the nao_server
nao_server = Flask(__name__)
nao_config = NaoConfig()


def check_request_data(request_data, keys):
    '''Helper function that checks if certain key(s) are present
    in the POST data and if they are, returns the data.'''
    errors = []
    for key in keys:
        if key not in request_data:
            errors.append('No data provided using the "{}" key.'.format(key))
    return errors


@nao_server.route("/test_connection", methods=["POST"])
def make_connection():
	'''Test the connection by sending back the received message'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['message'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        return json.dumps({'message': request_data['message'],
                           'robot_ip': IP,
                           'robot_port': PORT}), 200


@nao_server.route("/vision/capture", methods=["POST"])
def capture_image():
	'''Capture an image and return the path where it is stored'''
    vision = NaoVision(cfg=nao_config)
    pathname = vision.capture()
    return json.dumps({'pathname': pathname}), 200


@nao_server.route("/vision/analyse", methods=["POST"])
def analyze_image():
	'''Analyze the image at the given pathname.
	Returns both the data of the analysis and the pathname
	of the image + bboxes'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['filename'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        vision = NaoVision(cfg=nao_config)
        pathname, data = vision.analyse(request_data['filename'])
        return json.dumps({'pathname': pathname,
                           'data': data}), 200


@nao_server.route("/vision/capture_analyse", methods=["POST"])
def capture_analyze_image():
	'''Capture an image and immediately analyse it. This returns
	the pathname of the original image, the pathname of the image
	with bbox and the analysis data.'''
    vision = NaoVision(ip=IP, port=PORT)
    orig_pathname, bbox_pathname, data = vision.capture_and_analyze()
    return json.dumps({'pathname': orig_pathname,
    				   'analysis_pathname': bbox_pathname,
                       'data': data}), 200


@nao_server.route("/posture/get", methods=["POST"])
def get_posture():
	'''Get the current posture'''
    actions = NaoActions(cfg=nao_config)
    current_posture = actions.get_current_posture()
    return json.dumps({'posture': current_posture}), 200



@nao_server.route("/posture/set", methods=["POST"])
def set_posture():
	'''Set the current posture'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['posture'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        actions = NaoActions(cfg=nao_config)
        if 'speed' in request_data:
            success = posture.set(posture=str(request_data['posture']),
                                  speed=float(request_data['speed']))
        else:
            success = posture.set(posture=str(request_data['posture']))
        return json.dumps({'success': success}), 200


@nao_server.route("/set_joint", methods=["POST"])
def set_joint():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['joint', 'value'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        joints = NaoJoints(ip=IP, port=PORT)
        if 'speed' in request_data:
            success = joints.set(joint=str(request_data['joint']),
                                 value=request_data['value'])
        else:
            success = joints.set(joint=str(request_data['joint']),
                                 value=request_data['value'],
                                 speed=float(request_data['speed']))
        return json.dumps({'success': success}), 200


@nao_server.route("/raise_arm", methods=["POST"])
def raise_arm():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['arm'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        joints = NaoJoints(ip=IP, port=PORT)
        success = joints.raise_arm(arm=request_data['arm'])
        return json.dumps({'success': success}), 200


@nao_server.route("/move_head", methods=["POST"])
def move_head():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['yesno'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        joints = NaoJoints(ip=IP, port=PORT)
        success = joints.move_head(yesno=request_data['yesno'])
        return json.dumps({'success': success}), 200


@nao_server.route("/speech/say", methods=["POST"])
def speak():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['speech'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech = NaoSpeak(ip=IP, port=PORT)
        success = speech.say(request_data['speech'])
        return json.dumps({'success': success}), 200


@nao_server.route("/speech/start_recognition", methods=["POST"])
def start_speech_recognition():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['vocabulary'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech_recog = NaoSpeechRecognition(ip=IP,
                                            port=PORT)
        subscriber = speech_recog.start_speech_recognition(
            vocabulary=request_data['vocabulary'])
        return json.dumps({'subscriber': subscriber}), 200


@nao_server.route("/speech/stop_recognition", methods=["POST"])
def stop_speech_recognition():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['subscriber'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech_recog = NaoSpeechRecognition(ip=IP,
                                            port=PORT)
        recognised = speech_recog.stop_speech_recognition(
            subscriber=request_data['subscriber'])
        return json.dumps({'recognised': recognised}), 200


@nao_server.route("/headtouch/detect", methods=["POST"])
def detect_headtouch():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['region'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        touch = NaoHeadTouch(ip=IP, port=PORT)
        success = touch.detect_touch(region=request_data['region'])
        return json.dumps({'success': success}), 200


@nao_server.route("/headtouch/yes_no", methods=["POST"])
def yes_no_headtouch():
    touch = NaoHeadTouch(ip=IP, port=PORT)
    result = touch.front_or_back()
    return json.dumps({'result': result}), 200