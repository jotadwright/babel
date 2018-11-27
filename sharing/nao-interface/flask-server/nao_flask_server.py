#!python2.7
# !/usr/bin/env python

from __future__ import print_function

import json
import naoqi

from flask import Flask, request
from naoVision import NaoVision
from naoActions import NaoPosture, NaoJoints, NaoSpeak, NaoHeadTouch, NaoSpeechRecognition

# The Flask app is the nao_server
nao_server = Flask(__name__)


def check_request_data(request_data, keys):
    # Helper function that checks if certain key(s) are present
    # in the POST data and if they are, returns the data.
    errors = []
    # Check for the given key(s)
    for key in keys:
        if key not in request_data:
            errors.append('No data provided using the "{}" key.'.format(key))
    return errors


@nao_server.route("/test_connection", methods=["POST"])
def test_connection():
    # Get data from request
    request_data = request.get_json(force=True)
    # Check data
    errors = check_request_data(request_data, ['ip', 'port', 'test_message'])
    # Return
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        return json.dumps({'test_message': request_data['test_message']}), 200


@nao_server.route("/vision/capture", methods=["POST"])
def capture_image():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        vision = NaoVision(ip=request_data['ip'], port=request_data['port'])
        directory, name, type = vision.capture()
        return json.dumps({'directory': directory,
                           'name': name,
                           'type': type}), 200


@nao_server.route("/vision/analyse", methods=["POST"])
def analyze_image():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'filename'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        vision = NaoVision(ip=request_data['ip'], port=request_data['port'])
        name, data = vision.analyse(request_data['filename'])
        return json.dumps({'filename': name,
                           'data': data}), 200


@nao_server.route("/posture/get", methods=["POST"])
def get_posture():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        posture = NaoPosture(ip=request_data['ip'], port=request_data['port'])
        current_posture = posture.get()
        return json.dumps({'posture': current_posture}), 200


@nao_server.route("/posture/set", methods=["POST"])
def set_posture():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'posture'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        posture = NaoPosture(ip=request_data['ip'], port=request_data['port'])
        if 'speed' in request_data:
            success = posture.set(posture=str(request_data['posture']),
                                  speed=float(request_data['speed']))
        else:
            success = posture.set(posture=request_data['posture'])
        return json.dumps({'success': success}), 200


@nao_server.route("/move_joint", methods=["POST"])
def move_joint():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'joint', 'value'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        joints = NaoJoints(ip=request_data['ip'], port=request_data['port'])
        if 'speed' in request_data:
            success = joints.move(joint=str(request_data['joint']),
                                  value=request_data['value'])
        else:
            success = joints.move(joint=str(request_data['joint']),
                                  value=request_data['value'],
                                  speed=float(request_data['speed']))
        return json.dumps({'success': success}), 200


@nao_server.route("/speech/say", methods=["POST"])
def speak():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'speech'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech = NaoSpeak(ip=request_data['ip'], port=request_data['port'])
        speech_data = speech.say(request_data['speech'])
        return json.dumps({'speech': speech_data}), 200


@nao_server.route("/speech/recognise/start", methods=["POST"])
def start_speech_recognition():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'vocabulary'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech_recog = NaoSpeechRecognition(ip=request_data['ip'],
                                            port=request_data['port'])
        subscriber = speech_recog.start_speech_recognition(
            vocabulary=request_data['vocabulary'])
        return json.dumps({'subscriber': subscriber}), 200


@nao_server.route("/speech/recognise/stop", methods=["POST"])
def stop_speech_recognition():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'subscriber'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech_recog = NaoSpeechRecognition(ip=request_data['ip'],
                                            port=request_data['port'])
        recognised = speech_recog.stop_speech_recognition(
            subscriber=request_data['subscriber'])
        return json.dumps({'recognised': recognised}), 200


@nao_server.route("/headtouch/detect", methods=["POST"])
def detect_headtouch():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port', 'region'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        touch = NaoHeadTouch(ip=request_data['ip'], port=request_data['port'])
        success = touch.detect_touch(region=request_data['region'])
        return json.dumps({'success': success}), 200


@nao_server.route("/headtouch/yes_no", methods=["POST"])
def yes_no_headtouch():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['ip', 'port'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        touch = NaoHeadTouch(ip=request_data['ip'], port=request_data['port'])
        result = touch.front_or_back()
        return json.dumps({'result': result}), 200
