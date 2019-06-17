#!python2.7
# !/usr/bin/env python

from __future__ import print_function

import json
import naoqi
import argparse

from flask import Flask, request
from naoVision import NaoVision
from naoActions import NaoPosture, NaoJoints, NaoSpeak, NaoHeadTouch, NaoSpeechRecognition

# The Flask app is the nao_server
nao_server = Flask(__name__)
IP = ""
PORT = ""


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
def make_connection():
    # Get data from request
    request_data = request.get_json(force=True)
    # Check data
    errors = check_request_data(request_data, ['message'])
    # Return
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        return json.dumps({'message': request_data['message'],
                           'robot_ip': IP,
                           'robot_port': PORT}), 200


@nao_server.route("/vision/capture", methods=["POST"])
def capture_image():
    vision = NaoVision(ip=IP, port=PORT)
    directory, name, file_type = vision.capture()
    return json.dumps({'directory': directory,
                       'name': name,
                       'type': file_type}), 200


@nao_server.route("/vision/analyse", methods=["POST"])
def analyze_image():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['filename'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        vision = NaoVision(ip=IP, port=PORT)
        name, data = vision.analyse(filename=request_data['filename'])
        return json.dumps({'filename': name,
                           'data': data}), 200


@nao_server.route("/posture/get", methods=["POST"])
def get_posture():
    posture = NaoPosture(ip=IP, port=PORT)
    current_posture = posture.get()
    return json.dumps({'posture': current_posture}), 200


@nao_server.route("/posture/set", methods=["POST"])
def set_posture():
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['posture'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        posture = NaoPosture(ip=IP, port=PORT)
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


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--robot-ip',
                        action="store",
                        dest="robot_ip",
                        default="192.168.1.2",
                        help="The robot's IP address")
    parser.add_argument('--robot-port',
                        action="store",
                        dest="robot_port",
                        default=9559,
                        type=int,
                        help="The robot's port number")
    cmd = parser.parse_args()

    if cmd.robot_ip is not None:
        IP = cmd.robot_ip
    if cmd.robot_port is not None:
        PORT = cmd.robot_port
    nao_server.run(host='0.0.0.0', port=80)
