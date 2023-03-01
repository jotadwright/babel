#!/usr/bin/env python

from __future__ import print_function

import json
import argparse

from flask import Flask, request
from nao_config import NaoConfig
from nao_vision import NaoVision
from nao_actions import NaoActions
from nao_speech import NaoSpeech
from nao_touch import NaoTouch

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
                           'robot_ip': nao_config.ROBOT_IP,
                           'robot_port': nao_config.ROBOT_PORT}), 200


@nao_server.route("/vision/capture", methods=["POST"])
def capture_image():
    '''Capture an image and return the path where it is stored'''
    vision = NaoVision(nao_config)
    pathname = vision.capture()
    return json.dumps({'pathname': pathname}), 200

@nao_server.route("/posture/get", methods=["POST"])
def get_posture():
    '''Get the current posture'''
    actions = NaoActions(nao_config)
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
        actions = NaoActions(nao_config)
        if 'speed' in request_data:
            success = actions.set_current_posture(str(request_data['posture']), speed=float(request_data['speed']))
        else:
            success = actions.set_current_posture(str(request_data['posture']))
        return json.dumps({'success': success}), 200


@nao_server.route("/set_joint", methods=["POST"])
def set_joint():
    '''Set a certain joint to a given angle'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['joint', 'value'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        actions = NaoActions(nao_config)
        if 'speed' in request_data:
            success = actions.set_joint(str(request_data['joint']), request_data['value'])
        else:
            success = actions.set_joint(str(request_data['joint']), request_data['value'],
                                        speed=float(request_data['speed']))
        return json.dumps({'success': success}), 200


@nao_server.route("/raise_arm", methods=["POST"])
def raise_arm():
    '''Raise Nao's arm'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['arm'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        actions = NaoActions(nao_config)
        success = actions.raise_arm(request_data['arm'])
        return json.dumps({'success': success}), 200


@nao_server.route("/move_head", methods=["POST"])
def move_head():
    '''Move Nao's head to knod or shake'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['yesno'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        actions = NaoActions(nao_config)
        success = actions.move_head(request_data['yesno'])
        return json.dumps({'success': success}), 200


@nao_server.route("/speech/say", methods=["POST"])
def speak():
    '''Make Nao say something'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['speech'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        speech = NaoSpeech(nao_config)
        success = speech.say(request_data['speech'].encode("utf-8"))
        return json.dumps({'success': success}), 200


@nao_server.route("/speech/start_recognition", methods=["POST"])
def start_speech_recognition():
    '''Start the ASR module with a certain vocabulary'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['vocabulary'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        asr = NaoSpeech(nao_config)
        subscriber = asr.start_speech_recognition(request_data['vocabulary'])
        return json.dumps({'subscriber': subscriber}), 200


@nao_server.route("/speech/stop_recognition", methods=["POST"])
def stop_speech_recognition():
    '''Stop the speech recognition and return the found word'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['subscriber'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        asr = NaoSpeech(nao_config)
        recognised = asr.stop_speech_recognition(request_data['subscriber'])
        return json.dumps({'recognised': recognised}), 200


@nao_server.route("/headtouch/detect", methods=["POST"])
def detect_headtouch():
    '''Activate certain touch sensors on the head and wait for touch'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['region'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        touch = NaoTouch(nao_config)
        success = touch.detect_touch(request_data['region'])
        return json.dumps({'success': success}), 200


@nao_server.route("/headtouch/yes_no", methods=["POST"])
def yes_no_headtouch():
    '''Activate the front and back touch sensor
    and return which one was touched'''
    touch = NaoTouch(nao_config)
    result = touch.front_or_back()
    return json.dumps({'result': result}), 200


@nao_server.route("/shutdown", methods=["GET"])
def shutdown_nao_server():
    shutdown_fn = request.environ.get('werkzeug.server.shutdown')
    shutdown_fn()
    return json.dumps({'result': 'Server shutting down'}), 200


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--robot-ip',
                        action="store",
                        dest="robot_ip",
                        default="192.168.1.4",
                        help="The robot's IP address")
    parser.add_argument('--robot-lang',
                        action="store",
                        dest="robot_lang",
                        default="English",
                        help="The language the robot should speak")
    parser.add_argument('--robot-port',
                        action="store",
                        dest="robot_port",
                        default=9559,
                        type=int,
                        help="The robot's port number")
    parser.add_argument('--server-host',
                        action='store',
                        dest='server_host',
                        default='127.0.0.1',
                        help='The server host address')
    parser.add_argument('--server-port',
                        action='store',
                        dest='server_port',
                        default=7850,
                        help='The server port number')
    cmd = parser.parse_args()

    if cmd.robot_ip is not None:
        nao_config.ROBOT_IP = cmd.robot_ip
    if cmd.robot_lang is not None:
        nao_config.ROBOT_LANG = cmd.robot_lang
    if cmd.robot_port is not None:
        nao_config.ROBOT_PORT = cmd.robot_port
    nao_server.run(host=cmd.server_host, port=cmd.server_port)
