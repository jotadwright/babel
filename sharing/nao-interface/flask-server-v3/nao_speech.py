#!python2.7
#!/usr/bin/env python

import naoqi
from naoqi import ALProxy
import time

class NaoSpeech(object):
	'''The class NaoSpeech controls everything that has to do
	with speech and speech recognition.'''

	def __init__(self, cfg):
        self.cfg = cfg
		self.ttsProxy = ALProxy("ALTextToSpeech", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		self.asrProxy = ALProxy("ALSpeechRecognition", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		self.memoryProxy = ALProxy("ALMemory", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		self.ledProxy = ALProxy("ALLeds", cfg.ROBOT_IP, cfg.ROBOT_PORT)


	def say(self, speech):
        self.ttsProxy.say(str(speech))
        return True


    def start_speech_recognition(self, vocabulary):
        self.asrProxy.pause(True)
        self.asrProxy.setLanguage("English")
        vocab = [str(v) for v in vocabulary]
        self.asrProxy.setVocabulary(vocab, False)
        subscriber = "Nao_ASR_" + str(int(time.time()))
        self.asrProxy.pause(False)
        self.asrProxy.subscribe(subscriber)
        self.ledProxy.on("EarLeds")
        return subscriber


    def stop_speech_recognition(self, subscriber=""):
        detected = self.memoryProxy.getData("WordRecognized")
        self.asrProxy.unsubscribe(str(subscriber))
        self.ledProxy.off("EarLeds")
        return detected