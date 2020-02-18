#!python2.7
#!/usr/bin/env python

import naoqi
from naoqi import ALProxy
import time

class NaoTouch(object):
	'''The class NaoTouch controls everything that has to do
	with the touch sensors.'''

	def __init__(self, cfg):
        self.cfg = cfg
		self.memoryProxy = ALProxy("ALMemory", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		self.ledProxy = ALProxy("ALLeds", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		self.front_sensor = "Device/SubDeviceList/Head/Touch/Front/Sensor/Value"
        self.middle_sensor = "Device/SubDeviceList/Head/Touch/Middle/Sensor/Value"
        self.rear_sensor = "Device/SubDeviceList/Head/Touch/Rear/Sensor/Value"
        self.front_leds = "BrainLedsFront"
        self.middle_leds = "BrainLedsMiddle"
        self.back_leds = "BrainLedsBack"
        self.brain_leds = "BrainLeds"
        self.delay = 0.5


    def _all_leds_on(self):
        self.ledProxy.on(self.brain_leds)


    def _all_leds_off(self):
        self.ledProxy.off(self.brain_leds)


    def detect_touch(self, region):
        self._all_leds_off()
        if region == "Front":
            self.ledProxy.on(self.front_leds)
            while self.memoryProxy.getData(self.front_sensor) < 0.5:
                time.sleep(self.delay)
            self._all_leds_on()
            return True
        elif region == "Rear":
            self.ledProxy.on(self.back_leds)
            while self.memoryProxy.getData(self.rear_sensor) < 0.5:
                time.sleep(self.delay)
            self._all_leds_on()
            return True
        elif region == "Middle":
            self.ledProxy.on(self.middle_leds)
            while self.memoryProxy.getData(self.middle_sensor) < 0.5:
                time.sleep(self.delay)
            self._all_leds_on()
            return True


     def front_or_back(self):
        self._all_leds_on()
        self.ledProxy.off(self.middle_leds)
        while True:
            if self.memoryProxy.getData(self.front_sensor) > 0.5:
                self._all_leds_on()
                return "front"
            elif self.memoryProxy.getData(self.rear_sensor) > 0.5:
                self._all_leds_on()
                return "back"
            time.sleep(self.delay)