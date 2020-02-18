#!python2.7
#!/usr/bin/env python

import naoqi
from naoqi import ALProxy

class NaoActions(object):
	'''The class NaoActions controls everything that has to
	do with robot movement.'''

	def __init__(self, cfg):
        self.cfg = cfg
		self.motionProxy = ALProxy("ALMotion", cfg.ROBOT_IP, cfg.ROBOT_PORT)
		self.postureProxy = ALProxy("ALRobotPosture", cfg.ROBOT_IP, cfg.ROBOT_PORT)


	def stiffness_on(self):
        if not self.motionProxy.robotIsWakeUp():
            self.motionProxy.wakeUp()


    def stiffness_off(self, proxy):
        if self.motionProxy.robotIsWakeUp():
            self.motionProxy.rest()


    def set_current_posture(self, posture, speed=0.3):
        self.stiffness_on()
        success = self.postureProxy.goToPosture(posture, speed)
        return success


    def get_current_posture(self):
        posture = self.postureProxy.getPosture()
        return posture


    def set_joint(self, joint, value, speed=0.3):
        self.stiffness_on()
        self.motionProxy.setAngles(joint, value, speed)
        return True


    def _raise_left(self):
        LScurrentPitch = self.motionProxy.getAngles("LShoulderPitch", False)[0]
        LScurrentRoll = self.motionProxy.getAngles("LShoulderRoll", False)[0]
        LEcurrentRoll = self.motionProxy.getAngles("LElbowRoll", False)[0]

        jointList = ["LShoulderPitch", "LShoulderRoll", "LElbowRoll"]
        angleList = [[0.4, LScurrentPitch],
                     [0.0, LScurrentRoll],
                     [-0.1, LEcurrentRoll]]
        timeList = [[2.0, 6.0], [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True

        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return True


    def _raise_right(self):
        RScurrentPitch = self.motionProxy.getAngles("RShoulderPitch", False)[0]
        RScurrentRoll = self.motionProxy.getAngles("RShoulderRoll", False)[0]
        REcurrentRoll = self.motionProxy.getAngles("RElbowRoll", False)[0]

        jointList = ["RShoulderPitch", "RShoulderRoll", "RElbowRoll"]
        angleList = [[0.4, RScurrentPitch],
                     [0.0, RScurrentRoll],
                     [0.1, REcurrentRoll]]
        timeList = [[2.0, 6.0], [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True

        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return True


    def _raise_both(self):
        LScurrentPitch = self.motionProxy.getAngles("LShoulderPitch", False)[0]
        LScurrentRoll = self.motionProxy.getAngles("LShoulderRoll", False)[0]
        LEcurrentRoll = self.motionProxy.getAngles("LElbowRoll", False)[0]
        RScurrentPitch = self.motionProxy.getAngles("RShoulderPitch", False)[0]
        RScurrentRoll = self.motionProxy.getAngles("RShoulderRoll", False)[0]
        REcurrentRoll = self.motionProxy.getAngles("RElbowRoll", False)[0]

        jointList = ["LShoulderPitch", "LShoulderRoll", "LElbowRoll",
                     "RShoulderPitch", "RShoulderRoll", "RElbowRoll"]
        angleList = [[0.4, LScurrentPitch], [-0.3, LScurrentRoll], [-0.1, LEcurrentRoll],
                     [0.4, RScurrentPitch], [0.3, RScurrentRoll], [0.1, REcurrentRoll]]
        timeList = [[2.0, 6.0], [2.0, 6.0], [2.0, 6.0],
                    [2.0, 6.0], [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True
        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return True


    def raise_arm(self, arm):
        if arm == "Left":
            return self._raise_left()
        elif arm == "Right":
            return self._raise_right()
        elif arm == "Both":
            return self._raise_both()
        else:
            return False


    def _say_yes(self):
        current_head_pitch = self.motionProxy.getAngles("HeadPitch", False)[0]
        names = ["HeadPitch"]
        angleList = [-0.4, 0.3, -0.4, 0.3, current_head_pitch]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.angleInterpolation(names, angleList, timeList, True)
        return True


    def _say_no(self):
        current_head_yaw = self.motionProxy.getAngles("HeadYaw", False)[0]
        names = ["HeadYaw"]
        angleList = [1.0, -1.0, 1.0, -1.0, current_head_yaw]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.angleInterpolation(names, angleList, timeList, True)
        return True


    def move_head(self, yesno):
        if yesno == "yes":
            return self._say_yes()
        elif yesno == "no":
            return self._say_no()
        else:
            return False