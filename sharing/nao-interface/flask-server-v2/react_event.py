
from naoqi import ALProxy, ALBroker, ALModule
import time
import sys

# Global variable to store the HumanGreeter module instance
WordRecognizer = None
memory = None


class WordRecognizerModule(ALModule):
    """ A simple module able to react to facedetection events"""
    def __init__(self, name):
        ALModule.__init__(self, name)
        self.leds = ALProxy("ALLeds")
        # Subscribe to the FaceDetected event:
        global memory
        memory = ALProxy("ALMemory")
        memory.subscribeToEvent("WordRecognized",
                                "WordRecognizer",
                                "onWordRecognized")

    def onWordRecognized(self, event, status, subscriber):
        '''docstring'''
        print(status)
        self.leds.rasta(2.0)


def main(pip, pport, vocabulary):
    """ Main entry point"""
    myBroker = ALBroker("myBroker", "0.0.0.0", 0, pip, pport)
    global WordRecognizer
    WordRecognizer = WordRecognizerModule("WordRecognizer")
    asrProxy = ALProxy("ALSpeechRecognition")
    ledProxy = ALProxy("ALLeds")
    memoryProxy = ALProxy("ALMemory")
    asrProxy.pause(True)
    asrProxy.setLanguage("English")
    vocab = [str(v) for v in vocabulary]
    asrProxy.setVocabulary(vocab, False)
    asrProxy.pause(False)
    subscriber = "Nao_ASR_" + str(int(time.time()))
    asrProxy.subscribe(subscriber)
    ledProxy.on("EarLeds")
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print
        print "Interrupted by user, shutting down"
        detected = memoryProxy.getData("WordRecognized")
        asrProxy.unsubscribe(str(subscriber))
        ledProxy.off("EarLeds")
        print "Detected words {}".format(detected)
        myBroker.shutdown()
        sys.exit(0)


main("192.168.1.4", 9559, ["green", "blue", "red"])