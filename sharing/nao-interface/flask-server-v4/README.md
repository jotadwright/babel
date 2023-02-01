# Flask-server-v4

## Improvements/changes:
- Instead of running a computer vision model for object detection on the Python 2 flask server, this version provides **two flask servers: one for communicating with the Nao** through its python SDK (in Python 2), and **one for performing object detection and feature extraction** (Python 3). This allows the usage of modern algorithms for object detection that are not supported in a Python 2 environment. 
- Integration of pretrained [Detectron2](https://ai.facebook.com/tools/detectron2/) for object detection and feature extraction. 
- Addition of `--robot-lang` argument for setting the output language of the robot

## Installation


### Python 2 environment
- step 1: install python 2.7
- step 2: navigate to `babel/sharing/nao-interface/flask-server-v4/py2-server` in the terminal and install the requirements in `requirements.txt` using pip2.
- step 3: install the python SDK by following the instructions provided on the [softbank installation page](http://doc.aldebaran.com/2-5/dev/python/install_guide.html#python-install-guide). The installation page mentions downloading the software from their community page, but since this page is shut down, you can download the software that fits your system from their [newer software page](https://www.aldebaran.com/en/support/nao-6/downloads-softwares).
- step 4: test the installation by navigating to `babel/sharing/nao-interface/flask-server-v4/py2-server` and by running the following command:

```sh
$ python2 flask_server.py --robot-ip 192.168.2.4 --robot-lang "French"
```

### Python 3 environment
- step 1: navigate to `babel/sharing/nao-interface/flask-server-v4/py3-server` in the terminal and create a virtual environment for the flask server using pip or conda.
- step 2: activate the environment 
- step 4: install detectron2 and other dependencies inside `babel/sharing/nao-interface/flask-server-v4/py3-server/requirements.txt`.
- step 5: test the installation by navigating to `babel/sharing/nao-interface/flask-server-v4/py3-server` and by running the following command:

```sh
$ python flask_server.py 
```

