# CLEVR World

_Defining a high-level API for the CLEVR dataset_

## The data storage format

This package expects the CLEVR data to be stored in certain directories and with certain filenames and encodings. In this section, we elaborate on the data storage format. If you are missing some data or it is stored in a different way, contact Jens.

### Directory Structure

The CLEVR World package will start its search for the CLEVR dataset from the *babel-corpora* global variable. This global variable points to a directory where all datasets are stored. Typically, we call this folder 'Corpora', hence the name of this variable. Make sure this variable is defined in your `init-babel-user.lisp` file. If you don't have this file, create it at the top-level of your Babel folder. Here is an example of how your `init-babel-user.lisp` file could look:

```
(in-package :cl-user)

(export '(*babel-corpora*))

(defparameter *babel-corpora* (parse-namestring "/path/to/Corpora"))
```

In your 'Corpora' folder (or however you want to call it), there should be a folder for the CLEVR dataset called 'CLEVR-v1.0'. This folder should have 3 subfolders: 'scenes', 'questions' and 'images'. In each of these 3 folders, there should again be subfolders for each dataset-split, i.e. 'scenes/val', 'scenes/train', 'questions/val' and so on. In short, the directory structure should look like this:

```
Corpora/
- CLEVR-v1.0/
-- images/
--- train/
--- val/
-- questions/
--- train/
--- val/
-- scenes/
--- train/
--- val/
```

We note that only the 'scenes' folder is required. The 'images' and 'questions' folders can be left out if these are not needed in your use case.

### File Content

Each scene file should contain a single scene, i.e. a single JSON object. This scene can be stored on a single line or in pretty-printed format. The clevr-world package will load the entire file and treat it as a single JSON object. The question files, on the other hand, should contain a single JSON object per line. A single question file contains all 10 questions for a particular scene, one per line. The clevr-world package will treat each line of such a file as a single json object.

### Filenames

The clevr-world package uses the filenames to connect each scene with the correct set of questions. Therefore, these should have the same filename. We will illustrate this with an example. The scenes of the validation set are stored in the folder 'scenes/val'. Each of these files should have a filename according to the pattern `CLEVR_val_xxxxxx.json`, where the 6 x's indicate the scene number. These files can be easily extracted from the large `CLEVR_val_scenes.json` file that comes with the dataset. The questions for the validation set, stored in 'questions/val' should have the same filenames, thus also of the form `CLEVR_val_xxxxxx.json`. This is also true for the corresponding images, stored in 'images/val'. Their filenames are also `CLEVR_val_xxxxxx.png`.

## The API

The API of the clevr-world package revolves around the `clevr-world` class. To start using the CLEVR dataset, simply create an instance of this class. While doing so, you can specify which datasets should be loaded, which scenes should be excluded and whether or not to load the questions together with the scenes. In the example below, only the scenes of the validation set are loaded and the questions are not loaded:

```
(make-instance 'clevr-world :data-sets '("val"))
```

In the following example, both scenes and questions of both the training and validation set are loaded.

```
(make-instance 'clevr-world :data-sets '("val" "train") :load-questions t)
```

When making an instance of `clevr-world`, the instance keeps track of all filenames of all scenes (and possibly question-sets). Only a single scene (and possibly question-set) is loaded into memory. This to keep the initialization of the world fairly quick. Next, the clevr-world package has a number of methods for interacting with scenes and question-sets.

 - `current-scene` Returns the current scene
 - `current-question-set` Returns the current question set
 - `random-scene` This method will choose a random scene, load it from file and return it. When the question-set paths are also loaded, the associated question-set will also be loaded and return as second value. The scene (and possibly question-set) become the current-scene (and current-question-set) of the world instance.
 - `all-scenes` Loads all scenes into memory and returns them as a list.
 - `all-questions` Loads all questions into memory and returns them as a flat list, _so not in question-sets!_
 - `all-scenes-and-questions` Loads all scenes and associated question-sets into memory and returns them as a list of pairs.
 - `do-for-scenes` Load each scene in turn and call a function on it. The `function` argument should be a function of one argument, expecting a scene object.
 - `do-for-scenes-and-questions` Load each scene and associated question-set in turn and call a function on it. The `function` argument should be a function of two arguments, expecting a scene object and question-set object.
