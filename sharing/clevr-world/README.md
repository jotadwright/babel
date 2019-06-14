# CLEVR World

_Defining a high-level API for the CLEVR dataset_

## The data storage format

This package expects the CLEVR data to be stored in certain directories and with certain filenames and encodings. In this section, we elaborate on the data storage format. If you are missing some data or it is stored in a different way, contact Jens.

### Directory Structure

First off, make sure the global variable *babel-corpora* is defined in your `init-babel-user.lisp` file and points to your 'Corpora' folder. In the 'Corpora' folder, there should be a 'CLEVR' folder which in turn contains the 'CLEVR-v1.0' folder. In turn, this folder should have 3 subfolders: 'scenes', 'questions' and 'images'. In each of these 3 folders, there should again be subfolders for each dataset-split, i.e. 'scenes/val', 'scenes/train', 'questions/val' and so on.

In short, the directory structure should look like this:

```
Corpora/
- CLEVR/
-- CLEVR-v1.0/
--- images/
---- train/
---- val/
--- questions/
---- train/
---- val/
--- scenes/
---- train/
---- val/
```
### Filenames

The clevr-world package uses the filenames to connect each scene with the correct question set. Therefore, these should have the same filename. We will illustrate this with an example. The scenes of the validation set are stored in the folder 'scenes/val'. Each of these files should have a filename according to the pattern `CLEVR_val_xxxxxx.json`, where the x's indicate the scene number. The questions for the validation set, stored in 'questions/val' should have the same filenames, thus also of the form `CLEVR_val_xxxxxx.json`. This is also true for the corresponding images, stored in 'images/val'. Their filenames are also `CLEVR_val_xxxxxx.png`.

### File Encodings

Each scene-file should contain a single scene, i.e. a single JSON object. This scene can be stored on a single line or in pretty-printed format. The clevr-world package will load the entire file and treat it as a single JSON object. The question-files, on the other hand, should be encoding in ndjson format (newline delimited json). In other words, there should be a single JSON object per line. A single question file contains all questions for a particular scene. The clevr-world package will treat each line of such a file as a single json object.

## The API

The API of the clevr-world package revolves around the `clevr-world` class. To start using the CLEVR dataset, simply create an instance of this class. While doing so, you can specify which datasets should be loaded, which scenes should be excluded and whether or not to load the questions together with the scenes. In the example below, only the scenes of the validation set are loaded.

```
(make-instance 'clevr-world :data-sets '("val"))
```

In the following example, both scenes and questions of both the training and validation set are loaded.

```
(make-instance 'clevr-world :data-sets '("val" "train") :load-questions t)
```

When making an instance of `clevr-world`, the instance keeps track of all filenames of all scenes (and possibly question-sets). Only a single scene (and possibly question-set) is loaded into memory. This to keep the initialization of the world fairly quick. Next, the clevr-world package has a number of methods for interacting with scenes and question-sets.

 - `current-scene world => scene` Returns the current scene
 - `current-question-set world => question-set` Returns the current question set
 - `random-scene world => scene (question-set)` This method will choose a random scene, load it from file and return it. When the question-set paths are also loaded, the associated question-set will also be loaded and return as second value. The scene (and possibly question-set) become the current-scene (and current-question-set) of the world instance.
 - `all-scenes world => list of scenes` Loads all scenes and returns them as a list.
 - `all-questions world => list of questions` Loads all questions and returns them as a flat list, _so not in question-sets!_
 - `all-scenes-and-questions world => scene question-set pairs` Loads all scenes and associated question-sets and returns them as a list of pairs.
 - `do-for-scenes world function =>` Load each scene in turn and call the function on it. `function` should be a function of one argument, expecting a scene object.
 - `do-for-scenes-and-questions world function =>` Load each scene and associated question-set in turn and call the function on it. `function` should be a function of two arguments, expecting a scene object and question-set object.


## Class structure

```
clevr-world (entity)
  id <symbol> (entity)
  scenes <list of pathname>
  question-sets <list of pathname>
  data-sets <list of string>
  current-scene <clevr-scene>
  current-question-set <clevr-question-set or nil>
```

```
clevr-object-set (entity)
  id <symbol> (entity)
  objects <list of clevr-object>
```

```
clevr-scene (clevr-object-set)
  id <symbol> (entity)
  objects <list of clevr-object> (clevr-object-set)
  index <number>
  name <string>
  source-path <pathname>
  data-set <string>
  image <pathname>
```

```
clevr-object (entity)
  id <symbol> (entity)
  size <symbol>
  color <symbol>
  shape <symbol>
  material <symbol>
  relationships <list of symbol>
  coordinates <list of float>
  rotation <float>
```

```
clevr-question-set (entity)
  id <symbol> (entity)
  questions <list of clevr-question>
  scene-index <number>
  data-set <string>
  source-path <pathname>
```

```
clevr-question (entity)
  id <symbol> (entity)
  question <string>
  answer
  program <clevr-program>
  index <number>
  scene-index <number>
  template <string>
  question-family-index <number>
```

```
clevr-program (entity tree)
  id <symbol> (entity)
  nodes <list of tree-node> (tree)
  top <tree-node> (tree)
```

```
clevr-function (entity tree-node)
  id <symbol> (entity)
  parent <tree-node> (tree-node)
  children <list of tree-node> (tree-node)
  function-name <string>
  args <list of string>
```
