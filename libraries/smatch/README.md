# SMATCH SCORE

This library is a customized version of the Smatch library available on [PyPi](https://pypi.org/project/smatch/).  
Modifications have been made in order to support input consisting of two predicate network meaning representations in the form of strings, since the original library only supports input files containing one or more AMR networks in Penman notation.

More information about the original Smatch library can be found in [README_ORIGINAL.md](README_ORIGINAL.md).

## Usage

To run the smatch library for obtaining the f-score:

```
python smatch.py -m 'parsed meaning network' 'expected meaning network'
```

e.g. ```python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)'```

If you also want to have the precision and recall, then run:

```
python smatch.py -m 'parsed meaning network' 'expected meaning network' --pr
```

e.g. ```python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)' --pr```