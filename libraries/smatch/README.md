To run the smatch score to obtain the f-score:

```
python smatch.py -m 'parsed meaning network' 'expected meaning network'
```

e.g. ```python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)'```

If you also want to have the precision and recall, then run:

```
python smatch.py -m 'parsed meaning network' 'expected meaning network' --pr
```

e.g. ```python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)' --pr```

More information about the original Python SMATCH library can be found in [README_ORIGINAL.md](README_ORIGINAL.md)
