# SMATCH SCORE

This library is a customized version of the Smatch library available on [PyPi](https://pypi.org/project/smatch/).  
Modifications have been made in order to support input consisting of two predicate network meaning representations in the form of strings or two text files that each contain a predicate network meaning representation or even a combination of one string and one file. These modifications were needed because the original library only supports input files containing one or more AMRs in Penman notation.

More information about the original Smatch library can be found in [README_ORIGINAL.md](README_ORIGINAL.md).

## Usage

To run the smatch library for obtaining the f-score:

```
python smatch.py -m 'parsed meaning network' 'expected meaning network'
```

e.g. ```python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)'```  
or ```python smatch.py -m 'path_to_file/file_parsed_network.txt' 'path_to_file/file_expected_network.txt'```

If you also want to have the precision and recall, then run:

```
python smatch.py -m 'parsed meaning network' 'expected meaning network' --pr
```

e.g. ```python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)' --pr```  
or ```python smatch.py -m 'path_to_file/file_parsed_network.txt' 'path_to_file/file_expected_network.txt' --pr```