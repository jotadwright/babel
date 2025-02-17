# Sign Language Processing
This Babel package provides all the necessary functionalities for representing and processing signed languages.

## Settings and installations

### Installing a modified version of menlo
It is highly advised to install a modified version of the menlo font with supplementary characters for HamNoSys characters. The font can be downloaded <a href="https://gitlab.unamur.be/beehaif/GeoQuery-LSFB/-/blob/master/HamNoSys/HamNoSys.ttf?ref_type=heads">here</a>. For more information on how to install new fonts, please consult the official guides for <a href="https://support.apple.com/en-gb/guide/font-book/fntbk1000/mac">mac</a> and <a href="https://support.microsoft.com/en-us/office/add-a-font-b7c5f17c-4426-4b53-967f-455339c564c1">windows</a>. Afterwards, set the font of your lisp editor to the modified menlo font. In Lispworks, you can do this through the ```preferences panel``` in the ```environment``` section. Go to the styles tab and and check ```override system default font```. Then, you can select the menlo-hamnosys font from the dropdown menu.

### Set file encodings to UTF-8
To be able to use the HamNoSys symbols in your files, be sure to set the file encodings of your files to UTF-8. In Lispworks, you can do this through the preferences panel in the ```environment```section. Go to the ```file encodings``` tab and ensure both input and output encoding are set to UTF-8.

## Contents

* <a href="start.lisp">start.lisp</a>: a file to get started using the package. It introduces all the different tools using examples. Most of the examples use data from the <a href="https://gitlab.unamur.be/beehaif/GeoQuery-LSFB">GeoQuery-LSFB corpus</a>. To be able to run the examples, download the root corpus folder and place it inside your ```babel-corpora``` folder (or whichever folder \*babel-corpora\* refers to in the configurations of your lisp interpreter).

* <a href="elan-to-predicates/">elan-to-predicates</a>: a module for transforming elan xml structures to the predicate notation used to represent signed expressions in FCG. Any new elan notation files should be created using the provided template (<a href="elan-to-predicates/elan-annotation-template.etf">elan-annotation-template.etf</a>), without changing the names of any of the existing tiers. Additional tiers can be added depending on your needs.


* <a href="make-fingerspelled-forms.lisp">make-fingerspelled-forms.lisp</a>: contains the fingerspelling alphabet for French Belgian Sign Language (LSFB) in HamNoSys symbols, and a function to transform any string of roman characters (single words, no spaces) into the hamnosys fingerspelled form.

* <a href="derender.lisp">derender.lisp</a>: contains the derender mode for derendering a set of predicates into an initial transient structure in FCG.

* <a href="visualization/">visualization module</a>: contains tools for visualising signed forms in the web-interface.