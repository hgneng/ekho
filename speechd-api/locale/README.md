# How to import symbols dictionaries

* Get gettext version 0.20 or later
* Install the latest version of orca master by hand
* Create a directory named symbolsrc in the locale directory of Speech-Dispatcher source tree.
* Download NVDA sources using this link: (https://github.com/nvaccess/nvda/archive/beta.zip)
* Extract the archive in the newly created directory which will create a nvda-beta folder
* Download the latest unicode CLDR release from this webpage: (http://cldr.unicode.org/index/downloads)
* In the table, chose the latest release which has a date specified and click on the "data" link
* Click on "core.zip and download it, then extract it into a sub-directory of symbolsrc named cldr
* Download UnicodeData.txt: (ftp://ftp.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt) and put it into the symbolsrc directory
* In locale, type:
```
make import-symbols
```
* When some dictionaries are added, add them to the variable nobase_localedata_DATA in the file Makefile.am located in the "locale" directory.
Use the following syntax to list the new file : `lang/file.dic \` with a tab character before.
Please keep the alphabetical order.

