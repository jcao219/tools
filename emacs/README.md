Emacs in CS3110
===============

This is the set of Emacs initialization files for CS 3110. When Emacs
starts up, it will read these files and process your
configurations. Read the comments in each of these files for a better
idea of how they work, and how to add any additional customizations
you may want.

## Installation

On the CS 3110 VM, these files are already installed for you. If you
want these files on your native machine, then follow these
instructions:

### MacOS and Linux

Run the following commands:

```
$ git clone git@github.com:cs3110/tools.git 3110-tools
$ cp -r 3110-tools/emacs $HOME/.emacs.d
```

### Windows

First, make sure you have `git` installed. There are many tutorials on
line for how to do this. After that, make sure that your `HOME`
environment variable is `C:\`. Then run

```
> git clone git@github.com:cs3110/tools.git C:\3110-tools
> xcopy C:\3110-tools\emacs C:\.emacs.d
```

