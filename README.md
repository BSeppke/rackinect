rackinect
==========

A lightweight extension to the functional programming language Racket (formerly Scheme) which allows to grab images from an attached Microsoft Kinect Sensor (1. generation). It uses OpenKinect's libfreenect and thus the libusb for the grabbing part and stores the results by means of:
- a 3-band vigracket image for the RGB data.
- a 1-band vigracket image for the (registered) depth data (scaled to mm).

1. Prerequisites
-----------------------------------

For Linux and Mac OS X, the libfreenect and the libusb libraries need to be installed. I recommend the use of a current version. The easiest way to do so, is using your favorite package manager under linux or using MacPorts und  Mac OS X. Otherwise you need to pay attention to install all the needed dependencies on your own.

Note, that for Windows, you need to have installed the MS VC-Runtime 11 (2012) in order to get these binaries running. The other libraries are bundled as binaries for Windows.
 
2. Installation
-----------------------------------

The installation of the cameracket-bindings is quite easy. Just unzip the release package, if you have downloaded a release package.

Inside the vigracket-directory you will find a file called "install.rkt". Open this file in DrRacket and execute it once. This will copy the rackinect files to the local collects directory and start the auto-build of the grabbing-C-bindings. This should either build the wrapper library under Linux and Mac OS X or copy the correct binaries for Windows.

If this does not run out-of the box, you may need to change the system's path-variable in order to find the necessary library dependencies.

3. Using the rackinect module
----------------------------------

After successful installation, you can include the package by calling
> (require rackinect)

You should be able to run the examples provided by means of loading the "examples.rkt" file into DrRacket and pressing the "Run" button.