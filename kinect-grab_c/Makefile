MACOSX_CXXFLAGS_32 = -I/src -I/opt/local/include -arch i386 -O3
MACOSX_LDFLAGS_32 = -L/opt/local/lib -lfreenect_sync -bundle  -arch i386 -undefined dynamic_lookup

MACOSX_CXXFLAGS_64 = -I/src -I/opt/local/include -arch x86_64 -O3
MACOSX_LDFLAGS_64 = -L/opt/local/lib -lfreenect_sync -bundle  -arch x86_64 -undefined dynamic_lookup

UNIX_CXXFLAGS_32 = -I/src -I/usr/local/include -m32 -O3 -fPIC
UNIX_LDFLAGS_32 = -L/usr/local/lib -lfreenect_sync -m32 -fPIC 

UNIX_CXXFLAGS_64 = -I/src -I/usr/local/include -m64 -O3 -fPIC
UNIX_LDFLAGS_64 = -L/usr/local/lib -lfreenect_sync -m64 -fPIC 
CC = gcc

macosx32:	src/kinect-grab_c.hxx
	cd bin && $(CC) $(MACOSX_CXXFLAGS_32) -c ../src/kinect-grab_c.cxx
	$(CC) $(MACOSX_LDFLAGS_32) bin/kinect-grab_c.o -o bin/libkinect-grab_c.dylib
    
macosx64:	src/kinect-grab_c.hxx
	cd bin && $(CC) $(MACOSX_CXXFLAGS_64) -c ../src/kinect-grab_c.cxx
	$(CC) $(MACOSX_LDFLAGS_64) bin/kinect-grab_c.o -o bin/libkinect-grab_c.dylib

unix32:	src/kinect-grab_c.hxx
	cd bin && $(CC) $(LINUX_CXXFLAGS_32) -c ../src/kinect-grab_c.cxx
	$(CC) bin/kinect-grab_c.o $(LINUX_LDFLAGS_32) -o bin/libkinect-grab_c.so
    
unix64:	src/kinect-grab_c.hxx
	cd bin && $(CC) $(LINUX_CXXFLAGS_64) -c ../src/kinect-grab_c.cxx 
	$(CC) bin/kinect-grab_c.o $(LINUX_LDFLAGS_64) -o bin/libkinect-grab_c.so

clean:
	rm bin/*.*

