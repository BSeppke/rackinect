cmake_minimum_required(VERSION 3.1)

project(kinect-grab_c)

#find . -type f -name \*.cxx | sed 's,^\./,,'
set(SOURCES 
	kinect-grab_c.cxx)

#find . -type f -name \*.hxx | sed 's,^\./,,'
set(HEADERS  
	kinect-grab_c.hxx
	os_settings.hxx)

# Tell CMake to create the library
add_library(kinect-grab_c SHARED ${SOURCES} ${HEADERS})
target_link_libraries(kinect-grab_c ${FREENECT_LIBRARIES})