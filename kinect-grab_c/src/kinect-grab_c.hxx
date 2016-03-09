#include "libfreenect_sync.h"
#include "os_settings.hxx"


LIBEXPORT int kinect_grab_get_video_c(float *arr_r, float *arr_g, float *arr_b, int width, int height, int device_id)
{
    
	int cam_width =   640;
	int cam_height =  480;
    
	// read image given as first argument
	// file type is determined automatically
	if( cam_width != width || cam_height != height)
    {
        return 1;
    }
    
    char * data;
	unsigned int timestamp;
    
    int res = freenect_sync_get_video((void**)(&data), &timestamp, device_id, FREENECT_VIDEO_RGB);
    
    if(res)
    {
        return 2;
    }
    
    float* ptr_arr_r = arr_r;
    float* ptr_arr_g = arr_g;
    float* ptr_arr_b = arr_b;
    
    for(unsigned int i=0; i<(unsigned int)width*height; ++i)
	{
    	*ptr_arr_r = (float)(*data); ptr_arr_r++; data++;
        *ptr_arr_g = (float)(*data); ptr_arr_g++; data++;
		*ptr_arr_b = (float)(*data); ptr_arr_b++; data++;
    }
	return 0;
}

LIBEXPORT int kinect_grab_get_depth_c(float *arr_d, int width, int height, int device_id)
{
    
	int cam_width =   640;
	int cam_height =  480;
    
	// read image given as first argument
	// file type is determined automatically
	if( cam_width != width || cam_height != height)
    {
        return 1;
    }
    
    uint16_t *data;
	unsigned int timestamp;
    
    int res = freenect_sync_get_depth((void**)(&data), &timestamp, device_id, FREENECT_DEPTH_REGISTERED);
    
    if(res)
    {
        return 2;
    }
    
    float* ptr_arr_d = arr_d;
    
    for(unsigned int i=0; i<(unsigned int)width*height; ++i)
	{
    	*ptr_arr_d = (float)(*data); data++;
        ptr_arr_d++;
    }
	return 0;
}

LIBEXPORT int kinect_grab_get_depth_and_video_c(float *arr_d, float *arr_r, float *arr_g, float *arr_b, int width, int height, int device_id)
{
    
	int cam_width =   640;
	int cam_height =  480;
    
	// read image given as first argument
	// file type is determined automatically
	if( cam_width != width || cam_height != height)
    {
        return 1;
    }
    
    char * data;
	unsigned int timestamp;
    
    int res = freenect_sync_get_video((void**)(&data), &timestamp, device_id, FREENECT_VIDEO_RGB);
    
    if(res)
    {
        return 2;
    }
    
    uint16_t * data_d;
    int res_d = freenect_sync_get_depth((void**)(&data_d), &timestamp, device_id, FREENECT_DEPTH_REGISTERED);
    
    if(res_d)
    {
        return 2;
    }
    
    float* ptr_arr_d = arr_d;
    float* ptr_arr_r = arr_r;
    float* ptr_arr_g = arr_g;
    float* ptr_arr_b = arr_b;
    
    for(unsigned int i=0; i<(unsigned int)width*height; ++i)
	{
        *ptr_arr_d = (float)(*data_d); ptr_arr_d++; data_d++;
        *ptr_arr_r = (float)(*data); ptr_arr_r++; data++;
        *ptr_arr_g = (float)(*data); ptr_arr_g++; data++;
        *ptr_arr_b = (float)(*data); ptr_arr_b++; data++;
    }
	return 0;
}
LIBEXPORT int kinect_grab_get_video_for_depth_interval_c(float *arr_r, float *arr_g, float *arr_b, int width, int height, int front, int back, int device_id)
{
    
	int cam_width =   640;
	int cam_height =  480;
    
	// read image given as first argument
	// file type is determined automatically
	if( cam_width != width || cam_height != height)
    {
        return 1;
    }
    
    char * data;
	unsigned int timestamp;
    
    int res = freenect_sync_get_video((void**)(&data), &timestamp, device_id, FREENECT_VIDEO_RGB);
    
    if(res)
    {
        return 2;
    }
    
    uint16_t * data_d;
    int res_d = freenect_sync_get_depth((void**)(&data_d), &timestamp, device_id, FREENECT_DEPTH_REGISTERED);
    
    if(res_d)
    {
        return 2;
    }
    
    float* ptr_arr_r = arr_r;
    float* ptr_arr_g = arr_g;
    float* ptr_arr_b = arr_b;
    
    for(unsigned int i=0; i<(unsigned int)width*height; ++i)
	{
        if(*data_d >= front && *data_d <= back)
        {
            *ptr_arr_r = (float)(*data); ptr_arr_r++; data++;
            *ptr_arr_g = (float)(*data); ptr_arr_g++; data++;
            *ptr_arr_b = (float)(*data); ptr_arr_b++; data++;
        }
        else
        {
            *ptr_arr_r = 0; ptr_arr_r++; data++;
            *ptr_arr_g = 0; ptr_arr_g++; data++;
            *ptr_arr_b = 0; ptr_arr_b++; data++;
        }
        data_d++;
    }
	return 0;
}

LIBEXPORT int kinect_grab_get_topleft_c(int* pos_x, int* pos_y, int front, int back, int device_id)
{
    
	int width =   640;
	int height =  480;
    
    *pos_x = *pos_y = 0;
    
    uint16_t *data;
	unsigned int timestamp;
    
    int res = freenect_sync_get_depth((void**)(&data), &timestamp, device_id, FREENECT_DEPTH_REGISTERED);
    
    if(res)
    {
        return 1;
    }
    
    uint16_t ui16_front = (uint16_t)front;
    uint16_t ui16_back  = (uint16_t)back;

    for(unsigned int y=0; y<(unsigned int)height; ++y)
	{
        for(unsigned int x=0; x<(unsigned int)width; ++x)
        {
            if (*data >= ui16_front && *data <= ui16_back)
            {
                *pos_x = x;
                *pos_y = y;
                
                return 0;
            }
            data++;
        }
    }
	return 0;
}
