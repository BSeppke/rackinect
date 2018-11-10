#include "os_settings.hxx"

/**
 * @file
 * @brief Header file for the libfreenect-wrapper
 *
 * @defgroup grabbing Grab from the Kinect using libfreenect
 * @{
 *    @brief Collection of all grabbing algorithms from the Kinect.
 */

/**
 * Stops the freenect_sync acquisition loop.
 *
 * \return 0, if the loop could be cancelled.
 */
LIBEXPORT int kinect_stop_c();

/**
 * Gets a kinect video frame from a device by means of three
 * different images: Red, Green and Blue.
 *
 * \param[out] arr_r flat float array of size 640*480 for the red image.
 * \param[out] arr_g flat float array of size 640*480 for the green image.
 * \param[out] arr_b flat float array of size 640*480 for the blue image.
 * \param width Width of the image. Has to be 640.
 * \param height Height of the image. Has to be 480.
 * \param device_id The id for the device, from which we want to acquire the image.
 *
 * \return 0 on success, 1 if sizes do not match, 2 if the acquisiton failed.
 */
LIBEXPORT int kinect_grab_get_video_c(float *arr_r, float *arr_g, float *arr_b, int width, int height, int device_id);

/**
 * Gets a kinect depth frame from a device by means of one depth image.
 *
 * \param[out] arr_d flat float array of size 640*480 for the depth image.
 * \param width Width of the image. Has to be 640.
 * \param height Height of the image. Has to be 480.
 * \param device_id The id for the device, from which we want to acquire the image.
 *
 * \return 0 on success, 1 if sizes do not match, 2 if the acquisiton failed.
 */
LIBEXPORT int kinect_grab_get_depth_c(float *arr_d, int width, int height, int device_id);

/**
 * Gets a kinect video and depth frame from a device by means of four
 * different images: Depth, Red, Green and Blue.
 *
 * \param[out] arr_d flat float array of size 640*480 for the depth image.
 * \param[out] arr_r flat float array of size 640*480 for the red image.
 * \param[out] arr_g flat float array of size 640*480 for the green image.
 * \param[out] arr_b flat float array of size 640*480 for the blue image.
 * \param width Width of the image. Has to be 640.
 * \param height Height of the image. Has to be 480.
 * \param device_id The id for the device, from which we want to acquire the image.
 *
 * \return 0 on success, 1 if sizes do not match, 2 if the acquisiton failed.
 */
LIBEXPORT int kinect_grab_get_depth_and_video_c(float *arr_d, float *arr_r, float *arr_g, float *arr_b, int width, int height, int device_id);

/**
 * Gets a kinect video frame contents for a certain depth range only
 * by means of three different images: Red, Green and Blue.
 *
 * \param[out] arr_r flat float array of size 640*480 for the red image.
 * \param[out] arr_g flat float array of size 640*480 for the green image.
 * \param[out] arr_b flat float array of size 640*480 for the blue image.
 * \param width Width of the image. Has to be 640.
 * \param height Height of the image. Has to be 480.
 * \param front The frontal distance.
 * \param back The back distance.
 * \param device_id The id for the device, from which we want to acquire the image.
 *
 * \return 0 on success, 1 if sizes do not match, 2 if the acquisiton failed.
 */
LIBEXPORT int kinect_grab_get_video_for_depth_interval_c(float *arr_r, float *arr_g, float *arr_b, int width, int height, int front, int back, int device_id);

/**
 * Gets the first response of the depth sensor in a given depth range only
 * by means of a two dimensional position (in image coordinates)..
 *
 * \param[out] pos_x The found x-position.
 * \param[out] pos_y The found y-position.
 * \param front The frontal distance.
 * \param back The back distance.
 * \param device_id The id for the device, from which we want to acquire the image.
 *
 * \return 0 on success, 1 if sizes do not match, 2 if the acquisiton failed.
 */
LIBEXPORT int kinect_grab_get_topleft_c(int* pos_x, int* pos_y, int front, int back, int device_id);

/**
 * @}
 */
