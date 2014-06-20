/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_cudartwrap.h" /* contains functions to be wrapped */

/* -- cuda_runtime_api.h:cudaMemset3D -- */
cudaError_t  cudaMemset3D(struct cudaPitchedPtr pitchedDevPtr, int value, struct cudaExtent extent)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemset3D",
    cudaError_t , (struct cudaPitchedPtr , int , struct cudaExtent ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pitchedDevPtr, value, extent));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaMallocHost -- */
cudaError_t  cudaMallocHost(void **ptr, size_t size)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMallocHost",
    cudaError_t , (void **, size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (ptr, size));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaFreeHost -- */
cudaError_t  cudaFreeHost(void *ptr)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaFreeHost",
    cudaError_t , (void *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (ptr));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaHostAlloc -- */
cudaError_t  cudaHostAlloc(void **pHost, size_t bytes, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaHostAlloc",
    cudaError_t , (void **, size_t , unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pHost, bytes, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaHostGetDevicePointer -- */
cudaError_t  cudaHostGetDevicePointer(void **pDevice, void *pHost, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaHostGetDevicePointer",
    cudaError_t , (void **, void *, unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pDevice, pHost, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemset -- */

cudaError_t  cudaMemset(void *devPtr, int value, size_t count)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemset",
    cudaError_t , (void *, int , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, value, count));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaMemset2D -- */

cudaError_t  cudaMemset2D(void *devPtr, size_t pitch, int value, size_t width, size_t height)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemset2D",
    cudaError_t , (void *, size_t , int , size_t , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, pitch, value, width, height));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetSymbolAddress -- */

cudaError_t  cudaGetSymbolAddress(void **devPtr, VT_CUDARTWRAP_COMPAT_PTR symbol)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetSymbolAddress",
    cudaError_t , (void **, VT_CUDARTWRAP_COMPAT_PTR),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, symbol));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetSymbolSize -- */

cudaError_t  cudaGetSymbolSize(size_t *size, VT_CUDARTWRAP_COMPAT_PTR symbol)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetSymbolSize",
    cudaError_t , (size_t *, VT_CUDARTWRAP_COMPAT_PTR),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (size, symbol));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetDeviceCount -- */

cudaError_t  cudaGetDeviceCount(int *count)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetDeviceCount",
    cudaError_t , (int *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (count));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetDeviceProperties -- */

cudaError_t  cudaGetDeviceProperties(struct cudaDeviceProp *prop, int device)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetDeviceProperties",
    cudaError_t , (struct cudaDeviceProp *, int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (prop, device));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaChooseDevice -- */

cudaError_t  cudaChooseDevice(int *device, const struct cudaDeviceProp *prop)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaChooseDevice",
    cudaError_t , (int *, const struct cudaDeviceProp *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (device, prop));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaSetDevice -- */

cudaError_t  cudaSetDevice(int device)
{
  cudaError_t  ret = cudaSuccess;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetDevice",
    cudaError_t , (int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (device));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetDevice -- */

cudaError_t  cudaGetDevice(int *device)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetDevice",
    cudaError_t , (int *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (device));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaSetValidDevices -- */

cudaError_t  cudaSetValidDevices(int *device_arr, int len)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetValidDevices",
    cudaError_t , (int *, int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (device_arr, len));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaBindTexture -- */

cudaError_t  cudaBindTexture(size_t *offset, const struct textureReference *texref, const void *devPtr, const struct cudaChannelFormatDesc *desc, size_t size)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaBindTexture",
    cudaError_t , (size_t *, const struct textureReference *, const void *, const struct cudaChannelFormatDesc *, size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (offset, texref, devPtr, desc, size));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaBindTexture2D -- */

cudaError_t  cudaBindTexture2D(size_t *offset, const struct textureReference *texref, const void *devPtr, const struct cudaChannelFormatDesc *desc, size_t width, size_t height, size_t pitch)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaBindTexture2D",
    cudaError_t , (size_t *, const struct textureReference *, const void *, const struct cudaChannelFormatDesc *, size_t , size_t , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (offset, texref, devPtr, desc, width, height, pitch));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaBindTextureToArray -- */

cudaError_t  cudaBindTextureToArray(const struct textureReference *texref, const struct cudaArray *array, const struct cudaChannelFormatDesc *desc)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaBindTextureToArray",
    cudaError_t , (const struct textureReference *, const struct cudaArray *, const struct cudaChannelFormatDesc *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (texref, array, desc));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaUnbindTexture -- */

cudaError_t  cudaUnbindTexture(const struct textureReference *texref)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaUnbindTexture",
    cudaError_t , (const struct textureReference *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (texref));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetTextureAlignmentOffset -- */

cudaError_t  cudaGetTextureAlignmentOffset(size_t *offset, const struct textureReference *texref)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetTextureAlignmentOffset",
    cudaError_t , (size_t *, const struct textureReference *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (offset, texref));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetTextureReference -- */

cudaError_t  cudaGetTextureReference(const struct textureReference **texref, VT_CUDARTWRAP_COMPAT_PTR symbol)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetTextureReference",
    cudaError_t , (const struct textureReference **, VT_CUDARTWRAP_COMPAT_PTR),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (texref, symbol));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetChannelDesc -- */

cudaError_t  cudaGetChannelDesc(struct cudaChannelFormatDesc *desc, const struct cudaArray *array)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetChannelDesc",
    cudaError_t , (struct cudaChannelFormatDesc *, const struct cudaArray *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (desc, array));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaCreateChannelDesc -- */
struct cudaChannelFormatDesc  cudaCreateChannelDesc(int x, int y, int z, int w, enum cudaChannelFormatKind f)
{
  struct cudaChannelFormatDesc  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaCreateChannelDesc",
    struct cudaChannelFormatDesc , (int , int , int , int , enum cudaChannelFormatKind ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (x, y, z, w, f));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetLastError -- */
cudaError_t  cudaGetLastError()
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetLastError",
    cudaError_t , (void),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, ());

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetErrorString -- */
const char * cudaGetErrorString(cudaError_t error)
{
  const char * ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetErrorString",
    const char *, (cudaError_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (error));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaSetupArgument -- */
cudaError_t  cudaSetupArgument(const void *arg, size_t size, size_t offset)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetupArgument",
    cudaError_t , (const void *, size_t , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (arg, size, offset));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaFuncGetAttributes -- */
cudaError_t  cudaFuncGetAttributes(struct cudaFuncAttributes *attr, VT_CUDARTWRAP_COMPAT_PTR func)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaFuncGetAttributes",
    cudaError_t , (struct cudaFuncAttributes *, VT_CUDARTWRAP_COMPAT_PTR),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (attr, func));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaStreamCreate -- */
cudaError_t  cudaStreamCreate(cudaStream_t *pStream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaStreamCreate",
    cudaError_t , (cudaStream_t *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pStream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaStreamSynchronize -- */
cudaError_t  cudaStreamSynchronize(cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaStreamSynchronize",
    cudaError_t , (cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaStreamQuery -- */
cudaError_t  cudaStreamQuery(cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaStreamQuery",
    cudaError_t , (cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventCreate -- */
cudaError_t  cudaEventCreate(cudaEvent_t *event)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventCreate",
    cudaError_t , (cudaEvent_t *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventRecord -- */
cudaError_t  cudaEventRecord(cudaEvent_t event, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventRecord",
    cudaError_t , (cudaEvent_t , cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventQuery -- */
cudaError_t  cudaEventQuery(cudaEvent_t event)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventQuery",
    cudaError_t , (cudaEvent_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventSynchronize -- */
cudaError_t  cudaEventSynchronize(cudaEvent_t event)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventSynchronize",
    cudaError_t , (cudaEvent_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventDestroy -- */
cudaError_t  cudaEventDestroy(cudaEvent_t event)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventDestroy",
    cudaError_t , (cudaEvent_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventElapsedTime -- */
cudaError_t  cudaEventElapsedTime(float *ms, cudaEvent_t start, cudaEvent_t end)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventElapsedTime",
    cudaError_t , (float *, cudaEvent_t , cudaEvent_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (ms, start, end));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaSetDoubleForDevice -- */
cudaError_t  cudaSetDoubleForDevice(double *d)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetDoubleForDevice",
    cudaError_t , (double *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (d));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaSetDoubleForHost -- */
cudaError_t  cudaSetDoubleForHost(double *d)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetDoubleForHost",
    cudaError_t , (double *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (d));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDriverGetVersion -- */
cudaError_t  cudaDriverGetVersion(int *driverVersion)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDriverGetVersion",
    cudaError_t , (int *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (driverVersion));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaRuntimeGetVersion -- */
cudaError_t  cudaRuntimeGetVersion(int *runtimeVersion)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaRuntimeGetVersion",
    cudaError_t , (int *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (runtimeVersion));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* CUDA 2.3 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 2030))

/* -- cuda_runtime_api.h:cudaHostGetFlags(unsigned int *pFlags, void *pHost) -- */
cudaError_t  cudaHostGetFlags(unsigned int *pFlags, void *pHost)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaHostGetFlags",
      cudaError_t, (unsigned int *, void *),
      NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pFlags, pHost));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif

/* CUDA 3.0 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 3000))

/* -- cuda_runtime_api.h:cudaMemGetInfo -- */
cudaError_t  cudaMemGetInfo(size_t *free, size_t *total)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemGetInfo",
    cudaError_t , (size_t *, size_t *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (free, total));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaFuncSetCacheConfig -- */
cudaError_t  cudaFuncSetCacheConfig(VT_CUDARTWRAP_COMPAT_PTR func, enum cudaFuncCache cacheConfig)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaFuncSetCacheConfig",
    cudaError_t , (VT_CUDARTWRAP_COMPAT_PTR, enum cudaFuncCache ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (func, cacheConfig));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsUnregisterResource -- */
cudaError_t  cudaGraphicsUnregisterResource(struct cudaGraphicsResource *resource)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsUnregisterResource",
    cudaError_t , (struct cudaGraphicsResource *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (resource));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsResourceSetMapFlags -- */
cudaError_t  cudaGraphicsResourceSetMapFlags(struct cudaGraphicsResource *resource, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsResourceSetMapFlags",
    cudaError_t , (struct cudaGraphicsResource *, unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (resource, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsMapResources -- */
cudaError_t  cudaGraphicsMapResources(int count, struct cudaGraphicsResource **resources, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsMapResources",
    cudaError_t , (int , struct cudaGraphicsResource **, cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (count, resources, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsUnmapResources -- */
cudaError_t  cudaGraphicsUnmapResources(int count, struct cudaGraphicsResource **resources, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsUnmapResources",
    cudaError_t , (int , struct cudaGraphicsResource **, cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (count, resources, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsResourceGetMappedPointer -- */
cudaError_t  cudaGraphicsResourceGetMappedPointer(void **devPtr, size_t *size, struct cudaGraphicsResource *resource)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsResourceGetMappedPointer",
    cudaError_t , (void **, size_t *, struct cudaGraphicsResource *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, size, resource));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsSubResourceGetMappedArray -- */
cudaError_t  cudaGraphicsSubResourceGetMappedArray(struct cudaArray **arrayPtr, struct cudaGraphicsResource *resource, unsigned int arrayIndex, unsigned int mipLevel)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsSubResourceGetMappedArray",
    cudaError_t , (struct cudaArray **, struct cudaGraphicsResource *, unsigned int , unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (arrayPtr, resource, arrayIndex, mipLevel));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif

/* CUDA 3.1 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 3010))

/* -- cuda_runtime_api.h:cudaBindSurfaceToArray -- */
cudaError_t  cudaBindSurfaceToArray(const struct surfaceReference *surfref, const struct cudaArray *array, const struct cudaChannelFormatDesc *desc)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaBindSurfaceToArray",
    cudaError_t , (const struct surfaceReference *, const struct cudaArray *, const struct cudaChannelFormatDesc *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (surfref, array, desc));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* CUDA 3.2 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION < 3020))

/* -- cuda_runtime_api.h:cudaGetSurfaceAlignmentOffset -- */
cudaError_t  cudaGetSurfaceAlignmentOffset(size_t *offset, const struct surfaceReference *surfref)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetSurfaceAlignmentOffset",
    cudaError_t , (size_t *, const struct surfaceReference *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (offset, surfref));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif

/* -- cuda_runtime_api.h:cudaGetSurfaceReference -- */
cudaError_t  cudaGetSurfaceReference(const struct surfaceReference **surfref, VT_CUDARTWRAP_COMPAT_PTR symbol)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetSurfaceReference",
    cudaError_t , (const struct surfaceReference **, VT_CUDARTWRAP_COMPAT_PTR),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (surfref, symbol));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaPeekAtLastError -- */
cudaError_t  cudaPeekAtLastError()
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaPeekAtLastError",
    cudaError_t , (void),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, ());

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaThreadSetLimit -- */
cudaError_t  cudaThreadSetLimit(enum cudaLimit limit, size_t value)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaThreadSetLimit",
    cudaError_t , (enum cudaLimit , size_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (limit, value));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaThreadGetLimit -- */
cudaError_t  cudaThreadGetLimit(size_t *pValue, enum cudaLimit limit)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaThreadGetLimit",
    cudaError_t , (size_t *, enum cudaLimit ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pValue, limit));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetExportTable -- */
cudaError_t cudaGetExportTable(const void **ppExportTable, const cudaUUID_t *pExportTableId)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetExportTable",
    cudaError_t , (const void **, const cudaUUID_t *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (ppExportTable, pExportTableId));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif /* CUDA 3.1 */


/*
 *  Adaptions for CUDA 3.2
 */

#if (defined(CUDART_VERSION) && (CUDART_VERSION < 3020))
/* -- cuda_runtime_api.h:cudaSetDeviceFlags -- */
cudaError_t  cudaSetDeviceFlags(int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetDeviceFlags",
    cudaError_t , (int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventCreateWithFlags -- */
cudaError_t  cudaEventCreateWithFlags(cudaEvent_t *event, int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventCreateWithFlags",
    cudaError_t , (cudaEvent_t *, int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#else

/* -- cuda_runtime_api.h:cudaSetDeviceFlags -- */
cudaError_t  cudaSetDeviceFlags(unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaSetDeviceFlags",
    cudaError_t , (int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaEventCreateWithFlags -- */
cudaError_t  cudaEventCreateWithFlags(cudaEvent_t *event, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaEventCreateWithFlags",
    cudaError_t , (cudaEvent_t *, int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (event, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cudaMemsetAsync -- */
cudaError_t  cudaMemsetAsync(void *devPtr, int value, size_t count, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemsetAsync",
    cudaError_t , (void *, int , size_t , cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, value, count, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cudaMemset2DAsync -- */
cudaError_t  cudaMemset2DAsync(void *devPtr, size_t pitch, int value, size_t width, size_t height, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemset2DAsync",
    cudaError_t , (void *, size_t , int , size_t , size_t , cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (devPtr, pitch, value, width, height, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cudaMemset3DAsync -- */
cudaError_t  cudaMemset3DAsync(struct cudaPitchedPtr pitchedDevPtr, int value, struct cudaExtent extent, cudaStream_t stream)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaMemset3DAsync",
    cudaError_t , (struct cudaPitchedPtr , int , struct cudaExtent , cudaStream_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pitchedDevPtr, value, extent, stream));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaStreamWaitEvent -- */
cudaError_t  cudaStreamWaitEvent(cudaStream_t stream, cudaEvent_t event, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaStreamWaitEvent",
    cudaError_t , (cudaStream_t , cudaEvent_t , unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (stream, event, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cudaThreadGetCacheConfig -- */
cudaError_t  cudaThreadGetCacheConfig(enum cudaFuncCache *pCacheConfig)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaThreadGetCacheConfig",
    cudaError_t , (enum cudaFuncCache *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pCacheConfig));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cudaThreadSetCacheConfig -- */
cudaError_t  cudaThreadSetCacheConfig(enum cudaFuncCache cacheConfig)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaThreadSetCacheConfig",
    cudaError_t , (enum cudaFuncCache ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (cacheConfig));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif /* CUDA 3.2 */


/*
 *  Adaptions for CUDA 4.0
 */

#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4000))

/* -- cuda_runtime_api.h:cudaDeviceSetLimit -- */
cudaError_t  cudaDeviceSetLimit(enum cudaLimit limit, size_t value)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceSetLimit",
    cudaError_t , (enum cudaLimit , size_t ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (limit, value));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceGetLimit -- */
cudaError_t  cudaDeviceGetLimit(size_t *pValue, enum cudaLimit limit)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceGetLimit",
    cudaError_t , (size_t *, enum cudaLimit ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pValue, limit));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceGetCacheConfig -- */
cudaError_t  cudaDeviceGetCacheConfig(enum cudaFuncCache *pCacheConfig)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceGetCacheConfig",
    cudaError_t , (enum cudaFuncCache *), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pCacheConfig));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceSetCacheConfig -- */
cudaError_t  cudaDeviceSetCacheConfig(enum cudaFuncCache cacheConfig)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceSetCacheConfig",
    cudaError_t , (enum cudaFuncCache ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (cacheConfig));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaHostRegister -- */
cudaError_t  cudaHostRegister(void *ptr, size_t size, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaHostRegister",
    cudaError_t , (void *, size_t , unsigned int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (ptr, size, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaHostUnregister -- */
cudaError_t  cudaHostUnregister(void *ptr)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaHostUnregister",
    cudaError_t , (void *), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (ptr));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaPointerGetAttributes -- */
#if (defined(CUDART_VERSION) && (CUDART_VERSION < 4010))
cudaError_t  cudaPointerGetAttributes(struct cudaPointerAttributes *attributes, void *ptr)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaPointerGetAttributes",
    cudaError_t , (struct cudaPointerAttributes *, void *), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (attributes, ptr));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}
#else
cudaError_t  cudaPointerGetAttributes(struct cudaPointerAttributes *attributes, const void *ptr)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaPointerGetAttributes",
    cudaError_t , (struct cudaPointerAttributes *, const void *), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (attributes, ptr));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}
#endif

/* -- cuda_runtime_api.h:cudaDeviceCanAccessPeer -- */
cudaError_t  cudaDeviceCanAccessPeer(int *canAccessPeer, int device, int peerDevice)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceCanAccessPeer",
    cudaError_t , (int *, int , int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (canAccessPeer, device, peerDevice));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceEnablePeerAccess -- */
cudaError_t  cudaDeviceEnablePeerAccess(int peerDevice, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceEnablePeerAccess",
    cudaError_t , (int , unsigned int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (peerDevice, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceDisablePeerAccess -- */
cudaError_t  cudaDeviceDisablePeerAccess(int peerDevice)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceDisablePeerAccess",
    cudaError_t , (int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (peerDevice));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaPeerRegister -- 
cudaError_t  cudaPeerRegister(void *peerDevicePointer, int peerDevice, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaPeerRegister",
    cudaError_t , (void *, int , unsigned int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (peerDevicePointer, peerDevice, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}*/

/* -- cuda_runtime_api.h:cudaPeerUnregister -- 
cudaError_t  cudaPeerUnregister(void *peerDevicePointer, int peerDevice)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaPeerUnregister",
    cudaError_t , (void *, int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (peerDevicePointer, peerDevice));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}*/

/* -- cuda_runtime_api.h:cudaPeerGetDevicePointer -- 
cudaError_t  cudaPeerGetDevicePointer(void **pDevice, void *peerDevicePointer, int peerDevice, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaPeerGetDevicePointer",
    cudaError_t , (void **, void *, int , unsigned int ), NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pDevice, peerDevicePointer, peerDevice, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}*/

#endif /* CUDA 4.0 */

/*
 *  Adaptations for CUDA 4.1
 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4010))

/* -- cuda_runtime_api.h:cudaArrayGetInfo -- */
cudaError_t  cudaArrayGetInfo(struct cudaChannelFormatDesc *desc, struct cudaExtent *extent, unsigned int *flags, struct cudaArray *array)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaArrayGetInfo",
    cudaError_t , (struct cudaChannelFormatDesc *, struct cudaExtent *, unsigned int *, struct cudaArray *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (desc, extent, flags, array));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif /* CUDA 4.1 */

/*
 *  Adaptations for CUDA 4.2
 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 4020))

/* -- cuda_runtime_api.h:cudaDeviceGetSharedMemConfig -- */
cudaError_t  cudaDeviceGetSharedMemConfig(enum cudaSharedMemConfig *pConfig)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceGetSharedMemConfig",
    cudaError_t , (enum cudaSharedMemConfig *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pConfig));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDeviceSetSharedMemConfig -- */
cudaError_t  cudaDeviceSetSharedMemConfig(enum cudaSharedMemConfig config)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDeviceSetSharedMemConfig",
    cudaError_t , (enum cudaSharedMemConfig ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (config));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaFuncSetSharedMemConfig -- */
cudaError_t  cudaFuncSetSharedMemConfig(VT_CUDARTWRAP_COMPAT_PTR func, enum cudaSharedMemConfig config)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaFuncSetSharedMemConfig",
    cudaError_t , (VT_CUDARTWRAP_COMPAT_PTR, enum cudaSharedMemConfig ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (func, config));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif

/*
 *  Adaptations for CUDA 5.0.7
 */
#if (defined(CUDART_VERSION) && (CUDART_VERSION >= 5000))

/* -- cuda_runtime_api.h:cudaStreamCreateWithFlags -- */
cudaError_t  cudaStreamCreateWithFlags(cudaStream_t *pStream, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaStreamCreateWithFlags",
    cudaError_t , (cudaStream_t *, unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pStream, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaStreamAddCallback -- */
cudaError_t  cudaStreamAddCallback(cudaStream_t stream, cudaStreamCallback_t callback, void *userData, unsigned int flags)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaStreamAddCallback",
    cudaError_t , (cudaStream_t , cudaStreamCallback_t , void *, unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (stream, callback, userData, flags));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetMipmappedArrayLevel -- */
cudaError_t  cudaGetMipmappedArrayLevel(cudaArray_t *levelArray, cudaMipmappedArray_const_t mipmappedArray, unsigned int level)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetMipmappedArrayLevel",
    cudaError_t , (cudaArray_t *, cudaMipmappedArray_const_t , unsigned int ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (levelArray, mipmappedArray, level));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGraphicsResourceGetMappedMipmappedArray -- */
cudaError_t  cudaGraphicsResourceGetMappedMipmappedArray(cudaMipmappedArray_t *mipmappedArray, cudaGraphicsResource_t resource)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGraphicsResourceGetMappedMipmappedArray",
    cudaError_t , (cudaMipmappedArray_t *, cudaGraphicsResource_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (mipmappedArray, resource));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaCreateTextureObject -- */
cudaError_t  cudaCreateTextureObject(cudaTextureObject_t *pTexObject, const struct cudaResourceDesc *pResDesc, const struct cudaTextureDesc *pTexDesc, const struct cudaResourceViewDesc *pResViewDesc)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaCreateTextureObject",
    cudaError_t , (cudaTextureObject_t *, const struct cudaResourceDesc *, const struct cudaTextureDesc *, const struct cudaResourceViewDesc *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pTexObject, pResDesc, pTexDesc, pResViewDesc));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDestroyTextureObject -- */
cudaError_t  cudaDestroyTextureObject(cudaTextureObject_t texObject)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDestroyTextureObject",
    cudaError_t , (cudaTextureObject_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (texObject));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetTextureObjectResourceDesc -- */
cudaError_t  cudaGetTextureObjectResourceDesc(struct cudaResourceDesc *pResDesc, cudaTextureObject_t texObject)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetTextureObjectResourceDesc",
    cudaError_t , (struct cudaResourceDesc *, cudaTextureObject_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pResDesc, texObject));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetTextureObjectTextureDesc -- */
cudaError_t  cudaGetTextureObjectTextureDesc(struct cudaTextureDesc *pTexDesc, cudaTextureObject_t texObject)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetTextureObjectTextureDesc",
    cudaError_t , (struct cudaTextureDesc *, cudaTextureObject_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pTexDesc, texObject));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetTextureObjectResourceViewDesc -- */
cudaError_t  cudaGetTextureObjectResourceViewDesc(struct cudaResourceViewDesc *pResViewDesc, cudaTextureObject_t texObject)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetTextureObjectResourceViewDesc",
    cudaError_t , (struct cudaResourceViewDesc *, cudaTextureObject_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pResViewDesc, texObject));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaCreateSurfaceObject -- */
cudaError_t  cudaCreateSurfaceObject(cudaSurfaceObject_t *pSurfObject, const struct cudaResourceDesc *pResDesc)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaCreateSurfaceObject",
    cudaError_t , (cudaSurfaceObject_t *, const struct cudaResourceDesc *),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pSurfObject, pResDesc));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaDestroySurfaceObject -- */
cudaError_t  cudaDestroySurfaceObject(cudaSurfaceObject_t surfObject)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaDestroySurfaceObject",
    cudaError_t , (cudaSurfaceObject_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (surfObject));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

/* -- cuda_runtime_api.h:cudaGetSurfaceObjectResourceDesc -- */
cudaError_t  cudaGetSurfaceObjectResourceDesc(struct cudaResourceDesc *pResDesc, cudaSurfaceObject_t surfObject)
{
  cudaError_t  ret;

  CUDARTWRAP_FUNC_INIT(vt_cudart_lw, vt_cudart_lw_attr, "cudaGetSurfaceObjectResourceDesc",
    cudaError_t , (struct cudaResourceDesc *, cudaSurfaceObject_t ),
    NULL, 0);

  CUDARTWRAP_FUNC_START(vt_cudart_lw);

  ret = VT_LIBWRAP_FUNC_CALL(vt_cudart_lw, (pResDesc, surfObject));

  CUDARTWRAP_FUNC_END(vt_cudart_lw);

  return ret;
}

#endif
