open Js

(** 5.1 Types *)
type clboolean = bool
type clint = int
type cllong = int
type cluint = int
type clenum
type void
type errorCode
type memflags
type contextInfo
type deviceInfo
type deviceType

class type webCLMemoryObject = object
  method release : unit meth
end

class type webCLSampler = object

end

class type webCLDevice = object
  method getSupportedExtensions : js_string js_array t meth
  method enableExtension : js_string -> clboolean meth
  method getInfo_DEVICENAME : deviceInfo -> js_string t meth
end

class type webCLBuffer = object inherit webCLMemoryObject
  method createSubBuffer : memflags -> cluint -> cluint -> webCLBuffer t meth
end

class type webCLKernel = object
  method setArg_fromBuffer : cluint -> webCLBuffer t -> unit meth
  method setArg_fromWebCLMemoryObject : cluint -> webCLBuffer t-> unit meth
  method setArg_fromWebCLSampler : cluint -> webCLBuffer t-> unit meth
  method setArg_fromFloatArray : cluint -> Typed_array.float32Array t-> unit meth
  method setArg_fromIntArray : cluint ->  Typed_array.int32Array t-> unit meth
end

class type webCLProgram = object
  method release : unit meth
  method createKernel : js_string t -> webCLKernel t meth
  method build : unit meth
  method build_fromDevice : webCLDevice t js_array t -> unit meth
end

class type webCLCommandQueue = object
  method enqueueWriteBuffer_float32array : webCLBuffer t -> clboolean -> cluint -> cluint -> Typed_array.float32Array t -> unit meth
  method enqueueWriteBuffer_int32array : webCLBuffer t -> clboolean -> cluint -> cluint -> Typed_array.int32Array t -> unit meth
  method enqueueNDRangeKernel : webCLKernel t -> cluint -> cluint js_array t opt -> cluint js_array t -> cluint js_array t -> unit meth
  method enqueueReadBuffer_float32array : webCLBuffer t -> clboolean -> cluint -> cluint -> Typed_array.float32Array t -> unit meth
  method enqueueReadBuffer_int32array : webCLBuffer t -> clboolean -> cluint -> cluint -> Typed_array.int32Array t -> unit meth
  method finish : unit meth
end

class type webCLPlatform = object
  method getDevices : webCLDevice t js_array t meth
  method getDevices_withType : deviceType -> webCLDevice t js_array t meth
end

class type webCLContext = object
  method createBuffer : memflags -> cluint -> webCLBuffer t meth
  method createProgram : js_string t -> webCLProgram t meth
  method createCommandQueue : webCLCommandQueue t meth
  method createCommandQueue_fromDevice : webCLDevice t -> webCLCommandQueue t meth
  method getInfo_fromDevices : contextInfo -> webCLDevice t js_array t meth
  method getInfo_fromNumDevices : contextInfo -> cluint meth
  (*
  method createImage
  method createSampler
  *)
end

class type webCL = object
  method createContext : webCLDevice t -> webCLContext t meth
  method releaseAll : unit meth

  method getPlatforms : webCLPlatform t js_array t meth

  method _CONTEXT_DEVICES_ : contextInfo readonly_prop
  method _CONTEXT_NUM_DEVICES_ : contextInfo readonly_prop
  method _SUCCESS : errorCode readonly_prop
  method _DEVICE_NOT_FOUND_ : errorCode readonly_prop

  method _MEM_READ_WRITE_ : memflags readonly_prop
  method _MEM_WRITE_ONLY_ : memflags readonly_prop
  method _MEM_READ_ONLY_ : memflags readonly_prop
  
  method _DEVICE_NAME_ : deviceInfo readonly_prop
  method _DEVICE_TYPE_CPU_ : deviceType readonly_prop
  method _DEVICE_TYPE_GPU_ : deviceType readonly_prop
end

class type window = object
  method webcl : webCL t prop
end

let getWebCL (w : Dom_html.window t) = 
  let w : window t = Js.Unsafe.coerce w in
    w##webcl
