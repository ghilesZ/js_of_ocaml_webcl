open Js

(** 5.1 Types *)
type clboolean = bool t
type clint = int
type cllong = int
type cluint = int
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
  method build_fromDeviceList : webCLDevice t js_array t -> unit meth
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
  method createContext : webCLContext t meth
  method createContext_withDevice : webCLDevice t -> webCLContext t meth
  method createContext_withDeviceList : webCLDevice t js_array t -> webCLContext t meth
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

let getAllGPUDevicesFrom plats webcl = 
  let nb_devices = ref 0 in
  for i = 0 to (plats##length) -1 do
    let p = Optdef.get (array_get plats i) 
		       (fun _ -> failwith "no platform found") in
      nb_devices := !nb_devices + (p##getDevices())##length
  done;
  let devs = jsnew array_length (!nb_devices) in
    for i = 0 to (plats##length) -1 do
      let p = Optdef.get (array_get plats i) 
	                 (fun _ -> failwith "no platform found") in
        let pdevs = (p##getDevices()) in
        for j = 0 to (pdevs##length -1) do
          let d = Optdef.get (array_get pdevs j) 
	                     (fun _ -> failwith "no dev found") in
          array_set devs (i+j) d
        done
    done; 
  devs

let getAllGPUDevicesFromAllPlatforms (webcl: webCL t) =
  getAllGPUDevicesFrom webcl##getPlatforms() webcl 

(* returns the name of the device dev.
   dev must be associated to the webcl object *)
let getDeviceName (webcl: webCL t) dev =
  dev##getInfo_DEVICENAME(webcl##_DEVICE_NAME_)

let buildProgram clprog device = 
  let dev_typedArray = jsnew array_length (1) in
    array_set dev_typedArray 0 device;
    clprog##build_fromDeviceList(dev_typedArray)
