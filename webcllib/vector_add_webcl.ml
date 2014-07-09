open Lwt
open Js
open Dom_html
open WebCL

(*print etc*)
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s)) f
let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f

(*qui transforme un array en float32array*)
let float32array a =
  let arr = jsnew Typed_array.float32Array(Array.length a) in
  Array.iteri (fun i v -> Typed_array.set arr i v) a;
  arr

(*qui transforme un array en int32array*)
let int32array a =
  let arr = jsnew Typed_array.int32Array(Array.length a) in
  Array.iteri (fun i v -> Typed_array.set arr i v) a;
  arr

let toString_float32 arr length= 
  let toString e = 
    (Str.string_before (string_of_float e) 5)^"|"
  in
    let str = ref "|" in
    for i = 0 to length - 1 do
      str := !str^toString (Typed_array.unsafe_get arr i)
    done;
  !str

let toString arr = 
  let toString e = 
    (Str.string_before (string_of_float e) 5)^"|"
  in
    let str = ref "|" in
    for i = 0 to (Array.length arr) - 1 do
      str := !str^toString arr.(i)
    done;
  !str

(* retourne le contenu du script d'id = src_id *)
let get_source src_id =
  let script = Opt.get
    (Opt.bind ( Dom_html.document##getElementById(string src_id) )
       Dom_html.CoerceTo.script)
    (fun () -> ignore (debug "can't find script element %s" src_id);
	       error "can't find script element %s" src_id) in
  script##text

let getDivFromID div_id =
    Opt.get
      (Opt.bind ( Dom_html.document##getElementById(string div_id) )
         Dom_html.CoerceTo.p)
      (fun () -> failwith "this id do not match any element")

let fillDivWithFloat f div_id =
  let div = getDivFromID div_id
  in
    (div##innerHTML <- string ((string_of_int (truncate f))^" ms"))

let fillDivWithArray liste div_id =
  let div = getDivFromID div_id
  in
    (div##innerHTML <- string (toString liste))

let fillDivWith_float32Array arr div_id l=
  let div = getDivFromID div_id
  in
    (div##innerHTML <- string (toString_float32 arr l))

let initArrays length =
  Random.self_init();
  let arr_A = Array.make length 0.
  and arr_B = Array.make length 0.
  in
    for i = 0 to length - 1 do
      arr_A.(i) <- (Random.float 100.);
      arr_B.(i) <- (Random.float 100.)
    done;
    fillDivWithArray arr_A "DIVA";
    fillDivWithArray arr_B "DIVB";
    arr_A,arr_B

(* retourne le temps que met un calcul et le résultat associé *)
let time computation = 
  let d = (jsnew date_now ())##getTime() in
    let res = computation () in
    (((jsnew date_now ())##getTime() -. d), res)  
    
let computeCpu a b length = 
  let compute () = 
    let res = Array.make length 0. in
    for i = 0 to length -1 do
      res.(i) <- a.(i) +. b.(i)
    done;
    res
  in
    let (t,res) = time compute in
    fillDivWithArray res "DIVC_CPU";
    fillDivWithFloat t "benchmarkCPU"

let array_from arr = 
  let res = jsnew array_length (Array.length arr) in
    for i = 0 to (Array.length arr) -1 do
      array_set res i arr.(i)
    done;
  res

let computeGpu (webcl: WebCL.webCL t) a b length =
  let inBuf1 = float32array a 
  and inBuf2 = float32array b
  and outBuf = float32array (Array.make length 0.)
  and src = get_source "clKernel"
  and devices = getAllGPUDevicesFromAllPlatforms webcl
  in 
    Firebug.console##log (Js.string ((string_of_int devices##length)^" device(s) found"));
    let gpu_device = Optdef.get (array_get devices 1) (fun _ -> failwith "no device found")
  in
    Firebug.console##log(Js.string "selected device : ");
    Firebug.console##log (WebCL.getDeviceName webcl gpu_device);
    let ctx = webcl##createContext_withDevice(gpu_device) 
    and bufSize = length * 4
  in
    let buf1 = ctx##createBuffer(webcl##_MEM_READ_ONLY_, bufSize)
    and buf2 = ctx##createBuffer(webcl##_MEM_READ_ONLY_, bufSize)
    and buf3 = ctx##createBuffer(webcl##_MEM_WRITE_ONLY_, bufSize)
    and clprog = ctx##createProgram(src)
  in
    WebCL.buildProgram clprog gpu_device;
    let kern = clprog##createKernel(Js.string "ckVectorAdd")
  in
    kern##setArg_fromBuffer(0, buf1);
    kern##setArg_fromBuffer(1, buf2);  
    kern##setArg_fromBuffer(2, buf3);
    kern##setArg_fromIntArray(3,(int32array (Array.make 1 length)));
    let localWS = [|8|]
    and globalWS = [|32|] in
    let cmdQueue = ctx##createCommandQueue_fromDevice(gpu_device) in
      cmdQueue##enqueueWriteBuffer_float32array(buf1, _true, 0, bufSize, inBuf1);
      cmdQueue##enqueueWriteBuffer_float32array(buf2, _true, 0, bufSize, inBuf2);
      cmdQueue##enqueueNDRangeKernel(kern,1,null,(array_from globalWS),(array_from localWS));
      cmdQueue##enqueueReadBuffer_float32array(buf3, _true, 0, bufSize, outBuf);
      cmdQueue##finish ();

      fillDivWith_float32Array outBuf "DIVC_GPU" length;
      ()

let initEvents cl a b length =
  let gpuButton = 
    Opt.get
      (Opt.bind ( Dom_html.document##getElementById(string "GPUButton") )
         Dom_html.CoerceTo.button)
      (fun () -> failwith "this id do not match any element")
  and cpuButton =
    Opt.get
      (Opt.bind ( Dom_html.document##getElementById(string "CPUButton") )
         Dom_html.CoerceTo.button)
      (fun () -> failwith "this id do not match any element")
  in
  gpuButton##onclick <-
      Dom_html.handler 
      (fun evt -> 
        computeGpu cl a b length;
	Js._true;
       );
  cpuButton##onclick <-
      Dom_html.handler 
      (fun evt -> 
        computeCpu a b length;
	Js._true;
       )

let start cl =
  let length = 30000 in
  let (a,b) = initArrays length in
  initEvents cl a b length
    
let go _ =
  let cl = WebCL.getWebCL Dom_html.window in
  ignore (start cl);
  Js._true

let _ = 
  Dom_html.window##onload <- Dom_html.handler go
