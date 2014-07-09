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
  let toString e = string_of_float e
    (*(Str.string_before (string_of_float e) 5)^"|"*)
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

let getElemFromID id coerceCible =
    Opt.get
      (Opt.bind ( Dom_html.document##getElementById(string id) )
         coerceCible)
      (fun () -> failwith ("this id : "^id^" do not match any element"))

let fillPWithFloat f id =
  let p = getElemFromID id Dom_html.CoerceTo.p
  in
    (p##innerHTML <- string ((string_of_int (truncate f))^" ms"))

let fillPWithArray arr id =
  let p = getElemFromID id Dom_html.CoerceTo.p
  in
    (p##innerHTML <- string (toString arr))

let fillPWith_float32Array arr id l =
  let p = getElemFromID id Dom_html.CoerceTo.p
  in
    (p##innerHTML <- string (toString_float32 arr l))

let initArrays length =
  Random.self_init();
  let arr_A = Array.make length 0.
  and arr_B = Array.make length 0.
  in
    for i = 0 to length - 1 do
      arr_A.(i) <- (Random.float 100.);
      arr_B.(i) <- (Random.float 100.)
    done;
    (*fillDivWithArray arr_A "DIVA";
    fillDivWithArray arr_B "DIVB";*)
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
    (*fillDivWithArray res "DIVC_CPU";*)
    fillPWithFloat t "computingTime"

let array_from arr = 
  let res = jsnew array_length (Array.length arr) in
    for i = 0 to (Array.length arr) -1 do
      array_set res i arr.(i)
    done;
  res

let compute (webcl: WebCL.webCL t) dev a b length =
  let inBuf1 = float32array a 
  and inBuf2 = float32array b
  and outBuf = float32array (Array.make length 0.) in
  
  let compute () = 
    let src = get_source "clKernel"
    and devices = getAllGPUDevicesFromAllPlatforms webcl
  in 
    let gpu_device = Optdef.get (array_get devices dev) (fun _ -> failwith "no device found")
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
    and globalWS = [|32|] 
    and cmdQueue = ctx##createCommandQueue_fromDevice(gpu_device) 
  in
    cmdQueue##enqueueWriteBuffer_float32array(buf1, _true, 0, bufSize, inBuf1);
    cmdQueue##enqueueWriteBuffer_float32array(buf2, _true, 0, bufSize, inBuf2);
    cmdQueue##enqueueNDRangeKernel(kern,1,null,(array_from globalWS),(array_from localWS));
    cmdQueue##enqueueReadBuffer_float32array(buf3, _true, 0, bufSize, outBuf);
    cmdQueue##finish ();
    ()
    (*fillDivWith_float32Array outBuf "DIVC_GPU" length;*)
  in
    let (t,_) = time compute in fillPWithFloat t "computingTime"

let getLength () =
  let inputElem = getElemFromID "inputsize" Dom_html.CoerceTo.input in
    (int_of_string (to_string inputElem##value))
    
let makeOption name = 
  let opt = createOption document in
    Dom.appendChild opt (document##createTextNode(name));
    opt
    
let initSelectPlats cl =
 let s = getElemFromID "selectDevice" Dom_html.CoerceTo.select
 and dev = getAllGPUDevicesFromAllPlatforms cl
 in 
   Firebug.console##log (Js.string ((string_of_int dev##length)^" device(s) found"));
   for i = 0 to dev##length -1 do
     let d = Optdef.get (array_get dev i) (fun _ -> failwith "no dev found") in
     let opt = makeOption (getDeviceName cl d)
     in Dom.appendChild s opt
   done
   
let getDev() = 0
  
let initEvents cl =
  let goButton = getElemFromID "GoButton" Dom_html.CoerceTo.button in
  goButton##onclick <- Dom_html.handler 
    (fun evt -> 
       let length = getLength () in
       Firebug.console##log(Js.string "random initialization of the arrays ...");
       let (a,b) = initArrays length
       and dev = getDev()
       in
         compute cl dev a b length;
         Js._true;)

let start cl =
  initSelectPlats cl;
  initEvents cl
    
let go _ =
  let cl = WebCL.getWebCL Dom_html.window in
  ignore (start cl);
  Js._true

let _ = 
  Dom_html.window##onload <- Dom_html.handler go
