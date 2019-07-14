(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Hashaux
open Hash
open Sha256
open Json
open Net
open Db
open Block

(*** mainnet ***)
let ltc_oldest_to_consider = ref (hexstring_hashval "ffc12159965e55a2b334a83d2241bc25b409c14bf77c41eacf4a445556fe71b0")
let ltc_oldest_to_consider_time = ref 1523203501L
let ltc_oldest_to_consider_height = ref 1400000L

let ltc_oldblocks = (*** add an ltc block every 50,000 blocks or so to help nodes with initial sync; order them from oldest to newest ***)
  ["01e0b239508af020122b2beb20985b59ad65effae4bf6449c25efcbd7b089b28";
   "3eb72e473ce35c3b88c0a3c96ffa4a7ab01f40e476034c9caf82fba8e0270cb0";
   "458bdbb339f30b349cb2478da3afb6a7b1eb02044ad504b156fa17da047f848b";
   "d136037e14d275b7b71c06994b83a297297e045b74d48b84dc0ad5c893fc7fa9"]

let ltc_testnet_oldblocks = (*** add an ltc block every 50,000 blocks or so to help nodes with initial sync; order them from oldest to newest ***)
  []

(*** testnet ***)
let ltctestnet () =
  ltc_oldest_to_consider := hexstring_hashval "52bed2a0bdafe1d44e978c3dde0ff9a0a824ca9d30bd1cae375b539f34855b4d";
  ltc_oldest_to_consider_time := 1552816988L;
  ltc_oldest_to_consider_height := 1021606L

let ltc_bestblock = ref (0l,0l,0l,0l,0l,0l,0l,0l)

let burntx : (hashval,string) Hashtbl.t = Hashtbl.create 100

type ltcdacstatus = LtcDacStatusPrev of hashval | LtcDacStatusNew of (hashval * hashval * hashval * int64 * int64) list list

let ltcrpc_url () =
  match !Config.ltcrpconion with
  | Some(o) ->
      "http://" ^ o ^ ":" ^ (string_of_int !Config.ltcrpcport) ^ "/"
  | None ->
      "http://" ^ !Config.ltcrpcip ^ ":" ^ (string_of_int !Config.ltcrpcport) ^ "/"

let seo_ltcdacstatus o s c =
  match s with
  | LtcDacStatusPrev(h) ->
      let c = o 1 0 c in
      seo_hashval o h c
  | LtcDacStatusNew(l) ->
      let c = o 1 1 c in
      seo_list (seo_list (seo_prod5 seo_hashval seo_hashval seo_hashval seo_int64 seo_int64)) o l c

let sei_ltcdacstatus i c =
  let (x,c) = i 1 c in
  if x = 0 then
    let (h,c) = sei_hashval i c in
    (LtcDacStatusPrev(h),c)
  else
    let (l,c) = sei_list (sei_list (sei_prod5 sei_hashval sei_hashval sei_hashval sei_int64 sei_int64)) i c in
    (LtcDacStatusNew(l),c)

let ltcdacstatush : (hashval,ltcdacstatus) Hashtbl.t = Hashtbl.create 1000

module DbLtcDacStatus = Dbbasic2 (struct type t = ltcdacstatus let basedir = "ltcdacstatus" let seival = sei_ltcdacstatus seic let seoval = seo_ltcdacstatus seoc end)

(*** h is the id of an ltcblock, so it should always uniquely determine the ltcdacstatus (all dalilcoin blockid burns from the past week in order). ***)
let rec ltcdacstatus_dbget h =
  try
    let z =
      try
	Hashtbl.find ltcdacstatush h
      with Not_found ->
	let z = DbLtcDacStatus.dbget h in
	Hashtbl.add ltcdacstatush h z;
	z
    in
    match z with
    | LtcDacStatusPrev(k) ->
	ltcdacstatus_dbget k
    | LtcDacStatusNew(l) -> (h,l)
  with Not_found -> (!ltc_oldest_to_consider,[])

let json_assoc_string k al =
  match List.assoc k al with
  | JsonStr(x) -> x
  | _ -> raise Not_found

let json_assoc_int64 k al =
  match List.assoc k al with
  | JsonNum(x) -> Int64.of_string x
  | _ -> raise Not_found

let json_assoc_int k al =
  match List.assoc k al with
  | JsonNum(x) -> int_of_string x
  | _ -> raise Not_found

let litecoins_of_litoshis v =
  let w = Int64.div v 100000000L in
  let d = Int64.to_string (Int64.rem v 100000000L) in
  let dl = String.length d in
  let ez = ref 0 in
  begin
    try
      for i = dl-1 downto 0 do
	if d.[i] = '0' then
	  incr ez
	else
	  raise Exit
      done
    with Exit -> ()
  end;
  let b = Buffer.create 20 in
  Buffer.add_string b (Int64.to_string w);
  Buffer.add_char b '.';
  for i = 1 to 11 - dl do
    Buffer.add_char b '0'
  done;
  for i = 0 to dl - (1 + !ez) do
    Buffer.add_char b d.[i]
  done;
  Buffer.contents b

let litoshis_of_litecoins s =
  let f = ref 0L in
  let w = ref true in
  let c = ref 0L in
  let d = ref 10000000L in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let cc = Char.code s.[!i] in
    incr i;
    if !w then
      if cc = 46 then
	w := false
      else if cc >= 48 && cc < 58 then
	f := Int64.add (Int64.mul !f 10L) (Int64.of_int (cc-48))
      else
	raise (Failure ("cannot interpret " ^ s ^ " as a number of litecoins"))
    else
      if cc >= 48 && cc < 58 then
	begin
	  c := Int64.add !c (Int64.mul !d (Int64.of_int (cc-48)));
	  d := Int64.div !d 10L
	end
      else
	raise (Failure ("cannot interpret " ^ s ^ " as a number of litecoins"))
  done;
  Int64.add (Int64.mul !f 100000000L) !c

let json_assoc_litoshis k al =
  match List.assoc k al with
  | JsonNum(x) -> litoshis_of_litecoins x
  | _ -> raise Not_found

let ltc_getbestblockhash () =
  try
    if !Config.ltcoffline then
      begin
	Printf.printf "call getbestblockhash in ltc\n>> "; flush stdout;
	let h = read_line() in
	h
      end
    else
      let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
      let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"gbbh\", \"method\": \"getbestblockhash\", \"params\": [] }'" in
      let url = ltcrpc_url() in
      let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
      let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
      let l = input_line inc in
      ignore (Unix.close_process_full (inc,outc,errc));
      match parse_jsonval l with
      | (JsonObj(al),_) -> json_assoc_string "result" al
      | _ ->
	  (Utils.log_string (Printf.sprintf "problem return from ltc getbestblockhash:\n%s\n" l));
	  raise Not_found
  with _ ->
    raise Not_found

let dalilcoin_candidate_p h =
  String.length h = 64 && h.[0] = '4' && h.[1] = '4' && h.[2] = '6' && h.[3] = '1'

let ltc_getblock h =
  try
    let l =
      if !Config.ltcoffline then
	begin
	  Printf.printf "call getblock %s in ltc\n>> " h; flush stdout;
	  read_line()
	end
      else
	let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
	let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"gb\", \"method\": \"getblock\", \"params\": [\"" ^ h ^ "\"] }'" in
	let url = ltcrpc_url() in
	let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
	let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
	let l = input_line inc in
	ignore (Unix.close_process_full (inc,outc,errc));
	l
    in
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonObj(bl) ->
	      begin
		let pbh = json_assoc_string "previousblockhash" bl in
		let tm = json_assoc_int64 "mediantime" bl in
		let hght = json_assoc_int64 "height" bl in
		let txl = ref [] in
		match List.assoc "tx" bl with
		| JsonArr(txs) ->
		    begin
		      List.iter
			(fun jtxh ->
			  match jtxh with
			  | JsonStr(txh) when dalilcoin_candidate_p txh -> txl := txh::!txl
			  | _ -> ())
			txs;
		      (pbh,tm,hght,!txl)
		    end
		| _ ->
		    (Utils.log_string (Printf.sprintf "problem return from ltc getblock:\n%s\n" l));
		    raise Not_found
	      end
	  | _ ->
	      (Utils.log_string (Printf.sprintf "problem return from ltc getblock:\n%s\n" l));
	      raise Not_found
	end
    | _ ->
	(Utils.log_string (Printf.sprintf "problem return from ltc getblock:\n%s\n" l));
	raise Not_found
  with _ ->
    raise Not_found

type ltcutxo =
  | LtcP2shSegwit of (string * int * string * string * string * int64)
  | LtcBech32 of (string * int * string * string * int64)

let ltc_listunspent () =
  try
    let l =
      if !Config.ltcoffline then
	begin
	  Printf.printf "call listunspent in ltc\n>> "; flush stdout;
	  read_line()
	end
      else
	let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
	let addrl = Buffer.create 40 in
	let fstaddr = ref true in
	List.iter
	  (fun a ->
	    if !fstaddr then fstaddr := false else Buffer.add_char addrl ',';
	    Buffer.add_char addrl '"';
	    Buffer.add_string addrl a;
	    Buffer.add_char addrl '"')
	  !Config.ltcaddresses;
	let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"lu\", \"method\": \"listunspent\", \"params\": [1,9999999,[" ^ (Buffer.contents addrl) ^ "]] }'" in
	let url = ltcrpc_url() in
	let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
	let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
	let l = input_line inc in
	ignore (Unix.close_process_full (inc,outc,errc));
	l
    in
    let utxol = ref [] in
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin
	  match List.assoc "result" al with
	  | JsonArr(ul) ->
	      begin
		List.iter
		  (fun u ->
		    match u with
		    | JsonObj(bl) ->
			begin
			  try
			    let ltcaddr = json_assoc_string "address" bl in
			    if ltcaddr = "" then raise Not_found;
			    if ltcaddr.[0] = 'M' then (*** p2sh segwit ***)
			      let txh = json_assoc_string "txid" bl in
			      let vout = json_assoc_int "vout" bl in
			      let rs = json_assoc_string "redeemScript" bl in
			      let spk = json_assoc_string "scriptPubKey" bl in
			      let amt = json_assoc_litoshis "amount" bl in
			      utxol := LtcP2shSegwit(txh,vout,ltcaddr,rs,spk,amt)::!utxol
			    else if ltcaddr.[0] = 'l' then (*** bech32 ***)
			      let txh = json_assoc_string "txid" bl in
			      let vout = json_assoc_int "vout" bl in
			      let spk = json_assoc_string "scriptPubKey" bl in
			      let amt = json_assoc_litoshis "amount" bl in
			      utxol := LtcBech32(txh,vout,ltcaddr,spk,amt)::!utxol
			    else
			      raise Not_found
			  with Not_found ->
			    ()
			end
		    | _ -> ())
		  ul;
		!utxol
	      end
	  | _ ->
	      (Utils.log_string (Printf.sprintf "problem return from ltc listunspent:\n%s\n" l));
	      raise Not_found
	end
    | _ ->
	(Utils.log_string (Printf.sprintf "problem return from ltc listunspent:\n%s\n" l));
	raise Not_found
  with _ ->
    raise Not_found
      
exception InsufficientLtcFunds

let le_num24 x =
  let strb = Buffer.create 3 in
  Buffer.add_char strb (Char.chr (x land 255));
  Buffer.add_char strb (Char.chr ((x lsr 8) land 255));
  Buffer.add_char strb (Char.chr ((x lsr 16) land 255));
  Buffer.contents strb

let finddatx txs1 txs2 =
  let i = ref (-1) in
  let rtxid = ref (0l,0l,0l,0l,0l,0l,0l,0l) in
  let txs = ref "" in
  let daid ri =
    let (_,_,_,_,_,_,_,x) = ri in
    Int32.logand x 0xffffl = 0x6144l
  in
  while not (daid !rtxid) do
    incr i;
    if !i >= 16777216 then raise Not_found; (** probably will never happen **)
    txs := txs1 ^ (le_num24 !i) ^ txs2;
    rtxid := Sha256.sha256dstr !txs
  done;
  (!i,!rtxid,!txs);;

let blnum32 x =
  [Int32.to_int (Int32.logand x 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 8) 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 16) 255l);
   Int32.to_int (Int32.logand (Int32.shift_right_logical x 24) 255l)]

let blnum64 x =
  [Int64.to_int (Int64.logand x 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 8) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 16) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 24) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 32) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 40) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 48) 255L);
   Int64.to_int (Int64.logand (Int64.shift_right_logical x 56) 255L)]

let ltc_createburntx h1 h2 toburn =
  let utxol = ltc_listunspent () in
  let toburn_plus_fee = Int64.add toburn !Config.ltctxfee in
  try
    Hashtbl.find burntx h2
  with Not_found ->
    try
      (Utils.log_string (Printf.sprintf "Searching for an unspent litecoin tx with at least %Ld litoshis.\n" toburn_plus_fee));
      let u = (*** only consider single spends ***)
	List.find
	  (fun u ->
	    match u with
	    | LtcP2shSegwit(txid,vout,_,_,_,amt) -> amt >= toburn_plus_fee
	    | LtcBech32(txid,vout,_,_,amt) -> amt >= toburn_plus_fee)
	  utxol
      in
      let createburntx txid vout spk amt redeemfn =
	let txs1b = Buffer.create 100 in
	let txs2b = Buffer.create 100 in
	let txs3b = Buffer.create 100 in
	Buffer.add_string txs1b "\001"; (*** assume one input ***)
	let txidrh = hashval_rev (hexstring_hashval txid) in
	ignore (seo_hashval seosb txidrh (txs1b,None));
	List.iter (fun z -> Buffer.add_char txs1b (Char.chr z)) (blnum32 (Int32.of_int vout));
	let txs1 = Buffer.contents txs1b in
	redeemfn txs1b;
	Buffer.add_string txs2b "\255\255\255\255\002";
	List.iter (fun z -> Buffer.add_char txs2b (Char.chr z)) (blnum64 toburn);
	let extradata =
	  begin
	    match !Config.onion with
	    | Some(onionaddr) ->
		begin
		  try
		    let dot = String.index onionaddr '.' in
		    if !Config.onionremoteport = 20808 then
		      Printf.sprintf "o%s." (String.sub onionaddr 0 dot)
		    else
		      Printf.sprintf "o%s:%s" (String.sub onionaddr 0 dot) (hexstring_string (Printf.sprintf "%04x" !Config.onionremoteport))
		  with Not_found -> ""
		end
	    | None ->
		match !Config.ip with
		| Some(ip) ->
		    begin
		      let (ip0,ip1,ip2,ip3) = extract_ipv4 ip in
		      if !Config.port = 20805 then
			begin
			  Printf.sprintf "I%s" (hexstring_string (Printf.sprintf "%02x%02x%02x%02x" ip0 ip1 ip2 ip3))
			end
		      else
			begin
			  Printf.sprintf "i%s" (hexstring_string (Printf.sprintf "%02x%02x%02x%02x%04x" ip0 ip1 ip2 ip3 !Config.port))
			end
		    end
		| None -> ""
	  end
	in
	let extradata = (*** before the May 2019 hard fork, do not burn enough to require pushing more than 75 bytes in the burn tx (and wait an extra day before burning more than 75 bytes even after the hard fork time, out of abundance of caution) ***)
	  if 67 + String.length extradata >= 76 && Int64.of_float (Unix.time()) < Int64.add 86400L !Utils.may2019hardforktime then
	    ""
	  else if 67 + String.length extradata >= 65536 then
	    ""
	  else
	    extradata
	in
	let datalen = 67 + String.length extradata in
	if datalen > 252 then raise (Failure "too much data to burn");
	if datalen < 76 then
	  begin
	    Buffer.add_char txs2b (Char.chr (datalen+2));
	    Buffer.add_char txs2b (Char.chr 0x6a); (*** OP_RETURN ***)
	    Buffer.add_char txs2b (Char.chr datalen); (*** PUSH datalen ***)
	  end
	else if datalen > 65535 then
	  raise (Failure "too much data to burn")
	else if datalen > 255 then
	  begin
	    Buffer.add_char txs2b (Char.chr (datalen+3));
	    Buffer.add_char txs2b (Char.chr 0x6a); (*** OP_RETURN ***)
	    Buffer.add_char txs2b (Char.chr 77); (*** PUSH datalen ***)
	    Buffer.add_char txs2b (Char.chr (datalen mod 256));
	    Buffer.add_char txs2b (Char.chr (datalen / 256));
	  end
	else 
	  begin
	    Buffer.add_char txs2b (Char.chr (datalen+3));
	    Buffer.add_char txs2b (Char.chr 0x6a); (*** OP_RETURN ***)
	    Buffer.add_char txs2b (Char.chr 76); (*** PUSH datalen ***)
	    Buffer.add_char txs2b (Char.chr datalen);
	  end;
	ignore (seo_hashval seosb h1 (txs2b,None));
	ignore (seo_hashval seosb h2 (txs2b,None));
	for i = 0 to (String.length extradata) - 1 do
	  Buffer.add_char txs2b extradata.[i]
	done;
	List.iter (fun z -> Buffer.add_char txs3b (Char.chr z)) (blnum64 (Int64.sub amt toburn_plus_fee));
	let spks = hexstring_string spk in
	Buffer.add_char txs3b (Char.chr (String.length spks));
	Buffer.add_string txs3b spks;
	Buffer.add_string txs3b "\000\000\000\000"; (*** locktime ***)
	let txs2 = Buffer.contents txs2b in
	let txs3 = Buffer.contents txs3b in
	let (i,rtxid,txs) = finddatx ("\002\000\000\000" ^ (Buffer.contents txs1b) ^ txs2) txs3 in
	let txsb = Buffer.create 100 in
	Buffer.add_string txsb "\002\000\000\000";
	Buffer.add_string txsb txs1;
	Buffer.add_string txsb "\000";
	Buffer.add_string txsb txs2;
	Buffer.add_string txsb (le_num24 i);
	Buffer.add_string txsb txs3;
	let s = Buffer.contents txsb in
	Hashtbl.add burntx h2 s;
	s
      in
      match u with
      | LtcP2shSegwit(txid,vout,_,rs,spk,amt) ->
	  let rs2 = hexstring_string rs in
	  let rsl = String.length rs2 in
	  if rsl < 1 || rsl > 75 then raise Not_found;
	  createburntx
	    txid vout spk amt
	    (fun txs1b ->
	      Buffer.add_char txs1b (Char.chr (1 + rsl));
	      Buffer.add_char txs1b (Char.chr rsl);
	      Buffer.add_string txs1b rs2)
      | LtcBech32(txid,vout,ltcaddr,spk,amt) ->
	  createburntx
	    txid vout spk amt
	    (fun txs1b ->
	      Buffer.add_char txs1b '\000')
    with Not_found -> raise InsufficientLtcFunds

let ltc_signrawtransaction txs =
  try
    let l =
      if !Config.ltcoffline then
	begin
	  Printf.printf "call signrawtransaction %s in ltc\n>> " txs; flush stdout;
	  read_line()
	end
      else
	let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
	let call =
	  if !Config.ltcversion >= 17 then
	    "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"signrawtransactionwithwallet\", \"params\": [\"" ^ txs ^ "\"] }'"
	  else
	    "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"signrawtransaction\", \"params\": [\"" ^ txs ^ "\"] }'"
	in
	let url = ltcrpc_url() in
	let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
	let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
	let l = input_line inc in
	ignore (Unix.close_process_full (inc,outc,errc));
	l
    in
    match parse_jsonval l with
    | (JsonObj(al),_) ->
	begin 
	  match List.assoc "result" al with
	  | JsonObj(bl) -> json_assoc_string "hex" bl
	  | _ ->
	      (Utils.log_string (Printf.sprintf "problem return from ltc signrawtransaction:\n%s\n" l));
	      raise Not_found
	end
    | _ ->
	(Utils.log_string (Printf.sprintf "problem return from ltc signrawtransaction:\n%s\n" l));
	raise Not_found
  with _ -> raise Not_found

let ltc_sendrawtransaction txs =
  try
    let l =
      if !Config.ltcoffline then
	begin
	  Printf.printf "call sendrawtransaction %s in ltc\n>> " txs; flush stdout;
	  read_line()
	end
      else
	let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
	let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"srtx\", \"method\": \"sendrawtransaction\", \"params\": [\"" ^ txs ^ "\"] }'" in
	let url = ltcrpc_url() in
	let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
	let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
	let l = input_line inc in
	ignore (Unix.close_process_full (inc,outc,errc));
	l
    in
    match parse_jsonval l with
    | (JsonObj(al),_) -> json_assoc_string "result" al
    | _ ->
	(Utils.log_string (Printf.sprintf "problem return from ltc sendrawtransaction:\n%s\n" l));
	raise Not_found
  with _ -> raise Not_found

exception NotAnLtcBurnTx

let ltc_gettransactioninfo h =
  let l =
    if !Config.ltcoffline then
      begin
	Printf.printf "call getrawtransaction %s in ltc\n>> " h; flush stdout;
	read_line()
      end
    else
      let userpass = !Config.ltcrpcuser ^ ":" ^ !Config.ltcrpcpass in
      let call = "'{\"jsonrpc\": \"1.0\", \"id\":\"grtx\", \"method\": \"getrawtransaction\", \"params\": [\"" ^ h ^ "\",1] }'" in
      let url = ltcrpc_url() in
      let fullcall = !Config.curl ^ " --user " ^ userpass ^ " --data-binary " ^ call ^ " -H 'content-type: text/plain;' " ^ url in
      let (inc,outc,errc) = Unix.open_process_full fullcall [| |] in
      let l = input_line inc in
      ignore (Unix.close_process_full (inc,outc,errc));
      l
  in
  match parse_jsonval l with
  | (JsonObj(al),_) ->
      begin
	match List.assoc "result" al with
	| JsonObj(bl) ->
	    begin
	      match List.assoc "vout" bl with
	      | JsonArr(JsonObj(vout1)::_) ->
		  let litoshisburned = json_assoc_litoshis "value" vout1 in
		  begin
		    match List.assoc "scriptPubKey" vout1 with
		    | JsonObj(cl) ->
			let hex = json_assoc_string "hex" cl in
			if String.length hex >= 132 && hex.[0] = '6' && hex.[1] = 'a' && hex.[2] = '4' then
			  begin
			    let hex =
			      if Int64.of_float (Unix.time()) < !Utils.may2019hardforktime then
				String.sub hex 4 ((String.length hex) - 4)
			      else if hex.[3] = 'c' then (*** pushing up to 255 bytes ***)
				String.sub hex 6 ((String.length hex) - 6)
			      else if hex.[3] = 'd' then (*** pushing up to 64K bytes ***)
				String.sub hex 8 ((String.length hex) - 8)
			      else
				String.sub hex 4 ((String.length hex) - 4)
			    in
			    let lprevtx = hexstring_hashval (String.sub hex 0 64) in
			    let dnxt = hexstring_hashval (String.sub hex 64 64) in
			    begin
			      let hexl = String.length hex in
			      if hexl > 132 then
				let extradata = hexstring_string (String.sub hex 128 ((String.length hex) - 128)) in
				if extradata.[0] = 'o' then
				  begin
				    if List.length !netconns < !Config.maxconns then
				      begin
					let onionaddr = Buffer.create 10 in
					try
					  for i = 1 to ((String.length extradata) - 1) do
					    if extradata.[i] = '.' then
					      begin
						let peer = Printf.sprintf "%s.onion:20808" (Buffer.contents onionaddr) in
						ignore (tryconnectpeer peer);
						ignore (addknownpeer (Int64.of_float (Unix.time())) peer);
						raise Exit
					      end
					    else if extradata.[i] = ':' then
					      begin
						if i+2 < String.length extradata then
						  begin
						    let port = (Char.code extradata.[i+1]) * 256 + (Char.code extradata.[i+2]) in
						    let peer = Printf.sprintf "%s.onion:%d" (Buffer.contents onionaddr) port in
						    ignore (tryconnectpeer peer);
						    ignore (addknownpeer (Int64.of_float (Unix.time())) peer);
						  end
						else
						  raise Exit
					      end
					    else
					      Buffer.add_char onionaddr extradata.[i]
					  done
					with Exit -> ()
				      end
				  end
				else if extradata.[0] = 'I' then
				  begin
				    if List.length !netconns < !Config.maxconns && ((String.length extradata) > 4) then
				      begin
					let ip0 = Char.code extradata.[1] in
					let ip1 = Char.code extradata.[2] in
					let ip2 = Char.code extradata.[3] in
					let ip3 = Char.code extradata.[4] in
					let peer = Printf.sprintf "%d.%d.%d.%d:20805" ip0 ip1 ip2 ip3 in
					ignore (tryconnectpeer peer);
					ignore (addknownpeer (Int64.of_float (Unix.time())) peer);
				      end
				  end
				else if extradata.[0] = 'i' then
				  begin
				    if List.length !netconns < !Config.maxconns && ((String.length extradata) > 6) then
				      begin
					let ip0 = Char.code extradata.[1] in
					let ip1 = Char.code extradata.[2] in
					let ip2 = Char.code extradata.[3] in
					let ip3 = Char.code extradata.[4] in
					let port = (Char.code extradata.[5]) * 256 + Char.code extradata.[6] in
					let peer = Printf.sprintf "%d.%d.%d.%d:%d" ip0 ip1 ip2 ip3 port in
					ignore (tryconnectpeer peer);
					ignore (addknownpeer (Int64.of_float (Unix.time())) peer);
				      end
				  end
			    end;
			    let lblkh =
			      begin
				try
				  match List.assoc "blockhash" bl with
				  | JsonStr(lblkh) -> Some(lblkh)
				  | _ -> None
				with Not_found -> None
			      end
			    in
			    let confs =
			      begin
				try
				  match List.assoc "confirmations" bl with
				  | JsonNum(c) -> Some(int_of_string c)
				  | _ -> None
				with _ -> None
			      end
			    in
			    (litoshisburned,lprevtx,dnxt,lblkh,confs)
			  end
			else
			  begin
			    (Utils.log_string (Printf.sprintf "problem return from ltc getrawtransaction:\n%s\n" l));
			    raise NotAnLtcBurnTx
			  end
		    | _ ->
			(Utils.log_string (Printf.sprintf "problem return from ltc getrawtransaction:\n%s\n" l));
			raise NotAnLtcBurnTx
		  end
	      | _ ->
		  (Utils.log_string (Printf.sprintf "problem return from ltc getrawtransaction:\n%s\n" l));
		  raise Not_found
	    end
	| _ ->
	    (Utils.log_string (Printf.sprintf "problem return from ltc getrawtransaction:\n%s\n" l));
	    raise Not_found
      end
  | _ ->
      (Utils.log_string (Printf.sprintf "problem return from ltc getrawtransaction:\n%s\n" l));
      raise Not_found

module DbLtcBurnTx = Dbbasic2 (struct type t = int64 * hashval * hashval let basedir = "ltcburntx" let seival = sei_prod3 sei_int64 sei_hashval sei_hashval seic let seoval = seo_prod3 seo_int64 seo_hashval seo_hashval seoc end)

module DbLtcBlock = Dbbasic2 (struct type t = hashval * int64 * int64 * hashval list let basedir = "ltcblock" let seival = sei_prod4 sei_hashval sei_int64 sei_int64 (sei_list sei_hashval) seic let seoval = seo_prod4 seo_hashval seo_int64 seo_int64 (seo_list seo_hashval) seoc end)

let rec ltc_process_block h =
  let hh = hexstring_hashval h in
  if not (hh = !ltc_oldest_to_consider) && not (DbLtcBlock.dbexists hh) then
    begin
      let (prev,tm,hght,txhs) = ltc_getblock h in
      ltc_process_block prev;
      let prevh = hexstring_hashval prev in
      let genl = ref [] in
      let succl = ref [] in
      let txhhs = ref [] in
      List.iter
	  (fun txh ->
	    let txhh = hexstring_hashval txh in
	    let handle burned lprevtx dnxt =
	      if not (List.mem (hh,txhh) (Hashtbl.find_all blockburns dnxt)) then Hashtbl.add blockburns dnxt (hh,txhh);
	      if lprevtx = (0l,0l,0l,0l,0l,0l,0l,0l) then
		begin
		  (Utils.log_string (Printf.sprintf "Adding burn %s for genesis header %s\n" txh (hashval_hexstring dnxt)));
		  txhhs := txhh :: !txhhs;
		  genl := (txhh,burned,dnxt)::!genl;
		  if not (Hashtbl.mem outlinevals (hh,txhh)) then
		    begin
		      Hashtbl.add outlinevals (hh,txhh) (dnxt,tm,burned,None,hashpair hh txhh,1L);
		      (*** since the burn is presumably new, add to missing lists; this should never happen since the genesis phase has passed. ***)
		      missingheaders := List.merge (fun (i,_) (j,_) -> compare i j) [(1L,dnxt)] !missingheaders;
		      missingdeltas := List.merge (fun (i,_) (j,_) -> compare i j) [(1L,dnxt)] !missingdeltas
		    end
		end
	      else
		begin
		  DbLtcBurnTx.dbput txhh (burned,lprevtx,dnxt);
		  try
		    let (_,_,dprev,lprevblkh,_) = ltc_gettransactioninfo (hashval_hexstring lprevtx) in
		    (Utils.log_string (Printf.sprintf "Adding burn %s for header %s\n" txh (hashval_hexstring dnxt)));
		    txhhs := txhh :: !txhhs;
		    succl := (dprev,txhh,burned,dnxt)::!succl;
		    if not (Hashtbl.mem outlinevals (hh,txhh)) then
		      begin
			try
			  match lprevblkh with
			  | Some(lprevblkh) ->
			      let lprevblkh = hexstring_hashval lprevblkh in
			      let (_,_,_,_,_,dhght) = Hashtbl.find outlinevals (lprevblkh,lprevtx) in
			      let currhght = Int64.add 1L dhght in
			      Hashtbl.add outlinevals (hh,txhh) (dnxt,tm,burned,Some(lprevblkh,lprevtx),hashpair hh txhh,currhght);
			      (*** since the burn is presumably new, add to missing lists (unless it was staked by the current node which is handled in staking module) ***)
			      missingheaders := List.merge (fun (i,_) (j,_) -> compare i j) [(currhght,dnxt)] !missingheaders;
			      missingdeltas := List.merge (fun (i,_) (j,_) -> compare i j) [(currhght,dnxt)] !missingdeltas
			  | None -> ()
			with Not_found -> ()
		      end
		  with Not_found ->
		    Utils.log_string (Printf.sprintf "Could not find parent ltc burn tx %s for burn %s for header %s\n" (hashval_hexstring lprevtx) txh (hashval_hexstring dnxt))
		end
	    in
	    try
	      let (burned,lprevtx,dnxt) = DbLtcBurnTx.dbget txhh in
	      handle burned lprevtx dnxt
	    with Not_found ->
	      begin
		try
		  let (burned,lprevtx,dnxt,_,_) = ltc_gettransactioninfo txh in
		  DbLtcBurnTx.dbput txhh (burned,lprevtx,dnxt);
		  handle burned lprevtx dnxt
		with Not_found ->
		  Utils.log_string (Printf.sprintf "Ignoring tx %s which does not appear to be a Dalilcoin burn tx\n" txh)
	      end)
	txhs;
      begin
	let (prevkey,pbds) = ltcdacstatus_dbget prevh in
	let change = ref false in
	let bds = ref [] in
	if not (!genl = []) then
	  begin
	    if tm > Int64.add !Config.genesistimestamp 604800L then
	      begin
		(Utils.log_string (Printf.sprintf "Ignoring unexpected genesis blocks burned during what appears to be after the genesis phase:\n"));
		List.iter (fun (txhh,burned,dnxt) -> Printf.printf "%s %Ld %s\n" (hashval_hexstring txhh) burned (hashval_hexstring dnxt)) !genl
	      end
	    else (*** there has already been a genesis block created during the genesis phase, but a competing one (or more) was created; include it too ***)
	      begin
		(Utils.log_string (Printf.sprintf "%d genesis block(s) found.\n" (List.length !genl)));
		let pbdl = List.map (fun (txhh,burned,dnxt) -> (dnxt,hh,txhh,tm,hght)) !genl in
		change := true;
		bds := [pbdl]
	      end
	  end;
	List.iter
	  (fun pbdl ->
	    let pbdl2 =
	      List.filter
		(fun (bh,lbh,ltx,ltm,lhght) -> if Int64.sub tm ltm <= 604800L || Int64.sub hght lhght <= 4032L then true else (change := true; false)) (*** only allow building on blocks from the past week (either <= 604800 seconds in ltc median block time or 4032 ltc blocks) ***)
		pbdl
	    in
	    if not (pbdl2 = []) then bds := pbdl2 :: !bds;
	    let pbdl3 = ref [] in
	    List.iter
	      (fun (bh,lbh,ltx,ltm,lhght) ->
		List.iter
		  (fun (dprev,txhh,burned,dnxt) ->
		    if bh = dprev then
		      begin
			pbdl3 := (dnxt,hh,txhh,tm,hght)::!pbdl3;
			change := true
		      end)
		  !succl)
	      pbdl2;
	    if not (!pbdl3 = []) then bds := !pbdl3 :: !bds)
	  (List.rev pbds);
	if !change then
	  begin
	    DbLtcDacStatus.dbput hh (LtcDacStatusNew(!bds))
	  end
	else if not (prevkey = !ltc_oldest_to_consider) then
	  DbLtcDacStatus.dbput hh (LtcDacStatusPrev(prevkey)) (*** pointer to last ltc block where dalilcoin status changed ***)
      end;
      DbLtcBlock.dbput hh (prevh,tm,hght,!txhhs)
    end

let ltc_medtime () =
  try
    let (_,mtm,_,_) = DbLtcBlock.dbget !ltc_bestblock in
    mtm
  with Not_found -> Int64.of_float (Unix.time())

let ltc_synced () =
  try
    Utils.log_string (Printf.sprintf "Checking if ltc synced; bestblock %s\n" (hashval_hexstring !ltc_bestblock));
    let (_,tm,_,_) = DbLtcBlock.dbget !ltc_bestblock in
    Utils.log_string (Printf.sprintf "tm of ltc bestblock %Ld offset from now %f\n" tm (Unix.time() -. Int64.to_float tm));
    if Unix.time() -. Int64.to_float tm < 3600.0 then
      true
    else
      false
  with Not_found -> false

let ltc_tx_confirmed h =
  try
    let (u,h1,h2,lblkh,confs) = ltc_gettransactioninfo h in
    match confs with
    | Some(i) when i >= 1 -> true
    | _ -> false
  with Not_found -> false

let ltc_tx_poburn h =
  try
    let (u,h1,h2,lblkh,confs) = ltc_gettransactioninfo h in
    match lblkh with
    | Some(lbh) ->
	let (prev,tm,hght,txhs) = ltc_getblock lbh in
	Poburn(hexstring_md256 lbh,hexstring_md256 h,tm,u)
    | _ -> raise Not_found
  with _ -> raise Not_found

let ltc_best_chaintips () =
  let (lastchangekey,ctips0l) = ltcdacstatus_dbget !ltc_bestblock in
  let ctips1l =
    List.map (fun ctips -> List.filter (fun (h,_,_,_,_) -> not (DbBlacklist.dbexists h) && not (DbInvalidatedBlocks.dbexists h)) ctips) ctips0l
  in
  let ctips2l = List.filter (fun ctips -> not (ctips = [])) ctips1l in
  List.map (fun ctips -> List.map (fun (h,_,_,_,_) -> h) ctips) ctips2l

let find_dalilcoin_header_ltc_burn h =
  let tried : (hashval,unit) Hashtbl.t = Hashtbl.create 100 in
  let rec find_dalilcoin_header_ltc_burn_rec lbhl =
    match lbhl with
    | [] -> raise Not_found
    | lbh::lbhr ->
	if Hashtbl.mem tried lbh then
	  find_dalilcoin_header_ltc_burn_rec lbhr
	else
	  let (lastchangekey,ctips0l) = ltcdacstatus_dbget lbh in
	  let ctips1l =
	    List.map (fun ctips -> List.filter (fun (h,_,_,_,_) -> not (DbBlacklist.dbexists h) && not (DbInvalidatedBlocks.dbexists h)) ctips) ctips0l
	  in
	  let ctips2l = List.filter (fun ctips -> not (ctips = [])) ctips1l in
	  match ctips2l with
	  | [] -> raise Not_found
	  | (bestctips::_) ->
	      try
		let (dbh,lbh,ltx,ltm,lhght) = List.find (fun (dbh,_,_,_,_) -> dbh = h) bestctips in
		let (burned,lprevtx,dnxt) = DbLtcBurnTx.dbget ltx in
		let pob = ltc_tx_poburn (hashval_hexstring ltx) in
		let optionprevdalblock =
		  if lprevtx = (0l,0l,0l,0l,0l,0l,0l,0l) then
		    None
		  else
		    let (_,_,dprev) = DbLtcBurnTx.dbget lprevtx in
		    Some(dprev)
		in
		(pob,optionprevdalblock)
	      with Not_found ->
		let lbhlr = ref lbhl in
		List.iter
		  (fun (_,lbh,_,_,_) ->
		    try
		      let (prevlbh,_,_,_) = DbLtcBlock.dbget lbh in
		      lbhlr := prevlbh :: !lbhlr
		    with Not_found -> ())
		  bestctips;
		Hashtbl.add tried lbh ();
		find_dalilcoin_header_ltc_burn_rec !lbhlr
  in
  find_dalilcoin_header_ltc_burn_rec [!ltc_bestblock]

let ltc_old_sync () =
  List.iter
    ltc_process_block
    (if !Config.testnet then ltc_testnet_oldblocks else ltc_oldblocks)

