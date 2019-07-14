(* Copyright (c) 2015-2017 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json;;
open Big_int;;
open Utils;;
open Ser;;
open Sha256;;
open Ripemd160;;
open Hashaux;;
open Hash;;
open Net;;
open Db;;
open Secp256k1;;
open Signat;;
open Cryptocurr;;
open Mathdata;;
open Assets;;
open Tx;;
open Ctre;;
open Ctregraft;;
open Block;;
open Blocktree;;
open Ltcrpc;;
open Setconfig;;
open Staking;;
open Inputdraft;;

exception BadCommandForm;;

let get_ledgerroot b =
  match b with
  | None -> raise Not_found
  | Some(dbh,lbk,ltx) ->
      try
	let (_,_,lr,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
	lr
      with Not_found ->
	let (bhd,_) = DbBlockHeader.dbget dbh in
	bhd.newledgerroot

let get_3roots b =
  match b with
  | None -> raise Not_found
  | Some(dbh,lbk,ltx) ->
      try
	let (_,_,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
	(lr,tr,sr)
      with Not_found ->
	let (bhd,_) = DbBlockHeader.dbget dbh in
	(bhd.newledgerroot,bhd.newtheoryroot,bhd.newsignaroot)

let lock datadir =
  let lf = Filename.concat datadir "lock" in
  let c = open_out lf in
  close_out c;
  exitfn := (fun n -> Commands.save_txpool(); Sys.remove lf; exit n);;

let sinceltctime f =
  let snc = Int64.sub (ltc_medtime()) f in
  if snc >= 172800L then
    (Int64.to_string (Int64.div snc 86400L)) ^ " days"
  else if snc >= 7200L then
    (Int64.to_string (Int64.div snc 7200L)) ^ " hours"
  else if snc >= 120L then
    (Int64.to_string (Int64.div snc 60L)) ^ " minutes"
  else if snc = 1L then
    "1 second"
  else
    (Int64.to_string snc) ^ " seconds";;

let sincetime f =
  let snc = Int64.sub (Int64.of_float (Unix.time())) f in
  if snc >= 172800L then
    (Int64.to_string (Int64.div snc 86400L)) ^ " days"
  else if snc >= 7200L then
    (Int64.to_string (Int64.div snc 7200L)) ^ " hours"
  else if snc >= 120L then
    (Int64.to_string (Int64.div snc 60L)) ^ " minutes"
  else if snc = 1L then
    "1 second"
  else
    (Int64.to_string snc) ^ " seconds";;

let fstohash a =
  match a with
  | None -> None
  | Some(h,_) -> Some(h);;

let stkth : Thread.t option ref = ref None;;

let initnetwork sout =
  begin
    try
      let notlistening = ref true in
      begin
	match !Config.ip with
	| Some(ip) ->
	    let l = openlistener ip !Config.port 5 in
	    let efn = !exitfn in
	    exitfn := (fun n -> shutdown_close l; efn n);
	    Printf.fprintf sout "Listening for incoming connections via ip %s on port %d.\n" ip !Config.port;
	    flush sout;
	    notlistening := false;
	    netlistenerth := Some(Thread.create netlistener l)
	| None -> ()
      end;
      begin
	match !Config.onion with
	| Some(onionaddr) ->
	    let l = openonionlistener onionaddr !Config.onionlocalport !Config.onionremoteport 5 in
	    let efn = !exitfn in
	    exitfn := (fun n -> shutdown_close l; efn n);
	    Printf.fprintf sout "Listening for incoming connections via tor hidden service %s using port %d.\n" onionaddr !Config.onionremoteport;
	    flush sout;
	    notlistening := false;
	    onionlistenerth := Some(Thread.create onionlistener l)
	| None -> ()
      end;
      if !notlistening then
	begin
	  Printf.fprintf sout "Not listening for incoming connections.\n";
	  Printf.fprintf sout "If you want Dalilcoin to listen for incoming connections set ip to your ip address\n";
	  Printf.fprintf sout "using ip=... in dalilcoin.conf or -ip=... on the command line.\n";
	  Printf.fprintf sout "By default ip listeners use port 20805.\n";
	  Printf.fprintf sout "This can be changed by setting port=... in dalilcoin.conf or -port=... on the command line.\n";
	  Printf.fprintf sout "To listen as a tor hidden service set onion address\n";
	  Printf.fprintf sout "using onion=... in dalilcoin.conf or -onion=... on the command line.\n";
	  Printf.fprintf sout "By default onion listeners listen via the advertised port 20808.\n";
	  Printf.fprintf sout "This can be changed by setting onionremoteport=... in dalilcoin.conf or -onionremoteport=... on the command line.\n";
	  Printf.fprintf sout "By default onion listeners use the local (unadvertised) port 20807.\n";
	  Printf.fprintf sout "This can be changed by setting onionlocalport=... in dalilcoin.conf or -onionlocalport=... on the command line.\n";
	  flush sout
	end
    with _ -> ()
  end;
  netseeker ();
  let efn = !exitfn in
  exitfn := (fun n -> saveknownpeers(); efn n);;

let ltc_listener_th : Thread.t option ref = ref None;;

let ltc_init sout =
  if !Config.testnet then ltctestnet();
  try
    log_string (Printf.sprintf "syncing with ltc\n");
    ltc_old_sync(); (*** in case the node is far behind, start by syncing up with some historic ltc blocks first ***)
    begin (** if recentltcblocks file was given, then process the ones listed in the file **)
      match !recent_ltc_blocks with
      | None -> ()
      | Some(f) ->
	  try
	    let s = open_in f in
	    try
	      while true do
		let l = input_line s in
		ltc_process_block l
	      done
	    with _ -> close_in s
	  with _ -> ()
    end;
    let lbh = ltc_getbestblockhash () in
    log_string (Printf.sprintf "ltc bestblock %s\n" lbh);
    ltc_process_block lbh;
    ltc_bestblock := hexstring_hashval lbh;
    log_string (Printf.sprintf "finished initial syncing with ltc, now checking for new blocks\n");
    let lbh = ltc_getbestblockhash () in
    log_string (Printf.sprintf "ltc bestblock %s\n" lbh);
    ltc_process_block lbh;
    ltc_bestblock := hexstring_hashval lbh;
    log_string (Printf.sprintf "finished syncing with ltc\n");
  with exc ->
    log_string (Printf.sprintf "problem syncing with ltc. %s quitting.\n" (Printexc.to_string exc));
    Printf.fprintf sout "problem syncing with ltc. quitting.\n";
    !exitfn 2

let ltc_listener () =
  while true do
    try
      let lbh = ltc_getbestblockhash () in
      ltc_process_block lbh;
      ltc_bestblock := hexstring_hashval lbh;
      Thread.delay 60.0
    with _ ->
      Thread.delay 120.0
  done;;

(*** if only one ledger root is in the snapshot, assets, hconselts and ctreeelts will not be revisited, so no need to waste memory by saving them in fin ***)
let snapshot_fin_mem fin h = 
  (List.length !snapshot_ledgerroots > 1) && Hashtbl.mem fin h

let snapshot_fin_add fin h =
  if List.length !snapshot_ledgerroots > 1 then
    Hashtbl.add fin h ()

let dbledgersnapshot_asset assetfile fin h =
  if not (snapshot_fin_mem fin h) then
    begin
      snapshot_fin_add fin h;
      try
        let a = DbAsset.dbget h in
        seocf (seo_asset seoc a (assetfile,None))
      with Not_found ->
        Printf.printf "Could not find %s asset in database\n" (hashval_hexstring h)
    end

let rec dbledgersnapshot_hcons (hconseltfile,assetfile) fin h l =
  if not (snapshot_fin_mem fin h) then
    begin
      snapshot_fin_add fin h;
      try
	let (ah,hr) = DbHConsElt.dbget h in
        seocf (seo_prod seo_hashval (seo_option (seo_prod seo_hashval seo_int8)) seoc (ah,hr) (hconseltfile,None));
	dbledgersnapshot_asset assetfile fin ah;
	match hr with
	| Some(hr,l2) ->
	    if not (l = l2+1) then Printf.printf "Length mismatch in hconselt %s: expected length %d after cons but rest has claimed length %d.\n" (hashval_hexstring h) l l2;
	    dbledgersnapshot_hcons (hconseltfile,assetfile) fin hr l2
	| None ->
	    if not (l = 1) then Printf.printf "Length mismatch in hconselt %s: expected length %d after cons but claimed to have no extra elements.\n" (hashval_hexstring h) l;
	    ()
      with Not_found ->
	Printf.printf "Could not find %s hcons element in database\n" (hashval_hexstring h)
    end

let rec dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h =
  if not (snapshot_fin_mem fin h) && (!snapshot_full || not (supp = [])) then
    begin
      snapshot_fin_add fin h;
      try
	let c = DbCTreeElt.dbget h in
	seocf (seo_ctree seoc c (ctreeeltfile,None));
	dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin supp c
      with Not_found ->
	Printf.printf "Could not find %s ctree element in database\n" (hashval_hexstring h)
    end
and dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin supp c =
  match c with
  | CLeaf(bl,NehHash(h,l)) ->
      dbledgersnapshot_hcons (hconseltfile,assetfile) fin h l
  | CLeaf(bl,_) ->
      Printf.printf "non element ctree found in database\n"
  | CHash(h) -> dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h
  | CLeft(c0) -> dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0
  | CRight(c1) -> dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1
  | CBin(c0,c1) ->
      dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0;
      dbledgersnapshot_ctree (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1

let rec dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin supp c sl =
  if not (sl = []) then
    match c with
    | CLeaf(bl,NehHash(h,l)) ->
	dbledgersnapshot_hcons (hconseltfile,assetfile) fin h l
    | CLeaf(bl,_) ->
	Printf.printf "non element ctree found in database\n"
    | CHash(h) -> dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h
    | CLeft(c0) -> dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0 (strip_bitseq_false0 sl)
    | CRight(c1) -> dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1 (strip_bitseq_true0 sl)
    | CBin(c0,c1) ->
	dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_false0 supp) c0 (strip_bitseq_false0 sl);
	dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin (strip_bitseq_true0 supp) c1 (strip_bitseq_true0 sl)

let dbledgersnapshot_shards (ctreeeltfile,hconseltfile,assetfile) fin supp h sl =
  if not (snapshot_fin_mem fin h) && (!snapshot_full || not (supp = [])) then
    begin
      snapshot_fin_add fin h;
      try
	let c = DbCTreeElt.dbget h in
	seocf (seo_ctree seoc c (ctreeeltfile,None));
	dbledgersnapshot_ctree_shards (ctreeeltfile,hconseltfile,assetfile) fin supp c sl
      with Not_found ->
	Printf.printf "Could not find %s ctree element in database\n" (hashval_hexstring h)
    end

let dbledgersnapshot_ctree_top (ctreeeltfile,hconseltfile,assetfile) fin supp h s =
  match s with
  | None -> dbledgersnapshot (ctreeeltfile,hconseltfile,assetfile) fin supp h
  | Some(sl) ->
      let bitseq j =
	let r = ref [] in
	for i = 0 to 8 do
	  if ((j lsr i) land 1) = 1 then
	    r := true::!r
	  else
	    r := false::!r
	done;
	!r
      in
      dbledgersnapshot_shards (ctreeeltfile,hconseltfile,assetfile) fin supp h (List.map bitseq sl);;

let parse_json_privkeys kl =
  let (klj,_) = parse_jsonval kl in
  match klj with
  | JsonArr(kla) ->
      List.map
	(fun kj ->
	  match kj with
	  | JsonStr(k) ->
	    begin
	      let (k,b) = 
		try
		  privkey_from_wif k
		with _ ->
		  try
		    privkey_from_btcwif k
		  with _ -> raise (Failure "Bad private key")
	      in
	      match Secp256k1.smulp k Secp256k1._g with
	      | Some(x,y) ->
		  let h = hashval_md160 (pubkey_hashval (x,y) b) in
		  (k,b,(x,y),h)
	      | None -> raise (Failure "Bad private key")
	    end
	  | _ -> raise BadCommandForm)
	kla
  | _ -> raise BadCommandForm;;
	
let commandh : (string,(string * string * (out_channel -> string list -> unit))) Hashtbl.t = Hashtbl.create 100;;
let sortedcommands : string list ref = ref [];;

let local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy =
  try
    Hashtbl.find remgvtpth oidthy
  with Not_found ->
    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
    match hlist_lookup_obj_owner true true true oidthy hl with
    | None -> raise Not_found
    | Some(beta,r) -> (beta,r);;

let local_lookup_prop_thy_owner lr remgvknth pidthy alphathy =
  try
    Hashtbl.find remgvknth pidthy
  with Not_found ->
    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
    match hlist_lookup_prop_owner true true true pidthy hl with
    | None -> raise Not_found
    | Some(beta,r) -> (beta,r);;

let ac c h longhelp f =
  sortedcommands := List.merge compare [c] !sortedcommands;
  Hashtbl.add commandh c (h,longhelp,(fun oc al -> try f oc al with BadCommandForm -> Printf.fprintf oc "%s\n" h));;

let unconfirmedspentutxo : (hashval * hashval,unit) Hashtbl.t = Hashtbl.create 100;;

let find_spendable_utxo oc lr blkh mv =
  let b = ref None in
  List.iter
    (fun (alpha,a,v) ->
      if v >= mv && (match a with (aid,_,_,Currency(_)) when not (Hashtbl.mem unconfirmedspentutxo (lr,aid)) -> true | _ -> false) then
	match !b with
	| None -> b := Some(alpha,a,v)
	| Some(_,_,u) -> if v < u then b := Some(alpha,a,v))
    (Commands.get_spendable_assets_in_ledger oc lr blkh);
  match !b with
  | None -> raise Not_found
  | Some(alpha,a,v) ->
      Hashtbl.add unconfirmedspentutxo (lr,assetid a) ();
      (alpha,a,v);;

let rec find_marker_in_hlist hl =
  match hl with
  | HNil -> raise Not_found
  | HCons((aid,bday,obl,Marker),_) -> (aid,bday,obl)
  | HCons(_,hr) -> find_marker_in_hlist hr
  | HConsH(h,hr) ->
      let a = get_asset h in
      find_marker_in_hlist (HCons(a,hr))
  | HHash(h,_) ->
      find_marker_in_hlist (get_hlist_element h)

let find_marker_at_address tr beta =
  let hl = ctree_lookup_addr_assets true true tr (addr_bitseq beta) in
  find_marker_in_hlist hl

let initialize_commands () =
  ac "version" "version" "Print client description and version number"
    (fun oc _ ->
      Printf.fprintf oc "%s %s\n" Version.clientdescr Version.clientversion);
  ac "getaddressinfo" "getaddressinfo <address>" "Print information about address"
    (fun oc al ->
      match al with
      | [a] ->
	  let alpha = daliladdrstr_addr a in
	  let (p,x4,x3,x2,x1,x0) = alpha in
	  let jol = ref [] in
	  begin
	    if p = 0 then
	      begin
		jol := ("address",JsonStr("p2pkh"))::!jol;
		try
		  let s kl = List.find (fun (_,_,_,_,h,_) -> h = (x4,x3,x2,x1,x0)) kl in
		  let (_,b,(x,y),_,_,_) = s (!Commands.walletkeys_staking @ !Commands.walletkeys_nonstaking @ !Commands.walletkeys_staking_fresh @ !Commands.walletkeys_nonstaking_fresh) in
		  if b then
		    if evenp y then
		      jol := ("pubkey",JsonStr(Printf.sprintf "02%s" (md256_hexstring (big_int_md256 x))))::!jol
		    else
		      jol := ("pubkey",JsonStr(Printf.sprintf "03%s" (md256_hexstring (big_int_md256 x))))::!jol
		  else
		    jol := ("pubkey",JsonStr(Printf.sprintf "04%s%s" (md256_hexstring (big_int_md256 x)) (md256_hexstring (big_int_md256 y))))::!jol
		with Not_found -> ()
	      end
	    else if p = 1 then
	      begin
		jol := ("address",JsonStr("p2sh"))::!jol;
		let (_,_,bl) = List.find (fun (beta,_,_) -> (x4,x3,x2,x1,x0) = beta) !Commands.walletp2shs in
		let bu = Buffer.create 10 in
		List.iter (fun b -> Buffer.add_char bu (Char.chr b)) bl;
		jol := ("script",JsonStr(string_hexstring (Buffer.contents bu)))::!jol
	      end
	    else if p = 2 then
	      begin
		jol := ("address",JsonStr("term"))::!jol;
	      end
	    else if p = 3 then
	      begin
		jol := ("address",JsonStr("pub"))::!jol;
	      end
	    else
	      raise (Failure "apparently not an address");
	  end;
	  if not (!jol = []) then
	    begin
	      print_jsonval oc (JsonObj(List.rev !jol));
	      Printf.fprintf oc "\n";
	    end
      | _ -> raise BadCommandForm);
  ac "addnonce" "addnonce <file>" "Add a nonce to a theory specification file, a signature specification file or a document"
    (fun oc al ->
      match al with
      | [f] ->
	  begin
	    let ch = open_in f in
	    try
	      while true do
		let l = input_token ch in
		if l = "Nonce" then raise Exit
	      done
	    with
	    | Exit -> close_in ch; Printf.fprintf oc "A nonce was already declared.\nNo change was made.\n"
	    | End_of_file ->
		close_in ch;
		let ch = open_out_gen [Open_append] 0o660 f in
		let h = big_int_md256 (strong_rand_256()) in
		let nonce = hashval_hexstring h in
		Printf.fprintf ch "\nNonce %s\n" nonce;
		close_out ch
	    | e -> close_in ch; raise e
	  end
      | _ -> raise BadCommandForm);
  ac "addpublisher" "addpublisher <file> <payaddr>" "Add a publisher address to a theory specification file, a signature specification file or a document."
    (fun oc al ->
      match al with
      | [f;gammas] ->
	  begin
	    let gamma = Cryptocurr.daliladdrstr_addr gammas in
	    if not (payaddr_p gamma) then raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." gammas));
	    let ch = open_in f in
	    try
	      while true do
		let l = input_token ch in
		if l = "Publisher" then raise Exit
	      done
	    with
	    | Exit -> close_in ch; Printf.fprintf oc "A publisher was already declared.\nNo change was made.\n"
	    | End_of_file ->
		close_in ch;
		let ch = open_out_gen [Open_append] 0o660 f in
		Printf.fprintf ch "\nPublisher %s\n" gammas;
		close_out ch
	    | e -> close_in ch; raise e
	  end
      | _ -> raise BadCommandForm);
  ac "readdraft" "readdraft <file>" "Read a theory specification file, signature specification file or document file and give information."
    (fun oc al ->
      match al with
      | [f] ->
	  let ch = open_in f in
	  let l = input_token ch in
	  if l = "Theory" then
	    let (thyspec,nonce,gamma,_,prophrev,propownsh,proprightsh) = input_theoryspec ch in
	    let (lr,tr,sr) = get_3roots (get_bestblock_print_warnings oc) in
	    begin
	      let p = let s = Buffer.create 100 in seosbf (seo_theoryspec seosb thyspec (s,None)); String.length (Buffer.contents s) in
	      if p > 450000 then Printf.fprintf oc "Warning: Theory is too big: %d bytes. It probably will not fit in a block.\n" p;
	      match Checking.check_theoryspec thyspec with
	      | None -> raise (Failure "Theory spec does not check.\n")
	      | Some(thy,sg) ->
		  match hashtheory thy with
		  | None ->
		      Printf.fprintf oc "Theory is empty. It is correct but an empty theory is not allowed to be published.\n"
		  | Some(thyh) ->
		      let b = theoryspec_burncost thyspec in
		      Printf.fprintf oc "Theory is correct and has id %s and address %s.\n%s fraenks must be burned to publish the theory.\n" (hashval_hexstring thyh) (Cryptocurr.addr_daliladdrstr (hashval_pub_addr thyh)) (fraenks_of_cants b);
		      match nonce with
		      | None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
		      | Some(h) ->
			  Printf.fprintf oc "Nonce: %s\n" (hashval_hexstring h);
			  match gamma with
			  | None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
			  | Some(gamma) ->
			      if payaddr_p gamma then
				let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair h thyh)) in
				Printf.fprintf oc "Publisher address: %s\n" (Cryptocurr.addr_daliladdrstr gamma);
				Printf.fprintf oc "Marker Address: %s\n" (Cryptocurr.addr_daliladdrstr beta);
				let (_,kl) = thy in
				let pname h =
				  try
				    Hashtbl.find prophrev h
				  with Not_found -> ""
				in
				List.iter
				  (fun pidpure ->
				    let pidthy = hashtag (hashopair2 (Some(thyh)) pidpure) 33l in
				    let alphapure = hashval_term_addr pidpure in
				    let alphathy = hashval_term_addr pidthy in
				    let nm = pname pidpure in
				    begin
				      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
				      match hlist_lookup_prop_owner true true true pidpure hl with
				      | None ->
					  begin
					    let delta1str = try Printf.sprintf "address %s" (Cryptocurr.addr_daliladdrstr (payaddr_addr (Hashtbl.find propownsh pidpure))) with Not_found -> "publisher address" in
					    let rstr =
					      try
						let (delta2,r) = Hashtbl.find proprightsh pidpure in
						match r with
						| None -> "no rights available (unusable)"
						| Some(0L) -> "free to use"
						| Some(x) -> Printf.sprintf "right for each use costs %Ld cants (%s fraenks) payable to %s" x (Cryptocurr.fraenks_of_cants x) (Cryptocurr.addr_daliladdrstr (payaddr_addr delta2))
					      with Not_found -> "free to use"
					    in
					    Printf.fprintf oc "Pure proposition '%s' has no owner.\nYou will be declared as the owner when the document is published with the following details:\nNew ownership: %s.\n (This can be changed prior to publication with NewOwner <defname> <payaddress>.)\nRights policy: %s\n (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)\n" nm delta1str rstr
					  end;
					  let bl = hlist_filter_assets_gen true true (fun a -> match a with (_,_,_,Bounty(_)) -> true | _ -> false) hl in
					  if not (bl = []) then
					    begin
					      Printf.fprintf oc "There are bounties at %s you can claim by becoming the owner of the pure prop:\n" (Cryptocurr.addr_daliladdrstr alphapure);
					      List.iter
						(fun (bid,_,_,b) ->
						  match b with
						  | Bounty(v) -> Printf.fprintf oc "Bounty %s fraenks (asset id %s)\n" (fraenks_of_cants v) (hashval_hexstring bid)
						  | _ -> raise (Failure "impossible"))
						bl
					    end
				      | Some(beta,r) ->
					  Printf.fprintf oc "Pure proposition '%s' is owned by %s: %s\n" nm (addr_daliladdrstr (payaddr_addr beta))
					    (match r with
					    | None -> "No right to use without defining; must leave as theorem in the document"
					    | Some(r) ->
						if r = 0L then
						  "free to use; consider changing to Known without proof"
						else
						  (Printf.sprintf "Declaring the proposition as Known without proving it would cost %Ld cants; consider this" r))
				    end;
				    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
				    begin
				      match hlist_lookup_prop_owner true true true pidthy hl with
				      | None ->
					  begin
					    let delta1str = try Printf.sprintf "address %s" (Cryptocurr.addr_daliladdrstr (payaddr_addr (Hashtbl.find propownsh pidpure))) with Not_found -> "publisher address" in
					    let rstr =
					      try
						let (delta2,r) = Hashtbl.find proprightsh pidpure in
						match r with
						| None -> "no rights available (unusable)"
						| Some(0L) -> "free to use"
						| Some(x) -> Printf.sprintf "right for each use costs %Ld cants (%s fraenks) payable to %s" x (Cryptocurr.fraenks_of_cants x) (Cryptocurr.addr_daliladdrstr (payaddr_addr delta2))
					      with Not_found -> "free to use"
					    in
					    Printf.fprintf oc "Proposition '%s' in theory has no owner.\nYou will be declared as the owner when the document is published with the following details:\nNew ownership: %s.\n (This can be changed prior to publication with NewOwner <defname> <payaddress>.)\nRights policy: %s\n (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)\n" nm delta1str rstr
					  end;
					  let bl = hlist_filter_assets_gen true true (fun a -> match a with (_,_,_,Bounty(_)) -> true | _ -> false) hl in
					  if not (bl = []) then
					    begin
					      Printf.fprintf oc "There are bounties at %s you can claim by becoming the owner of the theory prop:\n" (Cryptocurr.addr_daliladdrstr alphathy);
					      List.iter
						(fun (bid,_,_,b) ->
						  match b with
						  | Bounty(v) -> Printf.fprintf oc "Bounty %s fraenks (asset id %s)\n" (fraenks_of_cants v) (hashval_hexstring bid)
						  | _ -> raise (Failure "impossible"))
						bl
					    end
				      | Some(beta,r) ->
					  Printf.fprintf oc "Proposition '%s' in theory is owned by %s: %s\n" nm (addr_daliladdrstr (payaddr_addr beta))
					    (match r with
					    | None -> "No right to use without defining; must leave as definition in the document"
					    | Some(r) ->
						if r = 0L then
						  "free to use; consider changing Thm to Known"
						else
						  (Printf.sprintf "Declaring the proposition as Known without proving it would cost %Ld cants; consider this" r))
				    end)
				  kl;
			      else
				raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
	    end
	  else if l = "Signature" then
	    let thyid = input_token ch in
	    let th = if thyid = "Empty" then None else Some(hexstring_hashval thyid) in
	    let (lr,tr,sr) = get_3roots (get_bestblock_print_warnings oc) in
	    let tht = lookup_thytree tr in
	    let thy = try ottree_lookup tht th with Not_found -> raise (Failure (Printf.sprintf "Theory %s not found" thyid)) in
	    let sgt = lookup_sigtree sr in
	    let (signaspec,nonce,gamma,_,objhrev,_,prophrev) = input_signaspec ch th sgt in
	    begin
	      let p = let s = Buffer.create 100 in seosbf (seo_signaspec seosb signaspec (s,None)); String.length (Buffer.contents s) in
	      if p > 450000 then Printf.fprintf oc "Warning: Signature is too big: %d bytes. It probably will not fit in a block. Split it into multiple signatures.\n" p;
	      let remgvtpth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let remgvknth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let gvtp th1 h1 a =
		if th1 = th then
		  let oid = hashtag (hashopair2 th (hashpair h1 (hashtp a))) 32l in
		  let alpha = hashval_term_addr oid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_obj_owner true true true oid hl with
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvtpth oid (beta,r); true
		else
		  false
	      in
	      let gvkn th1 k =
		if th1 = th then
		  let pid = hashtag (hashopair2 th k) 33l in
		  let alpha = hashval_term_addr pid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_prop_owner true true true pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvknth pid (beta,r); true
		else
		  false
	      in
	      match Checking.check_signaspec gvtp gvkn th thy sgt signaspec with
	      | None -> raise (Failure "Signature does not check.\n")
	      | Some((tml,knl),imported) ->
		  let id = hashopair2 th (hashsigna (signaspec_signa signaspec)) in
		  let b = signaspec_burncost signaspec in
		  Printf.fprintf oc "Signature is correct and has id %s and address %s.\n" (hashval_hexstring id) (addr_daliladdrstr (hashval_pub_addr id));
		  Printf.fprintf oc "%s fraenks must be burned to publish signature.\n" (Cryptocurr.fraenks_of_cants b);
		  Printf.fprintf oc "Signature imports %d signatures:\n" (List.length imported);
		  List.iter (fun h -> Printf.fprintf oc " %s\n" (hashval_hexstring h)) imported;
		  let oname h =
		    try
		      Hashtbl.find objhrev h
		    with Not_found -> ""
		  in
		  let pname h =
		    try
		      Hashtbl.find prophrev h
		    with Not_found -> ""
		  in
		  Printf.fprintf oc "Signature exports %d objects:\n" (List.length tml);
		  List.iter (fun ((h,_),m) -> Printf.fprintf oc " '%s' %s %s\n" (oname h) (hashval_hexstring h) (match m with None -> "(opaque)" | Some(_) -> "(transparent)")) tml;
		  Printf.fprintf oc "Signature exports %d props:\n" (List.length knl);
		  List.iter (fun (h,_) -> Printf.fprintf oc " '%s' %s\n" (pname h) (hashval_hexstring h)) knl;
		  let usesobjs = signaspec_uses_objs signaspec in
		  let usesprops = signaspec_uses_props signaspec in
		  let refusesig = ref false in
		  Printf.fprintf oc "Signature uses %d objects:\n" (List.length usesobjs);
		  List.iter
		    (fun (oidpure,k) ->
		      let oidthy = hashtag (hashopair2 th (hashpair oidpure k)) 32l in
		      let alphapure = hashval_term_addr oidpure in
		      let alphathy = hashval_term_addr oidthy in
		      let nm = oname oidpure in
		      try
			let (beta,r) = local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy in
			Printf.fprintf oc " Theory Object '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring oidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | Some(0L) -> "free to use"
			  | _ -> refusesig := true; "not free to use; signature cannot be published unless you redefine the object or buy the object and make it free for everyone.");
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_obj_owner true true true oidpure hl with
			| None ->
			    refusesig := true;
			    Printf.fprintf oc "** Somehow the theory object has an owner but the pure object %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring oidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc " Pure Object '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring oidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | Some(0L) -> "free to use"
			      | _ -> refusesig := true; "not free to use; signature cannot be published unless you redefine the object or buy the object and make it free for everyone.");
		      with Not_found ->
			refusesig := true;
			Printf.fprintf oc "  Did not find owner of theory object %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring oidthy) (addr_daliladdrstr alphathy))
		    usesobjs;
		  Printf.fprintf oc "Signature uses %d props:\n" (List.length usesprops);
		  List.iter
		    (fun pidpure ->
		      let pidthy = hashtag (hashopair2 th pidpure) 33l in
		      let alphapure = hashval_term_addr pidpure in
		      let alphathy = hashval_term_addr pidthy in
		      let nm = pname pidpure in
		      try
			let (beta,r) = local_lookup_prop_thy_owner lr remgvknth pidthy alphathy in
			Printf.fprintf oc " Theory Prop '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring pidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | Some(0L) -> "free to use"
			  | _ -> refusesig := true; "not free to use; signature cannot be published unless you buy the proposition and make it free for everyone.");
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_prop_owner true true true pidpure hl with
			| None ->
			    Printf.fprintf oc "** Somehow the theory prop has an owner but the pure prop %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring pidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc "  Pure Prop %s (%s)\n  Owner %s: %s\n" (hashval_hexstring pidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | Some(0L) -> "free to use"
			      | _ -> refusesig := true; "not free to use; signature cannot be published unless you buy the proposition and make it free for everyone.");
		      with Not_found ->
			refusesig := true;
			Printf.fprintf oc "  Did not find owner of theory proposition '%s' %s at %s when checking. Unexpected case.\n"
			  nm (hashval_hexstring pidthy) (addr_daliladdrstr alphathy))
		    usesprops;
		  if !refusesig then Printf.fprintf oc "Cannot publish signature without resolving the issues above.\n";
	    end
	  else if l = "Document" then
	    let thyid = input_token ch in
	    let th = if thyid = "Empty" then None else Some(hexstring_hashval thyid) in
	    let (lr,tr,sr) = get_3roots (get_bestblock_print_warnings oc) in
	    let tht = lookup_thytree tr in
	    let thy = try ottree_lookup tht th with Not_found -> raise (Failure (Printf.sprintf "Theory %s not found" thyid)) in
	    let sgt = lookup_sigtree sr in
	    let (dl,nonce,gamma,_,objhrev,_,prophrev,conjh,objownsh,objrightsh,propownsh,proprightsh,bountyh) = input_doc ch th sgt in
	    begin
	      let p = let s = Buffer.create 100 in seosbf (seo_doc seosb dl (s,None)); String.length (Buffer.contents s) in
	      if p > 450000 then Printf.fprintf oc "Warning: Document is too big: %d bytes. It probably will not fit in a block. Split it into multiple documents.\n" p;
	      let remgvtpth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let remgvknth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let gvtp th1 h1 a =
		if th1 = th then
		  let oid = hashtag (hashopair2 th (hashpair h1 (hashtp a))) 32l in
		  let alpha = hashval_term_addr oid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_obj_owner true true true oid hl with
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvtpth oid (beta,r); true
		else
		  false
	      in
	      let gvkn th1 k =
		if th1 = th then
		  let pid = hashtag (hashopair2 th k) 33l in
		  let alpha = hashval_term_addr pid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_prop_owner true true true pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvknth pid (beta,r); true
		else
		  false
	      in
	      match Checking.check_doc gvtp gvkn th thy sgt dl with
	      | None -> raise (Failure "Document does not check.\n")
	      | Some((tml,knl),imported) ->
		  let id = hashopair2 th (hashdoc dl) in
		  Printf.fprintf oc "Document is correct and has id %s and address %s.\n" (hashval_hexstring id) (addr_daliladdrstr (hashval_pub_addr id));
		  Printf.fprintf oc "Document imports %d signatures:\n" (List.length imported);
		  List.iter (fun h -> Printf.fprintf oc " %s\n" (hashval_hexstring h)) imported;
		  let oname h =
		    try
		      Hashtbl.find objhrev h
		    with Not_found -> ""
		  in
		  let pname h =
		    try
		      Hashtbl.find prophrev h
		    with Not_found -> ""
		  in
		  Printf.fprintf oc "Document mentions %d objects:\n" (List.length tml);
		  List.iter (fun ((h,_),_) -> Printf.fprintf oc " '%s' %s\n" (oname h) (hashval_hexstring h)) tml;
		  Printf.fprintf oc "Document mentions %d props:\n" (List.length knl);
		  List.iter (fun (h,_) -> Printf.fprintf oc " '%s' %s\n" (pname h) (hashval_hexstring h)) knl;
		  let usesobjs = doc_uses_objs dl in
		  let usesprops = doc_uses_props dl in
		  let createsobjs = doc_creates_objs dl in
		  let createsprops = doc_creates_props dl in
		  let createsnegpropsaddrs2 = List.map (fun h -> hashval_term_addr (hashtag (hashopair2 th h) 33l)) (doc_creates_neg_props dl) in
		  Printf.fprintf oc "Document uses %d objects:\n" (List.length usesobjs);
		  List.iter
		    (fun (oidpure,k) ->
		      let oidthy = hashtag (hashopair2 th (hashpair oidpure k)) 32l in
		      let alphapure = hashval_term_addr oidpure in
		      let alphathy = hashval_term_addr oidthy in
		      let nm = oname oidpure in
		      try
			let (beta,r) = local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy in
			Printf.fprintf oc " Theory Object '%s' %s (%s) Owner %s: %s\n" nm (hashval_hexstring oidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | None -> "No right to use; document cannot be published unless this is redefined.\n"
			  | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_obj_owner true true true oidpure hl with
			| None ->
			    Printf.fprintf oc "** Somehow the theory object has an owner but the pure object %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring oidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc " Pure Object '%s' %s (%s) Owner %s: %s\n" nm (hashval_hexstring oidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> "No right to use; document cannot be published unless this is redefined.\n"
			      | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
		      with Not_found ->
			Printf.fprintf oc "  Did not find owner of theory object %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring oidthy) (addr_daliladdrstr alphathy))
		    usesobjs;
		  Printf.fprintf oc "Document uses %d props:\n" (List.length usesprops);
		  List.iter
		    (fun pidpure ->
		      let pidthy = hashtag (hashopair2 th pidpure) 33l in
		      let alphapure = hashval_term_addr pidpure in
		      let alphathy = hashval_term_addr pidthy in
		      let nm = pname pidpure in
		      try
			let (beta,r) = local_lookup_prop_thy_owner lr remgvknth pidthy alphathy in
			Printf.fprintf oc " Theory Prop '%s' %s (%s) Owner %s: %s\n" nm (hashval_hexstring pidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | None -> "No right to use; document cannot be published unless this is reproven."
			  | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_prop_owner true true true pidpure hl with
			| None ->
			    Printf.fprintf oc "** Somehow the theory prop has an owner but the pure prop %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring pidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc "  Pure Prop %s (%s) Owner %s: %s\n" (hashval_hexstring pidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> "No right to use; document cannot be published unless this is redefined."
			      | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
		      with Not_found ->
			Printf.fprintf oc "  Did not find owner of theory proposition %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring pidthy) (addr_daliladdrstr alphathy))
		    usesprops;
		  Printf.fprintf oc "Document creates %d objects:\n" (List.length createsobjs);
		  List.iter
		    (fun (h,k) ->
		      let oidpure = h in
		      let oidthy = hashtag (hashopair2 th (hashpair h k)) 32l in
		      let alphapure = hashval_term_addr oidpure in
		      let alphathy = hashval_term_addr oidthy in
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
		      let nm = oname oidpure in
		      begin
			match hlist_lookup_obj_owner true true true oidpure hl with
			| None ->
			    begin
			      let delta1str = try Printf.sprintf "address %s" (Cryptocurr.addr_daliladdrstr (payaddr_addr (Hashtbl.find objownsh oidpure))) with Not_found -> "publisher address" in
			      let rstr =
				try
				  let (delta2,r) = Hashtbl.find objrightsh oidpure in
				  match r with
				   | None -> "no rights available (unusable)"
				   | Some(0L) -> "free to use"
				   | Some(x) -> Printf.sprintf "right for each use costs %Ld cants (%s fraenks) payable to %s" x (Cryptocurr.fraenks_of_cants x) (Cryptocurr.addr_daliladdrstr (payaddr_addr delta2))
				  with Not_found -> "free to use"
			      in
			      Printf.fprintf oc "Pure object '%s' has no owner.\nYou will be declared as the owner when the document is published with the following details:\nNew ownership: %s.\n (This can be changed prior to publication with NewOwner <defname> <payaddress>.)\nRights policy: %s\n (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)\n" nm delta1str rstr
			    end
			| Some(beta,r) ->
			    Printf.fprintf oc "Pure object '%s' is owned by %s: %s\n" nm (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> "No right to use without defining; must leave as definition in the document"
			      | Some(r) ->
				  if r = 0L then
				    (Printf.sprintf "free to use; consider changing Def to Param %s if the definition is not needed" (hashval_hexstring oidpure))
				  else
				    (Printf.sprintf "Using the object without defining it would cost %Ld cants; consider changing Def to Param %s if the definition is not needed" r (hashval_hexstring oidpure)))
		      end;
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
		      begin
			match hlist_lookup_obj_owner true true true oidthy hl with
			| None ->
			    begin
			      let delta1str = try Printf.sprintf "address %s" (Cryptocurr.addr_daliladdrstr (payaddr_addr (Hashtbl.find objownsh oidpure))) with Not_found -> "publisher address" in
			      let rstr =
				try
				  let (delta2,r) = Hashtbl.find objrightsh oidpure in
				  match r with
				  | None -> "no rights available (unusable)"
				  | Some(0L) -> "free to use"
				  | Some(x) -> Printf.sprintf "right for each use costs %Ld cants (%s fraenks) payable to %s" x (Cryptocurr.fraenks_of_cants x) (Cryptocurr.addr_daliladdrstr (payaddr_addr delta2))
				with Not_found -> "free to use"
			      in
			      Printf.fprintf oc "Object '%s' in theory has no owner.\nYou will be declared as the owner when the document is published with the following details:\nNew ownership: %s.\n (This can be changed prior to publication with NewOwner <defname> <payaddress>.)\nRights policy: %s\n (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)\n" nm delta1str rstr
			    end
			| Some(beta,r) ->
			    Printf.fprintf oc "Object '%s' in theory is owned by %s: %s\n" nm (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> "No right to use without defining; must leave as definition in the document"
			      | Some(r) ->
				  if r = 0L then
				    (Printf.sprintf "free to use; consider changing Def to Param %s if the definition is not needed" (hashval_hexstring oidpure))
				  else
				    (Printf.sprintf "Using the object without defining it would cost %Ld cants; consider changing Def to Param %s if the definition is not needed" r (hashval_hexstring oidpure)))
		      end)
		    createsobjs;
		  Printf.fprintf oc "Document creates %d props:\n" (List.length createsprops);
		  List.iter
		    (fun h ->
		      let pidpure = h in
		      let pidthy = hashtag (hashopair2 th h) 33l in
		      let alphapure = hashval_term_addr pidpure in
		      let alphathy = hashval_term_addr pidthy in
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
		      let nm = pname pidpure in
		      begin
			match hlist_lookup_prop_owner true true true pidpure hl with
			| None ->
			    begin
			      let delta1str = try Printf.sprintf "address %s" (Cryptocurr.addr_daliladdrstr (payaddr_addr (Hashtbl.find propownsh pidpure))) with Not_found -> "publisher address" in
			      let rstr =
				try
				  let (delta2,r) = Hashtbl.find proprightsh pidpure in
				  match r with
				   | None -> "no rights available (unusable)"
				   | Some(0L) -> "free to use"
				   | Some(x) -> Printf.sprintf "right for each use costs %Ld cants (%s fraenks) payable to %s" x (Cryptocurr.fraenks_of_cants x) (Cryptocurr.addr_daliladdrstr (payaddr_addr delta2))
				  with Not_found -> "free to use"
			      in
			      Printf.fprintf oc "Pure proposition '%s' has no owner.\nYou will be declared as the owner when the document is published with the following details:\nNew ownership: %s.\n (This can be changed prior to publication with NewOwner <defname> <payaddress>.)\nRights policy: %s\n (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)\n" nm delta1str rstr
			    end;
			    let bl = hlist_filter_assets_gen true true (fun a -> match a with (_,_,_,Bounty(_)) -> true | _ -> false) hl in
			    if not (bl = []) then
			      begin
				Printf.fprintf oc "There are bounties at %s you can claim by becoming the owner of the pure prop:\n" (Cryptocurr.addr_daliladdrstr alphapure);
				List.iter
				  (fun (bid,_,_,b) ->
				    match b with
				    | Bounty(v) -> Printf.fprintf oc "Bounty %s fraenks (asset id %s)\n" (fraenks_of_cants v) (hashval_hexstring bid)
				    | _ -> raise (Failure "impossible"))
				  bl
			      end
			| Some(beta,r) ->
			    Printf.fprintf oc "Pure proposition '%s' is owned by %s: %s\n" nm (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> "No right to use without defining; must leave as theorem in the document"
			      | Some(r) ->
				  if r = 0L then
				    "free to use; consider changing to Known without proof"
				  else
				    (Printf.sprintf "Declaring the proposition as Known without proving it would cost %Ld cants; consider this" r))
		      end;
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
		      begin
			match hlist_lookup_prop_owner true true true pidthy hl with
			| None ->
			    begin
			      let delta1str = try Printf.sprintf "address %s" (Cryptocurr.addr_daliladdrstr (payaddr_addr (Hashtbl.find propownsh pidpure))) with Not_found -> "publisher address" in
			      let rstr =
				try
				  let (delta2,r) = Hashtbl.find proprightsh pidpure in
				  match r with
				   | None -> "no rights available (unusable)"
				   | Some(0L) -> "free to use"
				   | Some(x) -> Printf.sprintf "right for each use costs %Ld cants (%s fraenks) payable to %s" x (Cryptocurr.fraenks_of_cants x) (Cryptocurr.addr_daliladdrstr (payaddr_addr delta2))
				  with Not_found -> "free to use"
			      in
			      Printf.fprintf oc "Proposition '%s' in theory has no owner.\nYou will be declared as the owner when the document is published with the following details:\nNew ownership: %s.\n (This can be changed prior to publication with NewOwner <defname> <payaddress>.)\nRights policy: %s\n (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)\n" nm delta1str rstr
			    end;
			    let bl = hlist_filter_assets_gen true true (fun a -> match a with (_,_,_,Bounty(_)) -> true | _ -> false) hl in
			    if not (bl = []) then
			      begin
				Printf.fprintf oc "There are bounties at %s you can claim by becoming the owner of the theory prop:\n" (Cryptocurr.addr_daliladdrstr alphathy);
				List.iter
				  (fun (bid,_,_,b) ->
				    match b with
				    | Bounty(v) -> Printf.fprintf oc "Bounty %s fraenks (asset id %s)\n" (fraenks_of_cants v) (hashval_hexstring bid)
				    | _ -> raise (Failure "impossible"))
				  bl
			      end
			| Some(beta,r) ->
			    Printf.fprintf oc "Proposition '%s' in theory is owned by %s: %s\n" nm (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> "No right to use without defining; must leave as definition in the document"
			      | Some(r) ->
				  if r = 0L then
				    "free to use; consider changing Thm to Known"
				  else
				    (Printf.sprintf "Declaring the proposition as Known without proving it would cost %Ld cants; consider this" r))
		      end)
		    createsprops;
		  Printf.fprintf oc "Document creates %d negprops:\n" (List.length createsnegpropsaddrs2);
		  List.iter
		    (fun alphathy ->
		      Printf.fprintf oc "%s\n" (addr_daliladdrstr alphathy);
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
		      if hlist_lookup_neg_prop_owner true true true hl then
			Printf.fprintf oc "The negated proposition already has an owner.\n"
		      else
			begin
			  Printf.fprintf oc "Negated proposition has no owner.\nThe publisher address will be used to declare ownership of the negated proposition when publishing the document.\n";
			  let bl = hlist_filter_assets_gen true true (fun a -> match a with (_,_,_,Bounty(_)) -> true | _ -> false) hl in
			  if not (bl = []) then
			    begin
			      Printf.fprintf oc "There are bounties you can claim by becoming the owner of the negated prop:\n";
			      List.iter
				(fun (bid,_,_,b) ->
				  match b with
				  | Bounty(v) -> Printf.fprintf oc "Bounty %s fraenks (asset id %s)\n" (fraenks_of_cants v) (hashval_hexstring bid)
				  | _ -> raise (Failure "impossible"))
				bl
			    end
			end)
		    createsnegpropsaddrs2;
		  let countbounties = ref 0 in
		  let totalbounties = ref 0L in
		  Hashtbl.iter
		    (fun _ (amt,_) ->
		      incr countbounties;
		      totalbounties := Int64.add amt !totalbounties)
		    bountyh;
		  if !countbounties > 0 then Printf.printf "%d new bounties worth a total of %s fraenks.\n" !countbounties (fraenks_of_cants !totalbounties)
	    end
	  else
	    begin
	      close_in ch;
	      raise (Failure (Printf.sprintf "Draft file has incorrect header: %s" l))
	    end
      | _ -> raise BadCommandForm);
  ac "commitdraft" "commitdraft <draftfile> <newtxfile>" "Form a transaction to publish a commitment for a draft file."
    (fun oc al ->
      match al with
      | [f;g] ->
	  let ch = open_in f in
	  let l = input_token ch in
	  let mkcommittx blkh lr beta =
	    try
	      let (aid,bday,obl) = find_marker_at_address (CHash(lr)) beta in
	      if Int64.add bday 3L <= blkh then (** this means 4 confirmations **)
		Printf.fprintf oc "A commitment marker for this draft has already been published and matured.\nThe draft can be published with the publishdraft command.\n"
	      else
		Printf.fprintf oc "A commitment marker for this draft has already been published and will mature after %Ld more blocks.\nAfter that the draft can be published with the publishdraft command.\n" (Int64.sub (Int64.add bday 3L) blkh )
	    with Not_found ->
	      try
		let minfee = Int64.mul 1000L !Config.defaulttxfee in (** very rough overestimate of 1K bytes for commitment tx **)
		let (alpha,(aid,_,_,_),v) = find_spendable_utxo oc lr blkh minfee in
		let txinl = [(alpha,aid)] in
		let txoutl =
		  if v >= Int64.add 10000L !Config.defaulttxfee then (** only create change if it is at least 10000 cants ***)
		    [(alpha,(None,Currency(Int64.sub v !Config.defaulttxfee)));(beta,(None,Marker))]
		  else
		    [(beta,(None,Marker))]
		in
		let stau = ((txinl,txoutl),([],[])) in
		let c2 = open_out_bin g in
		begin
		  try
		    Commands.signtxc oc lr stau c2 None;
		    close_out c2;
		    Printf.fprintf oc "The commitment transaction (to publish the marker) was created.\nTo inspect it:\n> decodetxfile %s\nTo validate it:\n> validatetxfile %s\nTo send it:\n> sendtxfile %s\n" g g g
		  with e ->
		    close_out c2;
		    raise e
		end
	      with Not_found ->
		Printf.fprintf oc "Cannot find a spendable utxo to use to publish the marker.\n"
	  in
	  if l = "Theory" then
	    let (thyspec,nonce,gamma,_,_,_,_) = input_theoryspec ch in
	    begin
	      match Checking.check_theoryspec thyspec with
	      | None -> raise (Failure "Theory spec does not check.\n")
	      | Some(thy,sg) ->
		  match hashtheory thy with
		  | None ->
		      Printf.fprintf oc "Theory is empty. It is correct but an empty theory is not allowed to be published.\n"
		  | Some(thyh) ->
		      match get_bestblock_print_warnings oc with
		      | None -> Printf.fprintf oc "No blocks yet\n"
		      | Some(h,lbk,ltx) ->
			  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
			  let (_,_,lr,tr,_) = Hashtbl.find validheadervals (lbk,ltx) in
			  try
			    let tht = lookup_thytree tr in
			    let _ = ottree_lookup tht (Some(thyh)) in
			    Printf.fprintf oc "Theory %s has already been published.\n" (hashval_hexstring thyh)
			  with Not_found ->
			    match nonce with
			    | None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
			    | Some(nonce) ->
				match gamma with
				| None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
				| Some(gamma) ->
				    if payaddr_p gamma then
				      let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair nonce thyh)) in
				      mkcommittx blkh lr beta
				    else
				      raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
	    end
	  else if l = "Signature" then
	    let thyid = input_token ch in
	    let th = if thyid = "Empty" then None else Some(hexstring_hashval thyid) in
	    let (blkh,lr,tr,sr) =
	      match get_bestblock_print_warnings oc with
	      | None -> raise Not_found
	      | Some(dbh,lbk,ltx) ->
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  let (_,_,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		  (blkh,lr,tr,sr)
	    in
	    let tht = lookup_thytree tr in
	    let thy = try ottree_lookup tht th with Not_found -> raise (Failure (Printf.sprintf "Theory %s not found" thyid)) in
	    let sgt = lookup_sigtree sr in
	    let (signaspec,nonce,gamma,_,objhrev,_,prophrev) = input_signaspec ch th sgt in
	    begin
	      let remgvtpth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let remgvknth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let gvtp th1 h1 a =
		if th1 = th then
		  let oid = hashtag (hashopair2 th (hashpair h1 (hashtp a))) 32l in
		  let alpha = hashval_term_addr oid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_obj_owner true true true oid hl with
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvtpth oid (beta,r); true
		else
		  false
	      in
	      let gvkn th1 k =
		if th1 = th then
		  let pid = hashtag (hashopair2 th k) 33l in
		  let alpha = hashval_term_addr pid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_prop_owner true true true pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvknth pid (beta,r); true
		else
		  false
	      in
	      match Checking.check_signaspec gvtp gvkn th thy sgt signaspec with
	      | None -> raise (Failure "Signature does not check.\n")
	      | Some((tml,knl),imported) ->
		  let id = hashopair2 th (hashsigna (signaspec_signa signaspec)) in
		  Printf.fprintf oc "Signature is correct and has id %s and address %s.\n" (hashval_hexstring id) (addr_daliladdrstr (hashval_pub_addr id));
		  Printf.fprintf oc "Signature imports %d signatures:\n" (List.length imported);
		  List.iter (fun h -> Printf.fprintf oc " %s\n" (hashval_hexstring h)) imported;
		  let oname h =
		    try
		      Hashtbl.find objhrev h
		    with Not_found -> ""
		  in
		  let pname h =
		    try
		      Hashtbl.find prophrev h
		    with Not_found -> ""
		  in
		  Printf.fprintf oc "Signature exports %d objects:\n" (List.length tml);
		  List.iter (fun ((h,_),m) -> Printf.fprintf oc " '%s' %s %s\n" (oname h) (hashval_hexstring h) (match m with None -> "(opaque)" | Some(_) -> "(transparent)")) tml;
		  Printf.fprintf oc "Signature exports %d props:\n" (List.length knl);
		  List.iter (fun (h,_) -> Printf.fprintf oc " '%s' %s\n" (pname h) (hashval_hexstring h)) knl;
		  let usesobjs = signaspec_uses_objs signaspec in
		  let usesprops = signaspec_uses_props signaspec in
		  let refusesig = ref false in
		  Printf.fprintf oc "Signature uses %d objects:\n" (List.length usesobjs);
		  List.iter
		    (fun (oidpure,k) ->
		      let oidthy = hashtag (hashopair2 th (hashpair oidpure k)) 32l in
		      let alphapure = hashval_term_addr oidpure in
		      let alphathy = hashval_term_addr oidthy in
		      let nm = oname oidpure in
		      try
			let (beta,r) = local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy in
			Printf.fprintf oc " Theory Object '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring oidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | Some(0L) -> "free to use"
			  | _ -> refusesig := true; "not free to use; signature cannot be published unless you redefine the object or buy the object and make it free for everyone.");
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_obj_owner true true true oidpure hl with
			| None ->
			    refusesig := true;
			    Printf.fprintf oc "** Somehow the theory object has an owner but the pure object %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring oidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc " Pure Object '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring oidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | Some(0L) -> "free to use"
			      | _ -> refusesig := true; "not free to use; signature cannot be published unless you redefine the object or buy the object and make it free for everyone.");
		      with Not_found ->
			refusesig := true;
			Printf.fprintf oc "  Did not find owner of theory object %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring oidthy) (addr_daliladdrstr alphathy))
		    usesobjs;
		  Printf.fprintf oc "Signature uses %d props:\n" (List.length usesprops);
		  List.iter
		    (fun pidpure ->
		      let pidthy = hashtag (hashopair2 th pidpure) 33l in
		      let alphapure = hashval_term_addr pidpure in
		      let alphathy = hashval_term_addr pidthy in
		      let nm = pname pidpure in
		      try
			let (beta,r) = local_lookup_prop_thy_owner lr remgvknth pidthy alphathy in
			Printf.fprintf oc " Theory Prop '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring pidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | Some(0L) -> "free to use"
			  | _ -> refusesig := true; "not free to use; signature cannot be published unless you buy the proposition and make it free for everyone.");
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_prop_owner true true true pidpure hl with
			| None ->
			    Printf.fprintf oc "** Somehow the theory prop has an owner but the pure prop %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring pidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc "  Pure Prop %s (%s)\n  Owner %s: %s\n" (hashval_hexstring pidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | Some(0L) -> "free to use"
			      | _ -> refusesig := true; "not free to use; signature cannot be published unless you buy the proposition and make it free for everyone.");
		      with Not_found ->
			refusesig := true;
			Printf.fprintf oc "  Did not find owner of theory proposition %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring pidthy) (addr_daliladdrstr alphathy))
		    usesprops;
		  if !refusesig then
		    Printf.fprintf oc "Cannot publish signature without resolving the issues above.\n"
		  else
		    match nonce with
		    | None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
		    | Some(nonce) ->
			match gamma with
			| None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
			| Some(gamma) ->
			    if payaddr_p gamma then
			      let signaspech = hashsigna (signaspec_signa signaspec) in
			      let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair nonce (hashopair2 th signaspech))) in
			      mkcommittx blkh lr beta
			    else
			      raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
	    end
	  else if l = "Document" then
	    let thyid = input_token ch in
	    let th = if thyid = "Empty" then None else Some(hexstring_hashval thyid) in
	    let (blkh,lr,tr,sr) =
	      match get_bestblock_print_warnings oc with
	      | None -> raise Not_found
	      | Some(dbh,lbk,ltx) ->
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  let (_,_,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		  (blkh,lr,tr,sr)
	    in
	    let tht = lookup_thytree tr in
	    let thy = try ottree_lookup tht th with Not_found -> raise (Failure (Printf.sprintf "Theory %s not found" thyid)) in
	    let sgt = lookup_sigtree sr in
	    let (dl,nonce,gamma,_,_,_,_,_,_,_,_,_,_) = input_doc ch th sgt in
	    let doch = hashdoc dl in
	    let alphadoc = hashval_pub_addr (hashopair2 th doch) in
	    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphadoc) in
	    match hlist_lookup_asset_gen true true true (fun a -> match a with (_,_,_,DocPublication(_,_,_,_)) -> true | _ -> false) hl with
	    | Some(aid,_,_,_) ->
		Printf.fprintf oc "Document has already been published: address %s asset id %s\n" (Cryptocurr.addr_daliladdrstr alphadoc) (hashval_hexstring aid)
	    | None ->
		begin
		  let remgvtpth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
		  let remgvknth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
		  let refusecommit = ref false in
		  let gvtp th1 h1 a =
		    if th1 = th then
		      let oid = hashtag (hashopair2 th (hashpair h1 (hashtp a))) 32l in
		      let alpha = hashval_term_addr oid in
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		      match hlist_lookup_obj_owner true true true oid hl with
		      | None -> false
		      | Some(beta,r) -> Hashtbl.add remgvtpth oid (beta,r); true
		    else
		      false
		  in
		  let gvkn th1 k =
		    if th1 = th then
		      let pid = hashtag (hashopair2 th k) 33l in
		      let alpha = hashval_term_addr pid in
		      let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		      match hlist_lookup_prop_owner true true true pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		      | None -> false
		      | Some(beta,r) -> Hashtbl.add remgvknth pid (beta,r); true
		    else
		      false
		  in
		  match Checking.check_doc gvtp gvkn th thy sgt dl with
		  | None -> raise (Failure "Document does not check.\n")
		  | Some(_) ->
		      let id = hashopair2 th (hashdoc dl) in
		      Printf.fprintf oc "Document is correct and has id %s and address %s.\n" (hashval_hexstring id) (addr_daliladdrstr (hashval_pub_addr id));
		      let usesobjs = doc_uses_objs dl in
		      let usesprops = doc_uses_props dl in
		      Printf.fprintf oc "Document uses %d objects:\n" (List.length usesobjs);
		      List.iter
			(fun (oidpure,k) ->
			  let oidthy = hashtag (hashopair2 th (hashpair oidpure k)) 32l in
			  let alphapure = hashval_term_addr oidpure in
			  let alphathy = hashval_term_addr oidthy in
			  try
			    let (beta,r) = local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy in
			    Printf.fprintf oc "  Theory Object %s (%s) Owner %s: %s\n" (hashval_hexstring oidthy) (addr_daliladdrstr alphathy)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> refusecommit := true; "No right to use; document cannot be published unless this is redefined.\n"
			      | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
			    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			    match hlist_lookup_obj_owner true true true oidpure hl with
			    | None ->
				Printf.fprintf oc "** Somehow the theory object has an owner but the pure object %s (%s) did not. Invariant failure. **\n"
				  (hashval_hexstring oidpure)
				  (addr_daliladdrstr alphapure)
			    | Some(beta,r) ->
				Printf.fprintf oc "  Pure Object %s (%s) Owner %s: %s\n" (hashval_hexstring oidpure) (addr_daliladdrstr alphapure)
				  (addr_daliladdrstr (payaddr_addr beta))
				  (match r with
				  | None -> refusecommit := true; "No right to use; document cannot be published unless this is redefined.\n"
				  | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
			  with Not_found ->
			    refusecommit := true;
			    Printf.fprintf oc "  Did not find owner of theory object %s at %s when checking. Unexpected case.\n"
			      (hashval_hexstring oidthy) (addr_daliladdrstr alphathy))
			usesobjs;
		      Printf.fprintf oc "Document uses %d props:\n" (List.length usesprops);
		      List.iter
			(fun pidpure ->
			  let pidthy = hashtag (hashopair2 th pidpure) 33l in
			  let alphapure = hashval_term_addr pidpure in
			  let alphathy = hashval_term_addr pidthy in
			  try
			    let (beta,r) = local_lookup_prop_thy_owner lr remgvknth pidthy alphathy in
			    Printf.fprintf oc "  Theory Prop %s (%s) Owner %s: %s\n" (hashval_hexstring pidthy) (addr_daliladdrstr alphathy)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | None -> refusecommit := true; "No right to use; document cannot be published unless this is reproven."
			      | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
			    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			    match hlist_lookup_prop_owner true true true pidpure hl with
			    | None ->
				Printf.fprintf oc "** Somehow the theory prop has an owner but the pure prop %s (%s) did not. Invariant failure. **\n"
				  (hashval_hexstring pidpure)
				  (addr_daliladdrstr alphapure)
			    | Some(beta,r) ->
				Printf.fprintf oc "  Pure Prop %s (%s) Owner %s: %s\n" (hashval_hexstring pidpure) (addr_daliladdrstr alphapure)
				  (addr_daliladdrstr (payaddr_addr beta))
				  (match r with
				  | None -> refusecommit := true; "No right to use; document cannot be published unless this is redefined."
				  | Some(r) -> if r = 0L then "free to use" else Printf.sprintf "each use costs %Ld cants" r);
			  with Not_found ->
			    refusecommit := true;
			    Printf.fprintf oc "  Did not find owner of theory proposition %s at %s when checking. Unexpected case.\n"
			      (hashval_hexstring pidthy) (addr_daliladdrstr alphathy))
			usesprops;
		      if !refusecommit then
			Printf.fprintf oc "Refusing to commit to the draft until the issues above are result.\n"
		      else
			match nonce with
			| None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
			| Some(nonce) ->
			    match gamma with
			    | None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
			    | Some(gamma) ->
				if payaddr_p gamma then
				  let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair nonce (hashopair2 th doch))) in
				  mkcommittx blkh lr beta
				else
				  raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
		end
	  else
	    begin
	      close_in ch;
	      raise (Failure (Printf.sprintf "Draft file has incorrect header: %s" l))
	    end
      | _ -> raise BadCommandForm);
  ac "publishdraft" "publishdraft <draftfile> <newtxfile>" "Form a transaction to publish a committed draft file."
    (fun oc al ->
      match al with
      | [f;g] ->
	  let ch = open_in f in
	  let l = input_token ch in
	  if l = "Theory" then
	    let (thyspec,nonce,gamma,_,_,propownsh,proprightsh) = input_theoryspec ch in
	    begin
	      match Checking.check_theoryspec thyspec with
	      | None -> raise (Failure "Theory spec does not check.\n")
	      | Some(thy,sg) ->
		  match hashtheory thy with
		  | None ->
		      Printf.fprintf oc "Theory is empty. It is correct but an empty theory is not allowed to be published.\n"
		  | Some(thyh) ->
		      match get_bestblock_print_warnings oc with
		      | None -> Printf.fprintf oc "No blocks yet\n"
		      | Some(h,lbk,ltx) ->
			  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
			  let (_,_,lr,tr,_) = Hashtbl.find validheadervals (lbk,ltx) in
			  try
			    let tht = lookup_thytree tr in
			    let _ = ottree_lookup tht (Some(thyh)) in
			    Printf.fprintf oc "Theory %s has already been published.\n" (hashval_hexstring thyh)
			  with Not_found ->
			    match nonce with
			    | None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
			    | Some(h) ->
				match gamma with
				| None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
				| Some(gamma) ->
				    if payaddr_p gamma then
				      let gammap = let (i,x0,x1,x2,x3,x4) = gamma in (i = 1,x0,x1,x2,x3,x4) in
				      let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair h thyh)) in
				      begin
					try
					  let (markerid,bday,obl) = find_marker_at_address (CHash(lr)) beta in
					  try
					    if Int64.add bday 3L <= blkh then
					      begin
						let b = theoryspec_burncost thyspec in
						try
						  let delta = hashval_pub_addr thyh in
						  let txoutl = [(delta,(None,TheoryPublication(gammap,h,thyspec)))] in
						  let txoutlr = ref txoutl in
						  let (_,kl) = thy in
						  List.iter
						    (fun h ->
						      let gamma1p =
							try
							  Hashtbl.find propownsh h
							with Not_found -> gammap
						      in
						      let (gamma2p,rp) =
							try
							  Hashtbl.find proprightsh h
							with Not_found -> (gamma1p,Some(0L))
						      in
						      let h2 = hashtag (hashopair2 (Some(thyh)) h) 33l in
						      txoutlr := (hashval_term_addr h,(Some(gamma1p,0L,false),OwnsProp(h,gamma2p,rp)))::(hashval_term_addr h2,(Some(gamma1p,0L,false),OwnsProp(h2,gamma2p,rp)))::!txoutlr)
						    kl;
						  let esttxbytes = 2000 + stxsize (([],!txoutlr),([],[])) in (** rough overestimate for txin and signatures at 2000 bytes **)
						  let minfee = Int64.mul (Int64.of_int esttxbytes) !Config.defaulttxfee in
						  let minamt = Int64.add b minfee in
						  let (alpha,(aid,_,_,_),v) = find_spendable_utxo oc lr blkh minamt in
						  let change = Int64.sub v minamt in
						  if change >= 10000L then txoutlr := (alpha,(None,Currency(change)))::!txoutlr;
						  let txinl = [(alpha,aid);(beta,markerid)] in
						  let stau = ((txinl,!txoutlr),([],[])) in
						  let c2 = open_out_bin g in
						  begin
						    try
						      Commands.signtxc oc lr stau c2 None;
						      let p = pos_out c2 in
						      close_out c2;
						      if p > 450000 then Printf.fprintf oc "Warning: The transaction has %d bytes and may be too large to be confirmed in a block.\n" p;
						      Printf.fprintf oc "The transaction to publish the theory was created.\nTo inspect it:\n> decodetxfile %s\nTo validate it:\n> validatetxfile %s\nTo send it:\n> sendtxfile %s\n" g g g
						    with e ->
						      close_out c2;
						      raise e
						  end
						with Not_found ->
						  Printf.fprintf oc "Cannot find a spendable utxo to use to publish the marker.\n"
					      end
					    else
					      Printf.fprintf oc "The commitment will mature after %Ld more blocks.\nThe draft can only be published after the commitment matures.\n" (Int64.sub (Int64.add bday 3L) blkh)
					  with Not_found -> Printf.fprintf oc "Could not find a utxo sufficient to fund publication tx.\n"
					with Not_found ->
					  Printf.fprintf oc "No commitment marker for this draft found.\nUse commitdraft to create and publish a commitment marker.\n"
				      end
				    else
				      raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
	    end
	  else if l = "Signature" then
	    let thyid = input_token ch in
	    let th = if thyid = "Empty" then None else Some(hexstring_hashval thyid) in
	    let (blkh,lr,tr,sr) =
	      match get_bestblock_print_warnings oc with
	      | None -> raise Not_found
	      | Some(dbh,lbk,ltx) ->
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  let (_,_,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		  (blkh,lr,tr,sr)
	    in
	    let tht = lookup_thytree tr in
	    let thy = try ottree_lookup tht th with Not_found -> raise (Failure (Printf.sprintf "Theory %s not found" thyid)) in
	    let sgt = lookup_sigtree sr in
	    let (signaspec,nonce,gamma,_,objhrev,_,prophrev) = input_signaspec ch th sgt in
	    begin
	      let remgvtpth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let remgvknth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let gvtp th1 h1 a =
		if th1 = th then
		  let oid = hashtag (hashopair2 th (hashpair h1 (hashtp a))) 32l in
		  let alpha = hashval_term_addr oid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_obj_owner true true true oid hl with
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvtpth oid (beta,r); true
		else
		  false
	      in
	      let gvkn th1 k =
		if th1 = th then
		  let pid = hashtag (hashopair2 th k) 33l in
		  let alpha = hashval_term_addr pid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_prop_owner true true true pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvknth pid (beta,r); true
		else
		  false
	      in
	      match Checking.check_signaspec gvtp gvkn th thy sgt signaspec with
	      | None -> raise (Failure "Signature does not check.\n")
	      | Some((tml,knl),imported) ->
		  let id = hashopair2 th (hashsigna (signaspec_signa signaspec)) in
		  let delta = hashval_pub_addr id in
		  let hldelta = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq delta) in
		  if not (hldelta = HNil) then raise (Failure "Signature already seems to have been published.");
		  Printf.fprintf oc "Signature is correct and has id %s and address %s.\n" (hashval_hexstring id) (addr_daliladdrstr (hashval_pub_addr id));
		  Printf.fprintf oc "Signature imports %d signatures:\n" (List.length imported);
		  List.iter (fun h -> Printf.fprintf oc " %s\n" (hashval_hexstring h)) imported;
		  let oname h =
		    try
		      Hashtbl.find objhrev h
		    with Not_found -> ""
		  in
		  let pname h =
		    try
		      Hashtbl.find prophrev h
		    with Not_found -> ""
		  in
		  Printf.fprintf oc "Signature exports %d objects:\n" (List.length tml);
		  List.iter (fun ((h,_),m) -> Printf.fprintf oc " '%s' %s %s\n" (oname h) (hashval_hexstring h) (match m with None -> "(opaque)" | Some(_) -> "(transparent)")) tml;
		  Printf.fprintf oc "Signature exports %d props:\n" (List.length knl);
		  List.iter (fun (h,_) -> Printf.fprintf oc " '%s' %s\n" (pname h) (hashval_hexstring h)) knl;
		  let usesobjs = signaspec_uses_objs signaspec in
		  let usesprops = signaspec_uses_props signaspec in
		  let refusesig = ref false in
		  Printf.fprintf oc "Signature uses %d objects:\n" (List.length usesobjs);
		  List.iter
		    (fun (oidpure,k) ->
		      let oidthy = hashtag (hashopair2 th (hashpair oidpure k)) 32l in
		      let alphapure = hashval_term_addr oidpure in
		      let alphathy = hashval_term_addr oidthy in
		      let nm = oname oidpure in
		      try
			let (beta,r) = local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy in
			Printf.fprintf oc " Theory Object '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring oidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | Some(0L) -> "free to use"
			  | _ -> refusesig := true; "not free to use; signature cannot be published unless you redefine the object or buy the object and make it free for everyone.");
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_obj_owner true true true oidpure hl with
			| None ->
			    refusesig := true;
			    Printf.fprintf oc "** Somehow the theory object has an owner but the pure object %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring oidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc " Pure Object '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring oidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | Some(0L) -> "free to use"
			      | _ -> refusesig := true; "not free to use; signature cannot be published unless you redefine the object or buy the object and make it free for everyone.");
		      with Not_found ->
			refusesig := true;
			Printf.fprintf oc "  Did not find owner of theory object %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring oidthy) (addr_daliladdrstr alphathy))
		    usesobjs;
		  Printf.fprintf oc "Signature uses %d props:\n" (List.length usesprops);
		  List.iter
		    (fun pidpure ->
		      let pidthy = hashtag (hashopair2 th pidpure) 33l in
		      let alphapure = hashval_term_addr pidpure in
		      let alphathy = hashval_term_addr pidthy in
		      let nm = pname pidpure in
		      try
			let (beta,r) = local_lookup_prop_thy_owner lr remgvknth pidthy alphathy in
			Printf.fprintf oc " Theory Prop '%s' %s (%s)\n  Owner %s: %s\n" nm (hashval_hexstring pidthy) (addr_daliladdrstr alphathy)
			  (addr_daliladdrstr (payaddr_addr beta))
			  (match r with
			  | Some(0L) -> "free to use"
			  | _ -> refusesig := true; "not free to use; signature cannot be published unless you buy the proposition and make it free for everyone.");
			let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
			match hlist_lookup_prop_owner true true true pidpure hl with
			| None ->
			    Printf.fprintf oc "** Somehow the theory prop has an owner but the pure prop %s (%s) did not. Invariant failure. **\n"
			      (hashval_hexstring pidpure)
			      (addr_daliladdrstr alphapure)
			| Some(beta,r) ->
			    Printf.fprintf oc "  Pure Prop %s (%s)\n  Owner %s: %s\n" (hashval_hexstring pidpure) (addr_daliladdrstr alphapure)
			      (addr_daliladdrstr (payaddr_addr beta))
			      (match r with
			      | Some(0L) -> "free to use"
			      | _ -> refusesig := true; "not free to use; signature cannot be published unless you buy the proposition and make it free for everyone.");
		      with Not_found ->
			refusesig := true;
			Printf.fprintf oc "  Did not find owner of theory proposition %s at %s when checking. Unexpected case.\n"
			  (hashval_hexstring pidthy) (addr_daliladdrstr alphathy))
		    usesprops;
		  if !refusesig then
		    Printf.fprintf oc "Cannot publish signature without resolving the issues above.\n"
		  else
		    match nonce with
		    | None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
		    | Some(nonce) ->
			match gamma with
			| None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
			| Some(gamma) ->
			    if payaddr_p gamma then
			      let gammap = let (i,x0,x1,x2,x3,x4) = gamma in (i = 1,x0,x1,x2,x3,x4) in
			      let signaspech = hashsigna (signaspec_signa signaspec) in
			      let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair nonce (hashopair2 th signaspech))) in
			      begin
				try
				  let (markerid,bday,obl) = find_marker_at_address (CHash(lr)) beta in
				  try
				    if Int64.add bday 3L <= blkh then
				      begin
					let b = signaspec_burncost signaspec in
					let txinlr = ref [(beta,markerid)] in
					let txoutlr = ref [(delta,(None,SignaPublication(gammap,nonce,th,signaspec)))] in
					let esttxbytes = 2000 + stxsize (([],!txoutlr),([],[])) in (** rough overestimate for txin, possible change and signatures at 2000 bytes **)
					let minfee = Int64.mul (Int64.of_int esttxbytes) !Config.defaulttxfee in
					let tospend = ref (Int64.add b minfee) in
					try
					  let (alpha,(aid,_,_,_),v) = find_spendable_utxo oc lr blkh !tospend in
					  let tauin = (alpha,aid)::!txinlr in
					  let tauout = if Int64.sub v !tospend >= 10000L then (alpha,(None,Currency(Int64.sub v !tospend)))::!txoutlr else !txoutlr in
					  let stau = ((tauin,tauout),([],[])) in
					  let c2 = open_out_bin g in
					  begin
					    try
					      Commands.signtxc oc lr stau c2 None;
					      let p = pos_out c2 in
					      close_out c2;
					      if p > 450000 then Printf.fprintf oc "Warning: The transaction has %d bytes and may be too large to be confirmed in a block.\n" p;
					      Printf.fprintf oc "The transaction to publish the signature was created.\nTo inspect it:\n> decodetxfile %s\nTo validate it:\n> validatetxfile %s\nTo send it:\n> sendtxfile %s\n" g g g
					    with e ->
					      close_out c2;
					      raise e
					  end
					with Not_found -> Printf.fprintf oc "Could not find a utxo sufficient to fund publication tx.\n"
				      end
				    else
				      Printf.fprintf oc "The commitment will mature after %Ld more blocks.\nThe draft can only be published after the commitment matures.\n" (Int64.sub (Int64.add bday 3L) blkh)
				  with Not_found -> Printf.fprintf oc "Not_found was raised while trying to construct the publication tx.\n"
				with Not_found ->
				  Printf.fprintf oc "No commitment marker for this draft found.\nUse commitdraft to create and publish a commitment marker.\n"
			      end
			    else
			      raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
	    end
	  else if l = "Document" then
	    let thyid = input_token ch in
	    let th = if thyid = "Empty" then None else Some(hexstring_hashval thyid) in
	    let (blkh,lr,tr,sr) =
	      match get_bestblock_print_warnings oc with
	      | None -> raise Not_found
	      | Some(dbh,lbk,ltx) ->
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  let (_,_,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		  (blkh,lr,tr,sr)
	    in
	    let tht = lookup_thytree tr in
	    let thy = try ottree_lookup tht th with Not_found -> raise (Failure (Printf.sprintf "Theory %s not found" thyid)) in
	    let sgt = lookup_sigtree sr in
	    let (dl,nonce,gamma,paramh,objhrev,proph,prophrev,conjh,objownsh,objrightsh,propownsh,proprightsh,bountyh) = input_doc ch th sgt in
	    let id = hashopair2 th (hashdoc dl) in
	    let delta = hashval_pub_addr id in
	    let hldelta = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq delta) in
	    if not (hldelta = HNil) then raise (Failure "Document already seems to have been published.");
	    begin
	      let remgvtpth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let remgvknth : (hashval,payaddr * int64 option) Hashtbl.t = Hashtbl.create 100 in
	      let gvtp th1 h1 a =
		if th1 = th then
		  let oid = hashtag (hashopair2 th (hashpair h1 (hashtp a))) 32l in
		  let alpha = hashval_term_addr oid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_obj_owner true true true oid hl with
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvtpth oid (beta,r); true
		else
		  false
	      in
	      let gvkn th1 k =
		if th1 = th then
		  let pid = hashtag (hashopair2 th k) 33l in
		  let alpha = hashval_term_addr pid in
		  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alpha) in
		  match hlist_lookup_prop_owner true true true pid hl with (*** A proposition has been proven in a theory iff it has an owner. ***)
		  | None -> false
		  | Some(beta,r) -> Hashtbl.add remgvknth pid (beta,r); true
		else
		  false
	      in
	      match Checking.check_doc gvtp gvkn th thy sgt dl with
	      | None -> raise (Failure "Document does not check.\n")
	      | Some(_) ->
		  Printf.fprintf oc "Document is correct and has id %s and address %s.\n" (hashval_hexstring id) (addr_daliladdrstr delta);
		  match nonce with
		  | None -> Printf.fprintf oc "No nonce is given. Call addnonce to add one automatically.\n"
		  | Some(nonce) ->
		      match gamma with
		      | None -> Printf.fprintf oc "No publisher address. Call addpublisher to add one.\n"
		      | Some(gamma) ->
			  if payaddr_p gamma then
			    let gammap = let (i,x0,x1,x2,x3,x4) = gamma in (i = 1,x0,x1,x2,x3,x4) in
			    let doch = hashdoc dl in
			    let beta = hashval_pub_addr (hashpair (hashaddr gamma) (hashpair nonce (hashopair2 th doch))) in
			    begin
			      try
				let (markerid,bday,obl) = find_marker_at_address (CHash(lr)) beta in
				try
				  if Int64.add bday 3L <= blkh then
				    begin
				      let tospend = ref 0L in
				      let al = ref [(markerid,bday,obl,Marker)] in
				      let txinlr = ref [(beta,markerid)] in
				      let txoutlr = ref [(delta,(None,DocPublication(gammap,nonce,th,dl)))] in
				      let usesobjs = doc_uses_objs dl in
				      let usesprops = doc_uses_props dl in
				      let createsobjs = doc_creates_objs dl in
				      let createsprops = doc_creates_props dl in
				      let createsnegpropsaddrs2 = List.map (fun h -> hashval_term_addr (hashtag (hashopair2 th h) 33l)) (doc_creates_neg_props dl) in
				      let objrightsassets : (hashval,addr * asset) Hashtbl.t = Hashtbl.create 10 in
				      let proprightsassets : (hashval,addr * asset) Hashtbl.t = Hashtbl.create 10 in
				      List.iter
					(fun (alpha,a,v) ->
					  match a with
					  | (_,_,_,RightsObj(h,_)) -> Hashtbl.add objrightsassets h (alpha,a)
					  | (_,_,_,RightsProp(h,_)) -> Hashtbl.add proprightsassets h (alpha,a)
					  | _ -> ())
					(Commands.get_spendable_assets_in_ledger oc lr blkh);
				      let oname h =
					try
					  Hashtbl.find objhrev h
					with Not_found -> ""
				      in
				      let pname h =
					try
					  Hashtbl.find prophrev h
					with Not_found -> ""
				      in
				      List.iter
					(fun (oidpure,k) ->
					  let oidthy = hashtag (hashopair2 th (hashpair oidpure k)) 32l in
					  let alphapure = hashval_term_addr oidpure in
					  let alphathy = hashval_term_addr oidthy in
					  let (beta,r) = local_lookup_obj_thy_owner lr remgvtpth oidthy alphathy in
					  begin
					    match r with
					    | None -> raise (Failure (Printf.sprintf "No right to use theory object '%s' %s. It must be redefined." (oname oidpure) (hashval_hexstring oidthy)))
					    | Some(i) when i > 0L -> (*** look for owned rights; if not increase 'tospend' to buy the rights ***)
						begin
						  try
						    let (alpha,a) = Hashtbl.find objrightsassets oidthy in
						    match a with
						    | (aid,bday,obl,RightsObj(h,r)) ->
							if r > 0L then
							  begin
							    al := a::!al;
							    txinlr := (alpha,aid)::!txinlr;
							    if r > 1L then
							      txoutlr := (alpha,(obl,RightsObj(h,Int64.sub r 1L)))::!txoutlr
							  end
							else
							  raise Not_found
						    | _ -> raise Not_found
						  with Not_found ->
						    tospend := Int64.add !tospend i
						end
					    | _ -> ()
					  end;
					  begin
					    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
					    match hlist_lookup_obj_owner true true true oidpure hl with
					    | None -> raise (Failure (Printf.sprintf "** Somehow the theory object has an owner but the pure object %s (%s) did not. Invariant failure. **" (hashval_hexstring oidpure) (addr_daliladdrstr alphapure)))
					    | Some(beta,r) ->
						match r with
						| None -> raise (Failure (Printf.sprintf "No right to use pure object '%s' %s. It must be redefined." (oname oidpure) (hashval_hexstring oidpure)))
						| Some(i) when i > 0L -> (*** look for owned rights; if not increase 'tospend' to buy the rights ***)
						    begin
						      try
							let (alpha,a) = Hashtbl.find objrightsassets oidpure in
							match a with
							| (aid,bday,obl,RightsObj(h,r)) ->
							    if r > 0L then
							      begin
								al := a::!al;
								txinlr := (alpha,aid)::!txinlr;
								if r > 1L then
								  txoutlr := (alpha,(obl,RightsObj(h,Int64.sub r 1L)))::!txoutlr
							      end
							    else
							      raise Not_found
							| _ -> raise Not_found
						      with Not_found ->
							tospend := Int64.add !tospend i
						    end
						| _ -> ()
					  end)
					usesobjs;
				      List.iter
					(fun pidpure ->
					  let pidthy = hashtag (hashopair2 th pidpure) 33l in
					  let alphapure = hashval_term_addr pidpure in
					  let alphathy = hashval_term_addr pidthy in
					  let (beta,r) = local_lookup_prop_thy_owner lr remgvknth pidthy alphathy in
					  begin
					    match r with
					    | None -> raise (Failure (Printf.sprintf "No right to use theory proposition '%s' %s. It must be redefined." (pname pidpure) (hashval_hexstring pidthy)))
					    | Some(i) when i > 0L -> (*** look for owned rights; if not increase 'tospend' to buy the rights ***)
						begin
						  try
						    let (alpha,a) = Hashtbl.find proprightsassets pidthy in
						    match a with
						    | (aid,bday,obl,RightsProp(h,r)) ->
							if r > 0L then
							  begin
							    al := a::!al;
							    txinlr := (alpha,aid)::!txinlr;
							    if r > 1L then
							      txoutlr := (alpha,(obl,RightsProp(h,Int64.sub r 1L)))::!txoutlr
							  end
							else
							  raise Not_found
						    | _ -> raise Not_found
						  with Not_found ->
						    tospend := Int64.add !tospend i
						end
					    | _ -> ()
					  end;
					  begin
					    let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
					    match hlist_lookup_prop_owner true true true pidpure hl with
					    | None -> raise (Failure (Printf.sprintf "** Somehow the theory proposition has an owner but the pure object %s (%s) did not. Invariant failure. **" (hashval_hexstring pidpure) (addr_daliladdrstr alphapure)))
					    | Some(beta,r) ->
						match r with
						| None -> raise (Failure (Printf.sprintf "No right to use pure proposition '%s' %s. It must be redefined." (pname pidpure) (hashval_hexstring pidpure)))
						| Some(i) when i > 0L -> (*** look for owned rights; if not increase 'tospend' to buy the rights ***)
						    begin
						      try
							let (alpha,a) = Hashtbl.find proprightsassets pidpure in
							match a with
							| (aid,bday,obl,RightsProp(h,r)) ->
							    if r > 0L then
							      begin
								al := a::!al;
								txinlr := (alpha,aid)::!txinlr;
								if r > 1L then
								  txoutlr := (alpha,(obl,RightsProp(h,Int64.sub r 1L)))::!txoutlr
							      end
							    else
							      raise Not_found
							| _ -> raise Not_found
						      with Not_found ->
							tospend := Int64.add !tospend i
						    end
						| _ -> ()
					  end)
					usesprops;
				      List.iter
					(fun (h,k) ->
					  let oidpure = h in
					  let oidthy = hashtag (hashopair2 th (hashpair h k)) 32l in
					  let alphapure = hashval_term_addr oidpure in
					  let alphathy = hashval_term_addr oidthy in
					  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
					  begin
					    match hlist_lookup_obj_owner true true true oidpure hl with
					    | Some(_) -> ()
					    | None ->
						let delta1 = try Hashtbl.find objownsh oidpure with Not_found -> gammap in
						let (delta2,r) = try Hashtbl.find objrightsh oidpure with Not_found -> (gammap,Some(0L)) in
						txoutlr := (alphapure,(Some(delta1,0L,false),OwnsObj(oidpure,delta2,r)))::!txoutlr
					  end;
					  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
					  begin
					    match hlist_lookup_obj_owner true true true oidthy hl with
					    | Some(_) -> ()
					    | None ->
						let delta1 = try Hashtbl.find objownsh oidpure with Not_found -> gammap in
						let (delta2,r) = try Hashtbl.find objrightsh oidpure with Not_found -> (gammap,Some(0L)) in
						txoutlr := (alphathy,(Some(delta1,0L,false),OwnsObj(oidthy,delta2,r)))::!txoutlr
					  end)
					createsobjs;
				      List.iter
					(fun pidpure ->
					  let pidthy = hashtag (hashopair2 th pidpure) 33l in
					  let alphapure = hashval_term_addr pidpure in
					  let alphathy = hashval_term_addr pidthy in
					  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphapure) in
					  begin
					    match hlist_lookup_prop_owner true true true pidpure hl with
					    | Some(_) -> ()
					    | None ->
						let delta1 = try Hashtbl.find propownsh pidpure with Not_found -> gammap in
						let (delta2,r) = try Hashtbl.find proprightsh pidpure with Not_found -> (gammap,Some(0L)) in
						txoutlr := (alphapure,(Some(delta1,0L,false),OwnsProp(pidpure,delta2,r)))::!txoutlr
					  end;
					  let hl = ctree_lookup_addr_assets true true (CHash(lr)) (addr_bitseq alphathy) in
					  begin
					    match hlist_lookup_prop_owner true true true pidthy hl with
					    | Some(_) -> ()
					    | None ->
						let delta1 = try Hashtbl.find propownsh pidpure with Not_found -> gammap in
						let (delta2,r) = try Hashtbl.find proprightsh pidpure with Not_found -> (gammap,Some(0L)) in
						txoutlr := (alphathy,(Some(delta1,0L,false),OwnsProp(pidthy,delta2,r)))::!txoutlr
					  end)
					createsprops;
				      List.iter
					(fun alpha -> txoutlr := (alpha,(Some(gammap,0L,false),OwnsNegProp))::!txoutlr)
					createsnegpropsaddrs2;
				      Hashtbl.iter
					(fun pidpure (amt,olkh) ->
					  let pidthy = hashtag (hashopair2 th pidpure) 33l in
					  let alphathy = hashval_term_addr pidthy in
					  tospend := Int64.add amt !tospend;
					  match olkh with
					  | None -> txoutlr := (alphathy,(None,Bounty(amt)))::!txoutlr
					  | Some(deltap,lkh) -> txoutlr := (alphathy,(Some(deltap,lkh,false),Bounty(amt)))::!txoutlr)
					bountyh;
				      try
					let esttxbytes = 2000 + stxsize ((!txinlr,!txoutlr),([],[])) + 200 * estimate_required_signatures !al (!txinlr,!txoutlr) in (** rough overestimate for funding asset, possible change and signature for the funding asset 2000 bytes; overestimate of 200 bytes per other signature **)
					let minfee = Int64.mul (Int64.of_int esttxbytes) !Config.defaulttxfee in
					tospend := Int64.add !tospend minfee;
					let (alpha,(aid,_,_,_),v) = find_spendable_utxo oc lr blkh !tospend in
					let tauin = (alpha,aid)::!txinlr in
					let tauout = if Int64.sub v !tospend > 10000L then (alpha,(None,Currency(Int64.sub v !tospend)))::!txoutlr else !txoutlr in
					let stau = ((tauin,tauout),([],[])) in
					let c2 = open_out_bin g in
					begin
					  try
					    Commands.signtxc oc lr stau c2 None;
					    let p = pos_out c2 in
					    close_out c2;
					    if p > 450000 then Printf.fprintf oc "Warning: The transaction has %d bytes and may be too large to be confirmed in a block.\n" p;
					    Printf.fprintf oc "The transaction to publish the document was created.\nTo inspect it:\n> decodetxfile %s\nTo validate it:\n> validatetxfile %s\nTo send it:\n> sendtxfile %s\n" g g g
					  with e ->
					    close_out c2;
					    raise e
					end
				      with Not_found -> Printf.fprintf oc "Could not find a utxo sufficient to fund publication tx.\n"
				    end
				  else
				    Printf.fprintf oc "The commitment will mature after %Ld more blocks.\nThe draft can only be published after the commitment matures.\n" (Int64.sub (Int64.add bday 3L) blkh)
				with Not_found -> Printf.fprintf oc "Not_found was raised while trying to create the publication tx.\n"
			      with Not_found ->
				Printf.fprintf oc "No commitment marker for this draft found.\nUse commitdraft to create and publish a commitment marker.\n"
			    end
			  else
			    raise (Failure (Printf.sprintf "Publisher address %s is not a pay address." (Cryptocurr.addr_daliladdrstr gamma)))
	    end
	  else
	    begin
	      close_in ch;
	      raise (Failure (Printf.sprintf "Draft file has incorrect header: %s" l))
	    end
      | _ -> raise BadCommandForm);
  ac "missing" "missing" "Report current list of missing headers/deltas"
    (fun oc al ->
      Printf.fprintf oc "%d missing headers\n" (List.length !missingheaders);
      List.iter
	(fun (i,h) -> Printf.fprintf oc "%Ld %s\n" i (hashval_hexstring h))
	!missingheaders;
      Printf.fprintf oc "%d missing deltas\n" (List.length !missingdeltas);
      List.iter
	(fun (i,h) -> Printf.fprintf oc "%Ld %s\n" i (hashval_hexstring h))
	!missingdeltas;
      );
  ac "reportowned" "reportowned [<outputfile> [<ledgerroot>]]" "Give a report of all owned objects and propositions in the ledger tree."
    (fun oc al ->
      match al with
      | [] ->
	  let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
	  Commands.reportowned oc oc lr
      | [fn] ->
	  let f = open_out fn in
	  let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
	  begin
	    try
	      Commands.reportowned oc f lr;
	      close_out f
	    with exn -> close_out f; raise exn
	  end
      | [fn;lr] ->
	  let f = open_out fn in
	  begin
	    try
	      Commands.reportowned oc f (hexstring_hashval lr);
	      close_out f
	    with exn -> close_out f; raise exn
	  end
      | _ -> raise BadCommandForm);
  ac "reportbounties" "reportbounties [<outputfile> [<ledgerroot>]]" "Give a report of all bounties in the ledger tree."
    (fun oc al ->
      match al with
      | [] ->
	  let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
	  Commands.reportbounties oc oc lr
      | [fn] ->
	  let f = open_out fn in
	  let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
	  begin
	    try
	      Commands.reportbounties oc f lr;
	      close_out f
	    with exn -> close_out f; raise exn
	  end
      | [fn;lr] ->
	  let f = open_out fn in
	  begin
	    try
	      Commands.reportbounties oc f (hexstring_hashval lr);
	      close_out f
	    with exn -> close_out f; raise exn
	  end
      | _ -> raise BadCommandForm);
  ac "collectbounties" "collectbounties <outputaddress> <txfileout> [<ledgerroot>]" "Create a tx (stored in a file) paying all collectable bounties (if there are any) to the output address."
    (fun oc al ->
      let collb gammas fn lr =
	  let gamma = Cryptocurr.daliladdrstr_addr gammas in
	  if not (payaddr_p gamma) then raise (Failure (Printf.sprintf "Address %s is not a pay address." gammas));
	  let cbl = Commands.collectable_bounties oc lr in
	  if cbl = [] then
	    Printf.fprintf oc "No bounties can be collected.\n"
	  else
	    let txinl = ref [] in
	    let txoutl = ref [] in
	    let vtot = ref 0L in
	    List.iter
	      (fun (alpha,a1,a2) ->
		match (a1,a2) with
		| ((aid1,_,_,Bounty(v)),(aid2,_,obl2,pre2)) ->
		    vtot := Int64.add !vtot v;
		    txinl := (alpha,aid1)::!txinl;
		    if not (List.exists (fun (_,aid2b) -> aid2b = aid2) !txinl) then
		      begin
			txinl := (alpha,aid2)::!txinl;
			txoutl := (alpha,(obl2,pre2))::!txoutl
		      end
		| _ -> ())
	      cbl;
	    let esttxbytes = 2000 + stxsize ((!txinl,!txoutl),([],[])) in
	    let minfee = Int64.mul (Int64.of_int esttxbytes) !Config.defaulttxfee in
	    if !vtot < minfee then
	      Printf.fprintf oc "Total bounties are less than the tx fee, so refusing to make the tx.\n"
	    else
	      begin
		let totminusfee = Int64.sub !vtot minfee in
		txoutl := (gamma,(None,Currency(totminusfee)))::!txoutl;
		let stau = ((!txinl,!txoutl),([],[])) in
		let c2 = open_out_bin fn in
		begin
		  try
		    Commands.signtxc oc lr stau c2 None;
		    close_out c2;
		    Printf.fprintf oc "Transaction created to claim %s fraenks from bounties.\nTo validate it:\n> validatetxfile %s\nTo send it:\n> sendtxfile %s\n" (Cryptocurr.fraenks_of_cants totminusfee) fn fn
		  with e ->
		    close_out c2;
		    raise e
		end
	      end
      in
      match al with
      | [gammas;fn] -> let lr = get_ledgerroot (get_bestblock_print_warnings oc) in collb gammas fn lr
      | [gammas;fn;lr] -> collb gammas fn (hexstring_hashval lr)
      | _ -> raise BadCommandForm);
  ac "reportpubs" "reportpubs [<outputfile> [<ledgerroot>]]" "Give a report of all publications in the ledger tree."
    (fun oc al ->
      match al with
      | [] ->
	  let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
	  Commands.reportpubs oc oc lr
      | [fn] ->
	  let f = open_out fn in
	  let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
	  begin
	    try
	      Commands.reportpubs oc f lr;
	      close_out f
	    with exn -> close_out f; raise exn
	  end
      | [fn;lr] ->
	  let f = open_out fn in
	  begin
	    try
	      Commands.reportpubs oc f (hexstring_hashval lr);
	      close_out f
	    with exn -> close_out f; raise exn
	  end
      | _ -> raise BadCommandForm);
  ac "setbestblock" "setbestblock <blockid> [<blockheight> <ltcblockid> <ltcburntx>]" "Manually set the current best block. This is mostly useful if -ltcoffline is being used."
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    let h = hexstring_hashval a in
	    try
	      let bh = DbBlockHeader.dbget h in
	      let (bhd,_) = bh in
	      begin
		try
		  let (lbk,ltx) = get_burn h in
		  artificialbestblock := Some(h,lbk,ltx);
		  artificialledgerroot := Some(bhd.newledgerroot)
		with Not_found ->
		  Printf.fprintf oc "Cannot find burn for block.\n"
	      end
	    with Not_found ->
	      Printf.fprintf oc "Unknown block.\n"
	  end
      | [a;lblk;ltx] ->
	  begin
	    let h = hexstring_hashval a in
	    let lblk = hexstring_md256 lblk in
	    let ltx = hexstring_md256 ltx in
	    artificialbestblock := Some(h,lblk,ltx);
	  end
      | [a;_;lblk;ltx] -> (*** ignore blkh (second argument), but leave this format for backwards compatibility ***)
	  begin
	    let h = hexstring_hashval a in
	    let lblk = hexstring_md256 lblk in
	    let ltx = hexstring_md256 ltx in
	    artificialbestblock := Some(h,lblk,ltx);
	  end
      | _ ->
	  raise BadCommandForm);
  ac "setledgerroot" "setledgerroot <ledgerroot or blockhash>" "Manually set the current ledger root, either by giving the ledger root (Merkle root of a ctree)\nor by giving the hash of a block containing the new ledger root."
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    let h = hexstring_hashval a in
	    try
	      let (bhd,_) = DbBlockHeader.dbget h in
	      artificialledgerroot := Some(bhd.newledgerroot)
	    with Not_found ->
	      artificialledgerroot := Some(h)
	  end
      | _ -> raise BadCommandForm);
  ac "verifyfullledger" "verifyfullledger [<ledgerroot>]" "Ensure the node has the full ledger with the given ledger root. This may take serveral hours."
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    let h = hexstring_hashval a in
	    Commands.verifyfullledger oc h
	  end
      | [] ->
	  begin
	    try
	      let ledgerroot = get_ledgerroot (get_bestblock_print_warnings oc) in
	      Commands.verifyfullledger oc ledgerroot
	    with e ->
	      Printf.fprintf oc "Exception: %s\n" (Printexc.to_string e)
	  end
      | _ -> raise BadCommandForm);
  ac "requestfullledger" "requestfullledger [<ledgerroot>]" "try to request the full ledger from peers\nThis is an experimental command and can take several hours.\nCurrently it is more likely to be successful if the node already has most of the ledger.\nIf you have very little of the full ledger and you want it, consider downloading the initial full ledger from\nhttps://mega.nz/#!waQE1DiC!yRo9vTYPK9CZsfOxT-6eJ7vtl3WLeIMqK4LAcA2ASKc"
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    let h = hexstring_hashval a in
	    Commands.requestfullledger oc h
	  end
      | [] ->
	  begin
	    try
	      let ledgerroot = get_ledgerroot (get_bestblock_print_warnings oc) in
	      Commands.requestfullledger oc ledgerroot
	    with e ->
	      Printf.fprintf oc "Exception: %s\n" (Printexc.to_string e)
	  end
      | _ -> raise BadCommandForm);
  ac "requestblock" "requestblock <blockhash>" "Manually request a missing block from peers, if possible.\nThis is mostly useful if -ltcoffline is set.\nUnder normal operations dalilcoin will request the block when its hash is seen in the ltc burn tx."
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    let h = hexstring_hashval a in
	    try
	      if DbInvalidatedBlocks.dbexists h then DbInvalidatedBlocks.dbdelete h;
	      if DbBlacklist.dbexists h then DbBlacklist.dbdelete h;
	      if DbBlockHeader.dbexists h then
		Printf.fprintf oc "Already have header.\n"
	      else
		begin
		  find_and_send_requestdata GetHeader h;
		  Printf.fprintf oc "Block header requested.\n"
		end;
	      try
		if DbBlockDelta.dbexists h then
		  Printf.fprintf oc "Already have delta.\n"
		else
		  begin
		    find_and_send_requestdata GetBlockdelta h;
		    Printf.fprintf oc "Block delta requested.\n"
		  end
	      with Not_found ->
		Printf.fprintf oc "No peer has delta %s.\n" a
	    with Not_found ->
	      Printf.fprintf oc "No peer has header %s.\n" a
	  end
      | _ -> raise BadCommandForm);
  ac "query" "query <hashval or address or int[block height]> [<blockid or ledgerroot>]" "Get information (in json format) about some item.\nThis is intended to support exporers.\nThe query command gives more detailed information if -extraindex is set to true."
    (fun oc al ->
      match al with
      | [h] ->
	  begin
	    try
	      let blkh = Int64.of_string h in
	      let j = Commands.query_blockheight blkh in
	      print_jsonval oc j;
	      Printf.fprintf oc "\n"
	    with Failure(_) ->
	      let j = Commands.query h in
	      print_jsonval oc j;
	      Printf.fprintf oc "\n"
	  end
      | [h;kh] ->
	  let k = hexstring_hashval kh in
	  begin
	    try
	      let (lbk,ltx) = get_burn k in
	      let (_,lmedtm,burned,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
	      let (_,_,lr,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
	      let pbh = Some(k,Poburn(lbk,ltx,lmedtm,burned)) in
	      let j = Commands.query_at_block h pbh lr blkh in
	      print_jsonval oc j;
	      Printf.fprintf oc "\n"
	    with Not_found ->
	      if DbCTreeElt.dbexists k then
		begin
		  let j = Commands.query_at_block h None k (-1L) in
		  print_jsonval oc j;
		  Printf.fprintf oc "\n"
		end
	      else
		raise (Failure ("could not interpret " ^ kh ^ " as a block or ledger root"))
	  end
      | _ -> raise BadCommandForm);
  ac "dumpwallet" "dumpwallet <filename>" "Dump the current wallet keys, addresses, etc., to a given file."
    (fun oc al ->
      match al with
      | [fn] -> Commands.dumpwallet fn
      | _ -> raise BadCommandForm);
  ac "ltcstatusdump" "ltcstatusdump [<filename> [<ltcblockhash> [<how many ltc blocks back>]]]" "Dump the dalilcoin information about the current ltc status to a given file."
    (fun oc al ->
      let (fn,blkh,howfarback) =
	match al with
	| [] -> ("ltcstatusdumpfile",hexstring_hashval (Ltcrpc.ltc_getbestblockhash ()),1000)
	| [fn] -> (fn,hexstring_hashval (Ltcrpc.ltc_getbestblockhash ()),1000)
	| [fn;hh] -> (fn,hexstring_hashval hh,1000)
	| [fn;hh;b] -> (fn,hexstring_hashval hh,int_of_string b)
	| _ -> raise BadCommandForm
      in
      let cblkh = ref blkh in
      let f = open_out fn in
      begin
	try
	  for i = 1 to howfarback do
	    Printf.fprintf f "%d. ltc block %s DacStatus\n" i (hashval_hexstring !cblkh);
	    begin
	      try
		match DbLtcDacStatus.dbget !cblkh with
		| LtcDacStatusPrev(h) ->
		    Printf.fprintf f "  DacStatus unchanged since ltc block %s\n" (hashval_hexstring h)
		| LtcDacStatusNew(l) ->
		    Printf.fprintf f "  New DacStatus:\n";
		    let cnt = ref 0 in
		    List.iter
		      (fun li ->
			let i = !cnt in
			incr cnt;
			match li with
			| [] -> Printf.fprintf f "   %d. Empty tip? Should not be possible.\n" i;
			| ((bh,lbh,ltx,ltm,lhght)::r) ->
			    Printf.fprintf f "   (%d) - Dalilcoin Block: %s\n        Litecoin Block: %s\n        Litecoin Burn Tx: %s\n        Litecoin Time: %Ld\n        Litecoin Height: %Ld\n" i (hashval_hexstring bh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght;
			    List.iter (fun (bh,lbh,ltx,ltm,lhght) ->
			      Printf.fprintf f "       - Dalilcoin Block: %s\n        Litecoin Block: %s\n        Litecoin Burn Tx: %s\n        Litecoin Time: %Ld\n        Litecoin Height: %Ld\n" (hashval_hexstring bh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght)
			      r)
		      l
	      with Not_found ->
		Printf.fprintf f "  DacStatus not found\n"
	    end;
	    begin
	      try
		let (prevh,tm,hght,burntxhs) = DbLtcBlock.dbget !cblkh in
		Printf.fprintf f "%d. ltc block %s info\n" i (hashval_hexstring !cblkh);
		Printf.fprintf f "   Previous %s\n   Block Time %Ld\n    Height %Ld\n" (hashval_hexstring prevh) tm hght;
		cblkh := prevh;
		match burntxhs with
		| [] -> ()
		| [x] -> Printf.fprintf f "    Burn Tx: %s\n" (hashval_hexstring x)
		| _ ->
		    Printf.fprintf f "    %d Burn Txs:\n" (List.length burntxhs);
		    List.iter (fun x -> Printf.fprintf f "         %s\n" (hashval_hexstring x)) burntxhs
	      with Not_found ->
		Printf.fprintf f "  LtcBlock not found\n"
	    end
	  done
	with e -> Printf.fprintf f "Exception: %s\n" (Printexc.to_string e)
      end;
      close_out f);
  ac "ltcstatus" "ltcstatus [<ltcblockhash>]" "Print the dalilcoin blocks burned into the ltc blockchain from the past week.\nThe topmost is the current best block.\nIt is legal to stake on top of any of the dalilcoin blocks listed by ltcstatus,\n but such a new block will only become the best block if no one stakes on top of the preferred blocks for a week.\nIn case multiple dalilcoin blocks were burned into the same ltc block, all these dalilcoin blocks are preferred equally.\nEventually all but one should be orphaned when someone stakes on top of that one."
    (fun oc al ->
      let h =
	match al with
	| [hh] -> hexstring_hashval hh
	| [] ->
	    Printf.fprintf oc "ltcbest %s\n" (hashval_hexstring !ltc_bestblock);
	    !ltc_bestblock
	| _ -> raise BadCommandForm
      in
      let (lastchangekey,zll) = ltcdacstatus_dbget h in
      let tm = ltc_medtime() in
      if zll = [] && tm > Int64.add !Config.genesistimestamp 604800L then
	begin
	  Printf.fprintf oc "No blocks were created in the past week. Dalilcoin has reached terminal status.\nThe only recovery possible for the network is a hard fork.\n"
	end;
      let i = ref 0 in
      List.iter
	(fun zl ->
	  incr i;
	  Printf.fprintf oc "%d.\n" !i;
	  List.iter
	    (fun (dbh,lbh,ltx,ltm,lhght) ->
	      if DbBlacklist.dbexists dbh then
		Printf.fprintf oc "- %s (blacklisted, presumably invalid) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
	      else if DbInvalidatedBlocks.dbexists dbh then
		Printf.fprintf oc "- %s (marked invalid) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
	      else if Hashtbl.mem validblockvals (lbh,ltx) then
		Printf.fprintf oc "+ %s %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
	      else if Hashtbl.mem validheadervals (lbh,ltx) then
		if DbBlockDelta.dbexists dbh then
		  Printf.fprintf oc "* %s (have delta, but not fully validated) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		else
		  Printf.fprintf oc "* %s (missing delta) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
	      else
		if DbBlockHeader.dbexists dbh then
		  if DbBlockDelta.dbexists dbh then
		    Printf.fprintf oc "* %s (have block, but neither header nor delta fully valided) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		  else
		    Printf.fprintf oc "* %s (missing delta, header not fully validated) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght
		else
		  Printf.fprintf oc "* %s (missing header) %s %s %Ld %Ld\n" (hashval_hexstring dbh) (hashval_hexstring lbh) (hashval_hexstring ltx) ltm lhght)
	    zl)
	zll);
  ac "ltcgettxinfo" "ltcgettxinfo <txid>" "Get dalilcoin related information about an ltc burn tx."
    (fun oc al ->
      match al with
      | [h] ->
	  begin
	    try
	      let (burned,prev,nxt,lblkh,confs) = Ltcrpc.ltc_gettransactioninfo h in
	      match lblkh,confs with
	      | Some(lh),Some(confs) ->
		  Printf.fprintf oc "burned %Ld prev %s next %s in ltc block %s, %d confirmations\n" burned (hashval_hexstring prev) (hashval_hexstring nxt) lh confs
	      | _,_ ->
		  Printf.fprintf oc "burned %Ld prev %s next %s\n" burned (hashval_hexstring prev) (hashval_hexstring nxt)
	    with Not_found -> raise (Failure("problem"))
	  end
      | _ -> raise BadCommandForm);
  ac "ltcgetbestblockhash" "ltcgetbestblockhash" "Get the current tip of the ltc blockchain."
    (fun oc al ->
      if al = [] then
	begin
	  try
	    let x = Ltcrpc.ltc_getbestblockhash () in
	    Printf.fprintf oc "best ltc block hash %s\n" x
	  with Not_found ->
	    Printf.fprintf oc "could not find best ltc block hash\n"
	end
      else
	raise BadCommandForm);
  ac "ltcgetblock" "ltcgetblock <blockid>" "Print dalilcoin related information about the given ltc block."
    (fun oc al ->
      match al with
      | [h] ->
	  begin
	    try
	      let (pbh,tm,hght,txl) = Ltcrpc.ltc_getblock h in
	      Printf.fprintf oc "ltc block %s time %Ld height %Ld prev %s; %d dalilcoin candidate txs:\n" h tm hght pbh (List.length txl);
	      List.iter (fun tx -> Printf.fprintf oc "%s\n" tx) txl
	    with Not_found ->
	      Printf.fprintf oc "could not find ltc block %s\n" h
	  end
      | _ -> raise BadCommandForm);
  ac "ltclistunspent" "ltclistunspent" "List the current relevant utxos in the local ltc wallet.\nThese utxos are used to fund ltc burn txs during the creation of dalilcoin blocks."
    (fun oc al ->
      if al = [] then
	begin
	  try
	    let utxol = Ltcrpc.ltc_listunspent () in
	    Printf.fprintf oc "%d ltc utxos\n" (List.length utxol);
	    List.iter
	      (fun u ->
		match u with
		| LtcP2shSegwit(txid,vout,ltcaddr,_,_,amt) ->
		    Printf.fprintf oc "%s:%d %Ld (%s [p2sh-segwit])\n" txid vout amt ltcaddr
		| LtcBech32(txid,vout,ltcaddr,_,amt) ->
		    Printf.fprintf oc "%s:%d %Ld (%s [bech32])\n" txid vout amt ltcaddr)
	      utxol
	  with Not_found ->
	    Printf.fprintf oc "could not get unspent ltc list\n"
	end
      else
	raise BadCommandForm);
  ac "ltcsigntx" "ltcsigntx <txinhex>" "Use the local ltc wallet to sign an ltc tx."
    (fun oc al ->
      match al with
      | [tx] -> Printf.fprintf oc "%s\n" (Ltcrpc.ltc_signrawtransaction tx)
      | _ -> raise BadCommandForm);
  ac "ltcsendtx" "ltcsendtx <txinhex>" "Use the local ltc wallet to send an ltc tx."
    (fun oc al ->
      match al with
      | [tx] -> Printf.fprintf oc "%s\n" (Ltcrpc.ltc_sendrawtransaction tx)
      | _ -> raise BadCommandForm);
  ac "ltccreateburn" "ltccreateburn <hash1> <hash2> <litoshis to burn>" "Manually create an ltc burn tx to support a newly staked dalilcoin block."
    (fun oc al ->
      match al with
      | [h1;h2;toburn] ->
	  begin
	    try
	      let txs = Ltcrpc.ltc_createburntx (hexstring_hashval h1) (hexstring_hashval h2) (Int64.of_string toburn) in
	      Printf.fprintf oc "burntx: %s\n" (Hashaux.string_hexstring txs)
	    with
	    | Ltcrpc.InsufficientLtcFunds ->
		Printf.fprintf oc "no ltc utxo has %s litoshis\n" toburn
	    | Not_found ->
		Printf.fprintf oc "trouble creating burn tx\n"
	  end
      | _ -> raise BadCommandForm);
  ac "exit" "exit" "exit or stop kills the dalilcoin node"
    (fun oc _ -> (*** Could call Thread.kill on netth and stkth, but Thread.kill is not always implemented. ***)
      closelog();
      Printf.fprintf oc "Shutting down threads. Please be patient.\n"; flush oc;
      !exitfn 0);
  ac "stop" "stop" "exit or stop kills the dalilcoin node"
    (fun oc _ -> (*** Could call Thread.kill on netth and stkth, but Thread.kill is not always implemented. ***)
      closelog();
      Printf.fprintf oc "Shutting down threads. Please be patient.\n"; flush oc;
      !exitfn 0);
  ac "dumpstate" "dumpstate <textfile>" "Dump the current dalilcoin state to a file for debugging."
    (fun oc al ->
      match al with
      | [fa] -> Commands.dumpstate fa
      | _ -> raise BadCommandForm);
  ac "addnode" "addnode <address:port> [add|remove|onetry]" "Add or remove a peer by giving an address or port number.\nThe address may be an ip or an onion address."
    (fun oc al ->
      let addnode_add n =
	match tryconnectpeer n with
	| None -> raise (Failure "Failed to add node")
	| Some(lth,sth,(fd,sin,sout,gcs)) ->
	    match !gcs with
	    | None -> raise (Failure "Problem adding node")
	    | Some(cs) ->
		if cs.addrfrom = "" then Thread.delay 1.0;
		addknownpeer (Int64.of_float cs.conntime) cs.addrfrom
      in
      match al with
      | [n] -> addnode_add n
      | [n;"add"] -> addnode_add n
      | [n;"remove"] ->
          removeknownpeer n;
          List.iter
	    (fun (lth,sth,(fd,sin,sout,gcs)) -> if peeraddr !gcs = n then (shutdown_close fd; gcs := None))
	    !netconns
      | [n;"onetry"] ->
	  ignore (tryconnectpeer n)
      | _ -> raise BadCommandForm);
  ac "clearbanned" "clearbanned" "Clear the list of banned peers."
    (fun _ _ -> clearbanned());
  ac "listbanned" "listbanned" "List the current banned peers."
    (fun oc _ -> Hashtbl.iter (fun n () -> Printf.fprintf oc "%s\n" n) bannedpeers);
  ac "bannode" "bannode [<address:port>] ... [<address:port>]" "ban the given peers"
    (fun _ al -> List.iter (fun n -> banpeer n) al);
  ac "missingblocks" "missingblocks" "Print info about headers and deltas the node is missing.\nTypically a delta is only listed as missing after the header has been received and validated."
    (fun oc al ->
      Printf.fprintf oc "%d missing headers.\n" (List.length !missingheaders);
      List.iter (fun (h,k) -> Printf.fprintf oc "%Ld. %s\n" h (hashval_hexstring k)) !missingheaders;
      Printf.fprintf oc "%d missing deltas.\n" (List.length !missingdeltas);
      List.iter (fun (h,k) -> Printf.fprintf oc "%Ld. %s\n" h (hashval_hexstring k)) !missingdeltas);
  ac "getinfo" "getinfo" "Print a summary of the current dalilcoin node state including:\nnumber of connections, current best block, current difficulty, current balance."
    (fun oc al ->
      remove_dead_conns();
      let ll = List.length !netconns in
      Printf.fprintf oc "%d connection%s\n" ll (if ll = 1 then "" else "s");
      begin
	try
	  begin
	    match get_bestblock_print_warnings oc with
	    | None -> Printf.fprintf oc "No blocks yet\n"
	    | Some(h,lbk,ltx) ->
		let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		let (tar,tmstmp,ledgerroot,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
		let gtm = Unix.gmtime (Int64.to_float tmstmp) in
		Printf.fprintf oc "Best block %s at height %Ld\n" (hashval_hexstring h) blkh;
		Printf.fprintf oc "Time: %Ld (UTC %02d %02d %04d %02d:%02d:%02d)\n" tmstmp gtm.Unix.tm_mday (1+gtm.Unix.tm_mon) (1900+gtm.Unix.tm_year) gtm.Unix.tm_hour gtm.Unix.tm_min gtm.Unix.tm_sec;
		Printf.fprintf oc "Target: %s\n" (string_of_big_int tar);
		Printf.fprintf oc "Difficulty: %s\n" (string_of_big_int (difficulty tar));
		let (bal1,bal1u,bal2,bal2u,bal3,bal3u,bal4,bal4u) = Commands.get_cants_balances_in_ledger oc ledgerroot blkh in
		Printf.fprintf oc "Total p2pkh: %s fraenks (%s unlocked)\n" (fraenks_of_cants bal1) (fraenks_of_cants bal1u);
		Printf.fprintf oc "Total p2sh: %s fraenks (%s unlocked)\n" (fraenks_of_cants bal2) (fraenks_of_cants bal2u);
		Printf.fprintf oc "Total via endorsement: %s fraenks (%s unlocked)\n" (fraenks_of_cants bal3) (fraenks_of_cants bal3u);
		Printf.fprintf oc "Total watched: %s fraenks (%s unlocked)\n" (fraenks_of_cants bal4)  (fraenks_of_cants bal4u);
		Printf.fprintf oc "Sum of all: %s fraenks (%s unlocked)\n"
		  (fraenks_of_cants (Int64.add bal1 (Int64.add bal2 (Int64.add bal3 bal4))))
		  (fraenks_of_cants (Int64.add bal1u (Int64.add bal2u (Int64.add bal3u bal4u))))
	  end;
	with e ->
	  Printf.fprintf oc "Exception: %s\n" (Printexc.to_string e)
      end);
  ac "getpeerinfo" "getpeerinfo" "List the current peers and when the last message was received from each."
    (fun oc al ->
      remove_dead_conns();
      let ll = List.length !netconns in
      Printf.fprintf oc "%d connection%s\n" ll (if ll = 1 then "" else "s");
      List.iter
	(fun (_,_,(_,_,_,gcs)) ->
	  match !gcs with
	  | Some(cs) ->
	      Printf.fprintf oc "%s (%s): %s\n" cs.realaddr cs.addrfrom cs.useragent;
	      let snc1 = sincetime (Int64.of_float cs.conntime) in
	      let snc2 = sincetime (Int64.of_float cs.lastmsgtm) in
	      Printf.fprintf oc "Connected for %s; last message %s ago.\n" snc1 snc2;
	      if cs.handshakestep < 5 then Printf.fprintf oc "(Still in handshake phase)\n";
	  | None -> (*** This could happen if a connection died after remove_dead_conns above. ***)
	      Printf.fprintf oc "[Dead Connection]\n";
	)
	!netconns;
      flush oc);
  ac "nettime" "nettime" "Print the current network time (median of peers) and skew from local node."
    (fun oc al ->
      let (tm,skew) = network_time() in
      Printf.fprintf oc "network time %Ld (median skew of %d)\n" tm skew;
      flush oc);
  ac "invalidateblock" "invalidateblock <blockhash>" "Manually invalidate a dalilcoin block\nThis should be used if someone is attacking the network and nodes decide to ignore their blocks."
    (fun oc al ->
      match al with
      | [h] ->
	  let hh = hexstring_hashval h in
	  recursively_invalidate_blocks hh
      | _ -> raise BadCommandForm);
  ac "revalidateblock" "revalidateblock <blockhash>" "Manually mark a previously manually invalidated block as being valid.\nThis will also mark the previous blocks as valid."
    (fun oc al ->
      match al with
      | [h] ->
	  let hh = hexstring_hashval h in
	  recursively_revalidate_blocks hh
      | _ -> raise BadCommandForm);
  ac "rawblockheader" "rawblockheader <blockhash>" "Print the given block header in hex."
    (fun oc al ->
      match al with
      | [hh] ->
	  begin
	    let h = hexstring_hashval hh in
	    try
	      let bh = DbBlockHeader.dbget h in
	      let sb = Buffer.create 1000 in
	      seosbf (seo_blockheader seosb bh (sb,None));
	      let s = string_hexstring (Buffer.contents sb) in
	      Printf.fprintf oc "%s\n" s;
	    with Not_found ->
	      Printf.fprintf oc "Could not find header %s\n" hh
	  end
      | _ -> raise BadCommandForm);
  ac "rawblockdelta" "rawblockdelta <blockid>" "Print the given block delta in hex."
    (fun oc al ->
      match al with
      | [hh] ->
	  begin
	    let h = hexstring_hashval hh in
	    try
	      let bd = DbBlockDelta.dbget h in
	      let sb = Buffer.create 1000 in
	      seosbf (seo_blockdelta seosb bd (sb,None));
	      let s = string_hexstring (Buffer.contents sb) in
	      Printf.fprintf oc "%s\n" s;
	    with Not_found ->
	      Printf.fprintf oc "Could not find delta %s\n" hh
	  end
      | _ -> raise BadCommandForm);
  ac "rawblock" "rawblock <blockid>" "Print the block (header and delta) in hex."
    (fun oc al ->
      match al with
      | [hh] ->
	  begin
	    let h = hexstring_hashval hh in
	    try
	      let bh = DbBlockHeader.dbget h in
	      try
		let bd = DbBlockDelta.dbget h in
		let sb = Buffer.create 1000 in
		seosbf (seo_block seosb (bh,bd) (sb,None));
		let s = string_hexstring (Buffer.contents sb) in
		Printf.fprintf oc "%s\n" s;
	      with Not_found ->
		Printf.fprintf oc "Could not find delta %s\n" hh
	    with Not_found ->
	      Printf.fprintf oc "Could not find header %s\n" hh
	  end
      | _ -> raise BadCommandForm);
  ac "getblock" "getblock <blockhash>" "Print information about the block, or request it from a peer if it is missing."
    (fun oc al ->
      match al with
      | [hh] ->
	  begin
	    let h = hexstring_hashval hh in
	    try
	      let (bhd,_) = DbBlockHeader.dbget h in
	      Printf.fprintf oc "Time: %Ld\n" bhd.timestamp;
	      begin
		try
		  let bd = DbBlockDelta.dbget h in
		  Printf.fprintf oc "%d txs\n" (List.length (bd.blockdelta_stxl));
		  List.iter (fun (tx,txs) -> Printf.fprintf oc "%s\n" (hashval_hexstring (hashtx tx))) (bd.blockdelta_stxl);
		with Not_found ->
		  find_and_send_requestdata GetBlockdelta h;
		  Printf.fprintf oc "Missing block delta\n"
	      end
	    with Not_found ->
	      find_and_send_requestdata GetHeader h
	  end
      | _ -> raise BadCommandForm);
  ac "nextstakingchances" "nextstakingchances [<hours> [<max ltc to burn> [<blockid>]]" "Print chances for the node to stake\nincluding chances if the node were to hypothetically burn some ltc (see extraburn).\nBy default nextstakingchances checks for every chance from the time of the previous block to 24 hours in the future."
    (fun oc al ->
      let (scnds,maxburn,n) =
	match al with
	| [] ->
	    let n = get_bestblock_print_warnings oc in
	    (3600 * 24,100000000L,n)
	| [hrs] ->
	    let n = get_bestblock_print_warnings oc in
	    (3600 * (int_of_string hrs),100000000L,n)
	| [hrs;maxburn] ->
	    let n = get_bestblock_print_warnings oc in
	    (3600 * (int_of_string hrs),litoshis_of_ltc maxburn,n)
	| [hrs;maxburn;blockid] ->
	    begin
	      try
		let k = hexstring_hashval blockid in
		let (lbk,ltx) = get_burn k in
		(3600 * (int_of_string hrs),litoshis_of_ltc maxburn,Some(k,lbk,ltx))
	      with Not_found ->
		raise (Failure ("unknown block " ^ blockid))
	    end
	| _ -> raise BadCommandForm
      in
      begin
	match n with
	| None -> raise (Failure ("could not find block"))
	| Some(dbh,lbk,ltx) ->
	    let (_,tmstmp,_,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
	    let nw = ltc_medtime() in (*** for staking purposes, ltc is the clock to follow ***)
	    let fromnow_string i nw =
	      if i <= nw then
		"now"
	      else
		let del = Int64.to_int (Int64.sub i nw) in
		if del < 60 then
		  Printf.sprintf "%d seconds from now" del
		else if del < 3600 then
		  Printf.sprintf "%d minutes %d seconds from now" (del / 60) (del mod 60)
		else
		  Printf.sprintf "%d hours %d minutes %d seconds from now" (del / 3600) ((del mod 3600) / 60) (del mod 60)
	    in
	    compute_staking_chances (dbh,lbk,ltx) tmstmp (min (Int64.add tmstmp 604800L) (Int64.add nw (Int64.of_int scnds)));
	    begin
	      try
		match Hashtbl.find nextstakechances (lbk,ltx) with
		| NextStake(i,stkaddr,h,bday,obl,v,Some(toburn),_,_,_,_,_) ->
		    Printf.fprintf oc "Can stake at time %Ld (%s) with asset %s at address %s burning %Ld litoshis (%s ltc).\n" i (fromnow_string i nw) (hashval_hexstring h) (addr_daliladdrstr (p2pkhaddr_addr stkaddr)) toburn (ltc_of_litoshis toburn);
		| NextStake(i,stkaddr,h,bday,obl,v,None,_,_,_,_,_) -> () (*** should not happen; ignore ***)
		| NoStakeUpTo(_) -> Printf.fprintf oc "Found no chance to stake with current wallet and ltc burn limits.\n"
	      with Not_found -> ()
	    end;
	    List.iter
	      (fun z ->
		let il = ref [] in
		match z with
		| NextStake(i,stkaddr,h,bday,obl,v,Some(toburn),_,_,_,_,_) ->
		    if not (List.mem i !il) then
		      begin
			il := i::!il; (** while the info should not be on the hash table more than once, sometimes it is, so only report it once **)
			Printf.fprintf oc "With extraburn %Ld litoshis (%s ltc), could stake at time %Ld (%s) with asset %s at address %s.\n" toburn (ltc_of_litoshis toburn) i (fromnow_string i nw) (hashval_hexstring h) (addr_daliladdrstr (p2pkhaddr_addr stkaddr))
		      end
		| _ -> ())
	      (List.sort
		 (fun y z ->
		   match (y,z) with
		   | (NextStake(i,_,_,_,_,_,Some(_),_,_,_,_,_),NextStake(j,_,_,_,_,_,Some(_),_,_,_,_,_)) -> compare i j
		   | _ -> 0)
		 (List.filter
		    (fun z ->
		      match z with
		      | NextStake(i,stkaddr,h,bday,obl,v,Some(toburn),_,_,_,_,_) -> true
		      | _ -> false)
		    (Hashtbl.find_all nextstakechances_hypo (lbk,ltx))))
       end);
  ac "extraburn" "extraburn <ltc> or extraburn <litoshis> litoshis" "Order the node to burn up to the given amount of ltc given a chance to stake\nby doing the burn (see nextstakingchances)."
    (fun oc al ->
      match al with
      | [a] -> (extraburn := litoshis_of_ltc a; Hashtbl.clear nextstakechances)
      | [a;b] when b = "litoshis" -> (extraburn := Int64.of_string a; Hashtbl.clear nextstakechances)
      | _ -> raise BadCommandForm);
  ac "printassets" "printassets [<ledgerroot>] [<height>]" "Print the assets (in given ledger root assuming given block height).\nBy default the ledger root and height of the current best block is used."
    (fun oc al ->
      match al with
      | [] -> Commands.printassets oc
      | [lr;hght] -> Commands.printassets_in_ledger oc (hexstring_hashval lr) (Int64.of_string hght)
      | [lr] ->
	  begin
	    let n = get_bestblock_print_warnings oc in
	    match n with
	    | None -> raise (Failure ("could not find block"))
	    | Some(_,lbk,ltx) ->
		let (_,_,_,_,_,hght) = Hashtbl.find outlinevals (lbk,ltx) in
		Commands.printassets_in_ledger oc (hexstring_hashval lr) hght
	  end
      | _ -> raise BadCommandForm);
  ac "printtx" "printtx <txid> [<txid>] ... [<txid>]" "Print info about the given txs."
    (fun oc al -> List.iter (fun h -> Commands.printtx oc (hexstring_hashval h)) al);
  ac "importprivkey" "importprivkey <WIFkey> [staking|nonstaking|staking_fresh|nonstaking_fresh]" "Import a private key for a p2pkh address into the wallet."
    (fun oc al ->
      match al with
      | [w] -> Commands.importprivkey oc w "staking"
      | [w;cls] -> Commands.importprivkey oc w cls
      | _ -> raise BadCommandForm);
  ac "importbtcprivkey" "importbtcprivkey <btcWIFkey> [staking|nonstaking|staking_fresh|nonstaking_fresh]" "Import a btc private key for a p2pkh address into the wallet."
    (fun oc al ->
      match al with
      | [w] -> Commands.importbtcprivkey oc w "staking"
      | [w;cls] -> Commands.importbtcprivkey oc w cls
      | _ -> raise BadCommandForm);
  ac "importwatchaddr" "importwatchaddr <address> [offlinekey|offlinekey_fresh]" "Import a dalilcoin address to watch.\nofflinekey or offlinekey_fresh indicates that the user has the private key offline.\nofflinekey_fresh tells dalilcoin to use the address when it needs a fresh address controlled offline (e.g. for staking rewards)"
    (fun oc al ->
      match al with
      | [a] -> Commands.importwatchaddr oc a ""
      | [a;cls] ->
	  if cls = "offlinekey" || cls = "offlinekey_fresh" then
	    Commands.importwatchaddr oc a cls
	  else
	    raise BadCommandForm
      | _ -> raise BadCommandForm);
  ac "importwatchbtcaddr" "importwatchbtcaddr <address> [offlinekey|offlinekey_fresh]" "Import a dalilcoin address to watch by giving it as a bitcoin address.\nofflinekey or offlinekey_fresh indicates that the user has the private key offline.\nofflinekey_fresh tells dalilcoin to use the address when it needs a fresh address controlled offline (e.g. for staking rewards)"
    (fun oc al ->
      match al with
      | [a] -> Commands.importwatchbtcaddr oc a ""
      | [a;cls] ->
	  if cls = "offlinekey" || cls = "offlinekey_fresh" then
	    Commands.importwatchbtcaddr oc a cls
	  else
	    raise BadCommandForm
      | _ -> raise BadCommandForm);
  ac "importendorsement" "importendorsement <address> <address> <signature>" "Import a bitcoin signed endorsement message into the dalilcoin wallet.\nThis can be used to claim the dalilcoin airdrop without needing to import bitcoin private keys into the dalilcoin wallet.\nimportendorsement should be given three arguments: a b s where s is a signature made with the private key for address a endorsing to address b"
    (fun oc al ->
      match al with
      | [a;b;s] -> Commands.importendorsement oc a b s
      | _ -> raise BadCommandForm);
  ac "btctodaliladdr" "btctodaliladdr <btcaddress> [<btcaddress>] .. [<btcaddress>]" "Print the dalilcoin addresses corresponding to the given btc addresses."
    (fun oc al -> List.iter (Commands.btctodaliladdr oc) al);
  ac "printasset" "printasset <assethash>" "print information about the given asset"
    (fun oc al ->
      match al with
      | [h] -> Commands.printasset oc (hexstring_hashval h)
      | _ -> raise BadCommandForm);
  ac "printhconselt" "printhconselt <hashval>" "Print information about the given hconselt, which is an asset possibly followed by a hash referencing more assets."
    (fun oc al ->
      match al with
      | [h] -> Commands.printhconselt oc (hexstring_hashval h)
      | _ -> raise BadCommandForm);
  ac "printctreeelt" "printctreeelt <hashval>" "Print information about a ctree element with the given Merkle root."
    (fun oc al ->
      match al with
      | [h] -> Commands.printctreeelt oc (hexstring_hashval h)
      | _ -> raise BadCommandForm);
  ac "printctreeinfo" "printctreeinfo [ledgerroot]" "Print info about a ctree with the given Merkle root."
    (fun oc al ->
      match al with
      | [] ->
	  let best = get_bestblock_print_warnings oc in
	  let currledgerroot = get_ledgerroot best in
	  Commands.printctreeinfo oc currledgerroot
      | [h] -> Commands.printctreeinfo oc (hexstring_hashval h)
      | _ -> raise BadCommandForm);
  ac "newofflineaddress" "newofflineaddress" "Find an address in the watch wallet that was marked as offlinekey and fresh.\nPrint it and mark it as no longer fresh."
    (fun oc al ->
      let alpha = Commands.get_fresh_offline_address oc in
      Printf.fprintf oc "%s\n" (addr_daliladdrstr alpha));
  ac "newaddress" "newaddress [ledgerroot]" "If there is a key in the wallet classified as nonstaking_fresh, then print it and mark it as no longer fresh.\nOtherwise randomly generate a key, import the key into the wallet (as nonstaking) and print the correponding address.\nThe ledger root is used to ensure that the address is really empty (or was empty, given an old ledgerroot).\nSee also: newstakingaddress"
    (fun oc al ->
      match al with
      | [] ->
	  let best = get_bestblock_print_warnings oc in
	  let currledgerroot = get_ledgerroot best in
	  let (k,h) = Commands.generate_newkeyandaddress currledgerroot "nonstaking" in
	  let alpha = p2pkhaddr_addr h in
	  let a = addr_daliladdrstr alpha in
	  Printf.fprintf oc "%s\n" a
      | [clr] ->
	  let (k,h) = Commands.generate_newkeyandaddress (hexstring_hashval clr) "nonstaking" in
	  let alpha = p2pkhaddr_addr h in
	  let a = addr_daliladdrstr alpha in
	  Printf.fprintf oc "%s\n" a
      | _ -> raise BadCommandForm);
  ac "newstakingaddress" "newstakingaddress [ledgerroot]" "If there is a key in the wallet classified as staking_fresh, then print it and mark it as no longer fresh.\nOtherwise randomly generate a key, import the key into the wallet (as staking) and print the correponding address.\nThe ledger root is used to ensure that the address is really empty (or was empty, given an old ledgerroot).\nSee also: newaddress"
    (fun oc al ->
      match al with
      | [] ->
	  let best = get_bestblock_print_warnings oc in
	  let currledgerroot = get_ledgerroot best in
	  let (k,h) = Commands.generate_newkeyandaddress currledgerroot "staking" in
	  let alpha = p2pkhaddr_addr h in
	  let a = addr_daliladdrstr alpha in
	  Printf.fprintf oc "%s\n" a
      | [clr] ->
	  let (k,h) = Commands.generate_newkeyandaddress (hexstring_hashval clr) "staking" in
	  let alpha = p2pkhaddr_addr h in
	  let a = addr_daliladdrstr alpha in
	  Printf.fprintf oc "%s\n" a
      | _ -> raise BadCommandForm);
  ac "stakewith" "stakewith <address>" "Move an address in the wallet from nonstaking to staking.\nAttempts to spend assets from staking addresses might fail due to the asset being used to stake instead.\nSee also: donotstakewith"
    (fun oc al ->
      match al with
      | [alpha] -> Commands.reclassify_staking oc alpha true
      | _ -> raise BadCommandForm);
  ac "donotstakewith" "donotstakewith <address>" "Move an address in the wallet from staking to nonstaking.\nYou should mark an address as nonstaking if you want to ensure you can spend assets at the address.\nSee also: stakewith"
    (fun oc al ->
      match al with
      | [alpha] -> Commands.reclassify_staking oc alpha false
      | _ -> raise BadCommandForm);
  ac "createp2sh" "createp2sh <script in hex>" "Create a p2sh address by giving the script in hex"
    (fun oc al ->
      match al with
      | [a] ->
	  let s = hexstring_string a in
	  let bl = ref [] in
	  for i = (String.length s) - 1 downto 0 do
	    bl := Char.code s.[i]::!bl
	  done;
	  let alpha = Script.hash160_bytelist !bl in
	  Printf.fprintf oc "p2sh address: %s\n" (addr_daliladdrstr (p2shaddr_addr alpha));
      | _ -> raise BadCommandForm);
  ac "importp2sh" "importp2sh <script in hex>" "Create a p2sh address by giving the script in hex and import it into wallet"
    (fun oc al ->
      match al with
      | [a] ->
	  let s = hexstring_string a in
	  let bl = ref [] in
	  for i = (String.length s) - 1 downto 0 do
	    bl := Char.code s.[i]::!bl
	  done;
	  Commands.importp2sh oc !bl
      | _ -> raise BadCommandForm);
  ac "createtx" "createtx <inputs as json array> <outputs as json array>" "Create a simple tx spending some assets to create new currency assets.\neach input: {\"<addr>\":\"<assetid>\"}\neach output: {\"addr\":\"<addr>\",\"val\":<fraenks>,\"lock\":<height>,\"obligationaddress\":\"<addr>\"}\nwhere lock is optional (default null, unlocked output)\nand obligationaddress is optional (default null, meaning the holder address is implicitly the obligationaddress)\nSee also: creategeneraltx"
    (fun oc al ->
      match al with
      | [inp;outp] ->
	  begin
	    try
	      let (inpj,_) = parse_jsonval inp in
	      begin
		try
		  let (outpj,_) = parse_jsonval outp in
		  let tau = Commands.createtx inpj outpj in
		  let s = Buffer.create 100 in
		  seosbf (seo_stx seosb (tau,([],[])) (s,None));
		  let hs = Hashaux.string_hexstring (Buffer.contents s) in
		  Printf.fprintf oc "%s\n" hs
		with
		| JsonParseFail(i,msg) ->
		    Printf.fprintf oc "Problem parsing json object for tx inputs at position %d %s\n" i msg
	      end
	    with
	    | JsonParseFail(i,msg) ->
		Printf.fprintf oc "Problem parsing json object for tx outputs at position %d %s\n" i msg
	  end
      | _ -> raise BadCommandForm);
  ac "creategeneraltx" "creategeneraltx <tx as json object>" "Create a general tx given as as a json object.\nEvery possible transaction can be represented this way,\nincluding txs publishing mathematical documents and collecting bounties.\nSee also: createtx and createsplitlocktx"
    (fun oc al ->
      try
	match al with
	| [jtxstr] ->
	    let (jtx,_) = parse_jsonval jtxstr in
	    let tau = tx_from_json jtx in
	    let s = Buffer.create 100 in
	    seosbf (seo_stx seosb (tau,([],[])) (s,None));
	    let hs = Hashaux.string_hexstring (Buffer.contents s) in
	    Printf.fprintf oc "%s\n" hs
	| _ -> raise BadCommandForm
      with
      | JsonParseFail(i,msg) ->
	  Printf.fprintf oc "Problem parsing json object for tx at position %d %s\n" i msg);
  ac "createsplitlocktx" "createsplitlocktx <current address> <assetid> <number of outputs> <lockheight> <fee> [<new holding address> [<new obligation address> [<ledger root> <current block height>]]]" "Create a tx to spend an asset into several assets locked until a given height.\nOptionally the new assets can be held at a new address, and may be controlled by a different obligation address."
    (fun oc al ->
      match al with
      | (alp::aid::n::lkh::fee::r) ->
	  begin
	    let alpha2 = daliladdrstr_addr alp in
	    if not (payaddr_p alpha2) then raise (Failure (alp ^ " is not a pay address"));
	    let (p,a4,a3,a2,a1,a0) = alpha2 in
	    let alpha = (p=1,a4,a3,a2,a1,a0) in
	    let aid = hexstring_hashval aid in
	    let n = int_of_string n in
	    if n <= 0 then raise (Failure ("Cannot split into " ^ (string_of_int n) ^ " assets"));
	    let lkh = Int64.of_string lkh in
	    let fee = cants_of_fraenks fee in
	    if fee < 0L then raise (Failure ("Cannot have a negative fee"));
	    let (blkhght,lr) =
	      match r with
	      | [_;_;lr;blkhght] ->
		  (Int64.of_string blkhght,hexstring_hashval lr)
	      | _ ->
		  try
		    match get_bestblock_print_warnings oc with
		    | None -> raise Not_found
		    | Some(_,lbk,ltx) ->
			let (_,_,_,_,_,blkhght) = Hashtbl.find outlinevals (lbk,ltx) in
			let (_,_,lr,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
			(blkhght,lr)
		  with Not_found ->
		    raise (Failure("Could not find ledger root"))
	    in
	    match r with
	    | [] ->
		let gamma = alpha2 in
		let beta = alpha in
		Commands.createsplitlocktx oc lr blkhght alpha beta gamma aid n lkh fee
	    | (gam::r) ->
		let gamma = daliladdrstr_addr gam in
		if not (payaddr_p gamma) then raise (Failure (gam ^ " is not a pay address"));
		match r with
		| [] ->
		    let beta = alpha in
		    let lr = get_ledgerroot (get_bestblock_print_warnings oc) in
		    Commands.createsplitlocktx oc lr blkhght alpha beta gamma aid n lkh fee
		| (bet::r) ->
		    let beta2 = daliladdrstr_addr bet in
		    if not (payaddr_p beta2) then raise (Failure (bet ^ " is not a pay address"));
		    let (p,b4,b3,b2,b1,b0) = beta2 in
		    let beta = (p=1,b4,b3,b2,b1,b0) in
		    match r with
		    | [] -> Commands.createsplitlocktx oc lr blkhght alpha beta gamma aid n lkh fee
		    | [_;_] -> Commands.createsplitlocktx oc lr blkhght alpha beta gamma aid n lkh fee (** lr and blockheight given, handled above **)
		    | _ -> raise BadCommandForm
	  end
      | _ -> raise BadCommandForm);
  ac "signtx" "signtx <tx in hex> [<jsonarrayofprivkeys> [<ledgerroot>]]" "Sign a dalilcoin tx."
    (fun oc al ->
      match al with
      | [s] -> Commands.signtx oc (get_ledgerroot (get_bestblock_print_warnings oc)) s None
      | [s;kl] ->
	  let kl = parse_json_privkeys kl in
	  Commands.signtx oc (get_ledgerroot (get_bestblock_print_warnings oc)) s (Some(kl))
      | [s;kl;lr] ->
	  let kl = parse_json_privkeys kl in
	  Commands.signtx oc (hexstring_hashval lr) s (Some(kl))
      | _ -> raise BadCommandForm);
  ac "signtxfile" "signtxfile <infile> <outfile> [<jsonarrayofprivkeys> [<ledgerroot>]]" "Sign a dalilcoin tx.\n<infile> is an existing binary file with the (possibly partially signed) tx.\n<outfile> is a binary file created with the output tx."
    (fun oc al ->
      match al with
      | [s1;s2] ->
	  let c1 = open_in_bin s1 in
	  let (stau,_) = Tx.sei_stx seic (c1,None) in
	  close_in c1;
	  let c2 = open_out_bin s2 in
	  begin
	    try
	      Commands.signtxc oc (get_ledgerroot (get_bestblock_print_warnings oc)) stau c2 None;
	      close_out c2
	    with e ->
	      close_out c2;
	      raise e
	  end
      | [s1;s2;kl] ->
	  let c1 = open_in_bin s1 in
	  let (stau,_) = Tx.sei_stx seic (c1,None) in
	  close_in c1;
	  let kl = parse_json_privkeys kl in
	  let c2 = open_out_bin s2 in
	  begin
	    try
	      Commands.signtxc oc (get_ledgerroot (get_bestblock_print_warnings oc)) stau c2 (Some(kl));
	      close_out c2
	    with e ->
	      close_out c2;
	      raise e
	  end
      | [s1;s2;kl;lr] ->
	  let c1 = open_in_bin s1 in
	  let (stau,_) = Tx.sei_stx seic (c1,None) in
	  close_in c1;
	  let kl = parse_json_privkeys kl in
	  let c2 = open_out_bin s2 in
	  begin
	    try
	      Commands.signtxc oc (hexstring_hashval lr) stau c2 (Some(kl));
	      close_out c2
	    with e ->
	      close_out c2;
	      raise e
	  end
      | _ -> raise BadCommandForm);
  ac "signbatchtxsfiles" "signbatchtxsfiles <infile> <outfile> [<jsonarrayofprivkeys> [<ledgerroot>]]" "Sign a dalilcoin tx.\n<infile> is an existing binary file with several (possibly partially signed) txs.\n<outfile> is a binary file created with the txs after signing."
    (fun oc al ->
      let read_staul s1 =
	let staur = ref [] in
	let c1 = open_in_bin s1 in
	try
	  while true do
	    let (stau,_) = Tx.sei_stx seic (c1,None) in
	    staur := stau::!staur
	  done;
	  []
	with
	| End_of_file -> close_in c1; List.rev !staur
	| _ -> close_in c1; raise BadCommandForm
      in
      match al with
      | [s1;s2] ->
	  let staul = read_staul s1 in
	  let c2 = open_out_bin s2 in
	  begin
	    try
	      Commands.signbatchtxsc oc (get_ledgerroot (get_bestblock_print_warnings oc)) staul c2 None;
	      close_out c2
	    with e ->
	      close_out c2;
	      raise e
	  end
      | [s1;s2;kl] ->
	  let staul = read_staul s1 in
	  let kl = parse_json_privkeys kl in
	  let c2 = open_out_bin s2 in
	  begin
	    try
	      Commands.signbatchtxsc oc (get_ledgerroot (get_bestblock_print_warnings oc)) staul c2 (Some(kl));
	      close_out c2
	    with e ->
	      close_out c2;
	      raise e
	  end
      | [s1;s2;kl;lr] ->
	  let staul = read_staul s1 in
	  let kl = parse_json_privkeys kl in
	  let c2 = open_out_bin s2 in
	  begin
	    try
	      Commands.signbatchtxsc oc (hexstring_hashval lr) staul c2 (Some(kl));
	      close_out c2
	    with e ->
	      close_out c2;
	      raise e
	  end
      | _ -> raise BadCommandForm);
  ac "savetxtopool" "savetxtopool <tx in hex>" "Save a dalilcoin tx to the local pool without sending it to the network."
    (fun oc al ->
      match al with
      | [s] ->
	  let b = get_bestblock_print_warnings oc in
	  begin
	    match b with
	    | None -> Printf.fprintf oc "Cannot find best block\n"
	    | Some(dbh,lbk,ltx) ->
		try
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  try
		    let (_,tm,lr,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
		    Commands.savetxtopool blkh tm lr s
		  with Not_found ->
		    let (bhd,_) = DbBlockHeader.dbget dbh in
		    let lr = bhd.newledgerroot in
		    let tm = bhd.timestamp in
		    Commands.savetxtopool blkh tm lr s
		with Not_found ->
		  Printf.fprintf oc "Trouble finding current block height\n"
	  end
      | _ -> raise BadCommandForm);
  ac "sendtx" "sendtx <tx in hex>" "Send a dalilcoin tx to other nodes on the network."
    (fun oc al ->
      match al with
      | [s] ->
	  begin
	    match get_bestblock_print_warnings oc with
	    | None -> Printf.fprintf oc "Cannot find best block.\n"
	    | Some(dbh,lbk,ltx) ->
		try
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  let (_,tm,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		  Commands.sendtx oc (Int64.add 1L blkh) tm tr sr lr s
		with Not_found ->
		  Printf.fprintf oc "Cannot find block height for best block %s\n" (hashval_hexstring dbh)
	  end
      | _ -> raise BadCommandForm);
  ac "sendtxfile" "sendtxfile <file with tx in binary>" "Send a dalilcoin tx to other nodes on the network."
    (fun oc al ->
      match al with
      | [s] ->
	  begin
	    match get_bestblock_print_warnings oc with
	    | None -> Printf.fprintf oc "Cannot find best block.\n"
	    | Some(dbh,lbk,ltx) ->
		try
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  let (_,tm,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		  let c = open_in_bin s in
		  let (stau,_) = Tx.sei_stx seic (c,None) in
		  let txbytes = pos_in c in
		  close_in c;
		  if txbytes > 450000 then
		    Printf.fprintf oc "Refusing to send tx > 450K bytes\n"
		  else
		    Commands.sendtx2 oc (Int64.add 1L blkh) tm tr sr lr txbytes stau
		with Not_found ->
		  Printf.fprintf oc "Cannot find block height for best block %s\n" (hashval_hexstring dbh)
	  end
      | _ -> raise BadCommandForm);
  ac "validatetx" "validatetx <tx in hex>" "Print information about the tx and whether or not it is valid.\nIf the tx is not valid, information about why it is not valid is given."
    (fun oc al ->
      match al with
      | [s] ->
	  begin
	    let best = get_bestblock_print_warnings oc in
	    match best with
	    | None -> Printf.fprintf oc "Cannot determine best block\n"
	    | Some(dbh,lbk,ltx) ->
		try
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  try
		    let (_,tm,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		    try
		      Commands.validatetx oc (Int64.add 1L blkh) tm tr sr lr s
		    with exn ->
		      Printf.fprintf oc "Trouble validating tx %s\n" (Printexc.to_string exn)
		  with Not_found ->
		    Printf.fprintf oc "Cannot determine information about best block %s at height %Ld\n" (hashval_hexstring dbh) blkh
		with Not_found ->
		  Printf.fprintf oc "Cannot find block height for best block %s\n" (hashval_hexstring dbh)
	  end
      | _ -> raise BadCommandForm);
  ac "validatetxfile" "validatetxfile <file with tx in binary>" "Print information about the tx and whether or not it is valid.\nIf the tx is not valid, information about why it is not valid is given."
    (fun oc al ->
      match al with
      | [s] ->
	  begin
	    let best = get_bestblock_print_warnings oc in
	    match best with
	    | None -> Printf.fprintf oc "Cannot determine best block\n"
	    | Some(dbh,lbk,ltx) ->
		try
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  try
		    let (_,tm,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		    try
		      let c = open_in_bin s in
		      let (stau,_) = Tx.sei_stx seic (c,None) in
		      let txbytes = pos_in c in
		      close_in c;
		      if txbytes > 450000 then
			Printf.fprintf oc "Tx is > 450K bytes and will be considered too big to include in a block\n"
		      else
			Commands.validatetx2 oc (Int64.add 1L blkh) tm tr sr lr txbytes stau
		    with exn ->
		      Printf.fprintf oc "Trouble validating tx %s\n" (Printexc.to_string exn)
		  with Not_found ->
		    Printf.fprintf oc "Cannot determine information about best block %s at height %Ld\n" (hashval_hexstring dbh) blkh
		with Not_found ->
		  Printf.fprintf oc "Cannot find block height for best block %s\n" (hashval_hexstring dbh)
	  end
      | _ -> raise BadCommandForm);
  ac "validatebatchtxsfile" "validatebatchtxsfile <file with several tx in binary>" "Print information about the txs and whether or not it they valid.\nThe txs are considered in sequences with the previous txs modifying the ledger before evaluating the next.\nIf a tx is not valid, information about why it is not valid is given."
    (fun oc al ->
      match al with
      | [s] ->
	  begin
	    let best = get_bestblock_print_warnings oc in
	    match best with
	    | None -> Printf.fprintf oc "Cannot determine best block\n"
	    | Some(dbh,lbk,ltx) ->
		try
		  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
		  try
		    let (_,tm,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
		    try
		      let c = open_in_bin s in
		      let staur = ref [] in
		      begin
			try
			  while true do
			    let (stau,_) = Tx.sei_stx seic (c,None) in
			    staur := stau::!staur
			  done
			with End_of_file ->
			  close_in c;
			  Commands.validatebatchtxs oc (Int64.add 1L blkh) tm tr sr lr (List.rev !staur)
		      end
		    with exn ->
		      Printf.fprintf oc "Trouble validating tx %s\n" (Printexc.to_string exn)
		  with Not_found ->
		    Printf.fprintf oc "Cannot determine information about best block %s at height %Ld\n" (hashval_hexstring dbh) blkh
		with Not_found ->
		  Printf.fprintf oc "Cannot find block height for best block %s\n" (hashval_hexstring dbh)
	  end
      | _ -> raise BadCommandForm);
  ac "theory" "theory <theoryid>" "Print information about a confirmed theory"
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    let thyid = hexstring_hashval a in
	    let (_,tr,_) = get_3roots (get_bestblock_print_warnings oc) in
	    try
	      let tht = lookup_thytree tr in
	      let thy = ottree_lookup tht (Some(thyid)) in
	      let (prms,axs) = thy in
	      let i = ref 0 in
	      Printf.fprintf oc "Theory %s %d Prims %d Axioms:\nIds and Types of Prims:\n" a (List.length prms) (List.length axs);
	      List.iter
		(fun a ->
		  let h = tm_hashroot (Logic.Prim(!i)) in
		  incr i;
		  Printf.fprintf oc "%s %s\n" (hashval_hexstring h) (hashval_hexstring (hashtag (hashopair2 (Some(thyid)) (hashpair h (hashtp a))) 32l));
		  print_jsonval oc (json_stp a); Printf.fprintf oc "\n")
		prms;
	      Printf.printf "Ids of Axioms:\n";
	      List.iter
		(fun h -> Printf.fprintf oc "%s %s\n" (hashval_hexstring h) (hashval_hexstring (hashtag (hashopair2 (Some(thyid)) h) 33l)))
		axs;
	    with Not_found ->
	      Printf.fprintf oc "Theory not found.\n"
	  end
      | _ -> raise BadCommandForm);
  ac "signature" "signature <signatureid>" "Print information about a confirmed signature"
    (fun oc al ->
      let (a,sgid) =
	match al with
	| [a] -> (a,hexstring_hashval a)
	| _ -> raise BadCommandForm
      in
      let (_,_,sr) = get_3roots (get_bestblock_print_warnings oc) in
      try
	let sgt = lookup_sigtree sr in
	let (th,sg) = ostree_lookup sgt (Some(sgid)) in
	let ths = match th with Some(h) -> hashval_hexstring h | None -> "empty" in
	let (imps,(objs,kns)) = sg in
	Printf.fprintf oc "Signature %s in Theory %s\n%d Imported Signatures %d Objects %d Knowns:\n" a ths (List.length imps) (List.length objs) (List.length kns);
	Printf.fprintf oc "Imports:\n";
	List.iter
	  (fun h -> Printf.fprintf oc "%s\n" (hashval_hexstring h))
	  imps;
	Printf.fprintf oc "Objects:\n";
	List.iter
	  (fun ((h,_),_) -> Printf.fprintf oc "%s\n" (hashval_hexstring h))
	  objs;
	Printf.fprintf oc "Knowns:\n";
	List.iter
	  (fun (h,_) -> Printf.fprintf oc "%s\n" (hashval_hexstring h))
	  kns;
      with Not_found ->
	Printf.fprintf oc "Signature not found.\n");
  ac "preassetinfo" "preassetinfo <preasset as json>" "Print information about a preasset given in json form.\nTypes of assets are currency, bounties,\n ownership of objects, ownership of propositions, ownership of negations of propositions,\nrights to use an object, rights to use a proposition,\ncommitment markers published before publishing a document, theory or signature,\na theories, signatures and documents."
    (fun oc al ->
      match al with
      | [a] ->
	  begin
	    try
	      let (j,_) = parse_jsonval a in
	      let u = preasset_from_json j in
	      Commands.preassetinfo_report oc u
	    with
	    | JsonParseFail(i,msg) ->
		Printf.fprintf oc "Problem parsing json object for preasset at position %d %s\n" i msg
	  end
      | _ -> raise BadCommandForm);
  ac "terminfo" "terminfo <term as json> [<type as json>, with default 'prop'] [<theoryid, default of empty theory>]" "Print information about a mathematical term given in json format."
    (fun oc al ->
      let (jtm,jtp,thyid) =
	match al with
	| [jtm] -> (jtm,"'\"prop\"'",None)
	| [jtm;jtp] -> (jtm,jtp,None)
	| [jtm;jtp;theoryid] -> (jtm,jtp,Some(hexstring_hashval theoryid))
	| _ -> raise BadCommandForm
      in
      begin
	try
	  let (jtm,_) = parse_jsonval jtm in
	  begin
	    try
	      let (jtp,_) = parse_jsonval jtp in
	      let m =
		match jtm with
		| JsonStr(x) -> Logic.TmH(hexstring_hashval x) (*** treat a string as just the term root abbreviating the term ***)
		| _ -> trm_from_json jtm
	      in
	      let a =
		match jtp with
		| JsonStr(x) when x = "prop" -> Logic.Prop
		| JsonNum(x) -> Logic.Base(int_of_string x)
		| _ -> stp_from_json jtp
	      in (*** not checking if the term has the type; this could depend on the theory ***)
	      let h = tm_hashroot m in
	      let tph = hashtp a in
	      Printf.fprintf oc "term root: %s\n" (hashval_hexstring h);
	      Printf.fprintf oc "pure term address: %s\n" (addr_daliladdrstr (termaddr_addr (hashval_md160 h)));
	      if thyid = None then
		begin
		  let k = hashtag (hashopair2 None (hashpair h tph)) 32l in
		  Printf.fprintf oc "obj id in empty theory: %s\n" (hashval_hexstring k);
		  Printf.fprintf oc "obj address in empty theory: %s\n" (addr_daliladdrstr (termaddr_addr (hashval_md160 k)))
		end
	      else
		begin
		  let k = hashtag (hashopair2 thyid (hashpair h tph)) 32l in
		  Printf.fprintf oc "obj id in given theory: %s\n" (hashval_hexstring k);
		  Printf.fprintf oc "obj address in given theory: %s\n" (addr_daliladdrstr (termaddr_addr (hashval_md160 k)))
		end;
	      if a = Logic.Prop then
		begin
		  if thyid = None then
		    begin
		      let k = hashtag (hashopair2 None h) 33l in
		      Printf.fprintf oc "prop id in empty theory: %s\n" (hashval_hexstring k);
		      Printf.fprintf oc "prop address in empty theory: %s\n" (addr_daliladdrstr (termaddr_addr (hashval_md160 k)))
		    end
		  else
		    begin
		      let k = hashtag (hashopair2 thyid h) 33l in
		      Printf.fprintf oc "prop id in given theory: %s\n" (hashval_hexstring k);
		      Printf.fprintf oc "prop address in given theory: %s\n" (addr_daliladdrstr (termaddr_addr (hashval_md160 k)))
		    end
		end
	    with
	    | JsonParseFail(i,msg) ->
		Printf.fprintf oc "Problem parsing json object for tp at position %d %s\n" i msg
	  end
	with
	| JsonParseFail(i,msg) ->
	    Printf.fprintf oc "Problem parsing json object for tm at position %d %s\n" i msg
      end);
  ac "decodetx" "decodetx <raw tx in hex>" "Decode a dalilcoin tx."
    (fun oc al ->
      match al with
      | [a] ->
	  let s = hexstring_string a in
	  let (stx,_) = sei_stx seis (s,String.length s,None,0,0) in
	  print_jsonval oc (json_stx stx);
	  Printf.fprintf oc "\n"
      | _ -> raise BadCommandForm);
  ac "decodetxfile" "decodetxfile <file with binary tx>" "Decode a dalilcoin tx from a file."
    (fun oc al ->
      match al with
      | [s1] ->
	  let c1 = open_in_bin s1 in
	  let (stau,_) = Tx.sei_stx seic (c1,None) in
	  close_in c1;
	  print_jsonval oc (json_stx stau);
	  Printf.fprintf oc "\n"
      | _ -> raise BadCommandForm);
  ac "querybestblock" "querybestblock" "Print the current best block in json format.\nIn case of a tie, only one of the current best blocks is returned.\nThis command is intended to support explorers.\nSee also: bestblock"
    (fun oc al ->
      let best = get_bestblock_print_warnings oc in
      match best with
      | None -> Printf.fprintf oc "Cannot determine best block\n"
      | Some(h,lbk,ltx) ->
	  try
	    let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
	    try
	      let lr = get_ledgerroot best in
	      print_jsonval oc (JsonObj([("height",JsonNum(Int64.to_string blkh));("block",JsonStr(hashval_hexstring h));("ledgerroot",JsonStr(hashval_hexstring lr))]))
	    with Not_found ->
	      print_jsonval oc (JsonObj([("height",JsonNum(Int64.to_string blkh));("block",JsonStr(hashval_hexstring h))]))
	  with Not_found ->
	    Printf.fprintf oc "Cannot determine height of best block %s\n" (hashval_hexstring h));
  ac "bestblock" "bestblock" "Print the current best block in text format.\nIn case of a tie, only one of the current best blocks is returned.\nSee also: querybestblock"
    (fun oc al ->
      let best = get_bestblock_print_warnings oc in
      match best with
      | None -> Printf.fprintf oc "Cannot determine best block\n"
      | Some(h,lbk,ltx) ->
	  try
	    let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
	    try
	      let lr = get_ledgerroot best in
	      Printf.fprintf oc "Height: %Ld\nBlock hash: %s\nLedger root: %s\n" (Int64.sub blkh 1L) (hashval_hexstring h) (hashval_hexstring lr)
	    with Not_found ->
	      Printf.fprintf oc "Height: %Ld\nBlock hash: %s\n" (Int64.sub blkh 1L) (hashval_hexstring h)
	  with Not_found ->
	    Printf.fprintf oc "Block hash: %s\n" (hashval_hexstring h));
  ac "difficulty" "difficulty" "Print the current difficulty."
    (fun oc al ->
      let best = get_bestblock_print_warnings oc in
      match best with
      | None -> Printf.fprintf oc "Cannot determine best block\n"
      | Some(h,lbk,ltx) ->
	  try
	    let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
	    try
	      let (tar,_,_,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
	      Printf.fprintf oc "Current target (for block at height %Ld): %s\n" blkh (string_of_big_int tar)
	    with Not_found ->
	      Printf.fprintf oc "Cannot determine information about best block %s at height %Ld\n" (hashval_hexstring h) blkh
	  with Not_found ->
	    Printf.fprintf oc "Cannot find block height for best block %s\n" (hashval_hexstring h));
  ac "blockchain" "blockchain [<n>]" "Print the blockchain up to the most recent <n> blocks, with a default of 1000 blocks."
    (fun oc al ->
      match al with
      | [] -> Commands.pblockchain oc (get_bestblock_print_warnings oc) 1000
      | [n] -> let n = int_of_string n in Commands.pblockchain oc (get_bestblock_print_warnings oc) n
      | _ -> raise BadCommandForm);
  ac "reprocessblockchain" "reprocessblockchain [<n>]" "reprocess the block chain from the block at height n up to the current block, where by default n=1 (the genesis block)"
    (fun oc al ->
      match al with
      | [] -> Commands.reprocess_blockchain oc (get_bestblock_print_warnings oc) 1
      | [n] -> let n = int_of_string n in Commands.reprocess_blockchain oc (get_bestblock_print_warnings oc) n
      | _ -> raise BadCommandForm);
  ac "reprocessblock" "reprocessblock <blockhash>" "Manually reprocess a given block.\nThis is useful if either -ltcoffline is set or if part of the current ledger seems to be missing from the local node.\nIf the current node has the full ledger from before the block,\nthen processing the block should ensure the node has the resulting full ledger."
    (fun oc al ->
      match al with
      | [h] -> reprocessblock oc (hexstring_hashval h)
      | _ -> raise (Failure "reprocessblock <blockid>"));;

let rec parse_command_r l i n =
  if i < n then
    let j = ref i in
    while !j < n && l.[!j] = ' ' do
      incr j
    done;
    let b = Buffer.create 20 in
    while !j < n && not (List.mem l.[!j] [' ';'"';'\'']) do
      Buffer.add_char b l.[!j];
      incr j
    done;
    let a = Buffer.contents b in
    let c d = if a = "" then d else a::d in
    if !j < n && l.[!j] = '"' then
      c (parse_command_r_q l (!j+1) n)
    else if !j < n && l.[!j] = '\'' then
      c (parse_command_r_sq l (!j+1) n)
    else
      c (parse_command_r l (!j+1) n)
  else
    []
and parse_command_r_q l i n =
  let b = Buffer.create 20 in
  let j = ref i in
  while !j < n && not (l.[!j] = '"') do
    Buffer.add_char b l.[!j];
    incr j
  done;
  if !j < n then
    Buffer.contents b::parse_command_r l (!j+1) n
  else
    raise (Failure("missing \""))
and parse_command_r_sq l i n =
  let b = Buffer.create 20 in
  let j = ref i in
  while !j < n && not (l.[!j] = '\'') do
    Buffer.add_char b l.[!j];
    incr j
  done;
  if !j < n then
    Buffer.contents b::parse_command_r l (!j+1) n
  else
    raise (Failure("missing '"))

let parse_command l =
  let ll = parse_command_r l 0 (String.length l) in
  match ll with
  | [] -> raise Exit (*** empty command, silently ignore ***)
  | (c::al) -> (c,al)

let do_command oc l =
  let (c,al) = parse_command l in
  if c = "help" then
    begin
      match al with
      | [a] ->
	  begin
	    try
	      let (h,longhelp,_) = Hashtbl.find commandh a in
	      Printf.fprintf oc "%s\n" h;
	      if not (longhelp = "") then Printf.fprintf oc "%s\n" longhelp
	    with Not_found ->
	      Printf.fprintf oc "Unknown command %s\n" a;
	  end
      | _ ->
	  Printf.fprintf oc "Available Commands:\n";
	  List.iter
	    (fun c -> Printf.fprintf oc "%s\n" c)
	    !sortedcommands;
	  Printf.fprintf oc "\nFor more specific information: help <command>\n";
    end
  else
    try
      let (_,_,f) = Hashtbl.find commandh c in
      f oc al;
      flush oc
    with Not_found ->
      Printf.fprintf oc "Unknown command %s\n" c;;

let initialize () =
  begin
    let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in
    if !Config.testnet then
      begin
	Utils.may2019hardforktime := 1553277394L; (** move fork to earlier time on testnet **)
	Utils.july2019hardforktime := 1553277394L; (** move fork to earlier time on testnet **)
	if !Config.ltcrpcport = 9332 then Config.ltcrpcport := 19332;
	if !Config.genesistimestamp = 1523203501L then Config.genesistimestamp := 1552816800L
      end;
    if Sys.file_exists (Filename.concat datadir "lock") then
      begin
	if not !Config.daemon then
	  begin
	    Printf.printf "Cannot start Dalilcoin. Do you already have Dalilcoin running? If not, remove: %s\n" (Filename.concat datadir "lock");
	    flush stdout;
	    exit 1;
	  end;
      end;
    lock datadir;
    if not !Config.daemon then (Printf.printf "Initializing the database..."; flush stdout);
    let dbdir = Filename.concat datadir "db" in
    dbconfig dbdir; (*** configure the database ***)
    DbTheory.dbinit();
    DbTheoryTree.dbinit();
    DbSigna.dbinit();
    DbSignaTree.dbinit();
    DbAsset.dbinit();
    DbAssetIdAt.dbinit();
    DbSTx.dbinit();
    DbHConsElt.dbinit();
    DbHConsEltAt.dbinit();
    DbCTreeElt.dbinit();
    DbCTreeEltAt.dbinit();
    DbBlockHeader.dbinit();
    DbBlockDelta.dbinit();
    DbInvalidatedBlocks.dbinit();
    DbLtcDacStatus.dbinit();
    DbLtcBurnTx.dbinit();
    DbLtcBlock.dbinit();
    if not !Config.daemon then (Printf.printf "Initialized.\n"; flush stdout);
    openlog(); (*** Don't open the log until the config vars are set, so if we know whether or not it's testnet. ***)
    let sout = if !Config.daemon then !Utils.log else stdout in
    if !createsnapshot then
      begin
	match !snapshot_dir with
	| None ->
	    Printf.fprintf sout "No snapshot directory given.\n";
	    !exitfn 1
	| Some(dir) -> (*** then creating a snapshot ***)
	    Printf.fprintf sout "Creating snapshot.\n"; flush sout;
	    let fin : (hashval,unit) Hashtbl.t = Hashtbl.create 10000 in
	    begin
	      if Sys.file_exists dir then
		if Sys.is_directory dir then
		  ()
		else
		  raise (Failure (dir ^ " is a file not a directory"))
	      else
		begin
		  Unix.mkdir dir 0b111111000
		end
	    end;
	    let headerfile = open_out_bin (Filename.concat dir "headers") in
	    let blockfile = open_out_bin (Filename.concat dir "blocks") in
	    let ctreeeltfile = open_out_bin (Filename.concat dir "ctreeelts") in
	    let hconseltfile = open_out_bin (Filename.concat dir "hconselts") in
	    let assetfile = open_out_bin (Filename.concat dir "assets") in
	    List.iter
	      (fun h ->
		if not (Hashtbl.mem fin h) then
		  begin
		    Hashtbl.add fin h ();
                    try
		      let bh = DbBlockHeader.dbget h in
		      let bd = DbBlockDelta.dbget h in
		      seocf (seo_block seoc (bh,bd) (blockfile,None))
		    with e ->
		      Printf.fprintf sout "WARNING: Exception called when trying to save block %s: %s\n" (hashval_hexstring h) (Printexc.to_string e)
		  end)
	      !snapshot_blocks;
	    List.iter
	      (fun h ->
		if not (Hashtbl.mem fin h) then
		  begin
		    Hashtbl.add fin h ();
                    try
		      let bh = DbBlockHeader.dbget h in
		      seocf (seo_blockheader seoc bh (headerfile,None));
		    with e ->
		      Printf.fprintf sout "WARNING: Exception called when trying to save header %s: %s\n" (hashval_hexstring h) (Printexc.to_string e)
		  end)
	      !snapshot_headers;
	    let supp = List.map addr_bitseq !snapshot_addresses in
	    List.iter
	      (fun h -> dbledgersnapshot_ctree_top (ctreeeltfile,hconseltfile,assetfile) fin supp h !snapshot_shards)
	      !snapshot_ledgerroots;
	    close_out headerfile;
	    close_out blockfile;
	    close_out ctreeeltfile;
	    close_out hconseltfile;
	    close_out assetfile;
	    closelog();
	    !exitfn 0;
      end;
    if !importsnapshot then
      begin
	match !snapshot_dir with
	| None ->
	    Printf.fprintf sout "No snapshot directory given.\n";
	    !exitfn 1
	| Some(dir) -> (*** then creating a snapshot ***)
	    Printf.fprintf sout "Importing snapshot.\n"; flush sout;
	    let headerfile = open_in_bin (Filename.concat dir "headers") in
	    let blockfile = open_in_bin (Filename.concat dir "blocks") in
	    let ctreeeltfile = open_in_bin (Filename.concat dir "ctreeelts") in
	    let hconseltfile = open_in_bin (Filename.concat dir "hconselts") in
	    let assetfile = open_in_bin (Filename.concat dir "assets") in
	    begin
	      try
		while true do
		  let ((bh,bd),_) = sei_block seic (blockfile,None) in
		  let h = blockheader_id bh in
		  DbBlockHeader.dbput h bh;
		  DbBlockDelta.dbput h bd;
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let (bh,_) = sei_blockheader seic (headerfile,None) in
		  let h = blockheader_id bh in
		  DbBlockHeader.dbput h bh;
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let (c,_) = sei_ctree seic (ctreeeltfile,None) in
		  let h = ctree_hashroot c in
		  DbCTreeElt.dbput h c;
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let ((ah,hr),_) = sei_prod sei_hashval (sei_option (sei_prod sei_hashval sei_int8)) seic (hconseltfile,None) in
		  let (h,_) = nehlist_hashroot (NehConsH(ah,match hr with None -> HNil | Some(hr,l) -> HHash(hr,l))) in
		  DbHConsElt.dbput h (ah,hr)
		done
	      with _ -> ()
	    end;
	    begin
	      try
		while true do
		  let (a,_) = sei_asset seic (assetfile,None) in
		  let h = hashasset a in
		  DbAsset.dbput h a
		done
	      with _ -> ()
	    end;
	    close_in headerfile;
	    close_in blockfile;
	    close_in ctreeeltfile;
	    close_in hconseltfile;
	    close_in assetfile;
	    closelog();
	    !exitfn 0;
      end;
    begin
      match !check_ledger with
      | None -> ()
      | Some(lr) ->
	  let totcants = ref 0L in
	  let totbounties = ref 0L in
	  let rec check_asset h =
	    try
	      let a = DbAsset.dbget h in
	      match a with
	      | (_,_,_,Currency(v)) -> totcants := Int64.add v !totcants
	      | (_,_,_,Bounty(v)) -> totbounties := Int64.add v !totbounties
	      | _ -> ()
	    with Not_found ->
	      Printf.fprintf sout "WARNING: asset %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec check_hconselt h =
	    try
	      let (ah,hr) = DbHConsElt.dbget h in
	      check_asset ah;
	      match hr with
	      | Some(h,_) -> check_hconselt h
	      | None -> ()
	    with Not_found ->
	      Printf.fprintf sout "WARNING: hconselt %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec check_ledger_rec h =
	    try
	      let c = DbCTreeElt.dbget h in
	      check_ctree_rec c 9
	    with Not_found ->
	      Printf.fprintf sout "WARNING: ctreeelt %s is not in database\n" (hashval_hexstring h)
	  and check_ctree_rec c i =
	    match c with
	    | CHash(h) -> check_ledger_rec h
	    | CLeaf(_,NehHash(h,_)) -> check_hconselt h
	    | CLeft(c0) -> check_ctree_rec c0 (i-1)
	    | CRight(c1) -> check_ctree_rec c1 (i-1)
	    | CBin(c0,c1) ->
		check_ctree_rec c0 (i-1);
		check_ctree_rec c1 (i-1)
	    | _ ->
		Printf.fprintf sout "WARNING: unexpected non-element ctree at level %d:\n" i;
		print_ctree sout c
	  in
	  check_ledger_rec lr;
	  Printf.fprintf sout "Total Currency Assets: %Ld cants (%s fraenks)\n" !totcants (fraenks_of_cants !totcants);
	  Printf.fprintf sout "Total Bounties: %Ld cants (%s fraenks)\n" !totbounties (fraenks_of_cants !totbounties);
	  !exitfn 0
    end;
    begin
      match !build_extraindex with
      | None -> ()
      | Some(lr) ->
	  let rec extraindex_asset h alpha =
	    try
	      let a = DbAsset.dbget h in
	      DbAssetIdAt.dbput (assetid a) alpha
	    with Not_found ->
	      Printf.fprintf sout "WARNING: asset %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec extraindex_hconselt h alpha =
	    try
	      let (ah,hr) = DbHConsElt.dbget h in
	      DbHConsEltAt.dbput ah alpha;
	      extraindex_asset ah alpha;
	      match hr with
	      | Some(h,_) -> extraindex_hconselt h alpha
	      | None -> ()
	    with Not_found ->
	      Printf.fprintf sout "WARNING: hconselt %s is not in database\n" (hashval_hexstring h)
	  in
	  let rec extraindex_ledger_rec h pl =
	    try
	      let c = DbCTreeElt.dbget h in
	      DbCTreeEltAt.dbput h (List.rev pl);
	      extraindex_ctree_rec c 9 pl
	    with Not_found ->
	      Printf.fprintf sout "WARNING: ctreeelt %s is not in database\n" (hashval_hexstring h)
	  and extraindex_ctree_rec c i pl =
	    match c with
	    | CHash(h) -> extraindex_ledger_rec h pl
	    | CLeaf(bl,NehHash(h,_)) -> extraindex_hconselt h (bitseq_addr ((List.rev pl) @ bl))
	    | CLeft(c0) -> extraindex_ctree_rec c0 (i-1) (false::pl)
	    | CRight(c1) -> extraindex_ctree_rec c1 (i-1) (true::pl)
	    | CBin(c0,c1) ->
		extraindex_ctree_rec c0 (i-1) (false::pl);
		extraindex_ctree_rec c1 (i-1) (true::pl)
	    | _ ->
		Printf.fprintf sout "WARNING: unexpected non-element ctree at level %d:\n" i;
		print_ctree sout c
	  in
	  extraindex_ledger_rec lr [];
	  !exitfn 0
    end;
    begin
      match !netlogreport with
      | None -> ()
      | Some([]) ->
	  Printf.fprintf sout "Expected -netlogreport <sentlogfile> [<reclogfile>*]\n";
	  !exitfn 1
      | Some(sentf::recfl) ->
	  let extra_log_info mt ms = (*** for certain types of messages, give more information ***)
	    match mt with
	    | Inv ->
		begin
		  let c = ref (ms,String.length ms,None,0,0) in
		  let (n,cn) = sei_int32 seis !c in
		  Printf.fprintf sout "Inv msg %ld entries\n" n;
		  c := cn;
		  for j = 1 to Int32.to_int n do
		    let ((i,h),cn) = sei_prod sei_int8 sei_hashval seis !c in
		    c := cn;
		    Printf.fprintf sout "Inv %d %s\n" i (hashval_hexstring h);
		  done
		end
	    | GetHeader ->
		begin
		  let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
		  Printf.fprintf sout "GetHeader %s\n" (hashval_hexstring h)
		end
	    | GetHeaders ->
		begin
		  let c = ref (ms,String.length ms,None,0,0) in
		  let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
		  c := cn;
		  Printf.fprintf sout "GetHeaders requesting these %d headers:\n" n;
		  for j = 1 to n do
		    let (h,cn) = sei_hashval seis !c in
		    c := cn;
		    Printf.fprintf sout "%d. %s\n" j (hashval_hexstring h);
		  done
		end
	    | Headers ->
		begin
		  let c = ref (ms,String.length ms,None,0,0) in
		  let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
		  Printf.fprintf sout "Got %d Headers\n" n;
		  c := cn;
		  for j = 1 to n do
		    let (h,cn) = sei_hashval seis !c in
		    let (bh,cn) = sei_blockheader seis cn in
		    c := cn;
		    Printf.fprintf sout "%d. %s\n" j (hashval_hexstring h)
		  done
		end
	    | _ -> ()
	  in
	  Printf.fprintf sout "++++++++++++++++++++++++\nReport of all sent messages:\n";
	  let f = open_in_bin sentf in
	  begin
	    try
	      while true do
		let (tmstmp,_) = sei_int64 seic (f,None) in
		let gtm = Unix.gmtime (Int64.to_float tmstmp) in
		Printf.fprintf sout "Sending At Time: %Ld (UTC %02d %02d %04d %02d:%02d:%02d)\n" tmstmp gtm.Unix.tm_mday (1+gtm.Unix.tm_mon) (1900+gtm.Unix.tm_year) gtm.Unix.tm_hour gtm.Unix.tm_min gtm.Unix.tm_sec;
		let (magic,_) = sei_int32 seic (f,None) in
		if magic = 0x44616c54l then Printf.fprintf sout "Testnet message\n" else if magic = 0x44616c4dl then Printf.fprintf sout "Mainnet message\n" else Printf.fprintf sout "Bad Magic Number %08lx\n" magic;
		let rby = input_byte f in
		if rby = 0 then
		  Printf.fprintf sout "Not a reply\n"
		else if rby = 1 then
		  begin
		    let (h,_) = sei_hashval seic (f,None) in
		    Printf.fprintf sout "Reply to %s\n" (hashval_hexstring h)
		  end
		else
		  Printf.fprintf sout "Bad Reply Byte %d\n" rby;
		let mti = input_byte f in
		Printf.fprintf sout "Message type %d: %s\n" mti (try string_of_msgtype (msgtype_of_int mti) with Not_found -> "no such message type");
		let (msl,_) = sei_int32 seic (f,None) in
		Printf.fprintf sout "Message contents length %ld bytes\n" msl;
		let (mh,_) = sei_hashval seic (f,None) in
		Printf.fprintf sout "Message contents hash %s\n" (hashval_hexstring mh);
		let sb = Buffer.create 100 in
		for i = 1 to (Int32.to_int msl) do
		  let x = input_byte f in
		  Buffer.add_char sb (Char.chr x)
		done;
		let s = Buffer.contents sb in
		Printf.fprintf sout "Message contents: %s\n" (string_hexstring s);
		try let mt = msgtype_of_int mti in extra_log_info mt s with Not_found -> ()
	      done
	    with
	    | End_of_file -> ()
	    | e -> Printf.fprintf sout "Exception: %s\n" (Printexc.to_string e)
	  end;
	  close_in f;
	  List.iter
	    (fun fn ->
	      Printf.fprintf sout "++++++++++++++++++++++++\nReport of all messages received via %s:\n" fn;
	      let f = open_in_bin fn in
	      begin
		try
		  while true do
		    let tmstmp : float = input_value f in
		    let gtm = Unix.gmtime tmstmp in
		    Printf.fprintf sout "Received At Time: %f (UTC %02d %02d %04d %02d:%02d:%02d)\n" tmstmp gtm.Unix.tm_mday (1+gtm.Unix.tm_mon) (1900+gtm.Unix.tm_year) gtm.Unix.tm_hour gtm.Unix.tm_min gtm.Unix.tm_sec;
		    let rmmm : hashval option * hashval * msgtype * string = input_value f in
		    let (replyto,mh,mt,m) = rmmm in
		    begin
		      match replyto with
		      | None -> Printf.fprintf sout "Not a reply\n"
		      | Some(h) -> Printf.fprintf sout "Reply to %s\n" (hashval_hexstring h)
		    end;
		    Printf.fprintf sout "Message type %d: %s\n" (int_of_msgtype mt) (string_of_msgtype mt);
		    Printf.fprintf sout "Message contents hash %s\n" (hashval_hexstring mh);
		    Printf.fprintf sout "Message contents: %s\n" (string_hexstring m);
		    extra_log_info mt m
		  done
		with
		| End_of_file -> ()
		| e -> Printf.fprintf sout "Exception: %s\n" (Printexc.to_string e)
	      end;
	      close_in f)
	    recfl;
	  !exitfn 0
    end;
    if !Config.seed = "" && !Config.lastcheckpoint = "" then
      begin
	raise (Failure "Need either a seed (to validate the genesis block) or a lastcheckpoint (to start later in the blockchain); have neither")
      end;
    if not (!Config.seed = "") then
      begin
	if not (String.length !Config.seed = 64) then raise (Failure "Bad seed");
	try
	  genesisstakemod := hexstring_hashval !Config.seed
	with
	| Invalid_argument(_) ->
	    raise (Failure "Bad seed")
      end;
    if not !Config.offline && not !Config.ltcoffline then
      begin
	if not !Config.daemon then (Printf.fprintf sout "Syncing with ltc.\n"; flush sout);
	ltc_init sout;
	if not !Config.daemon then (Printf.fprintf sout "Building block tree.\n"; flush sout);
	initialize_dlc_from_ltc sout !ltc_bestblock;
      end;
    Printf.fprintf sout "Loading wallet\n"; flush sout;
    Commands.load_wallet();
    let efn = !exitfn in
    exitfn := (fun n -> Commands.save_wallet(); efn n);
    Printf.fprintf sout "Loading txpool\n"; flush sout;
    Commands.load_txpool();
    (*** We next compute a nonce for the node to prevent self conns; it doesn't need to be cryptographically secure ***)
    if not !random_initialized then initialize_random_seed();
    let n = rand_int64() in
    this_nodes_nonce := n;
    log_string (Printf.sprintf "Nonce: %Ld\n" n);
  end;;

exception Timeout;;

let run_with_timeout timeout f x =
  let old_handler = Sys.signal Sys.sigalrm
    (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    ignore (f x);
    finish ()
  with Timeout -> finish ()
  | exn -> finish (); raise exn;;

let main () =
  initialize_commands();
  datadir_from_command_line(); (*** if -datadir=... is on the command line, then set Config.datadir so we can find the config file ***)
  process_config_file();
  process_config_args(); (*** settings on the command line shadow those in the config file ***)
  let last_failure = ref None in
  let failure_count = ref 0 in
  let failure_delay() =
    let tm = ltc_medtime() in
    match !last_failure with
    | Some(tm0) ->
	let d = Int64.sub tm tm0 in
	if d > 21600L then (** first failure in 6 hours, reset failure count to 1 and only delay 1 second **)
	  begin
	    failure_count := 1;
	    last_failure := Some(tm);
	    Thread.delay 1.0
	  end
	else if !failure_count > 100 then (** after 100 regular failures, just exit **)
	  begin
	    closelog();
	    !exitfn 1
	  end
	else
	  begin
	    incr failure_count;
	    last_failure := Some(tm);
	    Thread.delay (float_of_int !failure_count) (** with each new failure, delay for longer **)
	  end
    | None ->
	incr failure_count;
	last_failure := Some(tm);
	Thread.delay 1.0
  in
  let readevalloop () =
    while true do
      try
	Printf.printf "%s" !Config.prompt; flush stdout;
	let l = read_line() in
	do_command stdout l
      with
      | GettingRemoteData -> Printf.printf "Requested some remote data; try again.\n"
      | Exit -> () (*** silently ignore ***)
      | End_of_file ->
	  closelog();
	  Printf.printf "Shutting down threads. Please be patient.\n"; flush stdout;
	  !exitfn 0
      | Failure(x) ->
	  Printf.fprintf stdout "Ignoring Uncaught Failure: %s\n" x; flush stdout;
	  failure_delay()
      | exn -> (*** unexpected ***)
	  Printf.fprintf stdout "Ignoring Uncaught Exception: %s\n" (Printexc.to_string exn); flush stdout;
	  failure_delay()
    done
  in
  let daemon_readevalloop () =
    let lst = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let ia = Unix.inet_addr_of_string "127.0.0.1" in
    begin
      try
	Unix.bind lst (Unix.ADDR_INET(ia,!Config.rpcport));
      with _ ->
	Printf.fprintf !Utils.log "Cannot bind to rpcport. Quitting.\n";
	!exitfn 1
    end;
    let efn = !exitfn in
    exitfn := (fun n -> shutdown_close lst; efn n);
    Unix.listen lst 1;
    while true do
      try
	let (s,a) = Unix.accept lst in
	let sin = Unix.in_channel_of_descr s in
	let sout = Unix.out_channel_of_descr s in
	let alrmh = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
	try
	  ignore (Unix.alarm 2);
	  let l = input_line sin in
	  if not (l = !Config.rpcuser) then raise (Failure "bad rpcuser");
	  let l = input_line sin in
	  if not (l = !Config.rpcpass) then raise (Failure "bad rpcpass");
	  let l = input_line sin in
	  ignore (Unix.alarm 60);
	  do_command sout l;
	  flush sout;
	  ignore (Unix.alarm 0);
	  ignore (Sys.signal Sys.sigalrm alrmh);
	  shutdown_close s
	with
	| Timeout -> 
	    flush sout;
	    ignore (Sys.signal Sys.sigalrm alrmh);
	    shutdown_close s
	| exn ->
	    flush sout;
	    ignore (Unix.alarm 0);
	    ignore (Sys.signal Sys.sigalrm alrmh);
	    Unix.close s;
	    raise exn
      with
      | Exit -> () (*** silently ignore ***)
      | End_of_file ->
	  closelog();
	  !exitfn 0
      | Failure(x) ->
	  log_string (Printf.sprintf "Ignoring Uncaught Failure: %s\n" x);
	  failure_delay()
      | exn -> (*** unexpected ***)
	  log_string (Printf.sprintf "Ignoring Uncaught Exception: %s\n" (Printexc.to_string exn));
	  failure_delay()
    done
  in
  if !Config.daemon then
    begin
      match Unix.fork() with
      | 0 ->
	  initialize();
	  if not !Config.offline then
	    begin
	      initnetwork !Utils.log;
	      if !Config.staking then stkth := Some(Thread.create stakingthread ());
	      if not !Config.ltcoffline then ltc_listener_th := Some(Thread.create ltc_listener ());
	    end;
	  daemon_readevalloop ()
      | pid -> Printf.printf "Dalilcoin daemon process %d started.\n" pid
    end
  else
    begin
      initialize();
      if not !Config.offline then
	begin
	  initnetwork stdout;
	  if !Config.staking then stkth := Some(Thread.create stakingthread ());
	  if not !Config.ltcoffline then ltc_listener_th := Some(Thread.create ltc_listener ());
	end;
      readevalloop()
    end;;

main();;

