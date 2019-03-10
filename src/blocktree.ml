(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Utils
open Ser
open Hashaux
open Sha256
open Hash
open Htree
open Net
open Db
open Assets
open Signat
open Tx
open Ctre
open Block
open Ltcrpc

let stxpooltm : (hashval,int64) Hashtbl.t = Hashtbl.create 1000;;
let stxpool : (hashval,stx) Hashtbl.t = Hashtbl.create 1000;;
let published_stx : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;
let unconfirmed_spent_assets : (hashval,hashval) Hashtbl.t = Hashtbl.create 100;;

let artificialledgerroot = ref None;;
let artificialbestblock = ref None;;

let recentheaders : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;
let blockinvalidated : (hashval,unit) Hashtbl.t = Hashtbl.create 1000;;

let delayed_headers : (hashval * hashval,big_int -> unit) Hashtbl.t = Hashtbl.create 100;;
let delayed_deltas : (hashval * hashval,hashval option -> hashval option -> big_int -> unit) Hashtbl.t = Hashtbl.create 100;;

let thytree : (hashval,Mathdata.ttree) Hashtbl.t = Hashtbl.create 1000;;
let sigtree : (hashval,Mathdata.stree) Hashtbl.t = Hashtbl.create 1000;;

let known_thytree_p thyroot =
  match thyroot with
  | None -> true
  | Some(r) -> Hashtbl.mem thytree r

let known_sigtree_p sigroot =
  match sigroot with
  | None -> true
  | Some(r) -> Hashtbl.mem sigtree r

let lookup_thytree thyroot =
  match thyroot with
  | None -> None
  | Some(r) -> Some(Hashtbl.find thytree r)

let lookup_sigtree sigroot =
  match sigroot with
  | None -> None
  | Some(r) -> Some(Hashtbl.find sigtree r)

let add_thytree thyroot otht =
  match thyroot,otht with
  | Some(r),Some(tht) -> if not (Hashtbl.mem thytree r) then Hashtbl.add thytree r tht
  | _,_ -> ()

let add_sigtree sigroot osigt =
  match sigroot,osigt with
  | Some(r),Some(sigt) -> if not (Hashtbl.mem sigtree r) then Hashtbl.add sigtree r sigt
  | _,_ -> ()

let rec get_all_theories t =
  match t with
  | None -> []
  | Some(HBin(tl,tr)) -> get_all_theories tl @ get_all_theories tr
  | Some(HLeaf(x)) ->
      match Mathdata.hashtheory x with
      | Some(h) -> [(h,x)]
      | None -> raise (Failure "empty theory ended up in the theory tree somehow")

let rec get_all_signas t loc =
  match t with
  | None -> []
  | Some(HLeaf(x)) -> [(bitseq_hashval (List.rev loc),Mathdata.hashsigna x,x)]
  | Some(HBin(tl,tr)) -> get_all_signas tl (false::loc) @ get_all_signas tr (true::loc)

let rec get_added_theories t1 t2 =
  match (t1,t2) with
  | (None,t2) -> get_all_theories t2
  | (Some(HLeaf(_)),Some(HLeaf(_))) -> [] (*** assume equal, which should be an invariant ***)
  | (Some(HBin(t1l,t1r)),Some(HBin(t2l,t2r))) -> get_added_theories t1l t2l @ get_added_theories t1r t2r (*** inefficient, but new theories should be rare ***)
  | (_,_) -> raise (Failure("Impossible pair of old and new theory trees"))

let rec get_added_signas t1 t2 loc =
  match (t1,t2) with
  | (None,t2) -> get_all_signas t2 loc
  | (Some(HLeaf(_)),Some(HLeaf(_))) -> [] (*** assume equal, which should be an invariant ***)
  | (Some(HBin(t1l,t1r)),Some(HBin(t2l,t2r))) -> get_added_signas t1l t2l (false::loc) @ get_added_signas t1r t2r (true::loc) (*** inefficient, but new signatures should be rare ***)
  | (_,_) -> raise (Failure("Impossible pair of old and new signature trees"))

(*** save information indicating how to rebuild the theory and signature trees upon initialization ***)
let update_theories oldthyroot oldthytree newthytree =
  let newthyroot = Mathdata.ottree_hashroot newthytree in
  if not (oldthyroot = newthyroot) then
    begin
      match newthyroot with
      | None -> raise (Failure "cannot go from nonempty thy tree to empty thy tree")
      | Some(newthyrootreal) ->
	  let addedtheories = get_added_theories oldthytree newthytree in
	  List.iter
	    (fun (h,thy) -> Mathdata.DbTheory.dbput h thy)
	    addedtheories;
	  let ttf = Filename.concat (datadir()) "theorytreeinfo" in
	  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 ttf in
	  seocf (seo_prod3 (seo_option seo_hashval) seo_hashval (seo_list seo_hashval) seoc
		   (oldthyroot,newthyrootreal,List.map (fun (h,_) -> h) addedtheories)
		   (ch,None));
	  close_out ch;
	  add_thytree newthyroot newthytree
    end

let update_signatures oldsigroot oldsigtree newsigtree =
  let newsigroot = Mathdata.ostree_hashroot newsigtree in
  if not (oldsigroot = newsigroot) then
    begin
      match newsigroot with
      | None -> raise (Failure "cannot go from nonempty sig tree to empty sig tree")
      | Some(newsigrootreal) ->
	  let addedsignas = get_added_signas oldsigtree newsigtree [] in
	  List.iter
	    (fun (_,k,signa) -> Mathdata.DbSigna.dbput k signa)
	    addedsignas;
	  let stf = Filename.concat (datadir()) "signatreeinfo" in
	  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 stf in
	  seocf (seo_prod3 (seo_option seo_hashval) seo_hashval (seo_list (seo_prod seo_hashval seo_hashval)) seoc
		   (oldsigroot,newsigrootreal,List.map (fun (h,k,_) -> (h,k)) addedsignas)
		   (ch,None));
	  close_out ch;
	  add_sigtree newsigroot newsigtree
    end

let init_thytrees () =
  let ttf = Filename.concat (datadir()) "theorytreeinfo" in
  if Sys.file_exists ttf then
    let ch = open_in_bin ttf in
    try
      while true do
	let ((oldroot,newroot,added),_) = sei_prod3 (sei_option sei_hashval) sei_hashval (sei_list sei_hashval) seic (ch,None) in
	try
	  let oldthytree = lookup_thytree oldroot in
	  let newthytree = ref oldthytree in
	  List.iter
	    (fun h ->
	      try
		let th = Mathdata.DbTheory.dbget h in
		newthytree := Some(Mathdata.ottree_insert !newthytree (hashval_bitseq h) th)
	      with Not_found ->
		raise (Failure("fatal error trying to initialize theory trees; unknown theory " ^ (hashval_hexstring h))))
	    added;
	  let newroot2 = Mathdata.ottree_hashroot !newthytree in
	  if newroot2 = Some(newroot) then
	    begin
	      match !newthytree with
	      | Some(ntt) -> Hashtbl.add thytree newroot ntt
	      | None -> () (*** should not happen ***)
	    end
	  else
	    begin
	      close_in ch;
	      raise (Failure("fatal error trying to initialize theory trees; theory tree root mismatch expected " ^ (hashval_hexstring newroot) ^ " but got " ^ (match newroot2 with None -> "None" | Some(h) -> hashval_hexstring h)))
	    end
	with Not_found ->
	  close_in ch;
	  raise (Failure("fatal error trying to initialize theory trees; did not build tree with root " ^ (match oldroot with None -> "None" | Some(h) -> hashval_hexstring h)))
      done
    with End_of_file ->
      close_in ch

let init_sigtrees () =
  let stf = Filename.concat (datadir()) "signatreeinfo" in
  if Sys.file_exists stf then
    let ch = open_in_bin stf in
    try
      while true do
	let ((oldroot,newroot,added),_) = sei_prod3 (sei_option sei_hashval) sei_hashval (sei_list (sei_prod sei_hashval sei_hashval)) seic (ch,None) in
	try
	  let oldsigtree = lookup_sigtree oldroot in
	  let newsigtree = ref oldsigtree in
	  List.iter
	    (fun (h,k) ->
	      try
		let s = Mathdata.DbSigna.dbget k in
		newsigtree := Some(Mathdata.ostree_insert !newsigtree (hashval_bitseq h) s)
	      with Not_found ->
		raise (Failure("fatal error trying to initialize signature trees; unknown signa " ^ (hashval_hexstring h))))
	    added;
	  let newroot2 = Mathdata.ostree_hashroot !newsigtree in
	  if newroot2 = Some(newroot) then
	    begin
	      match !newsigtree with
	      | Some(nst) -> Hashtbl.add sigtree newroot nst
	      | None -> ()
	    end
	  else
	    begin
	      close_in ch;
	      raise (Failure("fatal error trying to initialize signature trees; signa tree root mismatch expected " ^ (hashval_hexstring newroot) ^ " but got " ^ (match newroot2 with None -> "None" | Some(h) -> hashval_hexstring h)))
	    end
	with Not_found ->
	  close_in ch;
	  raise (Failure("fatal error trying to initialize signa trees; did not build tree with root " ^ (match oldroot with None -> "None" | Some(h) -> hashval_hexstring h)))
      done
    with End_of_file ->
      close_in ch

let invalid_or_blacklisted_p h =
  if Hashtbl.mem blockinvalidated h then
    true
  else
    begin
      try
	if DbInvalidatedBlocks.dbget h then
	  true
	else
	  raise Not_found
      with Not_found ->
	try
	  DbBlacklist.dbget h
	with Not_found ->
	  false
    end

(*** assumes ancestors have been validated and info for parent is on validheadervals ***)
let process_header sout validate forw dbp (lbh,ltxh) h (bhd,bhs) currhght csm tar lmedtm burned =
  if validate then
    begin
      if valid_blockheader currhght csm tar (bhd,bhs) lmedtm burned then
	begin
	  Hashtbl.add validheadervals (lbh,ltxh) (bhd.tinfo,bhd.timestamp,bhd.newledgerroot,bhd.newtheoryroot,bhd.newsignaroot);
	  if not (DbBlockDelta.dbexists h) then missingdeltas := List.merge (fun (i,_) (j,_) -> compare i j) [(currhght,h)] !missingdeltas;
	  if dbp then
	    begin
	      DbBlockHeader.dbput h (bhd,bhs);
	      missingheaders := List.filter (fun (_,k) -> not (h = k)) !missingheaders;
	    end;
	  if forw then
	    begin
	      List.iter
		(fun (lbh,ltxh) ->
		  try
		    let f = Hashtbl.find delayed_headers (lbh,ltxh) in
		    Hashtbl.remove delayed_headers (lbh,ltxh);
		    f bhd.tinfo
		  with Not_found -> ())
		(Hashtbl.find_all outlinesucc (lbh,ltxh))
	    end
	end
      else
	begin
	  Printf.fprintf sout "Alleged block %s had an invalid header.\n" (hashval_hexstring h);
	  verbose_blockcheck := Some(!Utils.log);
	  ignore (valid_blockheader currhght csm tar (bhd,bhs) lmedtm burned);
	  verbose_blockcheck := None;
	  DbInvalidatedBlocks.dbput h true
	end
    end
  else
    begin
      Hashtbl.add validheadervals (lbh,ltxh) (bhd.tinfo,bhd.timestamp,bhd.newledgerroot,bhd.newtheoryroot,bhd.newsignaroot);
      if not (DbBlockDelta.dbexists h) then missingdeltas := List.merge (fun (i,_) (j,_) -> compare i j) [(currhght,h)] !missingdeltas;
    end


(*** this is for saving the new ctree elements in the database ***)
let process_delta_ctree h blkhght blk =
  let (blkh,blkdel) = blk in
  List.iter
    (fun stau ->
      let txid = hashstx stau in
      DbSTx.dbput txid stau)
    blkdel.blockdelta_stxl;
  begin
    let prevc = load_expanded_ctree (ctree_of_block blk) in
    let (cstk,txl) = txl_of_block blk in (*** the coinstake tx is performed last, i.e., after the txs in the block. ***)
    try
      match tx_octree_trans false false blkhght cstk (txl_octree_trans false false blkhght txl (Some(prevc))) with (*** "false false" disallows database lookups and remote requests ***)
      | Some(newc) -> ignore (save_ctree_elements newc)
      | None -> raise (Failure("transformed tree was empty, although block seemed to be valid"))
    with MaxAssetsAtAddress -> raise (Failure("transformed tree would hold too many assets at an address"))
  end

(*** assumes ancestors have been validated and info for parent is on validheadervals and entry on validblockvals;
 also assumes header has been validated and info for it is on validheadervals ***)
let rec process_delta sout validate forw dbp (lbh,ltxh) h ((bhd,bhs),bd) thtr tht sgtr sgt currhght csm tar lmedtm burned =
  if validate then
    begin
      match valid_block tht sgt currhght csm tar ((bhd,bhs),bd) lmedtm burned with
      | Some(newtht,newsigt) ->
	  Hashtbl.add validblockvals (lbh,ltxh) ();
	  sync_last_height := max !sync_last_height currhght;
	  update_theories thtr tht newtht;
	  update_signatures sgtr sgt newsigt;
	  process_delta_ctree h currhght ((bhd,bhs),bd);
	  if dbp then
	    begin
	      DbBlockDelta.dbput h bd;
	      missingdeltas := List.filter (fun (_,k) -> not (h = k)) !missingdeltas;
	    end;
	  if forw then
	    begin
	      List.iter
		(fun (lbh,ltxh) ->
		  try
		    let f = Hashtbl.find delayed_deltas (lbh,ltxh) in
		    Hashtbl.remove delayed_deltas (lbh,ltxh);
		    f bhd.newtheoryroot bhd.newsignaroot bhd.tinfo
		  with Not_found -> ())
		(Hashtbl.find_all outlinesucc (lbh,ltxh))
	    end
      | None -> (*** invalid block ***)
	  Printf.fprintf sout "Alleged block %s at height %Ld is invalid.\n" (hashval_hexstring h) currhght;
	  verbose_blockcheck := Some(!Utils.log);
	  ignore (valid_block tht sgt currhght csm tar ((bhd,bhs),bd) lmedtm burned);
	  verbose_blockcheck := None;
	  DbInvalidatedBlocks.dbput h true
    end
  else
    begin
      Hashtbl.add validheadervals (lbh,ltxh) (bhd.tinfo,bhd.timestamp,bhd.newledgerroot,bhd.newtheoryroot,bhd.newsignaroot);
      Hashtbl.add validblockvals (lbh,ltxh) ();
    end
	    
(*** assumes ancestors have been validated and info for parent is on validheadervals and entry on validblockvals ***)
let rec process_block sout validate forw dbp (lbh,ltxh) h ((bhd,bhs),bd) thtr tht sgtr sgt currhght csm tar lmedtm burned =
  if validate then
    begin
      match valid_block tht sgt currhght csm tar ((bhd,bhs),bd) lmedtm burned with
      | Some(newtht,newsigt) ->
	  Hashtbl.add validheadervals (lbh,ltxh) (bhd.tinfo,bhd.timestamp,bhd.newledgerroot,bhd.newtheoryroot,bhd.newsignaroot);
	  Hashtbl.add validblockvals (lbh,ltxh) ();
	  sync_last_height := max !sync_last_height currhght;
	  update_theories thtr tht newtht;
	  update_signatures sgtr sgt newsigt;
	  process_delta_ctree h currhght ((bhd,bhs),bd);
	  if dbp then
	    begin
	      DbBlockHeader.dbput h (bhd,bhs);
	      missingheaders := List.filter (fun (_,k) -> not (h = k)) !missingheaders;
	      DbBlockDelta.dbput h bd;
	      missingdeltas := List.filter (fun (_,k) -> not (h = k)) !missingdeltas;
	    end;
	  if forw then
	    begin
	      List.iter
		(fun (lbh,ltxh) ->
		  begin
		    try
		      let f = Hashtbl.find delayed_headers (lbh,ltxh) in
		      Hashtbl.remove delayed_headers (lbh,ltxh);
		      f bhd.tinfo
		    with Not_found -> ()
		  end;
		  begin
		    try
		      let f = Hashtbl.find delayed_deltas (lbh,ltxh) in
		      Hashtbl.remove delayed_deltas (lbh,ltxh);
		      f bhd.newtheoryroot bhd.newsignaroot bhd.tinfo
		    with Not_found -> ()
		  end)
		(Hashtbl.find_all outlinesucc (lbh,ltxh))
	    end
      | None -> (*** invalid block ***)
	  Printf.fprintf sout "Alleged block %s at height %Ld is invalid.\n" (hashval_hexstring h) currhght;
	  verbose_blockcheck := Some(!Utils.log);
	  ignore (valid_block tht sgt currhght csm tar ((bhd,bhs),bd) lmedtm burned);
	  verbose_blockcheck := None;
          DbInvalidatedBlocks.dbput h true
    end
  else
    begin
      Hashtbl.add validheadervals (lbh,ltxh) (bhd.tinfo,bhd.timestamp,bhd.newledgerroot,bhd.newtheoryroot,bhd.newsignaroot);
      Hashtbl.add validblockvals (lbh,ltxh) ();
    end

let initialize_dlc_from_ltc sout lblkh =
  let liveblocks : (hashval,unit) Hashtbl.t = Hashtbl.create 1000 in
  let liveblocks2 : (hashval * hashval,unit) Hashtbl.t = Hashtbl.create 1000 in
  let ltx_lblk : (hashval,hashval) Hashtbl.t = Hashtbl.create 1000 in
  let rec marklivenodes lbh ltx =
    try
      let (dnxt,_,_,par,_,_) = Hashtbl.find outlinevals (lbh,ltx) in
      if not (Hashtbl.mem liveblocks2 (lbh,ltx)) then
	begin
	  Hashtbl.add liveblocks2 (lbh,ltx) ();
	  Hashtbl.add liveblocks dnxt ();
	  match par with
	  | None -> ()
	  | Some(lbh,ltx) -> marklivenodes lbh ltx
	end
    with Not_found -> ()
  in
  let handleltcburntx lbh lmedtm ltx =
    if not (Hashtbl.mem outlinevals (lbh,ltx)) then
      begin
	Hashtbl.add ltx_lblk ltx lbh;
	try
	  let (burned,lprevtx,dnxt) = DbLtcBurnTx.dbget ltx in
	  Hashtbl.add blockburns dnxt (lbh,ltx);
	  if lprevtx = (0l,0l,0l,0l,0l,0l,0l,0l) then
	    begin
	      if lmedtm >= !Config.genesistimestamp && lmedtm <= Int64.add !Config.genesistimestamp 604800L then
		begin
		  Hashtbl.add outlinevals (lbh,ltx) (dnxt,lmedtm,burned,None,hashpair lbh ltx,1L);
		  if invalid_or_blacklisted_p dnxt then
		    Hashtbl.add blockinvalidated dnxt ()
		  else (*** process header and delta if we have them ***)
		    begin
		      try
			let (bhd,bhs) = DbBlockHeader.dbget dnxt in
			if bhd.prevblockhash = None then
			  begin
			    try
			      let bd = DbBlockDelta.dbget dnxt in
			      process_block sout (Hashtbl.mem recentheaders dnxt) false false (lbh,ltx) dnxt ((bhd,bhs),bd) None None None None 1L !genesisstakemod !genesistarget lmedtm burned
			    with Not_found ->
			      process_header sout (Hashtbl.mem recentheaders dnxt) false false (lbh,ltx) dnxt (bhd,bhs) 1L !genesisstakemod !genesistarget lmedtm burned
			  end
			else
			  begin
			    Printf.fprintf sout "Alleged genesis block %s had an invalid header (claims point to a previous block).\n" (hashval_hexstring dnxt);
			    DbInvalidatedBlocks.dbput dnxt true
			  end
		      with Not_found ->
			missingheaders := (1L,dnxt)::!missingheaders
		    end
		end
	    end
	  else
	    begin
	      try
		begin
		  let lprevblkh = Hashtbl.find ltx_lblk lprevtx in
		  try
		    let (prevdbh,prevlmedtm,prevburned,_,csm,prevhght) = Hashtbl.find outlinevals (lprevblkh,lprevtx) in
		    let currhght = Int64.add 1L prevhght in
		    Hashtbl.add outlinevals (lbh,ltx) (dnxt,lmedtm,burned,Some(lprevblkh,lprevtx),hashpair lbh ltx,currhght);
		    Hashtbl.add outlinesucc (lprevblkh,lprevtx) (lbh,ltx);
		    if invalid_or_blacklisted_p dnxt then
		      Hashtbl.add blockinvalidated dnxt ()
		    else (*** process header and delta if we have them ***)
		      begin
			try
			  let (bhd,bhs) = DbBlockHeader.dbget dnxt in
			  begin
			    match bhd.prevblockhash with
			    | None ->
				Printf.fprintf sout "Alleged block %s at height %Ld had an invalid header, points to no previous block but should point to %s.\n" (hashval_hexstring dnxt) currhght (hashval_hexstring prevdbh);
				DbInvalidatedBlocks.dbput dnxt true
			    | Some(prevdbh2,Poburn(lbh2,ltx2,lmedtm2,burned2)) ->
				if prevdbh = prevdbh2 && lbh2 = lprevblkh && ltx2 = lprevtx && lmedtm2 = prevlmedtm && burned2 = prevburned then
				  begin
				    try
				      let (tar,tmstmp,lr,thtr,sgtr) = Hashtbl.find validheadervals (lprevblkh,lprevtx) in
				      if not (blockheader_succ_b prevdbh lr tmstmp tar (bhd,bhs)) then
					begin
					  Printf.fprintf sout "Block %s at height %Ld is not a valid successor for %s.\n" (hashval_hexstring dnxt) currhght (hashval_hexstring prevdbh);
					  DbInvalidatedBlocks.dbput dnxt true
					end
				      else
					begin
					  if Hashtbl.mem validblockvals (lprevblkh,lprevtx) then (*** full blocks up to here have been validated ***)
					    begin
					      try
						let bd = DbBlockDelta.dbget dnxt in
						let tht = lookup_thytree thtr in
						let sgt = lookup_sigtree sgtr in
						process_block sout (Hashtbl.mem recentheaders dnxt) false false (lbh,ltx) dnxt ((bhd,bhs),bd) thtr tht sgtr sgt currhght csm tar lmedtm burned;
					      with Not_found -> (*** check if header is valid, report delta as missing ***)
						process_header sout (Hashtbl.mem recentheaders dnxt) false false (lbh,ltx) dnxt (bhd,bhs) currhght csm tar lmedtm burned
					    end
					  else (*** an ancestor delta was not validated/is missing ***)
					    process_header sout (Hashtbl.mem recentheaders dnxt) false false (lbh,ltx) dnxt (bhd,bhs) currhght csm tar lmedtm burned
					end
				    with Not_found -> (*** an ancestor header was not validated/is missing ***)
				      if not (DbBlockHeader.dbexists dnxt) then missingheaders := List.merge (fun (i,_) (j,_) -> compare i j) [(currhght,dnxt)] !missingheaders
				  end
				else
				  begin
				    Printf.fprintf sout "Alleged block %s at height %Ld had an invalid header, pointing to an incorrect previous block or proof of burn.\n" (hashval_hexstring dnxt) currhght;
				    DbInvalidatedBlocks.dbput dnxt true
				  end
			  end
			with Not_found ->
			  missingheaders := List.merge (fun (i,_) (j,_) -> compare i j) [(currhght,dnxt)] !missingheaders
		      end
		  with Not_found ->
		    Printf.fprintf sout "Missing outline info for %s:%s\n" (hashval_hexstring lprevblkh) (hashval_hexstring lprevtx)
		end
	      with Not_found ->
		Printf.fprintf sout "Could not determine ltc block in which %s was confirmed.\n" (hashval_hexstring lprevtx)
	    end
	with Not_found ->
	  Printf.fprintf sout "Missing ltc burntx %s\n" (hashval_hexstring ltx)
      end
  in
  let rec handleltcblock lbh recent =
    try
      let lds = DbLtcDacStatus.dbget lbh in
      begin
	match lds with
	| LtcDacStatusPrev(lbh2) -> handleltcblock lbh2 recent
	| LtcDacStatusNew(bds) -> (*** not all of these will contain burntxs, but these are the only ltc blocks that might contain burntxs ***)
	    begin
	      if recent then
		begin
		  List.iter
		    (fun bdl ->
		      List.iter
			(fun (bh,_,_,_,_) -> Hashtbl.replace recentheaders bh ())
			bdl)
		    bds
		end;
	      try
		let (lprevbh,lmedtm,_,ltxl) = DbLtcBlock.dbget lbh in (*** if lbh is before ltc_oldest_to_consider, then there will be no entry in the database and this will raise Not_found ***)
		handleltcblock lprevbh false;
		List.iter (handleltcburntx lbh lmedtm) ltxl;
		if recent then
		  begin
		    List.iter
		      (fun bdl ->
			List.iter
			  (fun (_,lbh,ltx,_,_) -> marklivenodes lbh ltx)
			  bdl)
		      bds
		  end;
	      with Not_found -> ()
	    end
      end
    with Not_found -> ()
  in
  handleltcblock lblkh true;
  (*** remove dead blocks (no recent descendant/permanent orphans) ***)
  missingheaders := List.filter (fun (_,h) -> Hashtbl.mem liveblocks h) !missingheaders;
  missingdeltas := List.filter (fun (_,h) -> Hashtbl.mem liveblocks h) !missingdeltas

let collect_inv m cnt tosend txinv =
  let (lastchangekey,ctips0l) = ltcdacstatus_dbget !ltc_bestblock in
  let inclh : (hashval,unit) Hashtbl.t = Hashtbl.create 5000 in
  let collect_inv_rec_blocks tosend =
    let (lastchangekey,ctips0l) = ltcdacstatus_dbget !ltc_bestblock in
    List.iter
      (fun ctips ->
	List.iter
	  (fun (bh,_,_,_,_) ->
	    if not (Hashtbl.mem inclh bh) then
	      begin
		try
		  let (bhd,_) = DbBlockHeader.dbget bh in
		  Hashtbl.add inclh bh ();
		  tosend := (int_of_msgtype Headers,bh)::!tosend;
		  if DbCTreeElt.dbexists bhd.newledgerroot then tosend := (int_of_msgtype CTreeElement,bhd.newledgerroot)::!tosend;
		  if DbBlockDelta.dbexists bh then (tosend := (int_of_msgtype Blockdelta,bh)::!tosend);
		with Not_found -> ()
	      end)
	  ctips)
      ctips0l
  in
  let rec collect_inv_stxs m cnt tosend ctipsl txinv =
    if !cnt < m then
      begin
	match txinv with
	| (txid::txinvr) ->
	    tosend := (int_of_msgtype STx,txid)::!tosend; incr cnt;
	    collect_inv_stxs m cnt tosend ctipsl txinvr
	| []  -> ()
      end
  in
  collect_inv_rec_blocks tosend;
  collect_inv_stxs m cnt tosend ctips0l txinv

let send_inv m sout cs =
  let cnt = ref 0 in
  let tosend = ref [] in
  let txinv = ref [] in
  Hashtbl.iter (fun k _ -> txinv := k::!txinv) stxpool;
  collect_inv m cnt tosend !txinv;
  send_inv_to_one !tosend cs;;

send_inv_fn := send_inv;;

let rec insertnewdelayed (tm,n) btnl =
  match btnl with
  | [] -> [(tm,n)]
  | (tm2,n2)::btnr when tm < tm2 -> (tm,n)::btnl
  | (tm2,n2)::btnr -> (tm2,n2)::insertnewdelayed (tm,n) btnr

let equ_tinfo (x,(y3,y2,y1,y0),z) (u,(v3,v2,v1,v0),w) =
   x = u && y3 = v3 && y2 = v2 && y1 = v1 && Int64.logand y0 (Int64.lognot 1L) = Int64.logand v0 (Int64.lognot 1L) && eq_big_int z w

type consensuswarning =
  | ConsensusWarningMissing of hashval * hashval * hashval
  | ConsensusWarningBlacklist of hashval
  | ConsensusWarningInvalid of hashval
  | ConsensusWarningNoBurn of hashval
  | ConsensusWarningTerminal

exception NoReq

let get_burn dbh =
  List.find
    (fun (lbk,ltx) ->
      if !Config.ltcoffline then (** this may cause a problem if there are ltc orphans **)
	true
      else
	begin
	  try
	    let (_,_,_,lbk2,_) = ltc_gettransactioninfo (hashval_hexstring ltx) in
	    lbk2 = Some(hashval_hexstring lbk)
	  with Not_found -> false
	end)
    (Hashtbl.find_all blockburns dbh)

let rec get_bestblock () =
  match !artificialbestblock with
  | Some(h,lbk,ltx) -> (Some(h,lbk,ltx),[])
  | None ->
      let (lastchangekey,ctips0l) = ltcdacstatus_dbget !ltc_bestblock in
      let tm = ltc_medtime() in
      if ctips0l = [] && tm > Int64.add !Config.genesistimestamp 604800L then
	begin
	  Printf.printf "No blocks were created in the past week. Dalilcoin has reached terminal status.\n"
	end;
      let rec get_bestblock_r2 ctips ctipsr cwl =
	match ctips with
	| [] -> get_bestblock_r ctipsr cwl
	| (dbh,lbh,ltxh,ltm,lhght)::ctipr ->
	    begin
	      if DbInvalidatedBlocks.dbexists dbh then
		get_bestblock_r2 ctipr ctipsr (ConsensusWarningInvalid(dbh)::cwl)
	      else if DbBlacklist.dbexists dbh then
		get_bestblock_r2 ctipr ctipsr (ConsensusWarningBlacklist(dbh)::cwl)
	      else
		begin
		  try
		    let (lbk,ltx) = get_burn dbh in
		    if Hashtbl.mem validblockvals (lbk,ltx) then
		      (Some(dbh,lbk,ltx),cwl)
		    else
		      get_bestblock_r2 ctipr ctipsr (ConsensusWarningMissing(dbh,lbk,ltx)::cwl)
		  with Not_found ->
		    get_bestblock_r2 ctipr ctipsr (ConsensusWarningNoBurn(dbh)::cwl)
		end
	    end
      and get_bestblock_r ctipsl cwl =
	match ctipsl with
	| [] ->
	    let tm = ltc_medtime() in
	    if tm > Int64.add !Config.genesistimestamp 604800L then
	      begin
		raise (Failure "cannot find best validated header; probably out of sync")
	      end
	    else
	      (None,cwl)
	| ctips::ctipsr ->
	    get_bestblock_r2 ctips ctipsr cwl
      in
      let cwl =
	let tm = ltc_medtime() in
	if ctips0l = [] && tm > Int64.add !Config.genesistimestamp 604800L then
	  [ConsensusWarningTerminal]
	else
	  []
      in
      get_bestblock_r ctips0l cwl

let publish_stx txh stx1 =
  if not (Hashtbl.mem stxpool txh) then Hashtbl.add stxpool txh stx1;
  DbSTx.dbput txh stx1;
  Hashtbl.add published_stx txh ();
  broadcast_inv [(int_of_msgtype STx,txh)]

let publish_block blkh bhh (bh,bd) =
  log_string (Printf.sprintf "publishing block %s\n" (hashval_hexstring bhh));
  broadcast_inv [(int_of_msgtype Headers,bhh);(int_of_msgtype Blockdelta,bhh)];;

Hashtbl.add msgtype_handler GetHeader
  (fun (sin,sout,cs,ms) ->
    let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
    let i = int_of_msgtype GetHeader in
    let tm = Unix.time() in
    if recently_sent (i,h) tm cs.sentinv then (*** don't resend ***)
      begin
	log_string (Printf.sprintf "recently sent header %s to %s; not resending\n" (hashval_hexstring h) cs.addrfrom);
      end
    else
      try
	let (bhd,bhs) as bh = DbBlockHeader.dbget h in
	let s = Buffer.create 1000 in
	log_string (Printf.sprintf "sending header %s to %s upon request at time %f (GetHeader)\n" (hashval_hexstring h) cs.addrfrom (Unix.time()));
	seosbf (seo_blockheader seosb bh (seo_hashval seosb h (seo_int8 seosb 1 (s,None))));
	Hashtbl.replace cs.sentinv (i,h) tm;
	let ss = Buffer.contents s in
	ignore (queue_msg cs Headers ss)
      with Not_found ->
	(*** don't have it to send, ignore ***)
	());;

Hashtbl.add msgtype_handler GetHeaders
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let m = ref 0 in
    let bhl = ref [] in
    let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
    c := cn;
    let i = int_of_msgtype GetHeader in
    let tm = Unix.time() in
    for j = 1 to n do
      let (h,cn) = sei_hashval seis !c in
      c := cn;
      if recently_sent (i,h) tm cs.sentinv then (*** don't resend ***)
	begin
	  log_string (Printf.sprintf "recently sent header %s to %s; not resending\n" (hashval_hexstring h) cs.addrfrom);
	end
      else
	try
	  let (blkhd1,blkhs1) as bh = DbBlockHeader.dbget h in
	  if not (blockheader_id bh = h) then
	    log_string (Printf.sprintf "Serious bug: not sending blockheader %s since it does not have correct id but instead %s\n" (hashval_hexstring h) (hashval_hexstring (blockheader_id bh)))
	  else
	    begin
	      incr m;
	      bhl := (h,bh)::!bhl;
	      log_string (Printf.sprintf "sending header %s to %s upon request at time %f (GetHeaders)\n" (hashval_hexstring h) cs.addrfrom (Unix.time()));
	      Hashtbl.replace cs.sentinv (i,h) tm
	    end;
	with
	| Not_found ->
	  (*** don't have it to send, ignore ***)
	    ()
	| e -> (** ignore any other exception ***)
	    log_string (Printf.sprintf "unexpected exception when handling GetHeaders: %s\n" (Printexc.to_string e))
      done;
    let s = Buffer.create 10000 in
    log_string (Printf.sprintf "sending %d headers\n" !m);
    let co = ref (seo_int8 seosb !m (s,None)) in
    List.iter (fun (h,bh) -> co := seo_blockheader seosb bh (seo_hashval seosb h !co)) !bhl;
    seosbf !co;
    let ss = Buffer.contents s in
    ignore (queue_msg cs Headers ss)
  );;

let deserialize_exc_protect cs f =
  try
    f()
  with e ->
    log_string (Printf.sprintf "Deserialization exception: %s\nDisconnecting and banning node %s\n" (Printexc.to_string e) cs.realaddr);
    cs.banned <- true;
    raise e;;

Hashtbl.add msgtype_handler Headers
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let (n,cn) = sei_int8 seis !c in (*** peers can request at most 255 headers at a time **)
    log_string (Printf.sprintf "get %d Headers\n" n);
    c := cn;
    let tm = Unix.time() in
    for j = 1 to n do
      let (h,cn) = sei_hashval seis !c in
      let (bh,cn) = deserialize_exc_protect cs (fun () -> sei_blockheader seis cn) in (*** deserialize if only to get to the next one ***)
      c := cn;
      log_string (Printf.sprintf "Headers msg %d %s at time %f\n"j (hashval_hexstring h) tm);
      begin
	try
	  let (lbk,ltx) = get_burn h in
	  let (bhd,bhs) = bh in
	  if not (Hashtbl.mem validheadervals (lbk,ltx)) then
	    begin
	      let (dbh,lmedtm,burned,par,newcsm,currhght) = Hashtbl.find outlinevals (lbk,ltx) in
	      if not (dbh = h) then
		begin
		  log_string (Printf.sprintf "Impossible Error: Header burn mismatch %s %s %s != %s\n" (hashval_hexstring lbk) (hashval_hexstring ltx) (hashval_hexstring dbh) (hashval_hexstring h))
		end
	      else
		begin
		  match par with
		  | None -> (*** genesis ***)
		      if bhd.prevblockhash = None then
			process_header !Utils.log true true true (lbk,ltx) h (bhd,bhs) currhght !genesisstakemod !genesistarget lmedtm burned
		      else
			begin
			  Printf.fprintf !Utils.log "Alleged genesis block %s had an invalid header (claims point to a previous block).\n" (hashval_hexstring h);
			  DbInvalidatedBlocks.dbput h true
			end
		  | Some(plbk,pltx) ->
		      try
			let (pdbh,plmedtm,pburned,_,csm,_) = Hashtbl.find outlinevals (plbk,pltx) in
			if bhd.prevblockhash = Some(pdbh,Poburn(plbk,pltx,plmedtm,pburned)) then
			  begin
			    try
			      let (tar,_,_,_,_) = Hashtbl.find validheadervals (plbk,pltx) in
			      process_header !Utils.log true true true (lbk,ltx) h (bhd,bhs) currhght csm tar lmedtm burned
			    with Not_found ->
			      Hashtbl.add delayed_headers (lbk,ltx) (fun tar -> process_header !Utils.log true true true (lbk,ltx) h (bhd,bhs) currhght csm tar lmedtm burned)
			  end
			else
			  begin
			    Printf.fprintf !Utils.log "Alleged block %s at height %Ld had an invalid header, pointing to an incorrect previous block or proof of burn.\n" (hashval_hexstring h) currhght;
			    DbInvalidatedBlocks.dbput h true
			  end
		      with Not_found -> () (*** do not know the burn for the parent; ignore header ***)
		end
	    end
	with Not_found -> () (*** the burn is not there, so ignore the header ***)
      end
    done);;

let req_headers sout cs m nw =
  if m > 0 then
    begin
      let s = Buffer.create 1000 in
      let co = ref (seo_int8 seosb m (s,None)) in
      List.iter (fun h -> co := seo_hashval seosb h !co) nw;
      seosbf !co;
      ignore (queue_msg cs GetHeaders (Buffer.contents s))
    end;;

let rec req_header_batches sout cs m hl nw =
  if m = 255 then
    (req_headers sout cs m nw; req_header_batches sout cs 0 hl [])
  else
    match hl with
    | h::hr ->
	let i = int_of_msgtype GetHeader in
	let tm = Unix.time() in
	Hashtbl.replace cs.invreq (i,h) tm;
	req_header_batches sout cs (m+1) hr (h::nw)
    | [] -> req_headers sout cs m nw;;

Hashtbl.add msgtype_handler GetInvNbhd
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let ((i,h),cn) = sei_prod sei_int8 sei_hashval seis !c in
    c := cn;
    match msgtype_of_int i with
    | Headers ->
	begin
	  let tosend = ref [] in
	  collect_header_inv_nbhd 8 h tosend;
	  if not (!tosend = []) then send_inv_to_one !tosend cs
	end
    | CTreeElement ->
	begin
	  try
	    let c = DbCTreeElt.dbget h in
	    let tosend = ref [] in
	    collect_ctree_inv_nbhd c tosend;
	    if not (!tosend = []) then send_inv_to_one !tosend cs
	  with Not_found ->
	    ()
	end
    | HConsElement ->
	begin
	  let tosend = ref [] in
	  collect_hcons_inv_nbhd 8 h tosend;
	  if not (!tosend = []) then send_inv_to_one !tosend cs
	end
    | _ -> ());;

Hashtbl.add msgtype_handler Inv
  (fun (sin,sout,cs,ms) ->
    let c = ref (ms,String.length ms,None,0,0) in
    let hkl = ref [] in
    let hl = ref [] in
    let (n,cn) = sei_int32 seis !c in
(*    log_string (Printf.sprintf "Inv msg %ld entries\n" n); *)
    c := cn;
    for j = 1 to Int32.to_int n do
      let ((i,h),cn) = sei_prod sei_int8 sei_hashval seis !c in
      c := cn;
      Hashtbl.replace cs.rinv (i,h) ();
      begin
	try
	  hkl := (Hashtbl.find cs.invreqhooks (i,h))::!hkl; (*** collect hook functions waiting on this inventory message to execute at the end ***)
	  Hashtbl.remove cs.invreqhooks (i,h)
	with Not_found ->
	  ()
      end;
      if i = int_of_msgtype STx && not (DbArchived.dbexists h) then
	begin
	  if not (DbSTx.dbexists h) && not (Hashtbl.mem stxpool h) then
 	    begin
	      let tm = Unix.time() in
	      Hashtbl.replace cs.invreq (int_of_msgtype GetSTx,h) tm;
              let s = Buffer.create 1000 in
	      seosbf (seo_hashval seosb h (s,None));
	      log_string (Printf.sprintf "Sending GetSTx %s to %s at %f\n" (hashval_hexstring h) cs.realaddr tm);
	      ignore (queue_msg cs GetSTx (Buffer.contents s))
	    end
	end
    done;
    List.iter (fun hk -> ignore (Thread.create hk ())) !hkl;
    req_header_batches sout cs 0 !hl []);;

Hashtbl.add msgtype_handler GetBlockdelta
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetBlockdelta in
      let tm = Unix.time() in
      if recently_sent (i,h) tm cs.sentinv then (*** don't resend ***)
	begin
	  log_string (Printf.sprintf "recently sent delta %s to %s; not resending\n" (hashval_hexstring h) cs.addrfrom);
	end
      else
	try
	  let blkdel = DbBlockDelta.dbget h in
	  let bdsb = Buffer.create 100 in
	  seosbf (seo_blockdelta seosb blkdel (seo_hashval seosb h (bdsb,None)));
	  let bdser = Buffer.contents bdsb in
	  ignore (queue_msg cs Blockdelta bdser);
	  Hashtbl.replace cs.sentinv (i,h) tm
	with Not_found ->
	  log_string (Printf.sprintf "Unknown Block Delta %s (Bad Peer or Did I Advertize False Inventory?)\n" (hashval_hexstring h));
	  ());;

Hashtbl.add msgtype_handler Blockdelta
  (fun (sin,sout,cs,ms) ->
    let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
    begin
      try
	let (lbk,ltx) = get_burn h in
	let (dbh,lmedtm,burned,par,newcsm,currhght) = Hashtbl.find outlinevals (lbk,ltx) in
	if not (dbh = h) then
	  begin
	    log_string (Printf.sprintf "Impossible Error: Delta burn mismatch %s %s %s != %s\n" (hashval_hexstring lbk) (hashval_hexstring ltx) (hashval_hexstring dbh) (hashval_hexstring h))
	  end
	else
	  begin
	    if not (Hashtbl.mem validblockvals (lbk,ltx)) then
	      begin
		let (bd,_) = deserialize_exc_protect cs (fun () -> sei_blockdelta seis r) in
		let bh = DbBlockHeader.dbget h in
		match par with
		| None -> (*** genesis ***)
		    process_delta !Utils.log true true true (lbk,ltx) h (bh,bd) None None None None currhght !genesisstakemod !genesistarget lmedtm burned
		| Some(plbk,pltx) ->
		    let (_,_,_,_,csm,_) = Hashtbl.find outlinevals (plbk,pltx) in
		      try
			let (tar,_,_,thtr,sgtr) = Hashtbl.find validheadervals (plbk,pltx) in
			let tht = lookup_thytree thtr in
			let sgt = lookup_sigtree sgtr in
			process_delta !Utils.log true true true (lbk,ltx) h (bh,bd) thtr tht sgtr sgt currhght csm tar lmedtm burned
		      with Not_found ->
			Hashtbl.add delayed_deltas (lbk,ltx)
			  (fun thtr sgtr tar ->
			    let tht = lookup_thytree thtr in
			    let sgt = lookup_sigtree sgtr in
			    process_delta !Utils.log true true true (lbk,ltx) h (bh,bd) thtr tht sgtr sgt currhght csm tar lmedtm burned)
	      end
	  end
      with Not_found ->
	log_string (Printf.sprintf "Got delta %s but not enough info for it so dropping it" (hashval_hexstring h))
    end);;

Hashtbl.add msgtype_handler GetSTx
    (fun (sin,sout,cs,ms) ->
      let (h,_) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetSTx in
      let tm = Unix.time() in
      if not (recently_sent (i,h) tm cs.sentinv) then (*** don't resend ***)
	try
	  let stau = Hashtbl.find stxpool h in
	  let stausb = Buffer.create 100 in
	  seosbf (seo_stx seosb stau (seo_hashval seosb h (stausb,None)));
	  let stauser = Buffer.contents stausb in
	  log_string (Printf.sprintf "Sending Signed Tx (from pool) %s\n" (hashval_hexstring h));
	  ignore (queue_msg cs STx stauser);
	  Hashtbl.replace cs.sentinv (i,h) tm
	with Not_found ->
	  try
	    let stau = DbSTx.dbget h in
	    let stausb = Buffer.create 100 in
	    seosbf (seo_stx seosb stau (seo_hashval seosb h (stausb,None)));
	    let stauser = Buffer.contents stausb in
	    log_string (Printf.sprintf "Sending Signed Tx (from db) %s\n" (hashval_hexstring h));
	    ignore (queue_msg cs STx stauser);
	    Hashtbl.replace cs.sentinv (i,h) tm
	  with Not_found ->
	    log_string (Printf.sprintf "Unknown Tx %s\n" (hashval_hexstring h));
	    ());;

let add_to_txpool txid stau =
  Hashtbl.add stxpool txid stau;
  let ((txin,_),_) = stau in
  List.iter (fun (_,h) -> Hashtbl.add unconfirmed_spent_assets h txid) txin

let remove_from_txpool txid =
  try
    let stau = Hashtbl.find stxpool txid in
    Hashtbl.remove stxpool txid;
    let ((txin,_),_) = stau in
    List.iter (fun (_,h) -> Hashtbl.remove unconfirmed_spent_assets h) txin
  with Not_found -> ()

let savetxtopool_real txid stau =
  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 (Filename.concat (datadir()) "txpool") in
  seocf (seo_prod seo_hashval seo_stx seoc (txid,stau) (ch,None));
  close_out ch;
  let ch = open_out_gen [Open_creat;Open_append;Open_wronly;Open_binary] 0o660 (Filename.concat (datadir()) "txpooltm") in
  seocf (seo_prod seo_int64 seo_hashval seoc (Int64.of_float (Unix.time()),txid) (ch,None));
  close_out ch;;

Hashtbl.add msgtype_handler STx
    (fun (sin,sout,cs,ms) ->
      let (h,r) = sei_hashval seis (ms,String.length ms,None,0,0) in
      let i = int_of_msgtype GetSTx in
      let tm = Unix.time() in
      log_string (Printf.sprintf "Got Signed Tx %s from %s at %f\n" (hashval_hexstring h) cs.realaddr tm);
      if not (DbSTx.dbexists h) && not (Hashtbl.mem stxpool h) then (*** if we already have it, abort ***)
	if recently_requested (i,h) tm cs.invreq then (*** only continue if it was requested ***)
          let (((tauin,tauout) as tau,_) as stau,_) = deserialize_exc_protect cs (fun () -> sei_stx seis r) in
	  if hashstx stau = h then
	    begin
	      try
		begin
		  match get_bestblock() with
		  | (Some(dbh,lbk,ltx),_) -> (*** ignore consensus warnings here ***)
		      begin
			let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
			let (_,tmstmp,lr,tr,sr) = Hashtbl.find validheadervals (lbk,ltx) in
			if tx_valid tmstmp tau then
			  let unsupportederror alpha k = log_string (Printf.sprintf "Could not find asset %s at address %s in ledger %s; throwing out tx %s\n" (hashval_hexstring k) (Cryptocurr.addr_daliladdrstr alpha) (hashval_hexstring lr) (hashval_hexstring h)) in
			  let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin (CHash(lr)) unsupportederror) in
			  if tx_signatures_valid blkh tmstmp al stau then
			    begin
			      let nfee = ctree_supports_tx true false (lookup_thytree tr) (lookup_sigtree sr) blkh tau (CHash(lr)) in
			      let fee = Int64.sub 0L nfee in
			      if fee >= !Config.minrelayfee then
				begin
				  Hashtbl.add stxpool h stau;
				  log_string (Printf.sprintf "Accepting tx %s into pool\n" (hashval_hexstring h));
				  add_to_txpool h stau;
				  savetxtopool_real h stau
				end
			      else
				(log_string (Printf.sprintf "ignoring tx %s with low fee of %s fraenks (%Ld cants)\n" (hashval_hexstring h) (Cryptocurr.fraenks_of_cants fee) fee))
			    end
			  else
			    (log_string (Printf.sprintf "ignoring tx %s since signatures are not valid at the current block height of %Ld\n" (hashval_hexstring h) blkh))
			else
			  (log_string (Printf.sprintf "misbehaving peer? [invalid Tx %s]\n" (hashval_hexstring h)))
		      end
		    | _ -> raise Not_found
		end
	      with _ ->
		(log_string (Printf.sprintf "Tx %s is unsupported by the local ledger, dropping it.\n" (hashval_hexstring h)))
	    end
          else (*** otherwise, it seems to be a misbehaving peer --  ignore for now ***)
	    (log_string (Printf.sprintf "misbehaving peer? [malformed Tx]\n"))
	else (*** if something unrequested was sent, then seems to be a misbehaving peer ***)
	  (log_string (Printf.sprintf "misbehaving peer? [unrequested Tx %s]\n" (hashval_hexstring h))));;

let dumpblocktreestate sa =
  Printf.fprintf sa "=========\nstxpool:\n";
  Hashtbl.iter
    (fun h ((tauin,tauout) as tau,tausg) ->
      Printf.fprintf sa "- tx %s\n" (hashval_hexstring (hashtx tau));
      Printf.fprintf sa "inputs %d\n" (List.length tauin);
      let c = ref 0 in
      List.iter
	(fun (alpha,aid) ->
	  Printf.fprintf sa "%d. %s %s\n" !c (Cryptocurr.addr_daliladdrstr alpha) (hashval_hexstring aid);
	  incr c)
	tauin;
      Printf.fprintf sa "outputs %d\n" (List.length tauin);
      c := 0;
      List.iter (fun (alpha,(obl,u)) ->
	Printf.fprintf sa "%d. %s %s %s\n" !c (Cryptocurr.addr_daliladdrstr alpha) (obligation_string obl) (preasset_string u);
	incr c)
	tauout;
      let sb = Buffer.create 100 in
      seosbf (seo_stx seosb (tau,tausg) (sb,None));
      Printf.fprintf sa "%s\n" (string_hexstring (Buffer.contents sb))
    )
    stxpool;
  Printf.fprintf sa "=========\npublished_stx:\n";
  Hashtbl.iter (fun h () ->
      Printf.fprintf sa "- tx %s\n" (hashval_hexstring h))
    published_stx;
  Printf.fprintf sa "=========\nthytree:\n";
  Hashtbl.iter (fun h _ ->
    Printf.fprintf sa "- thytree root %s\n" (hashval_hexstring h))
    thytree;
  Printf.fprintf sa "=========\nsigtree:\n";
  Hashtbl.iter (fun h _ ->
    Printf.fprintf sa "- sigtree root %s\n" (hashval_hexstring h))
    sigtree;;

let print_consensus_warning s cw =
  match cw with
  | ConsensusWarningMissing(h,lbk,ltx) ->
      begin
	try
	  let (_,_,_,_,_,blkh) = Hashtbl.find outlinevals (lbk,ltx) in
	  Printf.fprintf s "Missing Block %Ld %s%s\n"
	    blkh (hashval_hexstring h)
	    (if Hashtbl.mem validblockvals (lbk,ltx) then
	      " Block Validated"
	    else if Hashtbl.mem validheadervals (lbk,ltx) then " Header Validated"
	    else "")
	with _ ->
	  Printf.fprintf s "Missing Block %s%s\n"
	    (hashval_hexstring h)
	    (if Hashtbl.mem validblockvals (lbk,ltx) then
	      " Block Validated"
	    else if Hashtbl.mem validheadervals (lbk,ltx) then " Header Validated"
	    else "")
      end
  | ConsensusWarningBlacklist(h) ->
      Printf.fprintf s "Blacklisted Block %s\n" (hashval_hexstring h)
  | ConsensusWarningInvalid(h) ->
      Printf.fprintf s "Invalid Block %s\n" (hashval_hexstring h)
  | ConsensusWarningNoBurn(h) ->
      Printf.fprintf s "Could not find ltc burn for block %s\n" (hashval_hexstring h)
  | ConsensusWarningTerminal ->
      Printf.fprintf s "No blocks were created in the past week. Dalilcoin has reached terminal status.\nThe only recovery possible for the network is a hard fork.\nSometimes this message means the node is out of sync with ltc.\n";;
	
let get_bestblock_print_warnings s =
  let (b,cwl) = get_bestblock() in
  List.iter (print_consensus_warning s) cwl;
  b;;

let get_bestblock_cw_exception e =
  let (best,cwl) = get_bestblock() in
  begin
    try
      let cw =
	List.find
	  (fun cw ->
	    match cw with
	    | ConsensusWarningMissing(_,_,_) -> true
	    | _ -> false)
	  cwl
      in
      print_consensus_warning !log cw;
      log_string (Printf.sprintf "possibly not synced; delaying staking\n");
      raise Exit
    with
    | Not_found -> ()
    | Exit -> raise e
  end;
  match best with
  | Some(dbh,lbk,ltx) -> (dbh,lbk,ltx)
  | None -> raise e;;

let print_best_block () =
  let (b,cwl) = get_bestblock () in
  List.iter (print_consensus_warning !Utils.log) cwl;
  match b with
  | Some(bh,lbk,ltx) -> log_string (Printf.sprintf "bestblock %s\nsupported by %s %s\n" (hashval_hexstring bh) (hashval_hexstring lbk) (hashval_hexstring ltx))
  | None -> log_string (Printf.sprintf "no bestblock\n")

let rec recursively_invalidate_blocks_2 lbk ltx =
  try
    begin
      let (h,_,_,_,_,_) = Hashtbl.find outlinevals (lbk,ltx) in
      if not (Hashtbl.mem blockinvalidated h) then
	begin
	  Hashtbl.add blockinvalidated h ();
	  DbInvalidatedBlocks.dbput h true;
	end;
      List.iter
	(fun (nlbk,nltx) -> recursively_invalidate_blocks_2 nlbk nltx)
	(Hashtbl.find_all outlinesucc (lbk,ltx))
    end
  with Not_found -> ()

let recursively_invalidate_blocks h =
  Hashtbl.add blockinvalidated h ();
  DbInvalidatedBlocks.dbput h true;
  List.iter
    (fun (lbk,ltx) -> recursively_invalidate_blocks_2 lbk ltx)
    (Hashtbl.find_all blockburns h)

let rec recursively_revalidate_blocks_2 lbk ltx =
  try
    begin
      let (h,_,_,p,_,_) = Hashtbl.find outlinevals (lbk,ltx) in
      Hashtbl.remove blockinvalidated h;
      if DbInvalidatedBlocks.dbexists h then DbInvalidatedBlocks.dbdelete h;
      if DbBlacklist.dbexists h then DbBlacklist.dbdelete h;
      match p with
      | Some(plbk,pltx) -> recursively_revalidate_blocks_2 plbk pltx
      | None -> ()
    end
  with Not_found -> ()

let recursively_revalidate_blocks h =
  Hashtbl.remove blockinvalidated h;
  if DbInvalidatedBlocks.dbexists h then DbInvalidatedBlocks.dbdelete h;
  if DbBlacklist.dbexists h then DbBlacklist.dbdelete h;
  List.iter
    (fun (lbk,ltx) -> recursively_revalidate_blocks_2 lbk ltx)
    (Hashtbl.find_all blockburns h)

let reprocessblock oc h =
  try
    let bh = DbBlockHeader.dbget h in
    let (bhd,bhs) = bh in
    try
      let bd = DbBlockDelta.dbget h in
      try
	let (lbk,ltx) = get_burn h in
	let (_,lmedtm,burned,par,_,currhght) = Hashtbl.find outlinevals (lbk,ltx) in
	let (csm,tar,thtr,sgtr) =
	  match par with
	  | None -> (*** genesis ***)
	      (!genesisstakemod,!genesistarget,None,None)
	  | Some(plbk,pltx) ->
	let (_,_,_,_,csm,_) = Hashtbl.find outlinevals (plbk,pltx) in
	      let (tar,_,_,thtr,sgtr) = Hashtbl.find validheadervals (plbk,pltx) in
	      (csm,tar,thtr,sgtr)
	in
	let prevc = load_expanded_ctree (ctree_of_block (bh,bd)) in
	let (cstk,txl) = txl_of_block (bh,bd) in (*** the coinstake tx is performed last, i.e., after the txs in the block. ***)
	begin
	  try
	    match tx_octree_trans false false currhght cstk (txl_octree_trans false false currhght txl (Some(prevc))) with (*** "false false" disallows database lookups and remote requests ***)
	    | Some(newc) -> ignore (save_ctree_elements newc)
	    | None -> raise (Failure("transformed tree was empty, although block seemed to be valid"))
	  with MaxAssetsAtAddress -> raise (Failure("transformed tree would hold too many assets at an address"))
	end;
	try
	  let thytree = lookup_thytree thtr in
	  try
	    let sigtree = lookup_sigtree sgtr in
	    try
	      match valid_block thytree sigtree currhght csm tar (bh,bd) lmedtm burned with
	      | Some(tht2,sigt2) ->
		  update_theories thtr thytree tht2;
		  update_signatures sgtr sigtree sigt2
	      | None -> (*** should not have happened, delete it from the database and request it again. ***)
		  Printf.fprintf oc "Invalid block %s\n" (hashval_hexstring h);
		  recursively_invalidate_blocks_2 lbk ltx;
		  flush oc
	    with _ ->
	      Printf.fprintf oc "Invalid block %s\n" (hashval_hexstring h);
	      recursively_invalidate_blocks_2 lbk ltx;
	      flush oc
	  with Not_found ->
	    Printf.fprintf oc "Could not find signature tree for block\n";
	    flush oc
	with Not_found ->
	  Printf.fprintf oc "Could not find theory tree for block\n";
	  flush oc
      with Not_found ->
	Printf.fprintf oc "Could not find information for parent block %s\n"
	  (match bhd.prevblockhash with Some(h,_) -> (hashval_hexstring h) | None -> "(genesis)");
	flush oc
    with Not_found ->
      Printf.fprintf oc "Do not have delta for block %s\n" (hashval_hexstring h);
      flush oc
  with Not_found ->
    Printf.fprintf oc "Do not have header for block %s\n" (hashval_hexstring h);
    flush oc
