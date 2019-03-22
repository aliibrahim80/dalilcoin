(* Copyright (c) 2015-2017 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Utils
open Sha256
open Hashaux
open Hash
open Ser
open Ltcrpc
open Net
open Mathdata
open Assets
open Tx
open Secp256k1
open Cryptocurr
open Signat
open Ctre
open Ctregraft
open Block
open Blocktree
open Commands

let stakegenesis = false (*** set this to true and recompile to stake a genesis block for the testnet; then reset it to false and recompile to continue ***)

let get_reward_locktime blkh =
  let rl =
    match !Config.reward_lock_relative with
    | None -> reward_locktime
    | Some(rl) -> if rl > reward_locktime then rl else reward_locktime
  in
  let m = Int64.add blkh rl in
  match !Config.reward_lock_absolute with
  | None -> m
  | Some(a) -> if a > m then a else m

let pendingltctxs : string list ref = ref [];;

type nextstakeinfo = NextStake of (int64 * p2pkhaddr * hashval * int64 * obligation * int64 * int64 option * (hashval * hashval) option ref * hashval option * ttree option * hashval option * stree option) | NoStakeUpTo of int64;;

let nextstakechances : (hashval * hashval,nextstakeinfo) Hashtbl.t = Hashtbl.create 100;;
let nextstakechances_hypo : (hashval * hashval,nextstakeinfo) Hashtbl.t = Hashtbl.create 100;;
let nextstakechances_checkedtotm : (hashval * hashval,int64) Hashtbl.t = Hashtbl.create 100;;

let stakingassetsmutex = Mutex.create();;

let compute_recid (r,s) k =
  match smulp k _g with
  | Some(x,y) ->
      if eq_big_int x r then
	if evenp y then 0 else 1
      else
	if evenp y then 2 else 3
  | None -> raise (Failure "bad0");;

let rec hlist_stakingassets blkh alpha hl n =
  if n > 0 then
    match hl with
    | HCons((aid,bday,obl,Currency(v)),hr) ->
	let ca = coinage blkh bday obl v in
(*	log_string (Printf.sprintf "Checking asset %s %Ld %Ld %s %Ld %s\n" (hashval_hexstring aid) blkh bday (obligation_string obl) v (string_of_big_int ca)); *)
	if gt_big_int ca zero_big_int && not (Hashtbl.mem unconfirmed_spent_assets aid) then
	  begin
	    log_string (Printf.sprintf "Staking asset: %s\n" (hashval_hexstring aid));
	    Mutex.lock stakingassetsmutex;
	    Commands.stakingassets := (alpha,aid,bday,obl,v)::!Commands.stakingassets;
	    Mutex.unlock stakingassetsmutex;
	  end;
	hlist_stakingassets blkh alpha hr (n-1)
    | HCons(_,hr) -> hlist_stakingassets blkh alpha hr (n-1)
    | HConsH(h,hr) ->
	begin
	  try
	    hlist_stakingassets blkh alpha (HCons(DbAsset.dbget h,hr)) n
	  with Not_found -> ()
	end
    | HHash(h,_) ->
	begin
	  try
	    let (h1,h2) = DbHConsElt.dbget h in
	    match h2 with
	    | Some(h2,l2) -> hlist_stakingassets blkh alpha (HConsH(h1,HHash(h2,l2))) n
	    | None -> hlist_stakingassets blkh alpha (HConsH(h1,HNil)) n
	  with Not_found -> ()
	end
    | _ -> ()
  else
    ();;

let extraburn : int64 ref = ref 0L;;

let lastburn : int64 ref = ref 0L;;

let allowedburn tm =
  if !extraburn > 0L then
    true
  else if !lastburn > 0L then
    let sinceburn = Int64.sub tm !lastburn in
    sinceburn >= !Config.mintimebetweenburns
  else
    true

let maxburnnow tm =
  let mbn =
    if !lastburn > 0L then
      let sinceburn = Int64.sub tm !lastburn in
      if sinceburn < !Config.mintimebetweenburns then
	0L
      else
	min !Config.maxburn (Int64.div (Int64.mul !Config.maxburnrate (Int64.sub tm !lastburn)) 86400L)
    else
      !Config.maxburn
  in
  max !extraburn (if mbn >= !Config.ltctxfee then Int64.sub mbn !Config.ltctxfee else 0L)

exception StakingPause of float
exception StakingProblemPause

let compute_staking_chances (prevblkh,lbk,ltx) fromtm totm =
  try
    let (_,lmedtm,burned,_,csm1,prevblkhght) = Hashtbl.find outlinevals (lbk,ltx) in
    let blkhght = Int64.add 1L prevblkhght in
    let (tar1,tmstamp,currledgerroot,thyroot,sigroot) = Hashtbl.find validheadervals (lbk,ltx) in
    Hashtbl.find validblockvals (lbk,ltx);
    let thytree =
      try
	lookup_thytree thyroot
      with Not_found ->
	log_string (Printf.sprintf "Do not know theory tree with root %s\n" (match thyroot with None -> "None" | Some(h) -> hashval_hexstring h));
	raise StakingProblemPause
    in
    let sigtree =
      try
	lookup_sigtree sigroot
      with Not_found ->
	log_string (Printf.sprintf "Do not know signature tree with root %s\n" (match sigroot with None -> "None" | Some(h) -> hashval_hexstring h));
	raise StakingProblemPause
    in
    let fromtm =
      try
	Hashtbl.find nextstakechances_checkedtotm (lbk,ltx)
      with Not_found -> fromtm
    in
    let i = ref (max fromtm (Int64.add 1L tmstamp)) in
    if !Config.maxburn < 0L then (*** if must burn but not willing to burn, don't bother computing next staking chances ***)
      ()
    else
      let c = CHash(currledgerroot) in
      (*** collect assets allowed to stake now ***)
      Commands.stakingassets := [];
      let minburntostake = ref None in
      log_string (Printf.sprintf "Collecting staking assets in ledger %s (block height %Ld).\n" (hashval_hexstring currledgerroot) blkhght);
      let stakingkeys : (md160,unit) Hashtbl.t = Hashtbl.create 10 in
      List.iter
	(fun (k,b,(x,y),w,h,alpha) ->
	  Hashtbl.add stakingkeys h (); (*** remember this is a staking key to decide whether to stake with related endorsed assets ***)
	  match try ctree_addr true true (p2pkhaddr_addr h) c None with _ -> (None,0) with
	  | (Some(hl),_) ->
              hlist_stakingassets blkhght h (nehlist_hlist hl) 30
	  | _ ->
	      ())
	!Commands.walletkeys_staking;
      List.iter
	(fun (alpha,beta,_,_,_,_) ->
	  let (p,x4,x3,x2,x1,x0) = alpha in
	  let (q,y4,y3,y2,y1,y0) = beta in
	  if not p && not q && Hashtbl.mem stakingkeys (y4,y3,y2,y1,y0) then (*** only p2pkh can stake ***)
	    match try ctree_addr true true (payaddr_addr alpha) c None with _ -> (None,0) with
	    | (Some(hl),_) ->
		hlist_stakingassets blkhght (x4,x3,x2,x1,x0) (nehlist_hlist hl) 50
	    | _ -> ())
	!Commands.walletendorsements;
      log_string (Printf.sprintf "%d staking assets\n" (List.length !Commands.stakingassets));
      if not (!Commands.stakingassets = []) then
	let nextstake i stkaddr h bday obl v toburn =
	  Hashtbl.add nextstakechances (lbk,ltx) (NextStake(i,stkaddr,h,bday,obl,v,toburn,ref None,thyroot,thytree,sigroot,sigtree));
	  raise Exit
	in
	try
	  while !i < totm do
	    i := Int64.add 1L !i;
	    (*** go through assets and check for staking at time !i ***)
	    List.iter
	      (fun (stkaddr,h,bday,obl,v) ->
		(** log_string (Printf.sprintf "Checking for staking of %s at time %Ld\n" (hashval_hexstring h) !i); **)
		let caf = coinagefactor blkhght bday obl in
		if gt_big_int caf zero_big_int then
		  begin
		    let hv = hitval !i h csm1 in
		    let mtar = mult_big_int tar1 caf in
		    let minv = div_big_int hv mtar in
		    let toburn = succ_big_int (div_big_int (sub_big_int minv (big_int_of_int64 v)) (big_int_of_int 1000000)) in
		    if lt_big_int zero_big_int toburn && lt_big_int toburn (big_int_of_string "1000000000") then (*** 10 ltc limit for reporting staking chances ***)
		      begin
			match !minburntostake with
			| None ->
			    Hashtbl.add nextstakechances_hypo (lbk,ltx) (NextStake(!i,stkaddr,h,bday,obl,v,Some(int64_of_big_int toburn),ref None,thyroot,thytree,sigroot,sigtree));
			    minburntostake := Some(toburn,!i,stkaddr,h)
			| Some(mburn,_,_,_) ->
			    if lt_big_int toburn mburn then
			      begin
				Hashtbl.add nextstakechances_hypo (lbk,ltx) (NextStake(!i,stkaddr,h,bday,obl,v,Some(int64_of_big_int toburn),ref None,thyroot,thytree,sigroot,sigtree));
				minburntostake := Some(toburn,!i,stkaddr,h)
			      end
		      end;
		    if lt_big_int minv (big_int_of_int64 v) then (*** hit without burn ***)
		      (if allowedburn !i then
			nextstake !i stkaddr h bday obl v (Some(0L))) (*** burn nothing, but announce in the pow chain (ltc) ***)
		    else if allowedburn !i then
		      if le_big_int toburn (big_int_of_int64 (maxburnnow !i)) then (*** hit with burn ***)
			nextstake !i stkaddr h bday obl v (Some(int64_of_big_int toburn))
		  end
	      )
	      !Commands.stakingassets
	  done;
	  log_string (Printf.sprintf "No staking chances up to time %Ld\n" totm);
	  Hashtbl.add nextstakechances (lbk,ltx) (NoStakeUpTo(totm));
	with
	| Exit -> ()
	| exn ->
	    log_string (Printf.sprintf "Unexpected Exception in Staking Loop: %s\n" (Printexc.to_string exn))
  with exn ->
    log_string (Printf.sprintf "Unexpected Exception in Staking: %s\n" (Printexc.to_string exn));
    raise StakingProblemPause;;

exception Genesis;;
exception SyncIssue;;

(***
 The staking code underwent a major rewrite in March 2019.
 ***)
let stakingthread () =
  let sleepuntil = ref (ltc_medtime()) in
  let pending = ref None in
  while true do
    try
      let nw = ltc_medtime() in
      let sleeplen = Int64.to_float (Int64.sub !sleepuntil nw) in
      log_string (Printf.sprintf "Staking sleeplen %f seconds\n" sleeplen);
      if sleeplen > 1.0 then Thread.delay sleeplen;
      log_string (Printf.sprintf "Staking after sleeplen %f seconds\n" sleeplen);
      if not (ltc_synced()) then (log_string (Printf.sprintf "ltc not synced yet; delaying staking\n"); raise (StakingPause(60.0)));
      pendingltctxs := List.filter (fun h -> not (ltc_tx_confirmed h)) !pendingltctxs;
      if not (!pendingltctxs = []) then (log_string (Printf.sprintf "there are pending ltc txs; delaying staking\n"); raise (StakingPause(60.0)));
      let (pbhh1,lbk,ltx) = get_bestblock_cw_exception (if stakegenesis then Genesis else SyncIssue) in
      try
	let (_,plmedtm,pburned,par,csm0,pblkh) = Hashtbl.find outlinevals (lbk,ltx) in
	let (tar0,pbhtm,prevledgerroot,thtr,sgtr) = Hashtbl.find validheadervals (lbk,ltx) in
	Hashtbl.find validblockvals (lbk,ltx); (** do not stake until best block is fully validated **)
        match Hashtbl.find nextstakechances (lbk,ltx) with
	| NextStake(tm,alpha,aid,bday,obl,v,toburn,already,thyroot,thytree,sigroot,sigtree) ->
	    begin
	      match !already with
	      | Some(_,_) -> raise SyncIssue
	      | None ->
		  begin
		    let nw = ltc_medtime() in
		    log_string (Printf.sprintf "NextStake tm = %Ld nw = %Ld\n" tm nw);
		    if tm >= Int64.add nw 60L || tm <= pbhtm then
		      begin (*** wait for a minute and then reevaluate; would be better to sleep until time to publish or until a new best block is found **)
			let tmtopub = Int64.sub tm nw in
			log_string ((Int64.to_string tmtopub) ^ " seconds until time to publish staked block\n");
			if tmtopub >= 60L then
			  sleepuntil := Int64.add nw 60L
			else
			  begin
			    sleepuntil := Int64.add nw tmtopub;
			  end
		      end
		    else
		      begin (** go ahead and form the block; then publish it at the right time **)
			let deltm = Int64.to_int32 (Int64.sub tm pbhtm) in
			let blkh = Int64.add 1L pblkh in
			let tar = retarget tm tar0 deltm in
			let alpha2 = p2pkhaddr_addr alpha in
			log_string (Printf.sprintf "Forming new block at height %Ld with prevledgerroot %s, prev block %s and new stake addr %s stake aid %s (bday %Ld).\n" blkh (hashval_hexstring prevledgerroot) (hashval_hexstring pbhh1) (addr_daliladdrstr alpha2) (hashval_hexstring aid) bday);
			let obl2 =
			  match obl with
			  | None ->  (* if the staked asset had the default obligation it can be left as the default obligation or locked for some number of blocks to commit to staking; there should be a configurable policy for the node *)
			      None
(**
   Some(p2pkhaddr_payaddr alpha,Int64.add blkh (Int64.logand 2048L (rand_int64())),false) (* it is not marked as a reward *)
 **)
			  | _ -> obl (* unless it's the default obligation, then the obligation cannot change when staking it *)
			in
			let prevc = Some(CHash(prevledgerroot)) in
			let octree_ctree c =
			  match c with
			  | Some(c) -> c
			  | None -> raise (Failure "tree should not be empty")
			in
			let dync = ref (octree_ctree prevc) in
			let dyntht = ref (lookup_thytree thtr) in
			let dynsigt = ref (lookup_sigtree sgtr) in
			let fees = ref 0L in
			let otherstxs = ref [] in
			let rembytesestimate = ref (maxblockdeltasize blkh - (2048 * 2)) in (*** estimate the remaining room in the block delta if the tx is added ***)
			Hashtbl.iter
			  (fun h ((tauin,tauout),sg) ->
(*		    log_string (Printf.sprintf "Trying to include tx %s\n" (hashval_hexstring h)); *)
			    try
			      ignore (List.find (fun (_,h) -> h = aid) tauin);
(*		      log_string (Printf.sprintf "tx spends the staked asset; removing tx from pool\n"); *)
			      remove_from_txpool h
			    with Not_found ->
			      try
				ignore (List.find (fun (alpha,_) -> alpha = alpha2) tauout) (*** Do not include txs that spend to the staking address, to avoid the possibility of ending up with too many assets at the stakign address ***)
			      with Not_found ->
				if tx_valid tm (tauin,tauout) then
				  try
				    let unsupportederror alpha h = log_string (Printf.sprintf "Could not find asset %s at address %s\n" (hashval_hexstring h) (addr_daliladdrstr alpha)) in
				    let al = List.map (fun (aid,a) -> a) (ctree_lookup_input_assets true false tauin !dync unsupportederror) in
				    if tx_signatures_valid blkh tm al ((tauin,tauout),sg) then
				      begin
					let nfee = ctree_supports_tx true false !dyntht !dynsigt blkh (tauin,tauout) !dync in
					if nfee > 0L then
					  begin
(*				  log_string (Printf.sprintf "tx %s has negative fees %Ld; removing from pool\n" (hashval_hexstring h) nfee); *)
					    remove_from_txpool h;
					  end
					else
					  let bytesestimate = 2048 * List.length tauin + 2048 * List.length tauout in (*** simple 2K per input and output (since must include relevant parts of ctree) ***)
					  if bytesestimate < !rembytesestimate then
					    begin
					      try
						let c = octree_ctree (tx_octree_trans true false blkh (tauin,tauout) (Some(!dync))) in
						otherstxs := (h,((tauin,tauout),sg))::!otherstxs;
						fees := Int64.sub !fees nfee;
						dync := c;
						dyntht := txout_update_ottree tauout !dyntht;
						dynsigt := txout_update_ostree tauout !dynsigt;
						rembytesestimate := !rembytesestimate - bytesestimate
					      with MaxAssetsAtAddress -> ()
					    end
					  else
					    begin
(*				    log_string (Printf.sprintf "tx %s not being included because estimated block size would be too big (rembytesestimate %d, bytesestimate %d)\n" (hashval_hexstring h) !rembytesestimate bytesestimate); *)
					    end
				      end
				    else
				      begin
(*			      log_string (Printf.sprintf "tx %s has an invalid signature; removing from pool\n" (hashval_hexstring h)); *)
					remove_from_txpool h;
				      end
				  with exn ->
				    begin
(*			    log_string (Printf.sprintf "Exception %s raised while trying to validate tx %s; this may mean the tx is not yet supported so leaving it in the pool\n" (Printexc.to_string exn) (hashval_hexstring h)); *)
				    end
				else
				  begin
(*			  log_string (Printf.sprintf "tx %s is invalid; removing from pool\n" (hashval_hexstring h)); *)
				    remove_from_txpool h;
				  end)
			  stxpool;
			let ostxs = !otherstxs in
			let otherstxs = ref [] in
			List.iter
			  (fun (h,stau) ->
			    remove_from_txpool h;
			    otherstxs := stau::!otherstxs)
			  ostxs;
			let othertxs = List.map (fun (tau,_) -> tau) !otherstxs in
			let alpha3 =
			  let default() =
			    let (i,x0,x1,x2,x3,x4) = alpha2 in
			    if i = 0 then
			      (x0,x1,x2,x3,x4)
			    else
			      begin
				log_string (Printf.sprintf "Apparent attempt to stake from non-p2pkh address %s\n" (addr_daliladdrstr alpha2));
				raise StakingProblemPause
			      end
			  in
			  if !Config.offlinestakerewardsdest then
			    begin
			      match !Commands.walletwatchaddrs_offlinekey_fresh with
			      | alpha::wr ->
				  let (i,x0,x1,x2,x3,x4) = alpha in
				  if i = 0 then
				    begin
				      Commands.walletwatchaddrs_offlinekey := alpha::!Commands.walletwatchaddrs_offlinekey;
				      Commands.walletwatchaddrs_offlinekey_fresh := wr;
				      (x0,x1,x2,x3,x4)
				    end
				  else
				    default()
			      | _ ->
				  default()
			    end
			  else
			    begin
			      if !Config.generatenewrewardaddresses then
				(let (_,alpha3) = Commands.generate_newkeyandaddress prevledgerroot (if !Config.stakewithrewards then "staking" else "nonstaking") in alpha3) (*** prevent staking address from ending up holding too many assets; max 32 are allowed ***)
			      else
				default()
			    end
			in
			let alpha4 =
			  match !Config.offlinestakerewardslock with
			  | None ->
			      if !Config.offlinestakerewardsdest then
				begin
				  match !Commands.walletwatchaddrs_offlinekey_fresh with
				  | alpha::wr ->
				      let (i,x0,x1,x2,x3,x4) = alpha in
				      if i = 0 || i = 1 then
					begin
					  Commands.walletwatchaddrs_offlinekey := alpha::!Commands.walletwatchaddrs_offlinekey;
					  Commands.walletwatchaddrs_offlinekey_fresh := wr;
					  (i=1,x0,x1,x2,x3,x4)
					end
				      else
					p2pkhaddr_payaddr alpha3
				  | _ ->
				      p2pkhaddr_payaddr alpha3
				end
			      else
				p2pkhaddr_payaddr alpha3
			  | Some(x) ->
			      try
				let (i,x0,x1,x2,x3,x4) = daliladdrstr_addr x in
				if i = 0 then
				  (false,x0,x1,x2,x3,x4) (*** p2pkh ***)
				else if i = 1 then
				  (true,x0,x1,x2,x3,x4) (*** p2sh ***)
				else
				  p2pkhaddr_payaddr alpha3
			      with _ -> p2pkhaddr_payaddr alpha3
			in
			let stkoutl = [(alpha2,(obl2,Currency(v)));(p2pkhaddr_addr alpha3,(Some(alpha4,get_reward_locktime blkh,true),Currency(Int64.add !fees (rewfn blkh))))] in
			let coinstk : tx = ([(alpha2,aid)],stkoutl) in
			try
			  dync := octree_ctree (tx_octree_trans true false blkh coinstk (Some(!dync)));
			  let prevcforblock =
			    match
			      get_txl_supporting_octree (coinstk::othertxs) prevc
			    with
			    | Some(c) -> c
			    | None -> raise (Failure "ctree should not have become empty")
			  in
			  if not (ctree_hashroot prevcforblock = prevledgerroot) then
			    begin
			      log_string (Printf.sprintf "prevcforblock has the wrong hash root. This should never happen.\n");
			      let s = Buffer.create 10000 in
			      seosbf (seo_option seo_ctree seosb prevc (s,None));
			      log_string (Printf.sprintf "prevc: %s\n" (Hashaux.string_hexstring (Buffer.contents s)));
			      let s = Buffer.create 10000 in
			      seosbf (seo_ctree seosb prevcforblock (s,None));
			      log_string (Printf.sprintf "prevcforblock: %s\nprevledgerroot: %s\n" (Hashaux.string_hexstring (Buffer.contents s)) (hashval_hexstring prevledgerroot));
			      let s = Buffer.create 10000 in
			      seosbf (seo_list seo_tx seosb (coinstk::othertxs) (s,None));
			      log_string (Printf.sprintf "txs: %s\n" (Hashaux.string_hexstring (Buffer.contents s)));
			      Hashtbl.remove nextstakechances (lbk,ltx);
			      raise StakingProblemPause;
			    end;
			  let (prevcforheader,cgr) = factor_inputs_ctree_cgraft [(alpha2,aid)] prevcforblock in
			  let newcr = save_ctree_elements !dync in
(*		log_string (Printf.sprintf "finished saving ctree elements of dync\n"); *)
(*		    Hashtbl.add recentledgerroots newcr (blkh,newcr); *)
			  let newthtroot = ottree_hashroot !dyntht in
			  let newsigtroot = ostree_hashroot !dynsigt in
(*		log_string (Printf.sprintf "Including %d txs in block\n" (List.length !otherstxs)); *)
			  let bdnew : blockdelta =
			    { stakeoutput = stkoutl;
			      prevledgergraft = cgr;
			      blockdelta_stxl = !otherstxs
			    }
			  in
			  let bdnewroot = blockdelta_hashroot bdnew in
			  let bhdnew : blockheaderdata
			      = { prevblockhash = Some(pbhh1,Poburn(lbk,ltx,plmedtm,pburned));
				  newtheoryroot = newthtroot;
				  newsignaroot = newsigtroot;
				  newledgerroot = newcr;
				  stakeaddr = alpha;
				  stakeassetid = aid;
				  timestamp = tm;
				  deltatime = deltm;
				  tinfo = tar;
				  prevledger = prevcforheader;
				  blockdeltaroot = bdnewroot;
				}
			  in
			  let bhdnewh = hash_blockheaderdata bhdnew in
			  let bhsnew =
			    try
			      let (prvk,b,_,_,_,_) = List.find (fun (_,_,_,_,beta,_) -> beta = alpha) !Commands.walletkeys_staking in
			      let r = rand_256() in
			      let sg : signat = signat_hashval bhdnewh prvk r in
			      { blocksignat = sg;
				blocksignatrecid = compute_recid sg r;
				blocksignatfcomp = b;
				blocksignatendorsement = None
			      }
			    with Not_found ->
			      try
				let (_,beta,(w,z),recid,fcomp,esg) =
				  List.find
				    (fun (alpha2,beta,(w,z),recid,fcomp,esg) ->
				      let (p,x0,x1,x2,x3,x4) = alpha2 in
				      let (q,_,_,_,_,_) = beta in
				      not p && (x0,x1,x2,x3,x4) = alpha && not q)
				    !Commands.walletendorsements
				in
				let (_,x0,x1,x2,x3,x4) = beta in
				let betah = (x0,x1,x2,x3,x4) in
				let (prvk,b,_,_,_,_) =
				  try
				    List.find
				      (fun (_,_,_,_,beta2,_) -> beta2 = betah)
				      !Commands.walletkeys_staking
				  with Not_found ->
				    List.find
				      (fun (_,_,_,_,beta2,_) -> beta2 = betah)
				      !Commands.walletkeys_nonstaking
				in
				let r = rand_256() in
				let sg : signat = signat_hashval bhdnewh prvk r in
				{ blocksignat = sg;
				  blocksignatrecid = compute_recid sg r;
				  blocksignatfcomp = b;
				  blocksignatendorsement = Some(betah,recid,fcomp,esg)
				}
			      with Not_found ->
				raise (Failure("Was staking for " ^ Cryptocurr.addr_daliladdrstr (p2pkhaddr_addr alpha) ^ " but have neither the private key nor an appropriate endorsement for it."))
			  in
			  let bhnew = (bhdnew,bhsnew) in
			  let newblkid = blockheader_id bhnew in
			  DbBlockHeader.dbput newblkid bhnew;
			  DbBlockDelta.dbput newblkid bdnew;
			  List.iter
			    (fun stau -> DbSTx.dbput (hashstx stau) stau)
			    bdnew.blockdelta_stxl;
			  begin
			    let s = Buffer.create 10000 in
			    seosbf (seo_blockdelta seosb bdnew (s,None));
			    let bds = Buffer.length s in
			    if bds > maxblockdeltasize blkh then
			      (log_string (Printf.sprintf "New block is too big (%d bytes)\n" bds); raise Not_found); (** in this case, probably the best option would be to switch back to an empty block **)
			    if valid_blockheader blkh csm0 tar0 bhnew tm (match toburn with Some(burn) -> burn | _ -> 0L) then
			      () (* (log_string (Printf.sprintf "New block header is valid\n")) *)
			    else
			      begin
				let b = Buffer.create 1000 in
				seosbf (seo_blockheader seosb bhnew (b,None));
				log_string (Printf.sprintf "New block header is not valid\nbhnew = %s\nfull header = %s\n" (hashval_hexstring newblkid) (string_hexstring (Buffer.contents b)));
				verbose_blockcheck := Some(!Utils.log);
				ignore (valid_blockheader blkh csm0 tar0 bhnew tm (match toburn with Some(burn) -> burn | _ -> 0L));
				verbose_blockcheck := None;
				let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in
				dumpstate (Filename.concat datadir "stakedinvalidblockheaderstate");
				Hashtbl.remove nextstakechances (lbk,ltx);
				raise StakingProblemPause
			      end;
			    begin
			      match valid_block thytree sigtree blkh csm0 tar0 (bhnew,bdnew) tm (match toburn with Some(burn) -> burn | _ -> 0L) with
			      | Some(tht2,sigt2) ->
				  update_theories thyroot thytree tht2;
				  update_signatures sigroot sigtree sigt2;
			      | None ->
				  log_string (Printf.sprintf "New block is not valid\n");
				  verbose_blockcheck := Some(!Utils.log); (* the next two calls are intended to log info about why the block is invalid *)
				  ignore (valid_block thytree sigtree blkh csm0 tar0 (bhnew,bdnew) tm (match toburn with Some(burn) -> burn | _ -> 0L));
				  ignore (valid_blockheader blkh csm0 tar0 bhnew tm (match toburn with Some(burn) -> burn | _ -> 0L));
				  verbose_blockcheck := None;
				  let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in dumpstate (Filename.concat datadir "stakedinvalidblockstate");
				  Hashtbl.remove nextstakechances (lbk,ltx);
				  raise StakingProblemPause
			    end;
			    if blkh = 1L then (log_string (Printf.sprintf "Previous block indicated but block height is 1\n"); Hashtbl.remove nextstakechances (lbk,ltx); raise StakingProblemPause);
			    let (pbhd,pbhs) = get_blockheader pbhh1 in
			    let tmpsucctest bhd1 bhs1 bhd2 =
			      match bhd2.prevblockhash with
			      | Some(pbh,Poburn(lblkh,ltxh,lmedtm,burned)) ->
				  bhd2.timestamp = Int64.add bhd1.timestamp (Int64.of_int32 bhd2.deltatime)
				    &&
				  pbh = blockheader_id (bhd1,bhs1) (*** the next block must also commit to the previous signature ***)
				    &&
				  let tar1 = bhd1.tinfo in
				  let tar2 = bhd2.tinfo in
				  eq_big_int tar2 (retarget bhd2.timestamp tar1 bhd2.deltatime)
			      | None -> false
			    in
			    if tmpsucctest pbhd pbhs bhdnew then
			      () (* (log_string (Printf.sprintf "Valid successor block\n")) *)
			    else
			      (log_string (Printf.sprintf "Not a valid successor block\n"); let datadir = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir in dumpstate (Filename.concat datadir "stakedinvalidsuccblockstate"); Hashtbl.remove nextstakechances (lbk,ltx); raise StakingProblemPause)
			  end;
			  begin
			    try
			      while true do
				let nw = ltc_medtime() in
				let tmtopub = Int64.sub tm nw in
				log_string (Printf.sprintf "tmtopub %Ld\n" tmtopub);
				if tmtopub > 0L then Thread.delay (Int64.to_float tmtopub) else raise Exit
			      done
			    with Exit -> ()
			  end;
			  let publish_new_block () =
			    log_string (Printf.sprintf "called publish_new_block\n");
			    if List.length !netconns < !Config.minconnstostake then
			      begin
				log_string (Printf.sprintf "Refusing to publish new block since node is insufficiently connected (only %d connections).\n" (List.length !netconns));
				Thread.delay 600.0 (*** delay for 10 minutes before continuing trying to stake to see if more connections arrive by then ***)
			      end
			    else
			      begin
				let ftm = Int64.add (ltc_medtime()) 3600L in
				if tm <= ftm then
				  begin
				    let (pbh2,lbk2,ltx2) = get_bestblock_cw_exception (StakingPause(300.0)) in
				    if (pbh2,lbk2,ltx2) = (pbhh1,lbk,ltx) then (*** if the best block has changed, don't publish it ***)
				      begin
					match toburn with
					| Some(u) ->
					    begin (*** actually burn u litoshis and wait for a confirmation to know the block hash ***)
					      try
						if !Config.ltcoffline then
						  begin
						    Printf.printf "to stake, after ltc medtm passes %Ld create ltc burn tx for %s %s burning %Ld litoshis\n" tm (hashval_hexstring ltx) (hashval_hexstring newblkid) u
						  end
						else
						  let btx = ltc_createburntx ltx newblkid u in
						  let btxhex = Hashaux.string_hexstring btx in
						  let btxs = ltc_signrawtransaction btxhex in
						  let h = ltc_sendrawtransaction btxs in
						  pendingltctxs := h::!pendingltctxs;
						  log_string (Printf.sprintf "Sending ltc burn %s for header %s\n" h (hashval_hexstring newblkid));
						  publish_block blkh newblkid ((bhdnew,bhsnew),bdnew);
						  extraburn := 0L;
						  already := Some(newblkid,hexstring_hashval h);
						  pending := !already;
						  log_string ("Burning " ^ (Int64.to_string u) ^ " litoshis in tx " ^ h ^ "\n")
					      with
					      | InsufficientLtcFunds ->
						  log_string ("insufficient ltc to burn " ^ (Int64.to_string u) ^ " litoshis" ^ "\n");
						  raise (StakingPause(300.0))
					      | Not_found ->
						  log_string ("problem trying to burn " ^ (Int64.to_string u) ^ " litoshis" ^ "\n");
						  raise (StakingPause(300.0))
					    end
					| None -> raise (Failure("must burn, should have known"))
				      end
				  end			      
			      end
			  in
 			  let (pbh2,lbk2,ltx2) = get_bestblock_cw_exception (StakingPause(300.0)) in
			  if (pbh2,lbk2,ltx2) = (pbhh1,lbk,ltx) then (*** if the best block has changed, don't publish it ***)
			    publish_new_block()
			with MaxAssetsAtAddress ->
			  log_string (Printf.sprintf "Refusing to stake since the coinstake tx would put too many assets in an address.\n")
		      end
		  end
	    end
	| NoStakeUpTo(tm) ->
	    begin (*** before checking for future chances to stake, make sure we are clearly at one of the best chaintips ***)
	      match ltc_best_chaintips () with
	      | [] ->
		  begin
		    (*** this should not have happened, since the header should not have been completely formed until the burn was complete ***)
		    log_string (Printf.sprintf "Refusing to stake on top of apparently unburned %s\nWaiting a few minutes to recheck for burn." (hashval_hexstring pbhh1));
		    raise (StakingPause(300.0))
		  end
	      | (bestctips::othctipsl) ->
		  begin
		    if List.mem pbhh1 bestctips then
		      (if List.length bestctips > 1 then (log_string (Printf.sprintf "Staking on top of %s, orphaning other equally good tips.\n" (hashval_hexstring pbhh1))))
		    else
		      begin
			log_string (Printf.sprintf "Refusing to stake on top of %s when there are better chaintips. Invalidate them by hand to force staking.\n" (hashval_hexstring pbhh1));
			raise (StakingPause(3600.0))
		      end
		  end
	    end;
	    let ltm = ltc_medtime() in
	    let stm = Int64.sub ltm 86400L in
	    let ftm = Int64.add ltm 86400L in
	    if tm < ftm && Int64.of_float (Unix.time()) < ftm then
	      begin
		try
		  let (dbh,_,_,_,_,_) = Hashtbl.find outlinevals (lbk,ltx) in
		  compute_staking_chances (dbh,lbk,ltx) (if tm > stm then tm else stm) ftm
		with Not_found -> Thread.delay 60.0
	      end
	    else
	      Thread.delay 60.0
      with
      | Not_found ->
	  log_string (Printf.sprintf "no nextstakechances\n");
	  Thread.delay 10.0;
	  log_string (Printf.sprintf "calling compute_staking_chances nextstakechances\n");
	  let ltm = ltc_medtime() in
	  let ftm = Int64.add ltm 86400L in
	  try
	    let (_,pbhtm,_,_,_) = Hashtbl.find validheadervals (lbk,ltx) in
	    compute_staking_chances (pbhh1,lbk,ltx) pbhtm ftm
	  with Not_found ->
	    Thread.delay 300.0
      | StakingProblemPause -> (*** there was some serious staking bug, try to recover by stopping staking for an hour and trying again ***)
	  log_string (Printf.sprintf "Pausing due to a staking bug; will retry staking in about an hour.\n");
	  Thread.delay 3600.0;
	  log_string (Printf.sprintf "Continuing staking.\n");
	  let ltm = ltc_medtime() in
	  let stm = Int64.sub ltm 86400L in
	  let ftm = Int64.add ltm 86400L in
	  compute_staking_chances (pbhh1,lbk,ltx) stm ftm
    with
    | Genesis -> (*** at this point, a new genesis phase can only happen when restarting the testnet ***)
	begin (*** since the code for staking a genesis block is only for the testnet, fix staking asset as the one at tDY6SUKGiWvrPy24p47YaSAFiaBqXQwUz1N with id 000000000000000000000000a1d7d38b18a9cedc356d0bf3a1379f6e6fa18687 and hash 67c9489211e9fa9a7b3e141b4490d106346549cf2768e13eca832953c5afd26d ***)
	  let i = ref !Config.genesistimestamp in
	  let nw = ltc_medtime() in
	  let upto = Int64.add 86400L nw in
	  let alpha2 = daliladdrstr_addr "tDY6SUKGiWvrPy24p47YaSAFiaBqXQwUz1N" in
	  let (_,a0,a1,a2,a3,a4) = alpha2 in
	  let alpha = (false,a0,a1,a2,a3,a4) in
	  let aid = hexstring_hashval "000000000000000000000000a1d7d38b18a9cedc356d0bf3a1379f6e6fa18687" in
	  let bday = 0L in
	  let obl = None in
	  let v = 13669000000L in
	  let csm0 = !genesisstakemod in
	  let tar0 = !genesistarget in
	  let caf = coinagefactor 1L bday obl in
	  begin
	    try
	      Printf.printf "checking for hits from %Ld up to %Ld\n" !i upto;
	      while !i < upto do
		let hv = hitval !i aid csm0 in
		let mtar = mult_big_int tar0 caf in
		let minv = div_big_int hv mtar in
		if lt_big_int minv (big_int_of_int64 v) then (*** hit without burn ***)
		  begin
		    if !i < nw then
		      begin
			let stx1s = hexstring_string "c92c5395c2f4985cc32311b83056c68402b1bd46040000000000000000000000e009f99f64de2f7b115bc6880849177acd5a51a095cc3255294c8fc9353c12810b63654c2810db6bc432cb54a5303d26d7f048042e8c9531a1406caf1101000000000000c800000040806a811c209965aa52981e936b78240217c6ca985020b6d7886596a94a617a4caee191085c182b634281d85e2302000000000000200300008000d502394032cb54a5303d26d7f048042e8c9531a1406caf11cb2c5395c2f4985cc32311b83056c68402b1bd4604000000000008600100000001aa0572806496a94a617a4caee191085c182b634281d85e239659a62a85e931b98647227061ac8c0905627b8d08000000000010000900000002540be40039c6e832c9aae110591e17d95a77a3f400f7cc4b0500008006fc232c00e3899d3738cc04ed91f4a37a7e827de22341304f7d0ba41706e07dc00237f4ec1993ebce26a99997f33ad00326fc6689476a6e84baa5b9739f57a7f2b265127d27d421b8de719e9bbb4ce876526d1f030d9307a33a42f41d27a14cacb050ce24f4c17900120f937b0e40770e145f9d5a89f96acbde3dd1ab1ae0757f70e0b2335900" in (*** an initial tx for the testnet's genesis block ***)
			let (stx1,_) = sei_stx seis (stx1s,String.length stx1s,None,0,0) in
			let ((tauin,tauout),_) = stx1 in
			(*** form the genesis block ***)
			let tm = !i in
			let deltm = Int64.to_int32 (Int64.sub tm !Config.genesistimestamp) in
			let blkh = 1L in
			let tar = retarget tm !genesistarget deltm in
			let prevc = Some(CHash(!genesisledgerroot)) in
			let octree_ctree c =
			  match c with
			  | Some(c) -> c
			  | None -> raise (Failure "tree should not be empty")
			in
			let c = octree_ctree (tx_octree_trans true false 1L (tauin,tauout) prevc) in
			let otherstxs = [stx1] in
			let othertxs = List.map (fun (tau,_) -> tau) otherstxs in
			let fees = 1000000000L in
			let stkoutl = [(alpha2,(None,Currency(v)));(alpha2,(Some(alpha,1000L,true),Currency(Int64.add fees (rewfn 1L))))] in
			let coinstk : tx = ([(alpha2,aid)],stkoutl) in
			let newc = octree_ctree (tx_octree_trans true false blkh coinstk (Some(c))) in
			let newcr = save_ctree_elements newc in
			let prevcforblock =
			  match
			    get_txl_supporting_octree (coinstk::othertxs) prevc
			  with
			  | Some(c) -> c
			  | None -> raise (Failure "ctree should not have become empty")
			in
			let (prevcforheader,cgr) = factor_inputs_ctree_cgraft [(alpha2,aid)] prevcforblock in
			let bdnew : blockdelta =
			  { stakeoutput = stkoutl;
			    prevledgergraft = cgr;
			    blockdelta_stxl = otherstxs
			  }
			in
			let bdnewroot = blockdelta_hashroot bdnew in
			let bhdnew : blockheaderdata
			    = { prevblockhash = None;
				newtheoryroot = None;
				newsignaroot = None;
				newledgerroot = newcr;
				stakeaddr = (a0,a1,a2,a3,a4);
				stakeassetid = aid;
				timestamp = tm;
				deltatime = deltm;
				tinfo = tar;
				prevledger = prevcforheader;
				blockdeltaroot = bdnewroot;
			      }
			in
			let bhdnewh = hash_blockheaderdata bhdnew in
			let bhsnew =
			  try
			    let (prvk,b,_,_,_,_) = List.find (fun (_,_,_,_,beta,_) -> beta = (a0,a1,a2,a3,a4)) !Commands.walletkeys_staking in
			    let r = rand_256() in
			    let sg : signat = signat_hashval bhdnewh prvk r in
			    { blocksignat = sg;
			      blocksignatrecid = compute_recid sg r;
			      blocksignatfcomp = b;
			      blocksignatendorsement = None
			    }
			  with Not_found -> log_string "Key for staking genesis block is not in wallet.\n"; raise (Failure "staking problem")
			in
			let bhnew = (bhdnew,bhsnew) in
			let newblkid = blockheader_id bhnew in
			DbBlockHeader.dbput newblkid bhnew;
			DbBlockDelta.dbput newblkid bdnew;
			List.iter
			  (fun stau -> DbSTx.dbput (hashstx stau) stau)
			  bdnew.blockdelta_stxl;
			begin
			  match valid_block None None blkh csm0 tar0 (bhnew,bdnew) tm 0L with
			  | Some(_,_) -> ()
			  | None -> log_string "Genesis block is not valid.\n"; raise (Failure "staked invalid genesis block")
			end;
			let btx = ltc_createburntx (0l,0l,0l,0l,0l,0l,0l,0l) newblkid 0L in
			let btxhex = Hashaux.string_hexstring btx in
			let btxs = ltc_signrawtransaction btxhex in
			let h = ltc_sendrawtransaction btxs in
			log_string (Printf.sprintf "Sending ltc burn %s for header %s. Waiting 5 minutes for it to confirm.\n" h (hashval_hexstring newblkid));
			Thread.delay 300.0;
			publish_block blkh newblkid ((bhdnew,bhsnew),bdnew);
			i := upto
		      end
		    else
		      begin
			log_string (Printf.sprintf "A genesis block can be staked at %Ld\n" !i);
			flush stdout;
			raise Exit
		      end;
		  end;
		i := Int64.add !i 1L
	      done
	    with Exit ->
	      Printf.printf "no hits found\n";
	  end;
	  Thread.delay 300.0;
	  sleepuntil := ltc_medtime()
	end
    | SyncIssue ->
	begin
	  try
	    match !pending with
	    | Some(newblkid,ltcburntxid) ->
		begin
		  let (lblkid,ltxid) =
		    List.find (fun (_,ltxid) -> ltxid = ltcburntxid) (Hashtbl.find_all blockburns newblkid)
		  in
		  let (bhdnew,bhsnew) = DbBlockHeader.dbget newblkid in
		  let bdnew = DbBlockDelta.dbget newblkid in
		  let (_,lmedtm,burned,_,newcsm,currhght) = Hashtbl.find outlinevals (lblkid,ltxid) in
		  match bhdnew.prevblockhash with
		  | Some(pbh,Poburn(plblkh,pltxh,lmedtm,burned)) ->
		      begin
			let (_,_,_,_,csm,_) = Hashtbl.find outlinevals (plblkh,pltxh) in
			let (tar,tmstmp,lr,thtr,sgtr) = Hashtbl.find validheadervals (plblkh,pltxh) in
			if not (blockheader_succ_b pbh lr tmstmp tar (bhdnew,bhsnew)) then
			  begin
			    log_string (Printf.sprintf "Staking problem at height %Ld: %s is not a valid succ of %s\n" currhght (hashval_hexstring newblkid) (hashval_hexstring pbh));
			    raise Exit
			  end;
			let tht = lookup_thytree thtr in
			let sgt = lookup_sigtree sgtr in
			process_block !Utils.log true false false (lblkid,ltxid) newblkid ((bhdnew,bhsnew),bdnew) thtr tht sgtr sgt currhght csm tar lmedtm burned;
			missingheaders := List.filter (fun (_,k) -> not (newblkid = k)) !missingheaders;
			missingdeltas := List.filter (fun (_,k) -> not (newblkid = k)) !missingdeltas;
			pending := None
		      end
		  | None ->
		      process_block !Utils.log true false false (lblkid,ltxid) newblkid ((bhdnew,bhsnew),bdnew) None None None None 1L !genesisstakemod !genesistarget lmedtm burned;
		      missingheaders := List.filter (fun (_,k) -> not (newblkid = k)) !missingheaders;
		      missingdeltas := List.filter (fun (_,k) -> not (newblkid = k)) !missingdeltas;
		      pending := None
		end
	    | None -> raise Exit
	  with _ ->
	    Thread.delay 300.0;
	    sleepuntil := ltc_medtime()
	end
    | StakingPause(del) ->
	log_string (Printf.sprintf "Staking pause of %f seconds\n" del);
	Thread.delay del;
	sleepuntil := ltc_medtime()
    | e ->
	log_string (Printf.sprintf "Staking exception: %s\nPausing staking for about an hour.\n" (Printexc.to_string e));
	Thread.delay 3600.0;
	sleepuntil := ltc_medtime()
  done;;

