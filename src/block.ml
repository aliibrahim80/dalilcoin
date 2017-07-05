(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Sha256
open Ripemd160
open Hash
open Net
open Db
open Big_int
open Mathdata
open Assets
open Signat
open Cryptocurr
open Tx
open Ctre
open Ctregraft

(*** 256 bits ***)
type stakemod = int64 * int64 * int64 * int64

let genesiscurrentstakemod : stakemod ref = ref (0L,0L,0L,0L)
let genesisfuturestakemod : stakemod ref = ref (0L,0L,0L,0L)

let stakemod_string (x3,x2,x1,x0) = (Int64.to_string x3) ^ " " ^ (Int64.to_string x2) ^ " " ^ (Int64.to_string x1) ^ " " ^ (Int64.to_string x0)

let compute_stakemods (x4,x3,x2,x1,x0) =
  sha256init();
  currblock.(0) <- x4;
  currblock.(1) <- x3;
  currblock.(2) <- x2;
  currblock.(3) <- x1;
  currblock.(4) <- x0;
  currblock.(5) <- 0x80000000l;
  for i = 6 to 14 do
    currblock.(i) <- 0l;
  done;
  currblock.(15) <- 160l;
  sha256round();
  let (y0,y1,y2,y3,y4,y5,y6,y7) = getcurrmd256() in
  sha256init();
  currblock.(0) <- y0;
  currblock.(1) <- y1;
  currblock.(2) <- y2;
  currblock.(3) <- y3;
  currblock.(4) <- y4;
  currblock.(5) <- y5;
  currblock.(6) <- y6;
  currblock.(7) <- y7;
  currblock.(8) <- 0x80000000l;
  for i = 9 to 14 do
    currblock.(i) <- 0l;
  done;
  currblock.(15) <- 256l;
  sha256round();
  let (z0,z1,z2,z3,z4,z5,z6,z7) = getcurrmd256() in
  let c a b =
    Int64.logor
      (Int64.shift_left (Int64.of_int32 (Int32.logand (Int32.shift_right_logical a 16) 0xffl)) 48)
      (Int64.logor
	 (Int64.shift_left (Int64.of_int32 (Int32.logand a 0xffl)) 32)
	 (Int64.logor
	    (Int64.shift_left (Int64.of_int32 (Int32.logand (Int32.shift_right_logical b 16) 0xffl)) 16)
	    (Int64.of_int32 (Int32.logand b 0xffl))))
  in
  ((c y0 y1,c y2 y3,c y4 y5,c y6 y7),(c z0 z1,c z2 z3,c z4 z5,c z6 z7));;
  
let set_genesis_stakemods x =
  let (csm,fsm) = compute_stakemods (hexstring_hashval x) in
  genesiscurrentstakemod := csm;
  genesisfuturestakemod := fsm;;

(*** Here the last 20 bytes (40 hex chars) of the block hash for a particular bitcoin block should be included.
 sha256 is used to extract 512 bits to set the genesis current and future stake modifiers.
 ***)
set_genesis_stakemods "0000000000000000000000000000000000000000"

(*** max target/min difficulty: 2^220 (for mainnet) ***)
let max_target = ref (shift_left_big_int unit_big_int 220)
let genesistarget = ref (shift_left_big_int unit_big_int 205) (* current estimate for initial difficulty *)
let genesisledgerroot : hashval ref = ref (hexstring_hashval "fc25150b4880e27235d4878637d32f0ffe2280e6");; (*** snapshot ledger root [This is the correct root when assets are referenced by hash instead of id.] ***)
(** let genesisledgerroot : hashval ref = ref (hexstring_hashval "66c029f4c29b351785c0480cedc9449b64332dfa");; (*** snapshot ledger root [This is the root when assets were referenced by id, which was the case in the old initdistr on mega] ***) **)

(*** base reward of 50 fraenks (5 trillion cants) like bitcoin, but assume the first 350000 blocks have passed. ***)
let basereward = 5000000000000L

(*** the block reward begins at 25 fraenks and halves with each era until era 43 when it is 0 ***)
let rewfn blkh = Int64.shift_right basereward (Utils.era blkh)

let hashstakemod sm =
  let (m3,m2,m1,m0) = sm in
  hashtag (hashlist [hashint64 m3;hashint64 m2;hashint64 m1;hashint64 m0]) 240l

let seo_stakemod o sm c =
  seo_prod4 seo_int64 seo_int64 seo_int64 seo_int64 o sm c

let sei_stakemod i c =
  sei_prod4 sei_int64 sei_int64 sei_int64 sei_int64 i c

(*** drop most significant bit of m3, shift everything, b is the new least siginificant bit of m0 ***)
let stakemod_pushbit b sm =
  let (m3,m2,m1,m0) = sm in
  let z3 = Int64.shift_left m3 1 in
  let z2 = Int64.shift_left m2 1 in
  let z1 = Int64.shift_left m1 1 in
  let z0 = Int64.shift_left m0 1 in
  ((if m2 < 0L then Int64.logor z3 1L else z3),
   (if m1 < 0L then Int64.logor z2 1L else z2),
   (if m0 < 0L then Int64.logor z1 1L else z1),
   (if b then Int64.logor z0 1L else z0))

let stakemod_lastbit sm =
  let (m3,_,_,_) = sm in
  m3 < 0L

let stakemod_firstbit sm =
  let (_,_,_,m0) = sm in
  Int64.logand m0 1L = 1L

(*** one round of sha256 combining the timestamp (least significant 32 bits only), the hash value of the stake's assetid and the stake modifier, then converted to a big_int to do arithmetic ***)
let hitval tm h sm =
  let (x0,x1,x2,x3,x4) = h in
  let (m3,m2,m1,m0) = sm in
  sha256init();
  currblock.(0) <- Int64.to_int32 tm;
  currblock.(1) <- x0;
  currblock.(2) <- x1;
  currblock.(3) <- x2;
  currblock.(4) <- x3;
  currblock.(5) <- x4;
  currblock.(6) <- Int64.to_int32 (Int64.shift_right_logical m3 32);
  currblock.(7) <- Int64.to_int32 m3;
  currblock.(8) <- Int64.to_int32 (Int64.shift_right_logical m2 32);
  currblock.(9) <- Int64.to_int32 m2;
  currblock.(10) <- Int64.to_int32 (Int64.shift_right_logical m1 32);
  currblock.(11) <- Int64.to_int32 m1;
  currblock.(12) <- Int64.to_int32 (Int64.shift_right_logical m0 32);
  currblock.(13) <- Int64.to_int32 m0;
  currblock.(14) <- 0x80000000l;
  currblock.(15) <- 448l;
  sha256round();
  md256_big_int (getcurrmd256())

(*** current stake modifier, future stake modifier, target (big_int, but assumed to be at most 256 bits ***)
type targetinfo = stakemod * stakemod * big_int

let targetinfo_string (csm,fsm,tar) = stakemod_string csm ^ ";" ^ stakemod_string fsm ^ ";" ^ string_of_big_int tar

let eq_tinfo (x,y,z) (u,v,w) =
  x = u && y = v && eq_big_int z w

let hashtargetinfo ti =
  let (csm,fsm,tar) = ti in
  hashpair (hashstakemod csm)
    (hashpair (hashstakemod fsm)
       (big_int_hashval tar))

let seo_targetinfo o ti c =
  seo_prod3 seo_stakemod seo_stakemod seo_big_int_256 o ti c

let sei_targetinfo i c =
  sei_prod3 sei_stakemod sei_stakemod sei_big_int_256 i c

type poburn =
  | Poburn of md256 * md256 * int64 (** ltc block hash id, ltc tx hash id, number of litecoin burned **)
  | SincePoburn of int (** how many blocks have passed since the last poburn; should be < 256 ***)

let hashpoburn p =
  match p with
  | Poburn(h,k,x) -> hashtag (hashpair (hashpair (ripemd160_md256 h) (ripemd160_md256 k)) (hashint64 x)) 194l
  | SincePoburn(i) -> hashtag (hashint32 (Int32.of_int i)) 195l

let seo_poburn o p c =
  match p with
  | Poburn(h,k,x) ->
      let c = o 1 0 c in
      let c = seo_md256 o h c in
      let c = seo_md256 o k c in
      let c = seo_int64 o x c in
      c
  | SincePoburn(i) ->
      let c = o 1 1 c in
      let c = seo_int32 o (Int32.of_int i) c in
      c

let sei_poburn i c =
  let (y,c) = i 1 c in
  if y = 0 then
    let (h,c) = sei_md256 i c in
    let (k,c) = sei_md256 i c in
    let (x,c) = sei_int64 i c in
    (Poburn(h,k,x),c)
  else
    let (j,c) = sei_int32 i c in
    (SincePoburn(Int32.to_int j),c)

type postor =
  | PostorTrm of hashval option * tm * tp * hashval
  | PostorDoc of payaddr * hashval * hashval option * pdoc * hashval

let hashpostor r =
  match r with
  | PostorTrm(th,m,a,h) -> hashtag (hashopair2 th (hashpair (hashpair (hashtm m) (hashtp a)) h)) 192l
  | PostorDoc(gamma,nonce,th,d,h) ->
      hashtag (hashpair (hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th (hashpdoc d)))) h) 193l

let hashopostor r =
  match r with
  | Some(r) -> Some(hashpostor r)
  | None -> None

let seo_postor o po c =
  match po with
  | PostorTrm(th,m,a,h) -> 
      let c = o 1 0 c in
      let c = seo_option seo_hashval o th c in
      let c = seo_tm o m c in
      let c = seo_tp o a c in
      let c = seo_hashval o h c in
      c
  | PostorDoc(gamma,nonce,th,d,h) ->
      let c = o 1 1 c in
      let c = seo_payaddr o gamma c in
      let c = seo_hashval o nonce c in
      let c = seo_option seo_hashval o th c in
      let c = seo_pdoc o d c in
      let c = seo_hashval o h c in
      c

let sei_postor i c =
  let (x,c) = i 1 c in
  if x = 0 then
    let (th,c) = sei_option sei_hashval i c in
    let (m,c) = sei_tm i c in
    let (a,c) = sei_tp i c in
    let (h,c) = sei_hashval i c in
    (PostorTrm(th,m,a,h),c)
  else
    let (gamma,c) = sei_payaddr i c in
    let (nonce,c) = sei_hashval i c in
    let (th,c) = sei_option sei_hashval i c in
    let (d,c) = sei_pdoc i c in
    let (h,c) = sei_hashval i c in
    (PostorDoc(gamma,nonce,th,d,h),c)

type blockheaderdata = {
    prevblockhash : hashval option;
    newtheoryroot : hashval option;
    newsignaroot : hashval option;
    newledgerroot : hashval;
    stakeaddr : p2pkhaddr;
    stakeassetid : hashval;
    announcedpoburn : poburn;
    stored : postor option;
    timestamp : int64;
    deltatime : int32;
    tinfo : targetinfo;
    prevledger : ctree;
  }

type blockheadersig = {
    blocksignat : signat;
    blocksignatrecid : int;
    blocksignatfcomp : bool;
    blocksignatendorsement : (p2pkhaddr * int * bool * signat) option;
  }

type blockheader = blockheaderdata * blockheadersig

(*** a fake blockheader to use when some data structure needs to be initialized ***)
let fake_blockheader : blockheader =
  ({ prevblockhash = None;
     newtheoryroot = None;
     newsignaroot = None;
     newledgerroot = (0l,0l,0l,0l,0l);
     stakeaddr = (0l,0l,0l,0l,0l);
     stakeassetid = (0l,0l,0l,0l,0l);
     announcedpoburn = SincePoburn(0);
     stored = None;
     timestamp = 0L;
     deltatime = 0l;
     tinfo = ((0L,0L,0L,0L),(0L,0L,0L,0L),zero_big_int);
     prevledger = CHash(0l,0l,0l,0l,0l);
   },
   { blocksignat = (zero_big_int,zero_big_int);
     blocksignatrecid = 0;
     blocksignatfcomp = false;
     blocksignatendorsement = None;
   })

let seo_blockheaderdata o bh c =
  let c = seo_option seo_hashval o bh.prevblockhash c in
  let c = seo_option seo_hashval o bh.newtheoryroot c in
  let c = seo_option seo_hashval o bh.newsignaroot c in
  let c = seo_hashval o bh.newledgerroot c in
  let c = seo_hashval o bh.stakeaddr c in (*** p2pkh addresses are hashvals ***)
  let c = seo_hashval o bh.stakeassetid c in
  let c = seo_poburn o bh.announcedpoburn c in
  let c = seo_option seo_postor o bh.stored c in
  let c = seo_int64 o bh.timestamp c in
  let c = seo_int32 o bh.deltatime c in
  let c = seo_targetinfo o bh.tinfo c in
  let c = seo_ctree o bh.prevledger c in
  c

let sei_blockheaderdata i c =
  let (x0,c) = sei_option sei_hashval i c in
  let (x1,c) = sei_option sei_hashval i c in
  let (x2,c) = sei_option sei_hashval i c in
  let (x3,c) = sei_hashval i c in
  let (x4,c) = sei_hashval i c in (*** p2pkh addresses are hashvals ***)
  let (x5,c) = sei_hashval i c in
  let (x6a,c) = sei_poburn i c in
  let (x6,c) = sei_option sei_postor i c in
  let (x7,c) = sei_int64 i c in
  let (x8,c) = sei_int32 i c in
  let (x9,c) = sei_targetinfo i c in
  let (x10,c) = sei_ctree i c in
  let bhd : blockheaderdata =
      { prevblockhash = x0;
	newtheoryroot = x1;
	newsignaroot = x2;
	newledgerroot = x3;
	stakeaddr = x4;
	stakeassetid = x5;
	announcedpoburn = x6a;
	stored = x6;
	timestamp = x7;
	deltatime = x8;
	tinfo = x9;
	prevledger = x10;
      }
  in
  (bhd,c)

let seo_blockheadersig o bhs c = 
  let c = seo_signat o bhs.blocksignat c in
  let c = o 2 bhs.blocksignatrecid c in
  let c = seo_bool o bhs.blocksignatfcomp c in
  let c = seo_option (seo_prod4 seo_hashval seo_varintb seo_bool seo_signat) o bhs.blocksignatendorsement c in
  c

let sei_blockheadersig i c = 
  let (x,c) = sei_signat i c in
  let (r,c) = i 2 c in
  let (f,c) = sei_bool i c in
  let (e,c) = sei_option (sei_prod4 sei_hashval sei_varintb sei_bool sei_signat) i c in
  let bhs : blockheadersig =
    { blocksignat = x;
      blocksignatrecid = r;
      blocksignatfcomp = f;
      blocksignatendorsement = e;
    }
  in
  (bhs,c)

let seo_blockheader o bh c = seo_prod seo_blockheaderdata seo_blockheadersig o bh c
let sei_blockheader i c = sei_prod sei_blockheaderdata sei_blockheadersig i c

type poforfeit = blockheader * blockheader * blockheaderdata list * blockheaderdata list * int64 * hashval list

let seo_poforfeit o pof c =
  seo_prod6 seo_blockheader seo_blockheader
    (seo_list seo_blockheaderdata) (seo_list seo_blockheaderdata)
    seo_int64 (seo_list seo_hashval)
    o pof c

let sei_poforfeit i c =
  sei_prod6 sei_blockheader sei_blockheader
    (sei_list sei_blockheaderdata) (sei_list sei_blockheaderdata)
    sei_int64 (sei_list sei_hashval)
    i c

type blockdelta = {
    stakeoutput : addr_preasset list;
    forfeiture : poforfeit option;
    prevledgergraft : cgraft;
    blockdelta_stxl : stx list
  }

type block = blockheader * blockdelta

let seo_blockdelta o bd c =
  let c = seo_list seo_addr_preasset o bd.stakeoutput c in
  let c = seo_option seo_poforfeit o bd.forfeiture c in
  let c = seo_cgraft o bd.prevledgergraft c in
  let c = seo_list seo_stx o bd.blockdelta_stxl c in
  c

let sei_blockdelta i c =
  let (stko,c) = sei_list sei_addr_preasset i c in
  let (forf,c) = sei_option sei_poforfeit i c in
  let (cg,c) = sei_cgraft i c in
  let (stxl,c) = sei_list sei_stx i c in
  ({ stakeoutput = stko;
     forfeiture = forf;
     prevledgergraft = cg;
     blockdelta_stxl = stxl;
   },
   c)

let seo_block o b c = seo_prod seo_blockheader seo_blockdelta o b c
let sei_block i c = sei_prod sei_blockheader sei_blockdelta i c

module DbBlockHeader = Dbbasic2keyiter (struct type t = blockheader let basedir = "blockheader" let seival = sei_blockheader seic let seoval = seo_blockheader seoc end)
module DbBlockDelta = Dbbasic2 (struct type t = blockdelta let basedir = "blockdelta" let seival = sei_blockdelta seic let seoval = seo_blockdelta seoc end)

let get_blockheader h = 
  try
    DbBlockHeader.dbget h
  with Not_found -> (*** request it and fail ***)
(*** missing code to ask peers for data ***)
    raise GettingRemoteData

let get_blockdelta h = 
  try
    DbBlockDelta.dbget h
  with Not_found -> (*** request it and fail ***)
(*** missing code to ask peers for data ***)
    raise GettingRemoteData

(*** multiply stake by 1.25 ***)
let incrstake s =
  Int64.add s (Int64.shift_right s 2)

exception InappropriatePostor

(*** m should be a term abbreviated except for one leaf ***)
let rec check_postor_tm_r m =
  match m with
  | TmH(h) -> raise InappropriatePostor
  | DB(i) -> hashtm m
  | Prim(i) -> hashtm m
  | Ap(m,TmH(k)) -> check_postor_tm_r m
  | Ap(TmH(h),m) -> check_postor_tm_r m
  | Ap(_,_) -> raise InappropriatePostor
  | Imp(m,TmH(k)) -> check_postor_tm_r m
  | Imp(TmH(h),m) -> check_postor_tm_r m
  | Imp(_,_) -> raise InappropriatePostor
  | Lam(a,m) -> check_postor_tm_r m
  | All(a,m) -> check_postor_tm_r m
  | TTpAp(m,a) -> check_postor_tm_r m
  | TTpLam(m) -> check_postor_tm_r m
  | TTpAll(m) -> check_postor_tm_r m

(*** alpha is a p2pkhaddr, beta is a termaddr, and these types are both the same as hashval ***)
let check_postor_tm tm csm mtar alpha beta m =
  try
    let h = check_postor_tm_r m in
    let betah = hashpair beta h in
    let (x,_,_,_,_) = hashpair alpha betah in
    Int32.logand x 0xffffl  = 0l (*** one of every 65536 (beta,h) pairs can be used by each address alpha ***)
      &&
    lt_big_int (hitval tm betah csm) mtar
  with InappropriatePostor -> false

(*** d should be a proof with everything abbreviated except for one leaf ***)
let rec check_postor_pf_r d =
  match d with
  | Gpa(_) -> raise InappropriatePostor
  | Hyp(i) -> hashpf d
  | Known(h) -> hashpf d
  | PLam(TmH(_),d) -> check_postor_pf_r d
  | PLam(m,Gpa(_)) -> check_postor_tm_r m
  | PLam(_,_) -> raise InappropriatePostor
  | TLam(_,d) -> check_postor_pf_r d
  | PTmAp(Gpa(_),m) -> check_postor_tm_r m
  | PTmAp(d,TmH(_)) -> check_postor_pf_r d
  | PTmAp(_,_) -> raise InappropriatePostor
  | PPfAp(Gpa(_),d) -> check_postor_pf_r d
  | PPfAp(d,Gpa(_)) -> check_postor_pf_r d
  | PPfAp(_,_) -> raise InappropriatePostor
  | PTpAp(d,_) -> check_postor_pf_r d
  | PTpLam(d) -> check_postor_pf_r d

(*** ensure there's no extra information: nil or hash of the rest ***)
let check_postor_pdoc_e d =
  match d with
  | PDocNil -> ()
  | PDocHash(_) -> ()
  | _ -> raise InappropriatePostor

(*** d should be a partial doc abbreviated except for one leaf ***)
let rec check_postor_pdoc_r d =
  match d with
  | PDocNil -> raise InappropriatePostor
  | PDocHash(_) -> raise InappropriatePostor
  | PDocSigna(h,dr) -> check_postor_pdoc_r dr
  | PDocParam(h,a,dr) ->
      check_postor_pdoc_e dr;
      hashpair h (hashtp a)
  | PDocParamHash(h,dr) -> check_postor_pdoc_r dr
  | PDocDef(_,m,dr) ->
      check_postor_pdoc_e dr;
      check_postor_tm_r m
  | PDocDefHash(h,dr) -> check_postor_pdoc_r dr
  | PDocKnown(TmH(h),dr) -> check_postor_pdoc_r dr
  | PDocKnown(m,dr) ->
      check_postor_pdoc_e dr;
      check_postor_tm_r m
  | PDocConj(TmH(h),dr) -> check_postor_pdoc_r dr
  | PDocConj(m,dr) ->
      check_postor_pdoc_e dr;
      check_postor_tm_r m
  | PDocPfOf(TmH(_),d,dr) ->
      check_postor_pdoc_e dr;
      check_postor_pf_r d
  | PDocPfOf(m,Gpa(_),dr) ->
      check_postor_pdoc_e dr;
      check_postor_tm_r m
  | PDocPfOf(_,_,dr) -> raise InappropriatePostor
  | PDocPfOfHash(h,dr) -> check_postor_pdoc_r dr

(*** alpha is a p2pkhaddr, beta is a pubaddr, and these types are both the same as hashval ***)
let check_postor_pdoc tm csm mtar alpha beta m =
  try
    let h = check_postor_pdoc_r m in
    let betah = hashpair beta h in
    let (_,_,_,_,x) = hashpair alpha betah in
    Int32.logand x 0xffffl  = 0l (*** one of every 65536 (beta,h) pairs can be used by each address alpha ***)
      &&
    lt_big_int (hitval tm betah csm) mtar
  with InappropriatePostor -> false

(***
 hitval computes a big_int by hashing the timestamp (in seconds), the stake's asset id and the current stake modifier.
 If there is no proof of burn or proof of storage, then there's a hit if the hitval is less than the target times the stake.
 If there is proof of burn, the number of litecoin satoshis * 10000 is added to the stake.
 With a proof of storage, the stake is multiplied by 1.25 before the comparison is made.
 A proof of storage is either a term or partial document which abbreviates everything except one
 leaf. That leaf hashed with the hash of the root of the term/pdoc should hash with the stake address
 in a way that has 16 0 bits as the least significant bits.
 That is, for each stake address there are 0.0015% of proofs-of-storage that can be used by that address.
***)
let check_hit_b blkh bday obl v csm tar tmstmp stkid stkaddr brn strd =
  let (v,sincepow) =
    match brn with
    | Poburn(_,_,u) -> (Int64.add v (Int64.mul u 1000L),0)
    | SincePoburn(j) -> (v,j)
  in
  match strd with
  | None -> lt_big_int (hitval tmstmp stkid csm) (mult_big_int tar (coinage blkh bday obl sincepow v))
  | Some(PostorTrm(th,m,a,h)) -> (*** h is not relevant here; it is the asset id to look it up in the ctree ***)
      let beta = hashopair2 th (hashpair (tm_hashroot m) (hashtp a)) in
      let mtar = (mult_big_int tar (coinage blkh bday obl sincepow (incrstake v))) in
      lt_big_int (hitval tmstmp stkid csm) mtar
	&&
      check_postor_tm tmstmp csm mtar stkaddr beta m
  | Some(PostorDoc(gamma,nonce,th,d,h)) -> (*** h is not relevant here; it is the asset id to look it up in the ctree ***)
      let prebeta = hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th (pdoc_hashroot d))) in
      let mtar = (mult_big_int tar (coinage blkh bday obl sincepow (incrstake v))) in
      lt_big_int (hitval tmstmp stkid csm) mtar
	&&
      check_postor_pdoc tmstmp csm mtar stkaddr prebeta d

let check_hit_a blkh bday obl v tinf tmstmp stkid stkaddr brn strd =
  let (csm,fsm,tar) = tinf in
  check_hit_b blkh bday obl v csm tar tmstmp stkid stkaddr brn strd

let check_hit blkh tinf bh bday obl v =
  check_hit_a blkh bday obl v tinf bh.timestamp bh.stakeassetid bh.stakeaddr bh.announcedpoburn bh.stored

let coinstake b =
  let ((bhd,bhs),bd) = b in
  match bd.forfeiture with
  | None -> ([p2pkhaddr_addr bhd.stakeaddr,bhd.stakeassetid],bd.stakeoutput)
  | Some((bhd1,_),_,_,_,_,fal) ->
      let a = p2pkhaddr_addr bhd1.stakeaddr in
      ((p2pkhaddr_addr bhd.stakeaddr,bhd.stakeassetid)::List.map (fun fid -> (a,fid)) fal,bd.stakeoutput)

let hash_blockheaderdata bh =
  hashtag
    (hashopair2 bh.prevblockhash
       (hashpair
	  (hashopair2 bh.newtheoryroot
	     (hashopair2 bh.newsignaroot
		bh.newledgerroot))
	  (hashpair
	     (hashpair bh.stakeaddr bh.stakeassetid)
	     (hashpair
		(hashpoburn bh.announcedpoburn)
		(hashopair2
		   (hashopostor bh.stored)
		   (hashpair
		      (hashtargetinfo bh.tinfo)
		      (hashpair (hashint64 bh.timestamp) (hashint32 bh.deltatime))))))))
    1028l

let valid_blockheader_allbutsignat blkh tinfo bhd (aid,bday,obl,u) =
  bhd.stakeassetid = aid
    &&
  match u with
  | Currency(v) ->
      begin
	check_hit blkh tinfo bhd bday obl v
	  &&
	bhd.deltatime > 0l
	  &&
	begin
	  match bhd.announcedpoburn with
	  | Poburn(_,_,_) -> true
	  | SincePoburn(i) -> i < 256 (*** insist on poburn at least every 256 blocks ***)
	end
	  &&
	begin
	  match bhd.stored with
	  | None -> true
	  | Some(PostorTrm(th,m,a,h)) ->
	      let beta = hashopair2 th (hashpair (tm_hashroot m) (hashtp a)) in
	      begin
		match ctree_lookup_asset false false h bhd.prevledger (addr_bitseq (termaddr_addr beta)) with
		| Some(_,_,_,OwnsObj(_,_)) -> true
		| _ -> false
	      end
	  | Some(PostorDoc(gamma,nonce,th,d,h)) ->
	      let prebeta = hashpair (hashaddr (payaddr_addr gamma)) (hashpair nonce (hashopair2 th (pdoc_hashroot d))) in
	      let beta = hashval_pub_addr prebeta in
	      begin
		match ctree_lookup_asset false false h bhd.prevledger (addr_bitseq beta) with
		| Some(_,_,_,DocPublication(_,_,_,_)) -> true
		| _ -> false
	      end
	end
      end
  | _ -> false

let valid_blockheader_signat (bhd,bhs) (aid,bday,obl,v) =
  begin
    match bhs.blocksignatendorsement with
    | None -> verify_p2pkhaddr_signat (hashval_big_int (hash_blockheaderdata bhd)) bhd.stakeaddr bhs.blocksignat bhs.blocksignatrecid bhs.blocksignatfcomp
    | Some(beta,recid,fcomp,esg) -> (*** signature via endorsement ***)
	begin
	  (verifybitcoinmessage bhd.stakeaddr recid fcomp esg ("endorse " ^ (addr_qedaddrstr (hashval_p2pkh_addr beta)))
	     &&
	   verify_p2pkhaddr_signat (hashval_big_int (hash_blockheaderdata bhd)) beta bhs.blocksignat bhs.blocksignatrecid bhs.blocksignatfcomp)
	|| (!Config.testnet (*** allow fake endorsements in testnet ***)
	      &&
	    verifybitcoinmessage (-916116462l, -1122756662l, 602820575l, 669938289l, 1956032577l) recid fcomp esg ("fakeendorsement " ^ (addr_qedaddrstr (hashval_p2pkh_addr beta)) ^ " (" ^ (addr_qedaddrstr (hashval_p2pkh_addr bhd.stakeaddr)) ^ ")")
	     &&
	   verify_p2pkhaddr_signat (hashval_big_int (hash_blockheaderdata bhd)) beta bhs.blocksignat bhs.blocksignatrecid bhs.blocksignatfcomp)
	end
  end

let valid_blockheader_a blkh tinfo (bhd,bhs) (aid,bday,obl,v) =
  valid_blockheader_signat (bhd,bhs) (aid,bday,obl,v)
    &&
  valid_blockheader_allbutsignat blkh tinfo bhd (aid,bday,obl,v)

exception HeaderNoStakedAsset
exception HeaderStakedAssetNotMin

let blockheader_stakeasset bhd =
  let bl = addr_bitseq (p2pkhaddr_addr bhd.stakeaddr) in
  match ctree_lookup_asset false false bhd.stakeassetid bhd.prevledger bl with
  | Some(a) ->
      let (aid,_,_,_) = a in
      if minimal_asset_supporting_ctree bhd.prevledger bl aid 50 then (*** ensure that the ctree contains no extra information; this is a condition to prevent headers from being large by including unnecessary information; also only allow the first 50 assets held at an address to be used for staking ***)
	a
      else
	raise HeaderStakedAssetNotMin
  | _ -> 
      raise HeaderNoStakedAsset
	
let valid_blockheader blkh tinfo (bhd,bhs) =
  try
    valid_blockheader_a blkh tinfo (bhd,bhs) (blockheader_stakeasset bhd)
  with
  | HeaderStakedAssetNotMin -> false
  | HeaderNoStakedAsset -> false

let ctree_of_block (b:block) =
  let ((bhd,bhs),bd) = b in
  ctree_cgraft bd.prevledgergraft bhd.prevledger

let rec stxs_allinputs stxl =
  match stxl with
  | ((inpl,_),_)::stxr -> inpl @ stxs_allinputs stxr
  | [] -> []

let rec stxs_alloutputs stxl =
  match stxl with
  | ((_,outpl),_)::stxr -> outpl @ stxs_alloutputs stxr
  | [] -> []

(*** all txs of the block combined into one big transaction; used for checking validity of blocks ***)
let tx_of_block b =
  let ((bhd,_),bd) = b in
  let (ci,co) = coinstake b in
  (ci @ stxs_allinputs bd.blockdelta_stxl,co @ stxs_alloutputs bd.blockdelta_stxl)

let txl_of_block b =
  let (_,bd) = b in
  (coinstake b,List.map (fun (tx,_) -> tx) bd.blockdelta_stxl)

let rec check_bhl pbh bhl oth =
  if pbh = Some(oth) then (*** if this happens, then it's not a genuine fork; one of the lists is a sublist of the other ***)
    raise Not_found
  else
    match bhl with
    | [] -> pbh
    | (bhd::bhr) ->
	if pbh = Some(hash_blockheaderdata bhd) then
	  check_bhl bhd.prevblockhash bhr oth
	else
	  raise Not_found

let rec check_poforfeit_a blkh alpha alphabs v fal tr =
  match fal with
  | [] -> v = 0L
  | fa::far ->
      match ctree_lookup_asset false false fa tr alphabs with
      | Some(_,bday,Some(alpha2,_,r),Currency(u)) when r && Int64.add bday 6L >= blkh && payaddr_addr alpha2 = alpha ->
	  check_poforfeit_a blkh alpha alphabs (Int64.sub v u) far tr
      | _ -> false

let check_poforfeit blkh ((bhd1,bhs1),(bhd2,bhs2),bhl1,bhl2,v,fal) tr =
  if hash_blockheaderdata bhd1 = hash_blockheaderdata bhd2 || not (bhd1.stakeaddr = bhd2.stakeaddr) || List.length bhl1 > 5 || List.length bhl2 > 5 then
    false
  else
    let bhd1h = hash_blockheaderdata bhd1 in
    let bhd2h = hash_blockheaderdata bhd2 in
    (*** we only need to check the signatures here at the heads by the bad actor bhd*.stakeaddr ***)
    if verify_p2pkhaddr_signat (hashval_big_int bhd1h) bhd1.stakeaddr bhs1.blocksignat bhs1.blocksignatrecid bhs1.blocksignatfcomp
	&&
      verify_p2pkhaddr_signat (hashval_big_int bhd2h) bhd2.stakeaddr bhs2.blocksignat bhs2.blocksignatrecid bhs2.blocksignatfcomp
    then
      try
	begin
	  if check_bhl (bhd1.prevblockhash) bhl1 bhd2h = check_bhl (bhd2.prevblockhash) bhl2 bhd1h then (*** bhd1.stakeaddr signed in two different forks within six blocks of fbh1 ***)
	    let alpha = p2pkhaddr_addr bhd1.stakeaddr in
	    check_poforfeit_a blkh alpha (addr_bitseq alpha) v fal tr
	  else
	    false
	end
      with Not_found -> false
    else
      false

let valid_block_a tht sigt blkh tinfo b ((aid,bday,obl,u) as a) stkaddr =
  let ((bhd,bhs),bd) = b in
  (*** The header is valid. ***)
  if (valid_blockheader_a blkh tinfo (bhd,bhs) (aid,bday,obl,u)
	&&
      tx_outputs_valid bd.stakeoutput
	&&
      (*** ensure that if the stake has an explicit obligation (e.g., it is borrowed for staking), then the obligation isn't changed; otherwise the staker could steal the borrowed stake; unchanged copy should be first output ***)
      begin
	match a with
	| (_,_,Some(beta,n,r),Currency(v)) -> (*** stake may be on loan for staking ***)
	    begin
	      match bd.stakeoutput with
	      | (alpha2,(Some(beta2,n2,r2),Currency(v2)))::remouts -> (*** the first output must recreate the loaned asset. It's a reward iff it was already a reward. The remaining outputs are marked as rewards and are subject to forfeiture. ***)
		  r2 = r
		    &&
		  alpha2 = stkaddr
		    &&
		  beta2 = beta
		    &&
		  n2 = n
		    &&
		  v2 = v
		    &&
		  begin
		    try (*** all other outputs must be marked as rewards and are subject to forfeiture; they also must acknowledge they cannot be spent for at least reward_locktime many blocks ***)
		      ignore (List.find (fun (alpha3,(obl,v)) -> not (alpha3 = stkaddr) || match obl with Some(_,n,r) when r && n >= Int64.add blkh (reward_locktime blkh) -> false | _ -> true) remouts);
		      false
		    with Not_found -> true
		  end
	      | _ ->
		  false
	    end
	| (_,_,None,Currency(v)) -> (*** stake has the default obligation ***)
	    begin (*** the first output is optionally the stake with the default obligation (not a reward, immediately spendable) with all other outputs must be marked as rewards and are subject to forfeiture; they also must acknowledge they cannot be spent for at least reward_locktime many blocks ***)
	      match bd.stakeoutput with
	      | (alpha2,(_,Currency(v2)))::remouts -> (*** allow the staker to choose the new obligation for the staked asset [Feb 2016] ***)
		  begin
		    alpha2 = stkaddr
		      &&
		    v2 = v
		      &&
		    try
		      ignore (List.find (fun (alpha3,(obl,v)) -> not (alpha3 = stkaddr) || match obl with Some(_,n,r) when r && n >= Int64.add blkh (reward_locktime blkh) -> false | _ -> true) remouts);
		      false
		    with Not_found -> true
		  end
	      | _ ->
		  try
		    ignore (List.find (fun (alpha3,(obl,v)) -> not (alpha3 = stkaddr) || match obl with Some(_,n,r) when r && n >= Int64.add blkh (reward_locktime blkh) -> false | _ -> true) bd.stakeoutput);
		    false
		  with Not_found -> true
	    end
	| _ -> false (*** this means the staked asset isn't currency, which is not allowed ***)
      end)
  then
    let tr = ctree_of_block b in (*** let tr be the ctree of the block, used often below ***)
    if ((try let z = ctree_supports_tx false false tht sigt blkh (coinstake b) tr in (*** the ctree must support the tx without the need to expand hashes using the database or requesting from peers ***)
    z >= rewfn blkh
    with NotSupported -> false)
	  &&
	(*** There are no duplicate transactions. (Comparing the signatures would be an error since they contain abstract values.) ***)
	no_dups (List.map (fun (tau,_) -> tau) bd.blockdelta_stxl)
	  &&
	(*** The cgraft is valid. ***)
	cgraft_valid bd.prevledgergraft
	  &&
	let stakein = (stkaddr,bhd.stakeassetid) in
	(*** Each transaction in the delta has supported elaborated assets and is appropriately signed. ***)
	(*** Also, each transaction in the delta is valid and supported without a reward. ***)
	(*** Also, no transaction has the stake asset as an input. ***)
	(*** None of the outputs say they are rewards. ***)
	begin
	  try
	    List.fold_left
	      (fun sgvb stau ->
		match stau with
		| ((inpl,outpl) as tau,_) ->
		    let norew =
		      begin
			try
			  ignore (List.find 
				    (fun (a,(obl,v)) ->
				      match obl with
				      | Some(_,_,r) when r -> true
				      | _ -> false)
				    outpl);
			  false
			with Not_found -> true
		      end
		    in
		    let aal = ctree_lookup_input_assets false false inpl tr in
		    let al = List.map (fun (_,a) -> a) aal in
		    norew
		      && sgvb
		      && not (List.mem stakein inpl)
		      && tx_signatures_valid blkh al stau
		      && tx_valid tau
		      && ctree_supports_tx_2 false false tht sigt blkh tau aal al tr <= 0L
	      )
	      true
	      bd.blockdelta_stxl
	  with NotSupported -> false
	end
	  &&
	(*** No distinct transactions try to spend the same asset. ***)
	(*** Also, ownership is not created for the same address alpha by two txs in the block. ***)
	begin
	  try
	    let stxlr = ref bd.blockdelta_stxl in
	    while not (!stxlr = []) do
	      match !stxlr with
	      | ((inpl1,outpl1),_)::stxr ->
		  let hl1 = List.map (fun (_,h) -> h) inpl1 in
		  let oo1 = ref [] in
		  let op1 = ref [] in
		  List.iter
		    (fun (alpha1,(obl1,u1)) ->
		      match u1 with
		      | OwnsObj(_,_) -> oo1 := alpha1::!oo1
		      | OwnsProp(_,_) -> op1 := alpha1::!op1
		      | _ -> ())
		    outpl1;
		  stxlr := stxr;
		  List.iter
		    (fun ((inpl2,outpl2),_) ->
		      List.iter
			(fun (_,h) ->
			  if List.mem h hl1 then
			    raise NotSupported (*** This is a minor abuse of this exception. There could be a separate exception for this case. ***)
			) inpl2;
		      List.iter
			(fun (alpha2,(obl2,u2)) ->
			  match u2 with
			  | OwnsObj(_,_) ->
			      if List.mem alpha2 !oo1 then raise NotSupported
			  | OwnsProp(_,_) ->
			      if List.mem alpha2 !op1 then raise NotSupported
			  | _ -> ()
			)
			outpl2
		    )
		    stxr
	      | [] -> ()
	    done;
	    true
	  with NotSupported -> false
	end
	  &&
	(*** Ownership is not created for the same address alpha by the coinstake and a tx in the block. ***)
	begin
	  try
	    List.iter
	      (fun (alpha,(obl,u)) ->
		match u with
		| OwnsObj(_,_) ->
		    List.iter
		      (fun ((_,outpl2),_) ->
			List.iter
			  (fun (alpha2,(obl2,u2)) ->
			    if alpha = alpha2 then
			      match u2 with
			      | OwnsObj(_,_) -> raise NotSupported
			      | _ -> ())
			  outpl2)
		      bd.blockdelta_stxl
		| OwnsProp(_,_) ->
		    List.iter
		      (fun ((_,outpl2),_) ->
			List.iter
			  (fun (alpha2,(obl2,u2)) ->
			    if alpha = alpha2 then
			      match u2 with
			      | OwnsProp(_,_) -> raise NotSupported
			      | _ -> ())
			  outpl2)
		      bd.blockdelta_stxl
		| _ -> ()
	      )
	      bd.stakeoutput;
	    true
	  with NotSupported -> false
	end)
    then
      let (forfeitval,forfok) =
	begin
	  match bd.forfeiture with
	  | None -> (0L,true)
	  | Some(bh1,bh2,bhl1,bhl2,v,fal) ->
	      let forfok = check_poforfeit blkh (bh1,bh2,bhl1,bhl2,v,fal) tr in
  	      (v,forfok)
	end
      in
      if (forfok
	    &&
	  (***
	      The root of the transformed ctree is the newledgerroot in the header.
	   ***)
	  begin
	    let (cstk,txl) = txl_of_block b in (*** the coinstake tx is performed last, i.e., after the txs in the block. ***)
	    match tx_octree_trans blkh cstk (txl_octree_trans blkh txl (Some(tr))) with
	    | Some(tr2) ->
		bhd.newledgerroot = ctree_hashroot tr2
	    | None -> false
	  end)
      then
	(*** The total inputs and outputs match up with the declared fee. ***)
	let tau = tx_of_block b in (*** let tau be the combined tx of the block ***)
	let (inpl,outpl) = tau in
	let aal = ctree_lookup_input_assets false false inpl tr in
	let al = List.map (fun (_,a) -> a) aal in
	(*** Originally I added totalfees to the out_cost, but this was wrong since the totalfees are in the stake output which is already counted in out_cost. I don't really need totalfees to be explicit. ***)
	if out_cost outpl = Int64.add (asset_value_sum blkh al) (Int64.add (rewfn blkh) forfeitval) then
	  let newtht = txout_update_ottree outpl tht in
	  let newsigt = txout_update_ostree outpl sigt in
	  if bhd.newtheoryroot = ottree_hashroot newtht
	      &&
	    bhd.newsignaroot = ostree_hashroot newsigt
	  then
	    Some(newtht,newsigt)
	  else
	    None
	else
	  None
      else
	None
    else
      None
  else
    None

let valid_block tht sigt blkh tinfo (b:block) =
  let ((bhd,_),_) = b in
  let stkaddr = p2pkhaddr_addr bhd.stakeaddr in
  try
    valid_block_a tht sigt blkh tinfo b (blockheader_stakeasset bhd) stkaddr
  with
  | HeaderStakedAssetNotMin -> None
  | HeaderNoStakedAsset -> None

type blockchain = block * block list
type blockheaderchain = blockheader * blockheader list

let blockchain_headers bc =
  let ((bh,bd),bl) = bc in
  (bh,List.map (fun b -> let (bh,bd) = b in bh) bl)

let ledgerroot_of_blockchain bc =
  let (((bhd,bhs),bd),bl) = bc in
  bhd.newledgerroot

(*** retargeting at each step ***)
let retarget tar deltm =
  min_big_int
    !max_target
    (div_big_int
       (mult_big_int tar
	  (big_int_of_int32 (Int32.add 9000l deltm)))
       (big_int_of_int 9600))

(*** cumulative stake ***)
let cumul_stake cs tar deltm =
  add_big_int
    cs
    (max_big_int unit_big_int (div_big_int !max_target (shift_right_towards_zero_big_int (mult_big_int tar (big_int_of_int32 deltm)) 20)))

let blockheader_succ_a prevledgerroot tmstamp1 announcedpoburn1 tinfo1 bh2 =
  let (bhd2,bhs2) = bh2 in
  ctree_hashroot bhd2.prevledger = prevledgerroot
    &&
  bhd2.timestamp = Int64.add tmstamp1 (Int64.of_int32 bhd2.deltatime)
    &&
  let (csm1,fsm1,tar1) = tinfo1 in
  let (csm2,fsm2,tar2) = bhd2.tinfo in
  begin
    match bhd2.announcedpoburn with
    | Poburn(h,k,x) -> (*** If proof of burn, then the new csm2 and fsm2 are determined by h and k (where h was the ltc block hash, which is unpredictable) ***)
	let (csm,fsm) = compute_stakemods (hashpair (ripemd160_md256 h) (ripemd160_md256 k)) in
	csm2 = csm && fsm2 = fsm
    | SincePoburn(i) ->
	stakemod_pushbit (stakemod_lastbit fsm1) csm1 = csm2 (*** new stake modifier is old one shifted with one new bit from the future stake modifier ***)
	  &&
	stakemod_pushbit (stakemod_firstbit fsm2) fsm1 = fsm2 (*** the new bit of the new future stake modifier fsm2 is freely chosen by the staker ***)
	  &&
	match announcedpoburn1 with (*** ensure that we are counting the number of blocks since the last poburn ***)
	| Poburn(_,_,_) -> i = 1
	| SincePoburn(j) -> i = j + 1
  end
    &&
  eq_big_int tar2 (retarget tar1 bhd2.deltatime)

let blockheader_succ bh1 bh2 =
  let (bhd1,bhs1) = bh1 in
  let (bhd2,bhs2) = bh2 in
  bhd2.prevblockhash = Some (hash_blockheaderdata bhd1)
    &&
  blockheader_succ_a bhd1.newledgerroot bhd1.timestamp bhd1.announcedpoburn bhd1.tinfo bh2

let rec valid_blockchain_aux blkh bl =
  match bl with
  | ((bh,bd)::(pbh,pbd)::br) ->
      if blkh > 1L then
	let (pbhd,_) = pbh in
	let (tht,sigt) = valid_blockchain_aux (Int64.sub blkh 1L) ((pbh,pbd)::br) in
	if blockheader_succ pbh bh then
	  begin
	    match valid_block tht sigt blkh pbhd.tinfo (bh,bd) with
	    | Some(tht2,sigt2) -> (tht2,sigt2)
	    | None -> raise NotSupported
	  end
	else
	  raise NotSupported
      else
	raise NotSupported
  | [(bh,bd)] ->
      let (bhd,bhs) = bh in
      if blkh = 1L && bhd.prevblockhash = None
	  && blockheader_succ_a !genesisledgerroot !Config.genesistimestamp (SincePoburn(0)) (!genesiscurrentstakemod,!genesisfuturestakemod,!genesistarget) bh
      then
	begin
	  match valid_block None None blkh (!genesiscurrentstakemod,!genesisfuturestakemod,!genesistarget) (bh,bd) with
	  | Some(tht2,sigt2) -> (tht2,sigt2)
	  | None -> raise NotSupported
	end
      else
	raise NotSupported
  | [] -> raise NotSupported

let valid_blockchain blkh bc =
  try
    let (b,bl) = bc in
    ignore (valid_blockchain_aux blkh (b::bl));
    true
  with NotSupported -> false

let rec valid_blockheaderchain_aux blkh bhl =
  match bhl with
  | (bh::pbh::bhr) ->
      if blkh > 1L then
	let (pbhd,_) = pbh in
	valid_blockheaderchain_aux (Int64.sub blkh 1L) (pbh::bhr)
	  && blockheader_succ pbh bh
	  && valid_blockheader blkh pbhd.tinfo bh
      else
	false
  | [(bhd,bhs)] ->
      blkh = 1L
	&&
      valid_blockheader blkh (!genesiscurrentstakemod,!genesisfuturestakemod,!genesistarget) (bhd,bhs)
	&&
      bhd.prevblockhash = None
	&&
      ctree_hashroot bhd.prevledger = !genesisledgerroot
	&&
      blockheader_succ_a !genesisledgerroot !Config.genesistimestamp (SincePoburn(0)) (!genesiscurrentstakemod,!genesisfuturestakemod,!genesistarget) (bhd,bhs)
  | [] -> false

let valid_blockheaderchain blkh bhc =
  match bhc with
  | (bh,bhr) -> valid_blockheaderchain_aux blkh (bh::bhr)
