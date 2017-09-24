(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Sha256
open Hash
open Big_int
open Mathdata
open Logic
open Assets
open Signat
open Tx
open Ctre
open Ctregraft

type stakemod = hashval
val genesisstakemod : stakemod ref
val genesisledgerroot : hashval ref
val genesistarget : big_int ref
val max_target : big_int ref

type targetinfo = big_int

val targetinfo_string : targetinfo -> string

val seo_targetinfo : (int -> int -> 'a -> 'a) -> targetinfo -> 'a -> 'a
val sei_targetinfo : (int -> 'a -> int * 'a) -> 'a -> targetinfo * 'a

val rewfn : int64 -> int64
val hitval : int64 -> hashval -> stakemod -> big_int

type poburn =
  | Poburn of md256 * md256 * int64 (** ltc block hash id, ltc tx hash id, number of litecoin burned **)

val poburn_stakemod : poburn -> stakemod

type blockheaderdata = {
    prevblockhash : (hashval * hashval) option;
    newtheoryroot : hashval option;
    newsignaroot : hashval option;
    newledgerroot : hashval;
    stakeaddr : p2pkhaddr;
    stakeassetid : hashval;
    timestamp : int64;
    deltatime : int32;
    tinfo : targetinfo;
    prevledger : ctree;
    blockdeltaroot : hashval;
  }

type blockheadersig = {
    announcedpoburn : poburn;
    blocksignat : signat;
    blocksignatrecid : int;
    blocksignatfcomp : bool;
    blocksignatendorsement : (p2pkhaddr * int * bool * signat) option;
  }

type blockheader = blockheaderdata * blockheadersig

val fake_blockheader : blockheader

val seo_blockheaderdata : (int -> int -> 'a -> 'a) -> blockheaderdata -> 'a -> 'a
val sei_blockheaderdata : (int -> 'a -> int * 'a) -> 'a -> blockheaderdata * 'a
val seo_blockheader : (int -> int -> 'a -> 'a) -> blockheader -> 'a -> 'a
val sei_blockheader : (int -> 'a -> int * 'a) -> 'a -> blockheader * 'a

type poforfeit = blockheader * blockheader * blockheaderdata list * blockheaderdata list * int64 * hashval list

type blockdelta = {
    stakeoutput : addr_preasset list;
    forfeiture : poforfeit option;
    prevledgergraft : cgraft;
    blockdelta_stxl : stx list
  }

type block = blockheader * blockdelta

val seo_blockdelta : (int -> int -> 'a -> 'a) -> blockdelta -> 'a -> 'a
val sei_blockdelta : (int -> 'a -> int * 'a) -> 'a -> blockdelta * 'a
val seo_block : (int -> int -> 'a -> 'a) -> block -> 'a -> 'a
val sei_block : (int -> 'a -> int * 'a) -> 'a -> block * 'a

module DbBlockHeaderData :
    sig
      val dbinit : unit -> unit
      val dbget : Hash.hashval -> blockheaderdata
      val dbexists : Hash.hashval -> bool
      val dbput : Hash.hashval -> blockheaderdata -> unit
      val dbdelete : Hash.hashval -> unit
    end

module DbBlockHeaderSig :
    sig
      val dbinit : unit -> unit
      val dbget : Hash.hashval -> blockheadersig
      val dbexists : Hash.hashval -> bool
      val dbput : Hash.hashval -> blockheadersig -> unit
      val dbdelete : Hash.hashval -> unit
    end

module DbBlockDelta :
    sig
      val dbinit : unit -> unit
      val dbget : Hash.hashval -> blockdelta
      val dbexists : Hash.hashval -> bool
      val dbput : Hash.hashval -> blockdelta -> unit
      val dbdelete : Hash.hashval -> unit
    end

module DbInvalidatedBlocks :
  sig
    val dbinit : unit -> unit
    val dbget : hashval -> bool
    val dbexists : hashval -> bool
    val dbput : hashval -> bool -> unit
    val dbdelete : hashval -> unit
  end

val get_blockheaderdata : hashval -> blockheaderdata
val get_blockheadersig : hashval -> blockheadersig
val get_blockheader : hashval -> blockheader
val get_blockdelta : hashval -> blockdelta

val coinstake : block -> tx

val check_hit_b : int64 -> int64 -> obligation -> int64
  -> stakemod -> big_int -> int64 -> hashval -> p2pkhaddr -> poburn -> bool
val check_hit : int64 -> stakemod -> targetinfo -> blockheaderdata -> int64 -> obligation -> int64 -> poburn -> bool

val hash_blockheaderdata : blockheaderdata -> hashval
val hash_blockheadersig : blockheadersig -> hashval
val blockdelta_hashroot : blockdelta -> hashval

exception HeaderNoStakedAsset
exception HeaderStakedAssetNotMin
val blockheader_stakeasset : blockheaderdata -> asset

val valid_blockheader_allbutsignat : int64 -> stakemod -> targetinfo -> blockheaderdata -> asset -> poburn -> bool
val valid_blockheader_signat : blockheader -> asset -> bool

val valid_blockheader : int64 -> stakemod -> targetinfo -> blockheader -> bool

val ctree_of_block : block -> ctree

val txl_of_block : block -> tx * tx list

val retarget : big_int -> int32 -> big_int
val difficulty : big_int -> big_int
val cumul_stake : big_int -> big_int -> int32 -> big_int

val valid_block : ttree option -> stree option -> int64 -> stakemod -> targetinfo -> block -> (ttree option * stree option) option

val blockheader_succ_a : hashval -> int64 -> targetinfo -> blockheader -> bool
val blockheader_succ : blockheader -> blockheader -> bool

type blockchain = block * block list
type blockheaderchain = blockheader * blockheader list

val blockchain_headers : blockchain -> blockheaderchain

val ledgerroot_of_blockchain : blockchain -> hashval

val valid_blockchain : int64 -> blockchain -> bool

val valid_blockheaderchain : int64 -> blockheaderchain -> bool
