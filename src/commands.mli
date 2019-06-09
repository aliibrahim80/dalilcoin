(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Json
open Big_int
open Hash
open Mathdata
open Assets
open Signat
open Tx
open Ctre
open Logic

(***
val addnode : string -> int -> bool
***)

val walletkeys_staking : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletkeys_nonstaking : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletkeys_staking_fresh : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletkeys_nonstaking_fresh : (big_int * bool * (big_int * big_int) * string * p2pkhaddr * string) list ref
val walletendorsements : (payaddr * payaddr * (big_int * big_int) * int * bool * signat) list ref
val walletp2shs : (p2shaddr * string * int list) list ref
val walletwatchaddrs : addr list ref
val walletwatchaddrs_offlinekey : addr list ref
val walletwatchaddrs_offlinekey_fresh : addr list ref
val stakingassets : (p2pkhaddr * hashval * int64 * obligation * int64) list ref

val get_spendable_assets_in_ledger : out_channel -> hashval -> int64 -> (addr * asset * int64) list
val get_cants_balances_in_ledger : out_channel -> hashval -> int64 -> int64 * int64 * int64 * int64 * int64 * int64 * int64 * int64

val load_txpool : unit -> unit
val save_txpool : unit -> unit
val load_wallet : unit -> unit
val save_wallet : unit -> unit

val printassets : out_channel -> unit
val printassets_in_ledger : out_channel -> hashval -> int64 -> unit
val printctreeinfo : out_channel -> hashval -> unit
val printctreeelt : out_channel -> hashval -> unit
val printhconselt : out_channel -> hashval -> unit
val printasset : out_channel -> hashval -> unit
val printtx : out_channel -> hashval -> unit

val btctodaliladdr : out_channel -> string -> unit
val importprivkey : out_channel -> string -> string -> unit
val importbtcprivkey : out_channel -> string -> string -> unit
val importendorsement : out_channel -> string -> string -> string -> unit
val importp2sh : out_channel -> int list -> unit
val importwatchaddr : out_channel -> string -> string -> unit
val importwatchbtcaddr : out_channel -> string -> string -> unit
val generate_newkeyandaddress : hashval -> string -> big_int * p2pkhaddr
val get_fresh_offline_address : out_channel -> addr

val reclassify_staking : out_channel -> string -> bool -> unit

val createtx : jsonval -> jsonval -> tx
val createsplitlocktx : out_channel -> hashval -> int64 -> payaddr -> payaddr -> addr -> hashval -> int -> int64 -> int64 -> unit

val signbatchtxsc : out_channel -> hashval -> stx list -> out_channel -> (big_int * bool * (big_int * big_int) * p2pkhaddr) list option -> unit
val signtx2 : out_channel -> hashval -> stx -> (big_int * bool * (big_int * big_int) * p2pkhaddr) list option -> stx * bool * bool
val signtxc : out_channel -> hashval -> stx -> out_channel -> (big_int * bool * (big_int * big_int) * p2pkhaddr) list option -> unit
val signtx : out_channel -> hashval -> string -> (big_int * bool * (big_int * big_int) * p2pkhaddr) list option -> unit
val savetxtopool : int64 -> int64 -> hashval -> string -> unit
val signtx : out_channel -> hashval -> string -> (big_int * bool * (big_int * big_int) * p2pkhaddr) list option -> unit
val validatebatchtxs : out_channel -> int64 -> int64 -> hashval option -> hashval option -> hashval -> stx list -> unit
val validatetx2 : out_channel -> int64 -> int64 -> hashval option -> hashval option -> hashval -> stx -> unit
val validatetx : out_channel -> int64 -> int64 -> hashval option -> hashval option -> hashval -> string -> unit
val sendtx2 : out_channel -> int64 -> int64 -> hashval option -> hashval option -> hashval -> stx -> unit
val sendtx : out_channel -> int64 -> int64 -> hashval option -> hashval option -> hashval -> string -> unit

val query_at_block : string -> (hashval * Block.poburn) option -> hashval -> int64 -> jsonval
val query : string -> jsonval
val query_blockheight : int64 -> jsonval

val preassetinfo_report : out_channel -> preasset -> unit

val verifyfullledger : out_channel -> hashval -> unit

val requestfullledger : out_channel -> hashval -> unit

val dumpwallet : string -> unit
val pblockchain : out_channel -> (hashval * hashval * hashval) option -> int -> unit
val dumpstate : string -> unit

val reportowned : out_channel -> out_channel -> hashval -> unit
val reportbounties : out_channel -> out_channel -> hashval -> unit
val reportpubs : out_channel -> out_channel -> hashval -> unit
