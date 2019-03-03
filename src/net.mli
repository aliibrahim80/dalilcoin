(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Hash

val sync_last_height : int64 ref

val shutdown_close : Unix.file_descr -> unit

(** associate (lbk,ltx) -- litecoin block id, litcoin burn tx id -- with various info.
 outlinevals associates pair with
   dalilcoin block id, litecoin median time, litoshis burned, optional previous (lbh,ltx) pair, stake modifier and dalilcoin block height
   This is all data that can be computed via the ltc blockchain.
 validheadervals associates pair with (if dalilcoin header has been validated and all previous headers have been validated)
   targetinfo, timestamp, newledgerroot, newtheorytreeroot, newsignatreeroot
   This information is all in the header, so the hash table is to make it easily accessible and to record that previous headers have been validated.
 validblockvals associates pair with () (if dalilcoin block has been validated and all previous blocks have been validated)
   just to record that we have the block (header and delta) and all have been validated.
 outlinesucc associates pair with several pairs that point back to this one.
 blockburns associates a dalilcoin block id with all the (lbh,ltx) burns supporting it.
    Typically there will be only one such burn, but this cannot be enforced.
 **)
val outlinevals : (hashval * hashval,hashval * int64 * int64 * (hashval * hashval) option * hashval * int64) Hashtbl.t
val validheadervals : (hashval * hashval,big_int * int64 * hashval * hashval option * hashval option) Hashtbl.t
val validblockvals : (hashval * hashval,unit)  Hashtbl.t
val outlinesucc : (hashval * hashval,hashval * hashval) Hashtbl.t
val blockburns : (hashval,hashval * hashval) Hashtbl.t

val missingheaders : (int64 * hashval) list ref
val missingdeltas : (int64 * hashval) list ref

exception GettingRemoteData
exception RequestRejected
exception IllformedMsg

val netblkh : int64 ref

type msgtype =
  | Version
  | Verack
  | Addr
  | Inv
  | GetSTx
  | GetHeaders
  | GetHeader
  | GetBlock
  | GetBlockdelta
  | STx
  | Block
  | Headers
  | Blockdelta
  | GetAddr
  | Alert
  | Ping
  | Pong
  | GetCTreeElement
  | GetHConsElement
  | GetAsset
  | CTreeElement
  | HConsElement
  | Asset
  | GetInvNbhd
  | GetElementsBelow
  | CompleteCTree
  | CompleteHList

val msgtype_of_int : int -> msgtype
val int_of_msgtype : msgtype -> int
val string_of_msgtype : msgtype -> string

val openlistener : string -> int -> int -> Unix.file_descr
val openonionlistener : string -> int -> int -> int -> Unix.file_descr

type connstate = {
    conntime : float;
    realaddr : string;
    connmutex : Mutex.t;
    sendqueue : (hashval * hashval option * msgtype * string) Queue.t;
    sendqueuenonempty : Condition.t;
    mutable nonce : int64 option;
    mutable handshakestep : int;
    mutable peertimeskew : int;
    mutable protvers : int32;
    mutable useragent : string;
    mutable addrfrom : string;
    mutable banned : bool;
    mutable lastmsgtm : float;
    mutable sentinv : (int * hashval,float) Hashtbl.t;
    mutable rinv : (int * hashval,unit) Hashtbl.t;
    mutable invreq : (int * hashval,float) Hashtbl.t;
    mutable invreqhooks : (int * hashval,unit -> unit) Hashtbl.t;
    mutable itemhooks : (int * hashval,unit -> unit) Hashtbl.t;
    mutable first_header_height : int64; (*** how much header history is stored at the node ***)
    mutable first_full_height : int64; (*** how much block/ctree history is stored at the node ***)
    mutable last_height : int64; (*** how up to date the node is ***)
  }

val peeraddr : connstate option -> string

val tryconnectpeer : string -> (Thread.t * Thread.t * (Unix.file_descr * in_channel * out_channel * connstate option ref)) option

val addknownpeer : int64 -> string -> unit
val removeknownpeer : string -> unit
val getknownpeers : unit -> string list
val loadknownpeers : unit -> unit
val saveknownpeers : unit -> unit

exception BannedPeer
val bannedpeers : (string,unit) Hashtbl.t
val banpeer : string -> unit
val clearbanned : unit -> unit

val send_inv_fn : (int -> out_channel -> connstate -> unit) ref
val msgtype_handler : (msgtype,in_channel * out_channel * connstate * string -> unit) Hashtbl.t

val netlistenerth : Thread.t option ref
val onionlistenerth : Thread.t option ref
val netseekerth : Thread.t option ref
val netconns : (Thread.t * Thread.t * (Unix.file_descr * in_channel * out_channel * connstate option ref)) list ref
val netconnsmutex : Mutex.t
val this_nodes_nonce : int64 ref

val remove_dead_conns : unit -> unit

val netlistener : Unix.file_descr -> unit
val onionlistener : Unix.file_descr -> unit
val netseeker : unit -> unit

val network_time : unit -> int64 * int

val queue_msg : connstate -> msgtype -> string -> hashval
val queue_reply : connstate -> hashval -> msgtype -> string -> hashval
val find_and_send_requestmissingblocks : connstate -> unit
val find_and_send_requestdata : msgtype -> hashval -> unit
val broadcast_requestdata : msgtype -> hashval -> unit
val broadcast_requestinv : msgtype -> hashval -> unit
val broadcast_inv : (int * hashval) list -> unit
val send_inv_to_one : (int * hashval) list -> connstate -> unit

val recently_requested : int * hashval -> float -> (int * hashval,float) Hashtbl.t -> bool
val recently_sent : int * hashval -> float -> (int * hashval,float) Hashtbl.t -> bool

val liberally_accept_elements_until : float -> unit
val liberally_accept_elements_p : float -> bool
