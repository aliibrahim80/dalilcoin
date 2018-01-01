(* Copyright (c) 2015-2016 The Qeditas developers *)
(* Copyright (c) 2017 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Hash

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

val int_of_msgtype : msgtype -> int
val string_of_msgtype : msgtype -> string

val openlistener : string -> int -> int -> Unix.file_descr

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
    mutable pending : (hashval * (bool * float * float * (msgtype * string -> unit))) list;
    mutable sentinv : (int * hashval * float) list;
    mutable rinv : (int * hashval) list;
    mutable invreq : (int * hashval * float) list;
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
val netseekerth : Thread.t option ref
val netconns : (Thread.t * Thread.t * (Unix.file_descr * in_channel * out_channel * connstate option ref)) list ref
val netconnsmutex : Mutex.t
val this_nodes_nonce : int64 ref

val remove_dead_conns : unit -> unit

val netlistener : Unix.file_descr -> unit
val netseeker : unit -> unit

val network_time : unit -> int64 * int

val queue_msg : connstate -> msgtype -> string -> hashval
val queue_reply : connstate -> hashval -> msgtype -> string -> hashval
val find_and_send_requestdata : msgtype -> hashval -> unit
val broadcast_requestdata : msgtype -> hashval -> unit
val broadcast_inv : (int * hashval) list -> unit

val recently_requested : int * hashval -> float -> (int * hashval * float) list -> bool
val recently_sent : int * hashval -> float -> (int * hashval * float) list -> bool
