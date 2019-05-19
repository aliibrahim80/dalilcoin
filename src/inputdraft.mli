(* Copyright (c) 2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash
open Logic

val input_token : in_channel -> string
val input_theoryspec : in_channel -> theoryspec * hashval option * addr option * (hashval,payaddr) Hashtbl.t * (hashval,payaddr * (int64 option)) Hashtbl.t
val input_signaspec : in_channel -> signaspec * hashval option * addr option * (string,stp * hashval) Hashtbl.t * (string,hashval) Hashtbl.t
val input_doc : in_channel -> doc * hashval option * addr option
    * (string,stp * hashval) Hashtbl.t * (string,hashval) Hashtbl.t * (string,hashval) Hashtbl.t
    * (hashval,payaddr) Hashtbl.t * (hashval,payaddr * (int64 option)) Hashtbl.t
    * (hashval,payaddr) Hashtbl.t * (hashval,payaddr * (int64 option)) Hashtbl.t
    * (hashval,int64 * (payaddr * int64) option) Hashtbl.t
