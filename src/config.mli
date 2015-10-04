(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

val datadir : string ref
val ctreedatadir : string ref
val chaindatadir : string ref
val staking : bool ref
val ip : string option ref
val port : int ref
val rpcport : int ref
val socks : int option ref
val socksport : int ref