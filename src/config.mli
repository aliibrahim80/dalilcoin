(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

val datadir : string ref
val offline : bool ref
val ltcoffline : bool ref
val daemon : bool ref
val testnet : bool ref
val staking : bool ref
val ip : string option ref
val ipv6 : bool ref
val port : int ref
val onion : string option ref
val onionlocalport : int ref
val onionremoteport : int ref
val socks : int option ref
val socksport : int ref
val rpcuser : string ref
val rpcpass : string ref
val rpcport : int ref
val ltcrpcip : string ref
val ltcrpconion : string option ref
val ltcrpcport : int ref
val ltcrpcuser : string ref
val ltcrpcpass : string ref
val ltcnotifyport : int ref
val ltcaddresses : string list ref
val curl : string ref
val maxconns : int ref
val lastcheckpoint : string ref
val ltcblockcheckpoint : string ref
val prompt : string ref
val genesistimestamp : int64 ref
val maxburn : int64 ref
val maxburnrate : int64 ref
val ltctxfee : int64 ref
val mintimebetweenburns : int64 ref
val burnifleq : int ref
val seed : string ref
val randomseed : string option ref
val minconnstostake : int ref
val minrelayfee : int64 ref
val defaulttxfee : int64 ref
val extraindex : bool ref
val offlinestakerewardsdest : bool ref
val offlinestakerewardslock : string option ref
val generatenewrewardaddresses : bool ref
val stakewithrewards : bool ref
val reward_lock_relative : int64 option ref
val reward_lock_absolute : int64 option ref
val may2019hardforktime : int64 ref
