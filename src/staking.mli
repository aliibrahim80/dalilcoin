(* Copyright (c) 2015-2017 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash
open Mathdata
open Assets

val extraburn : int64 ref

type nextstakeinfo = NextStake of (int64 * p2pkhaddr * hashval * int64 * obligation * int64 * int64 option * (hashval * hashval) option ref * hashval option * ttree option * hashval option * stree option) | NoStakeUpTo of int64;;

val nextstakechances : (hashval * hashval,nextstakeinfo) Hashtbl.t
val nextstakechances_hypo : (hashval * hashval,nextstakeinfo) Hashtbl.t

val compute_staking_chances : (hashval * hashval * hashval) -> int64 -> int64 -> unit

val stakingthread : unit -> unit


