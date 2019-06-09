(* Copyright (c) 2016 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

val exitfn : (int -> unit) ref

val log : out_channel ref
val log_string : string -> unit
val openlog : unit -> unit
val closelog : unit -> unit

val era : int64 -> int
val maxblockdeltasize : int64 -> int

val random_initialized : bool ref
val initialize_random_seed : unit -> unit
val rand_bit : unit -> bool
val rand_int32 : unit -> int32
val rand_int64 : unit -> int64

val may2019hardforktime : int64 ref
val july2019hardforktime : int64 ref
