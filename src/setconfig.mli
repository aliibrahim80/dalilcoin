(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Hash

val datadir_from_command_line : unit -> unit
val process_config_file : unit -> unit
val process_config_args : unit -> unit

val recent_ltc_blocks : string option ref

val createsnapshot : bool ref
val importsnapshot : bool ref
val snapshot_dir : string option ref
val snapshot_headers : hashval list ref
val snapshot_blocks : hashval list ref
val snapshot_ledgerroots : hashval list ref
val snapshot_full : bool ref
val snapshot_addresses : addr list ref
val snapshot_shards : int list option ref
val check_ledger : hashval option ref
val build_extraindex : hashval option ref
val netlogreport : string list option ref
