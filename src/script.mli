(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Json
open Sha256
open Hash
open Secp256k1
open Signat

val md160_bytelist : md160 -> int list
val md256_bytelist : md256 -> int list
val bytelist_string : int list -> string
val hash160_bytelist : int list -> p2shaddr
val sha256_bytelist : int list -> md256

val next_bytes : int -> int list -> int list * int list
val bytelist_to_pt : int list -> pt
val push_bytes : int list -> int list
val pop_bytes : int list -> int list * int list
val blnum_le : big_int -> int -> int list
val blnum_be : big_int -> int -> int list

val verify_p2sh : int64 option -> big_int -> p2shaddr -> int list -> bool * int64 option * int64 option

type gensignat =
  | P2pkhSignat of pt * bool * signat
  | P2shSignat of int list
  | EndP2pkhToP2pkhSignat of pt * bool * pt * bool * signat * signat
  | EndP2pkhToP2shSignat of pt * bool * md160 * signat * int list
  | EndP2shToP2pkhSignat of pt * bool * int list * signat
  | EndP2shToP2shSignat of md160 * int list * int list

val verify_gensignat : int64 option -> big_int -> gensignat -> addr -> bool * int64 option * int64 option

val seo_gensignat : (int -> int -> 'a -> 'a) -> gensignat -> 'a -> 'a
val sei_gensignat : (int -> 'a -> int * 'a) -> 'a -> gensignat * 'a

val json_gensignat : gensignat -> jsonval

val inum_be : int list -> big_int
val inum_le : int list -> big_int
