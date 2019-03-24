(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Json
open Hash
open Secp256k1
open Signat

val hash160_bytelist : int list -> p2shaddr

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
