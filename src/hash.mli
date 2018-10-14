(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int
open Json
open Sha256

type md160 = int32 * int32 * int32 * int32 * int32
type hashval = md256
type addr = int * int32 * int32 * int32 * int32 * int32
type p2pkhaddr = int32 * int32 * int32 * int32 * int32
type p2shaddr = int32 * int32 * int32 * int32 * int32
type payaddr = bool * int32 * int32 * int32 * int32 * int32
type termaddr = int32 * int32 * int32 * int32 * int32
type pubaddr = int32 * int32 * int32 * int32 * int32

val hash160 : string -> md160

val p2pkhaddr_payaddr : p2pkhaddr -> payaddr
val p2shaddr_payaddr : p2shaddr -> payaddr

val p2pkhaddr_addr : p2pkhaddr -> addr
val p2shaddr_addr : p2shaddr -> addr
val payaddr_addr : payaddr -> addr
val termaddr_addr : termaddr -> addr
val pubaddr_addr : pubaddr -> addr

val payaddr_p : addr -> bool
val p2pkhaddr_p : addr -> bool
val p2shaddr_p : addr -> bool
val termaddr_p : addr -> bool
val pubaddr_p : addr -> bool

val md160_bitseq : md160 -> bool list
val hashval_bitseq : hashval -> bool list
val bitseq_hashval : bool list -> hashval
val hashval_md160 : hashval -> md160
val hashval_p2pkh_payaddr : hashval -> payaddr
val hashval_p2sh_payaddr : hashval -> payaddr
val md160_p2pkh_addr : md160 -> addr
val hashval_p2pkh_addr : hashval -> addr
val md160_p2sh_addr : md160 -> addr
val hashval_p2sh_addr : hashval -> addr
val hashval_term_addr : hashval -> addr
val hashval_pub_addr : hashval -> addr

val addr_bitseq : addr -> bool list
val bitseq_addr : bool list -> addr

val hashval_hexstring : hashval -> string
val hexstring_hashval : string -> hashval
val printhashval : hashval -> unit
val hashint32 : int32 -> hashval
val hashint64 : int64 -> hashval
val hashaddr : addr -> hashval
val hashpayaddr : payaddr -> hashval
val hashtermaddr : termaddr -> hashval
val hashpubaddr : pubaddr -> hashval
val hashpair : hashval -> hashval -> hashval
val hashpubkey : md256 -> md256 -> hashval
val hashpubkeyc : int -> md256 -> hashval
val hashtag : hashval -> int32 -> hashval
val hashlist : hashval list -> hashval
val hashfold : ('a -> hashval) -> 'a list -> hashval
val ohashlist : hashval list -> hashval option
val hashopair : hashval option -> hashval option -> hashval option
val hashopair1 : hashval -> hashval option -> hashval
val hashopair2 : hashval option -> hashval -> hashval
val hashbitseq : bool list -> hashval

val hashval_big_int : hashval -> big_int
val big_int_hashval : big_int -> hashval

val seo_md160 : (int -> int -> 'a -> 'a) -> md160 -> 'a -> 'a
val sei_md160 : (int -> 'a -> int * 'a) -> 'a -> md160 * 'a
val seo_hashval : (int -> int -> 'a -> 'a) -> hashval -> 'a -> 'a
val sei_hashval : (int -> 'a -> int * 'a) -> 'a -> hashval * 'a
val seo_addr : (int -> int -> 'a -> 'a) -> addr -> 'a -> 'a
val sei_addr : (int -> 'a -> int * 'a) -> 'a -> addr * 'a
val seo_payaddr : (int -> int -> 'a -> 'a) -> payaddr -> 'a -> 'a
val sei_payaddr : (int -> 'a -> int * 'a) -> 'a -> payaddr * 'a
val seo_termaddr : (int -> int -> 'a -> 'a) -> termaddr -> 'a -> 'a
val sei_termaddr : (int -> 'a -> int * 'a) -> 'a -> termaddr * 'a
val seo_pubaddr : (int -> int -> 'a -> 'a) -> pubaddr -> 'a -> 'a
val sei_pubaddr : (int -> 'a -> int * 'a) -> 'a -> pubaddr * 'a

val merkle_root : hashval list -> hashval option

val hashval_rev : hashval -> hashval

val hashval_from_json : jsonval -> hashval
