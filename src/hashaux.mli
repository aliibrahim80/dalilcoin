(* Copyright (c) 2015 The Qeditas developers *)
(* Copyright (c) 2017-2019 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Big_int

val string_hexstring : string -> string
val hexstring_string : string -> string
val string_bytelist : string -> int list
val hexsubstring_int32 : string -> int -> int32
val int32_hexstring : Buffer.t -> int32 -> unit
val big_int_sub_int32 : big_int -> int -> int32
val int32_big_int_bits : int32 -> int -> big_int
val int32_rev : int32 -> int32
val hexstring_of_big_int : big_int -> int -> string
val big_int_of_hexstring : string -> big_int
