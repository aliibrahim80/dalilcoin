(* Copyright (c) 2017-2018 The Dalilcoin developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

open Ser
open Hash

val dbdir : string ref
val dbconfig : string -> unit

module type dbtype = functor (M:sig type t val basedir : string val seival : (seict -> t * seict) val seoval : (t -> seoct -> seoct) end) ->
  sig
    val dbinit : unit -> unit
    val dbget : hashval -> M.t
    val dbexists : hashval -> bool
    val dbput : hashval -> M.t -> unit
    val dbdelete : hashval -> unit
    val dbpurge : unit -> unit
  end

module type dbtypekeyiter = functor (M:sig type t val basedir : string val seival : (seict -> t * seict) val seoval : (t -> seoct -> seoct) end) ->
  sig
    val dbinit : unit -> unit
    val dbget : hashval -> M.t
    val dbexists : hashval -> bool
    val dbput : hashval -> M.t -> unit
    val dbdelete : hashval -> unit
    val dbpurge : unit -> unit
    val dbkeyiter : (hashval -> unit) -> unit
  end

module Dbbasic : dbtype
module Dbbasic2 : dbtype
module Dbbasic2keyiter : dbtypekeyiter

module DbBlacklist :
  sig
    val dbinit : unit -> unit
    val dbget : hashval -> bool
    val dbexists : hashval -> bool
    val dbput : hashval -> bool -> unit
    val dbdelete : hashval -> unit
    val dbpurge : unit -> unit
  end

module DbArchived :
  sig
    val dbinit : unit -> unit
    val dbget : hashval -> bool
    val dbexists : hashval -> bool
    val dbput : hashval -> bool -> unit
    val dbdelete : hashval -> unit
    val dbpurge : unit -> unit
  end
