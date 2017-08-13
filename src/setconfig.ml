(* Copyright (c) 2015 The Qeditas developers *)
(* Distributed under the MIT software license, see the accompanying
   file COPYING or http://www.opensource.org/licenses/mit-license.php. *)

let stringconfigvars = [
("seed",fun x -> Config.seed := x);
("lastcheckpoint",fun x -> Config.lastcheckpoint := x);
("prompt",fun x -> Config.prompt := x);
("rpcuser",fun x -> Config.rpcuser := x);
("rpcpass",fun x -> Config.rpcpass := x)
];;
let boolconfigvars = [
("daemon",fun x -> Config.daemon := x);
("staking",fun x -> Config.staking := x);
("ipv6",fun x -> Config.ipv6 := x)
];;
let intconfigvars = [
("port",fun x -> Config.port := x);
("socksport",fun x -> Config.socksport := x);
("rpcport",fun x -> Config.rpcport := x);
("maxconns",fun x -> Config.maxconns := x);
("burnifleq",fun x -> Config.burnifleq := x);
("minconnstostake",fun x -> Config.minconnstostake := x)
];;
let int64configvars = [
("genesistime",fun x -> Config.genesistimestamp := x);
("maxburn",fun x -> Config.maxburn := x);
("maxburnrate",fun x -> Config.maxburnrate := x);
("ltctxfee",fun x -> Config.ltctxfee := x);
("mintimebetweenburns",fun x -> Config.mintimebetweenburns := x)
];;
let stringoptionconfigvars = [
("ip",fun x -> Config.ip := x);
("randomseed",fun x -> Config.randomseed := x);
("checkpointskey",fun x -> Config.checkpointskey := x);
];;
let intoptionconfigvars = [
("socks",fun x -> Config.socks := x)
];;

exception Done

let setl = ref []

let process_config_line l =
  let ll = String.length l in
  begin
    try
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll >= 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (String.sub l (vl+1) (ll-(vl+1)));
	      raise Done
	    end
	  )
	stringconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if l = v then
	    begin
	      setl := v::!setl;
	      r true;
	      raise Done
	    end
	  else if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    let s = String.sub l (vl+1) (ll-(vl+1)) in
	    begin
	      setl := v::!setl;
	      r (s = "1" || s = "t" || s = "true");
	      raise Done
	    end
	  )
	boolconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (int_of_string (String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	intconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (Int64.of_string (String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	int64configvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (Some(String.sub l (vl+1) (ll-(vl+1))));
	      raise Done
	    end
	  )
	stringoptionconfigvars;
      List.iter
	(fun (v,r) ->
	  let vl = String.length v in
	  if ll > 1 + vl && String.sub l 0 (vl) = v && l.[vl] = '=' then
	    begin
	      setl := v::!setl;
	      r (Some(int_of_string (String.sub l (vl+1) (ll-(vl+1)))));
	      raise Done
	    end
	  )
	intoptionconfigvars;
      raise Not_found
    with Done -> ()
  end

let datadir () = if !Config.testnet then (Filename.concat !Config.datadir "testnet") else !Config.datadir

let process_config_file () =
  let fn = Filename.concat (datadir()) "qeditas.conf" in
  if Sys.file_exists fn then
    begin
      let ch = open_in fn in
      try
	while true do
	  let l = input_line ch in
	  try
	    if String.length l > 0 && not (l.[0] = '%') then
	      process_config_line l
	  with Not_found ->
	    Printf.printf "Do not understand %s in qeditas.conf; skipping\n" l
	done
      with End_of_file -> ()
    end
  else
    Printf.printf "No qeditas.conf file found. Using default configuration.\n";;

let datadir_from_command_line () =
  let a = Array.length Sys.argv in
  for i = 1 to a-1 do
    let arg = Sys.argv.(i) in
    try
      if String.length arg > 9 && String.sub arg 0 9 = "-datadir=" then
	Config.datadir := String.sub arg 9 (String.length arg - 9);
      if arg = "-testnet" || arg = "-testnet=1" then (*** if testnet, then change some default values ***)
        begin
          Config.testnet := true;
          if not (List.mem "port" !setl) then Config.port := 20804;
          if not (List.mem "seed" !setl) then Config.seed := "d3c195c68d64de93825bc1f0dd396286bf3108bebe73619223c8ae76883c0670"; (*** litecoin block 1,236,680 ***)
        end
    with Not_found -> ()
  done;;

let process_config_args () =
  let a = Array.length Sys.argv in
  for i = 1 to a-1 do
    let arg = Sys.argv.(i) in
    if String.length arg > 1 && arg.[0] = '-' then
      try
	process_config_line (String.sub arg 1 ((String.length arg) - 1))
      with Not_found -> ()
  done;;
