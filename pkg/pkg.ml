#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let mirage = Conf.with_pkg ~default:false "mirage"
let lwt    = Conf.with_pkg ~default:false "lwt"
let cdhcpc = Conf.with_pkg ~default:false "unix"

let () =
  Pkg.describe "charrua-client" @@ fun c ->
  let check = Conf.value c in
  let mirage = check mirage in
  let cdhcpc = check cdhcpc in
  let lwt = check lwt in
  Ok [ Pkg.mllib "src/charrua-client.mllib";
       Pkg.test  "test/test_client";
       Pkg.test  ~cond:lwt "test/test_client_lwt";
       Pkg.bin   ~cond:cdhcpc "src/unix/cdhcpc";
       Pkg.mllib ~cond:mirage "src/mirage/charrua-client-mirage.mllib";
  ]
