#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let mirage = Conf.with_pkg ~default:false "mirage"
let cdhcpc = Conf.with_pkg ~default:false "unix"

let () =
  Pkg.describe "charrua-client" @@ fun c ->
  let mirage = Conf.value c mirage in
  let cdhcpc = Conf.value c cdhcpc in
  Ok [ Pkg.mllib "src/charrua-client.mllib";
       Pkg.test  "test/test_client";
       Pkg.bin   ~cond:cdhcpc "src/unix/cdhcpc";
       Pkg.mllib ~cond:mirage "src/mirage/charrua-client-mirage.mllib";
  ]
