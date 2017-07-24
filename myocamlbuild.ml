open Ocamlbuild_plugin

let () =
  flag [ "js_of_ocaml"; "sourcemap_inline" ] (A "--source-map-inline");
  dispatch Ocamlbuild_js_of_ocaml.dispatcher
