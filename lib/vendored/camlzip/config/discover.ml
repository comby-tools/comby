module C = Configurator.V1

let () =
C.main ~name:"zip" (fun c ->

let stale_gzip : C.Pkg_config.package_conf = {
 libs = [ "-lz" ];
 cflags = []
} in

let conf =
  match C.Pkg_config.get c with
  | None -> C.die "'pkg-config' missing"
  | Some pc ->
    match (C.Pkg_config.query pc ~package:"zlib") with
      | None -> stale_gzip
      | Some deps -> deps
  in

  C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
