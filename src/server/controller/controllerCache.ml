open NesUnix

module Log = (val Logger.create "controller.cache": Logs.LOG)

let populate ~cache ~type_ ~ext ~pp_ext =
  Log.info (fun m -> m "Populating the %s %s cache" type_ pp_ext);
  let path = Filename.concat !Config.cache type_ in
  let files = Lwt_unix.files_of_directory path in
  Lwt_stream.iter
    (fun x ->
      if Filename.check_suffix x ext then
        try
          Log.debug (fun m -> m "Found %s file %s" pp_ext x);
          let base = Filename.chop_suffix x ext in
          let hash =
            String.split_on_char '-' base
            |> List.ft
            |> StorageCache.hash_from_string
          in
          StorageCache.add ~cache ~hash ~value: (Lwt.return (Filename.concat path x))
        with
          | exn ->
            Log.err (fun m ->
              m
                "%a"
                (Format.pp_multiline_sensible ("Could not determine hash from file `" ^ x ^ "`"))
                ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()))
            );
            exit 7
    )
    files
