open PolicyGenerator

let controller start file =
  let pol = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel (open_in file)) in
   let f p =
     let open LocalCompiler in
     (fun sw -> to_table (compile sw p)) in
   let main () = ignore (start ~f ~port:6633 ~pol) in
   Core.Std.never_returns (Async.Std.Scheduler.go_main ~max_num_open_file_descrs:1024 ~main ())

let () =
  match Array.to_list Sys.argv with
    | (_ :: [file])
    | (_ :: "ff" :: [file]) -> controller Async_Reactive.start_static file
    | (_ :: "lb" :: [file]) -> controller Async_Loadbalancer.start_static file
    | _ ->
      Format.printf "usage: reactive [ff|lb] FILE\n";
      exit 1
