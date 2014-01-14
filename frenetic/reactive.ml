open PolicyGenerator

let () =
  match Array.to_list Sys.argv with
    | (_ :: [file]) ->
      let exp = NetKAT_Parser.program NetKAT_Lexer.token (Lexing.from_channel (open_in file)) in
      let fre p =
        let open LocalCompiler in
        (fun sw -> to_table (compile sw p)) in
      let main () = ignore (Async_Reactive.start_static fre 6633 exp) in
      Core.Std.never_returns (Async.Std.Scheduler.go_main ~max_num_open_file_descrs:1024 ~main ());
      exit 0
    | _ ->
      Format.printf "usage: reactive FILE\n";
      exit 1
