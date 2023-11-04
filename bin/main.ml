open Lwt.Infix

let clients : ((string * bool), Dream.websocket) Hashtbl.t =
  Hashtbl.create 5

let join_regex = Str.regexp {|\* \([A-Za-z\-]+\)_\([0-9]\)|}

let track client =
  let rec loop () =
    match%lwt Dream.receive client with
    | Some message ->
      if Str.string_match join_regex message 0 then
        let client_id = (Str.matched_group 1 message, (Str.matched_group 2 message |> int_of_string) == 0) in
        Hashtbl.add clients client_id client;
        Lwt.return_some client_id
      else loop ()
    | None ->
      Dream.close_websocket client
      >>= fun () -> Lwt.return_none
  in
  loop ()

let forget client_id =
  Hashtbl.remove clients client_id

let send client_id message =
  let (room_id, player) = client_id in
  match Hashtbl.find_opt clients (room_id, not player) with
  | Some client -> Dream.send client message
  | None -> Lwt.return_unit


let handle_client client =
  match%lwt track client with
  | Some client_id ->
    let rec loop () =
      match%lwt Dream.receive client with
      | Some message ->
        let%lwt () = send client_id message in
        loop ()
      | None ->
        forget client_id;
        Dream.close_websocket client
    in
    loop ()
  | None -> Lwt.return_unit

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ -> Dream.websocket handle_client);

  ]
