open Lwt.Infix

module RList = struct

  let list t =
    let open ReactiveData.RList in
    from_event
      (React.S.value t)
      (React.E.map (fun e -> Set e) (React.S.changes t))

end

module Model = struct

  type state =
    | Init
    | Loaded

  type t = {
    state : state;
    links : string list option;
  }

  let init = { state = Init; links = None; }

end

type rs = Model.t React.signal
type rf = ?step:React.step -> Model.t -> unit
type rp = rs * rf

module Action = struct

  type action =
    | Charged of string list
    | NOP

end

module View = struct

  open Model
  open Tyxml_js.Html5

  let view_links (r, f) =
    [
    Tyxml_js.R.Html5.pcdata (React.S.map (fun m ->
        match m.links with
        | None -> "loading"
        | Some links -> "potato") r)
    ]

  let view (r, f) =
    div ~a:[a_class ["test"]] @@
      view_links (r, f);
end

module Controller = struct

  open Action
  open Model

  let update a ((r, f) : rp) =
    let m = React.S.value r in
    let mo =
      match a with
      | Charged l -> { m with links = Some l }
      | _ -> assert false in
    f mo


  let fetch_links addr _ =
    let uri = Uri.of_string "http://schoolido.lu/api/cards" in
    Cohttp_lwt_xhr.Client.get uri >>= (fun (response, body) ->
    update (Charged [""]) addr; Lwt.return_unit)

end



let main _ =
  let element = Dom_html.getElementById "candy-app" in
  let rp = React.S.create Model.init in
  Dom.appendChild element (Tyxml_js.To_dom.of_div (View.view rp));
  Controller.fetch_links rp ()

let _ =
  Lwt_js_events.onload () >>= main
