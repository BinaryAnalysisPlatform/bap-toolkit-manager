open Core_kernel

open Bap_report_types


module View = Bap_report_view

module Style = struct

  type align =
    | Left
    | Right
    | Center
  [@@deriving sexp]

  type style =
    | Align of align
    | Bgcolor of string
    | Rowspan of int
    | Custom of string
    | Spaced_data

  type t = style list

  let bgcolor x = [Bgcolor x]
  let custom x = [Custom x]
  let align x = [Align x]
  let rowspan x = [Rowspan (max 1 x)]
  let merge x y = x @ y
  let spaced_data = [Spaced_data]
  let of_list x = List.concat x

  let to_string x =
    List.fold x ~init:""
      ~f:(fun str s ->
          let s =
            match s with
            | Align a -> sprintf "align=\"%s\"" (Sexp.to_string (sexp_of_align a))
            | Bgcolor c -> sprintf "bgcolor=\"%s\"" c
            | Rowspan n -> sprintf "rowspan=\"%d\"" n
            | Custom x -> x
            | Spaced_data -> "" in
          sprintf "%s%s " str s)

  let get_rowspan (t : t) =
    List.find_map t
      ~f:(function
          | Rowspan x -> Some x
          | _ -> None)
end

module Href = struct

  type href_kind =
    | Local
    | External

  type t = href_kind * string

  let render (kind,link) = match kind with
    | Local -> sprintf "#%s" link
    | External -> link

  let create_local x = Local,x
  let create_external x = External,x
end

module Entry = struct
  type t = string * Style.t * Href.t option * string

  let create ?(style=[]) ?href ~tag data : t = tag,style,href,data

  let content (_,_,_,c) = c

  let style ((_,s,_,_) : t) = s
  let set_style ((a,_,c,d) : t) b = a,b,c,d

  let render (tag,style,href,data) =
    let data =
      if String.mem data '\n' then
        sprintf "<pre>%s</pre>" data
      else data in
    let data =
      if List.exists style ~f:(fun x -> x = Style.Spaced_data) then
        sprintf "&nbsp%s&nbsp" data
      else data in
    let style = Style.to_string style in
    match href with
    | None ->
      sprintf "<%s %s>%s</%s>" tag style data tag
    | Some href ->
      sprintf "<%s %s><a href=\"%s\">%s</a></%s>"
        tag style (Href.render href) data tag
end

module Tab = struct

  type t = {
    col : int;
    doc : Entry.t list;
    style : Style.t option;
  }

  let init_from_rows ?style () = {col = 0; doc = []; style}

  let mk_header h =
    Entry.create ~tag:"th" ~style:Style.(align Center) h

  let with_headers ?style hdrs =
    let col = List.length hdrs in
    let doc = List.rev_map hdrs ~f:mk_header in
    {doc;col;style;}

  let create ?style col =
    {doc=[];col; style;}

  let add_cell ?href ?style word t =
    {t with doc = Entry.create ?style ?href ~tag:"td" word :: t.doc}

  let add_row ?cell_style:style words t =
    let t = if t.col = 0 then {t with col = List.length words} else t in
    List.fold words ~init:t ~f:(fun t w -> add_cell ?style w t)

  let is_aligned e =
    List.exists (Entry.style e)
      ~f:(function
          | Style.Align _ -> true
          | _ -> false)

  let default_cell_align cols col =
    if col = cols - 1 then Style.(align Right)
    else Style.(align Left)

  let cell_align cols col e =
    if is_aligned e || cols = 1 then e
    else
      let s = Style.merge (Entry.style e) (default_cell_align cols col) in
      Entry.set_style e s

  let get t =
    let doc = List.rev t.doc in
    let style = match t.style with
      | None -> ""
      | Some s -> Style.to_string s in
    let tab = sprintf "<table %s>" style in
    let tab,_,_,_ =
      List.fold doc ~init:([tab],0,t.col,None)
        ~f:(fun (doc,pos,cols,row_in_rspan) e ->
            let pos' = pos + 1 in
            let cols, row_in_rspan =
              match Style.get_rowspan (Entry.style e) with
              | None -> cols, row_in_rspan
              | Some 0 | Some 1 -> cols, None
              | Some _ -> t.col - 1 , Some 0 in
            let max = match row_in_rspan with
              | None | Some 0 -> t.col
              | Some _  -> cols in
            let row_in_rspan' = match row_in_rspan with
              | Some n when pos' = max -> Some (n + 1)
              | x -> x in
            let start  = if pos = 0 then "<tr>" else "" in
            let finish = if pos' = max then "</tr>" else "" in
            let e = cell_align t.col pos e in
            let pos = if pos' = max then 0 else pos' in
            let e = Entry.render e in
            finish :: e :: start :: doc, pos, cols, row_in_rspan') in
    let tab = List.rev ("</table>" :: tab) in
    String.concat tab

  let spaces n = String.concat (List.init n ~f:(fun _ -> "&nbsp"))

end

let color_of_status = function
  | Confirmed -> "#D4EFE2"
  | False_neg -> "#F7D3CE"
  | False_pos -> "#ADD8E6"
  | Undecided -> "#F2F2F2"

let legend =
  let open Tab in
  let style s = Style.bgcolor (color_of_status s) in
  let datas = Style.(of_list [bgcolor "f2f2f2"; align Left]) in
  Tab.create 2 |>
  add_cell ~style:(style Confirmed) (spaces 5) |>
  add_cell ~style:datas "confirmed" |>
  add_cell ~style:(style False_neg) "" |>
  add_cell ~style:datas "false negative" |>
  add_cell ~style:(style False_pos) "" |>
  add_cell ~style:datas "false positive" |>
  add_cell ~style:(style Undecided) "" |>
  add_cell ~style:datas "undecided" |>
  get

let html_header =  {|
<!DOCTYPE html>
<html>
  <head>
  <style>

   table, th, td {
     border: 1px solid black;
     border-collapse: collapse;
   }

   table#top tr:nth-child(even) {
     background-color: #f2f2f2;
   }

   table#data tr {
     background-color: #F2F2F2;
   }

   table#data td {
     border: 0;
   }

   start-line {
     width: 100%;
     display: table;
     border: 1px solid #444444;
   }

   .line-elt {
     display: table-cell;
   }

  </style>
  </head>
  <body>
|}

let html_bottom = {|
  </body>
</html>
|}



let lineup_elements elts =
  let space_elt = {|<div class="line-elt">&nbsp&nbsp</div>|} in
  let s =
    List.fold elts ~init:{|<div class="start-line">|}
      ~f:(fun s e ->
          s ^ {|<div class="line-elt">|} ^ e ^ "</div>" ^ space_elt) in
  s ^ "</div>"


let description_of_check view inc =
  let link = Option.value ~default:"" (View.web view inc) in
  Href.create_external link

let string_of_check view inc = View.name view inc

let render_as_text data = match data with
  | [] -> ""
  | data ->
    let text = List.map data ~f:(fun (x,_) -> String.concat x) in
    let text = String.concat text ~sep:"\n" in
    sprintf "<pre>\n%s\n</pre>" text

let compare_strings (data,_) (data',_) =
  let rec compare xs ys =
    match xs, ys with
    | [],[] -> 0
    | x :: xs, y :: ys ->
      let r = String.compare x y in
      if r = 0 then compare xs ys
      else r
    | _ -> assert false in
  compare data data'

let is_tableable = function
  | [] -> false
  | (x,_) :: xs ->
    let len = List.length x in
    List.for_all xs ~f:(fun (y,_) -> List.length y = len)

let lines_number data =
  let len = List.length data in
  let cols = match len with
    | n when n < 10 -> 1
    | n when n < 30 -> 2
    | n when n < 60 -> 3
    | _ -> 4 in
  let rows = len / cols  in
  if len - rows * cols = 1 then len / (cols + 1)
  else rows

let render_data view data =
  let add_row (ws,status) tab =
    let style = Style.(of_list [spaced_data; bgcolor (color_of_status status)]) in
    match ws with
    | fst :: others ->
      let tab = Tab.add_cell ~style fst tab in
      List.fold others ~init:tab ~f:(fun tab w ->
          Tab.add_cell ~style w tab);
    | _ -> tab in
  let data = List.map data ~f:(fun (i,s) -> View.data view i, s) in
  match data with
  | [] -> ""
  | ((fst,_) :: _) as data  ->
    let cols, data =
      if not (is_tableable data) then
        let data = List.map data ~f:(fun (x,s) ->
            [String.concat ~sep:" " x], s) in
        1, data
      else List.length fst, data in
    let cols = List.length fst in
    let data = List.sort data ~compare:compare_strings in
    let empty () = Tab.create ~style:(Style.custom "id=\"data\"") cols in
    let rows = lines_number data in
    let tabs, last, _ =
      List.fold data ~init:([],empty (),0) ~f:(fun (acc,tab,i) ws ->
          let tab = add_row ws tab in
          if i + 1 < rows then acc, tab, i + 1
          else tab :: acc, empty (), 0) in
    let tabs = List.rev_map (last :: tabs) ~f:Tab.get in
    lineup_elements tabs


let render_checkname view arti kind =
  let id = sprintf "%s+%s" arti (Incident.Kind.to_string kind) in
  sprintf "<b id=\"%s\">%s</b>" id (string_of_check view kind)

let total_of_stat s = s.confirmed + s.false_pos + s.false_neg + s.undecided

let render_stat = function
  | None -> ""
  | Some s ->
    let total = total_of_stat s in
    if total = 0 then ""
    else
      sprintf
        "<pre>Total/Confirmed/False positive/False negative/Unclassified: %d/%d/%d/%d/%d</pre>"
        total s.confirmed s.false_pos s.false_neg s.undecided

let render_time = function
  | None -> ""
  | Some s ->
    sprintf "<pre>Time: %s</pre>" s

let no_incidents = "<pre>no incidents found</pre>"

let render_check view artifact kind =
  let name = Artifact.name artifact in
  let time = Artifact.time_hum artifact kind in
  match Artifact.incidents ~kind artifact with
  | [] ->
    String.concat [
      render_checkname view name kind;
      render_time time;
      no_incidents;
      "</br>";
    ]
  | data ->
    let stat = Artifact.summary artifact kind in
    String.concat [
      render_checkname view name kind;
      render_time time;
      render_stat (Some stat);
      render_data view data;
      "</br>";
    ]

let ref_to_top = {|<p><a href="#top">Top</a></p>|}


let arti_size arti =
  Option.value ~default:"unknown" (Artifact.size_hum arti)

let render_artifact view tab artifact =
  let name = Artifact.name artifact in
  let size = arti_size artifact in
  let kinds = Artifact.checks artifact in
  let arti =
    sprintf "<pre><h3>%s</h3>size: %s</pre>" name size in
  let tab = Tab.add_cell arti tab in
  let cell = [
    "</br>";
    legend;
    ref_to_top;
    sprintf "<p id=\"%s\"> </p>" name;
  ] in
  let cell = match kinds with
    | [] -> [no_incidents]
    | kinds ->
      List.fold kinds ~init:cell ~f:(fun cell kind ->
          render_check view artifact kind :: cell) in
  let cell = String.concat (List.rev cell) in
  Tab.add_cell ~style:Style.(align Left) cell tab

let render_artifacts view artis =
  let style = Style.custom
      "id=\"Results\" style=\"width:100%\" frame=void rules=rows" in
  let init = Tab.create ~style 2 in
  Tab.get @@ List.fold artis ~init ~f:(render_artifact view)

let render_summary view artifacts =
  let cell_style = Style.(align Center) in
  let digit = string_of_int in
  let hdr =
    Tab.with_headers
      ~style:(Style.custom "id=\"top\" style=\"width:50%\"")
      ["artifact"; "check"; "Total"; "Confirmed";
       "False positive"; "False negative"; "Unclassified";
       "Time\nhh:mm:ss"] in
  let arti_style = Style.bgcolor "white" in
  let tab =
    List.fold artifacts ~init:hdr ~f:(fun tab arti ->
        let checks = Artifact.checks arti in
        let style = Style.merge arti_style (Style.rowspan @@ List.length checks) in
        let name = Artifact.name arti in
        let info =
          sprintf "<pre><a href=\"#%s\">%s</a>\nsize: %s</pre>"
            name name (arti_size arti) in
        let tab = Tab.add_cell ~style info tab in
        match checks with
        | [] ->
          let xs = List.init 7 ~f:(fun _ -> "-") in
          List.fold xs ~init:tab ~f:(fun tab x ->
              Tab.add_cell ~style:cell_style x tab)
        | checks ->
          List.fold checks ~init:tab
            ~f:(fun tab check ->
                let res = Artifact.summary arti check in
                let time = Artifact.time_hum arti check in
                let time = Option.value ~default:"-" time in
                Tab.add_cell ~href:(description_of_check view check)
                  (string_of_check view check) tab |>
                Tab.add_cell ~style:cell_style (digit (total_of_stat res)) |>
                Tab.add_cell ~style:cell_style (digit res.confirmed) |>
                Tab.add_cell ~style:cell_style (digit res.false_pos) |>
                Tab.add_cell ~style:cell_style (digit res.false_neg) |>
                Tab.add_cell ~style:cell_style (digit res.undecided) |>
                Tab.add_cell ~style:cell_style time)) in
  String.concat [Tab.get tab; "</br>"]

let render view artifacts =
  let artifacts = List.rev artifacts in
  let summary   = render_summary view artifacts in
  let artifacts = render_artifacts view artifacts in
  String.concat [html_header; summary; artifacts; html_bottom;]
