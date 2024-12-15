type journal_entry = {
  date : string;
  content : string;
}

(* open Yojson.Basic *)

let entry_to_json (entry : journal_entry) : Yojson.Basic.t =
  `Assoc [
    ("date", `String entry.date);
    ("content", `String entry.content)
  ]

let entry_of_json (json : Yojson.Basic.t) : journal_entry =
  match json with
  | `Assoc [("date", `String date); ("content", `String content)] ->
      { date; content }
  | _ -> failwith "Invalid JSON format"

let add_entry date content entries =
  let new_entry = { date; content } in
  new_entry :: entries

let view_entries entries =
  List.iter (fun entry ->
    Printf.printf "Date: %s\nContent: %s\n\n" entry.date entry.content
  ) entries

let read_entries file =
  try
    let json = Yojson.Basic.from_file file in
    match json with
    | `List entries_json -> List.map entry_of_json entries_json
    | _ -> []
  with
  | _ -> []

let write_entries file entries =
  let json = `List (List.map entry_to_json entries) in
  Yojson.Basic.to_file file json


let () =
  let file = "journal.json" in
  let entries = read_entries file in
  match Sys.argv with
  | [|_; "add"; date; content|] ->
      let updated_entries = add_entry date content entries in
      write_entries file updated_entries;
      print_endline "Entry added!"
  | [|_; "view"|] ->
      view_entries entries
  | _ ->
      print_endline "Usage: add <date> <content> | view"
