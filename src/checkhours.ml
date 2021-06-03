(** Checking to make sure hours worked for each sprint is recorded *)

let _ = if Author.hours_worked_ms1 < 0 then exit 1

let _ = if Author.hours_worked_ms2 < 0 then exit 1

let _ = if Author.hours_worked_ms3 < 0 then exit 1
