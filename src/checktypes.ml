module type AuthorSig = sig
  val hours_worked_ms1 : int
  val hours_worked_ms2 : int
  val hours_worked_ms3 : int
end

module AuthorCheck : AuthorSig = Author
