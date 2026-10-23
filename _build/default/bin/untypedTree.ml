type utt =
  | Valuen of int
  | Valuef of float
  | Term of utt * utt
[@@deriving show]
