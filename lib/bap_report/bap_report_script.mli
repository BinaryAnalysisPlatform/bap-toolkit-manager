

type recipe = Bap_report_recipe.t
type limit  = Bap_report_limit.t

val create :
  ?limit:limit ->
  ?verbose:bool ->
  ?watch:bool ->
  workdir:string ->
  path:string ->
  recipe -> string
