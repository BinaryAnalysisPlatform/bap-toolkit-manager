

module Std = struct
  include Bap_report_types
  module Docker = Bap_report_docker

  type image = Docker.Image.t

  module Limit  = Bap_report_limit
  module Recipe = Bap_report_recipe
  module Job  = Bap_report_job

  type recipe = Recipe.t
  type limit  = Limit.t

  module Size = Bap_report_size
  module Read = Bap_report_read
  module View = Bap_report_view

  module Template = Bap_report_template

end
