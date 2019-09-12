

module Std = struct
  include Bap_report_types
  module Docker = Bap_report_docker

  type image = Docker.Image.t

  module Recipe = Bap_report_recipe
  module Job  = Recipe.Job

  type recipe = Recipe.t

  module Size = Bap_report_size
  module Read = Bap_report_read
  module View = Bap_report_view

  type view = View.t

  module Template = Bap_report_template

end
