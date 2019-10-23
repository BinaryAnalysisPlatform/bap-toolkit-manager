

module Std = struct
  include Bap_report_types
  module Docker = Bap_report_docker

  type image = Docker.Image.t

  module Limit  = Bap_report_limit
  module Recipe = Bap_report_recipe
  module Job  = Bap_report_job
  module Path = Bap_report_path
  module Tool = Bap_report_tool

  type recipe = Recipe.t
  type limit  = Limit.t
  type path  = Path.t
  type tool = Tool.t

  module Read = Bap_report_read
  module View = Bap_report_view

  module Template = Bap_report_template

end
