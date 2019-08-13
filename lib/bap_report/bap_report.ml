

module Std = struct
  include Bap_report_types
  module Docker = Bap_report_docker

  module Recipe = Bap_report_recipe
  type recipe = Recipe.t

  include Bap_report_size
  module Read = Bap_report_read
  module Template = Bap_report_template

end
