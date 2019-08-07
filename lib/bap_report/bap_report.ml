

module Std = struct
  include Bap_report_types
  module Docker = Bap_report_docker
  module Recipe = Bap_report_recipe
  include Bap_report_size
  include Bap_report_incidents
  module Template = Bap_report_template
end
