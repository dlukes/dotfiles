require "rubygems"
# require "ori"
# require "bond"
# require "bond/completion"
require 'irb/completion'

require 'yaml'

def reset
      Dispatcher.reset_application!
end

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:USE_READLINE] = true
IRB.conf[:LOAD_MODULES] = []  unless IRB.conf.key?(:LOAD_MODULES)
# unless IRB.conf[:LOAD_MODULES].include?('irb/completion')
#       IRB.conf[:LOAD_MODULES] << 'irb/completion'
# end

require 'irb/ext/save-history'
IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"

def setzone
      TzTime.zone = TimeZone['Pacific Time (US & Canada)'] 
end
