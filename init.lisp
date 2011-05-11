(require 'swank)
(swank:create-server :port 4004)
(cfy.downloads:fcgi-main)
