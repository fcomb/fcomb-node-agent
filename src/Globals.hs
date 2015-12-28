module Globals (
    module Globals
) where

scheduleToTerminateDocker = False
scheduledShutdown         = False
dockerBinaryURL           = "https://get.docker.com/builds/Linux/x86_64/docker-latest"

agentVersion          = "0.1.0"
defaultCertCommonName = ""
defaultDockerHost     = "tcp://0.0.0.0:2375"
defaultFcombHost      = "https://api.fcomb.io"

fcombHome = "/etc/fcomb/agent"
dockerDir = "/usr/lib/fcomb"
logDir    = "/var/log/fcomb"

dockerSymbolicLink     = "/usr/bin/docker"
keyFileName            = "key.pem"
certFileName           = "cert.pem"
cAFileName             = "ca.pem"
configFileName         = "fcomb-agent.conf"
dockerBinaryName       = "docker"
dockerNewBinaryName    = "docker.new"
dockerNewBinarySigName = "docker.new.sig"
fcombPidFile           = "/var/run/fcomb-agent.pid"

regEndpoint       = "/v1/agent/nodes/join"
defaultDockerSocket = "unix:///var/run/docker.sock"

maxWaitingTime    = 200 --seconds
heartBeatInterval = 5 * 1000 * 1000  --microseconds

dockerHostPort = "2375"