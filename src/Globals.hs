module Globals (
    module Globals
) where

dockerBinaryURL       = "https://get.docker.com/builds/Linux/x86_64/docker-latest"
defaultFcombHost      = ""
regEndpoint           = "/v1/agent/nodes/join"

agentVersion          = "0.1.0"
defaultDockerHost     = "tcp://0.0.0.0:2375"
defaultDockerSocket   = "unix:///var/run/docker.sock"

fcombHome = "/etc/fcomb/agent/"
dockerDir = "/usr/lib/fcomb/"

fcombPidFile           = "/var/run/fcomb-agent.pid"
dockerSymbolicLink     = "/usr/bin/docker"

keyFileName            = "key.pem"
certFileName           = "cert.pem"
caFileName             = "ca.pem"
configFileName         = "fcomb-agent.conf"
dockerBinaryName       = "docker"
dockerNewBinaryName    = "docker.new"

dockerBinPath = dockerDir ++ dockerBinaryName
dockerNewBinPath =  dockerDir ++ dockerNewBinaryName
configFilePath = fcombHome ++ configFileName
keyFilePath = fcombHome ++ keyFileName
certFilePath = fcombHome ++ certFileName
caFilePath = fcombHome ++ caFileName

heartBeatInterval = 60 * 60 * 1000 * 1000  --microseconds