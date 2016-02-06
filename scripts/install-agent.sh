#!/bin/sh
#
# Usage:
# sh install-agent.sh [FcombToken]
#

if [ -f "/etc/fcomb/agent/fcomb-agent.conf" ]; then
	echo "ERROR: Fcomb node agent is already installed in this host"
    echo "If the node failed to register properly with Fcomb, try to restart the agent by executing:"
	echo "    service fcomb-agent restart"
	exit 1
fi

if [ "$(uname -m)" != "x86_64" ]; then
	echo ERROR: Only x86_64 architectures are supported
	exit 1
fi

echo "-> Installing required dependencies..."
modprobe -q aufs || apt-get update -qq && apt-get install -yq linux-image-extra-$(uname -r) || \
    echo "!! Failed to install linux-image-extra package. AUFS support (which is recommended) may not be available."

# all of these must be included in the released distribution
echo "-> Installing fcomb-agent..."
cp upstart/fcomb-agent.conf /etc/init/
cp sysvinit/fcomb-agent /etc/init.d/
cp systemd/fcomb-agent.socket /lib/systemd/system/
cp systemd/fcomb-agent.service /lib/systemd/system/
cp logrotate/fcomb-agent /etc/logrotate.d/
cp build/fcomb-node-agent /usr/bin/fcomb-agent

DOCKER_UPSTART_CONF="/etc/init/docker.conf"
if [ -f "${DOCKER_UPSTART_CONF}" ]; then
  echo "Removing conflicting docker upstart configuration file at ${DOCKER_UPSTART_CONF}..."
  rm -f ${DOCKER_UPSTART_CONF}
fi

if ! getent group docker > /dev/null; then
  groupadd --system docker
fi

if [ -d /run/systemd/system ] ; then
	echo "-> Enabling fcomb-agent to start on boot on systemd..."
	systemctl enable fcomb-agent.service || true
fi

if [ ! -z "${1}" ]; then
	echo "-> Installing fcomb-agent service..."
    /usr/bin/fcomb-agent install ${1}
	service fcomb-agent stop > /dev/null 2>&1 || true
	service fcomb-agent start
fi

echo "-> Done!"
