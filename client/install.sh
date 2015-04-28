set -o verbose
pip install --user paramiko
sudo curl https://raw.githubusercontent.com/pkamenarsky/usercfg/master/client/usercfg > /usr/local/bin/usercfg
sudo chmod +x /usr/local/bin/usercfg
