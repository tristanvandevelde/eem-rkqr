### works only on Unix-family operations systems

### install development packages required to build Python

## debian family inc. Ubuntu, Mint, PopOS
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev curl

### install pyenv

pyenv install 3.9.5
pyenv local 3.9.5

### you can change this path as you wish, it's the default one for Linux
INSTALL_PATH="$HOME/.local/share/virtualenvs/thesis"

python -m venv $INSTALL_PATH
source $INSTALL_PATH/bin/activate

python -m pip install --upgrade pip
python -m pip install --upgrade setuptools
python -m pip install -r requirements.txt

