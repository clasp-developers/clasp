#! /bin/bash
# comment
sudo apt-get update
sudo DEBIAN_FRONTEND=noninteractive apt-get -y -o DPkg::options::="--force-confdef" -o DPkg::options::="--force-confold" upgrade
sudo apt -y install tmux emacs
sudo curl http://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
sudo sh -c 'echo -c "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch-6.0 main" >/etc/apt/sources.list.d/llvm-nightly.list'
sudo apt-get update
sudo apt-get install -y \
  cmake libgc-dev libunwind-dev liblzma-dev libgmp-dev binutils-gold binutils-dev \
  zlib1g-dev libncurses-dev libboost-filesystem-dev libboost-regex-dev \
  libboost-date-time-dev libboost-program-options-dev libboost-system-dev \
  libboost-iostreams-dev csh flex gfortran zlib1g-dev libbz2-dev patch \
  git sbcl libexpat-dev wget vim libzmq3-dev 
sudo apt-get install -y clang-6.0 libclang-common-6.0-dev libclang-6.0-dev libclang1-6.0 \
  libllvm6.0  lldb-6.0 llvm-6.0 llvm-6.0-dev llvm-6.0-doc \
  llvm-6.0-runtime clang-format-6.0 python-clang-6.0 lld-6.0
sudo apt -y install gcc g++ cmake
sudo apt -y install libgc-dev libgmp-dev binutils-gold binutils-dev
sudo apt -y install zlib1g-dev libncurses-dev
sudo apt -y install sbcl

sudo apt install -y awscli
export AWS_DEFAULT_REGION=us-east-2

sudo apt-get -y install python3.6

cd /tmp && wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh
cd /tmp && bash Miniconda3-latest-Linux-x86_64.sh -b -p $HOME/miniconda

export PATH="$HOME/miniconda/bin:$PATH"
/bin/bash -c "source $HOME/miniconda/bin/activate"

# AWS command-line utilities
sudo apt-get -y install python-setuptools python-dev build-essential
easy_install pip
pip install --upgrade pip
pip install awscli

whoami
python -m pip install numpy
echo STAGE INSTALL npm
sudo apt-get install -y nodejs npm
echo STAGE INSTALL webpack
## complains -->   npm install --save-dev webpack
conda create -n lab python=3.6 -y
/bin/bash -c "source activate lab"
#RUN conda install ipywidgets=7.1.2 -c conda-forge -y
echo STAGE conda install
conda install ipywidgets -c conda-forge -y
pip install nglview==1.1.2
nglview enable # might need this to enable nglview-js-widgets extension for notebook
conda install jupyterlab=0.32 -y -c conda-forge
pip install bqplot
jupyter-labextension install @jupyter-widgets/jupyterlab-manager nglview-js-widgets@1.1.2 bqplot


