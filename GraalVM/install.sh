sudo apt-get update
sudo apt-get install gcc zlib1g-dev build-essential unzip

wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-20.0.0/graalvm-ce-java8-linux-amd64-20.0.0.tar.gz
tar -xvzf graalvm-ce-java8-linux-amd64-20.0.0.tar.gz

sudo mkdir /usr/lib/graal
sudo mv graalvm-ce-java8-20.0.0/ /usr/lib/graal