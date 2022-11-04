name=iohk/dapps-certification:$(uname -m)-0.0.1
echo $name
docker build --progress=plain -q -t $name .
docker run -p 9671:9671 -it $name
