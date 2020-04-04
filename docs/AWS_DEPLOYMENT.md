# Deploying to Amazon Web Services

![](../.github/aws.png)

## Pre-Requisites

- Make sure to have an AWS account, and an `AWS_ACCESS_KEY_ID` & `AWS_SECRET_ACCESS_KEY`.

- Make sure to grant `AmazonEC2FullAccess` to your AWS user.

- You'll need to create a security group which allows inbound TCP connections. See also:
  https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html#WorkingWithSecurityGroups

- Install [docker-machine](https://docs.docker.com/machine/install-machine/)

## Create an SSH key/pair

```
$ ssh-keygen -t rsa -b 4096 -C "your_email@example.com" -f aws_rsa
$ eval "$(ssh-agent -s)"
$ ssh-add ~/.ssh/aws_rsa
```

## Create The Docker Machine

```bash
AWS_ACCESS_KEY_ID=<YOUR_ACCESS_KEY_ID>
AWS_SECRET_ACCESS_KEY=<YOUR_SECRET_KEY>

docker-machine create -d amazonec2 \
  --amazonec2-access-key $AWS_ACCESS_KEY_ID \
  --amazonec2-secret-key $AWS_SECRET_ACCESS_KEY \
  --amazonec2-region eu-west-3 \
  --amazonec2-instance-type "t2.micro" \
  --amazonec2-ssh-keypath ~/.ssh/aws_rsa \
  aws-ogmios
```

## Push The Stack

First, configure your shell and activate your docker-machine:

```
$ eval $(docker-machine env aws-ogmios)
```

Then, simply push the stack using docker-compose:

```
$ COMPOSE_TLS_VERSION=TLSv1_2 docker-compose up -d
```

> :warning: compose may use a wrong TLS version for pulling layers from dockerhub. Hence the ENV var. 


## :tada: Enjoy

```
$ docker-machine ls
NAME         ACTIVE   DRIVER      STATE     URL                         SWARM   DOCKER     ERRORS
aws-ogmios   *        amazonec2   Running   tcp://xx.xxx.xxx.xxx:xxxx           v19.03.8 
```
