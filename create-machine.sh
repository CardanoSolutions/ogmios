docker-machine create -d amazonec2 \
  --amazonec2-region eu-west-3 \
  --amazonec2-instance-type "t3.medium" \
  --amazonec2-ssh-keypath ~/.ssh/aws_rsa \
  ogmios-aws
