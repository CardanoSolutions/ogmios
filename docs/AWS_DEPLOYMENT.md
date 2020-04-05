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
  --amazonec2-instance-type "t2.medium" \
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

## (Optional) Adding TLS with a registered domain

Install [nginx](https://nginx.org/).

<details>
  <summary>Configure a new server as <code>/etc/nginx/sites-enabled/domain.extension</code></summary>

```nginx
server {
    server_name DOMAIN.EXTENSION;
    listen 80;

    location ^~ /.well-known/acme-challenge/ {
        try_files $uri /dev/null =404;
    }
}
```
</details>

> :point_up: Make sure to replace 'DOMAIN.EXTENSION' with your actual registered domain.

Make sure to reload your nginx configuration with: `sudo systemctl reload nginx.service`.

Install [certbot](https://certbot.eff.org/lets-encrypt/ubuntubionic-nginx) and let certbot configure your nginx server (`sudo certbot --nginx`).
Once done, edit your nginx configuration one more time...

  1. <details>
         <summary>Remove (no longer needed after certbot has successfully configured the server):</summary>

     ```nginx
     location ^~ /.well-known/acme-challenge/ {
         try_files $uri /dev/null =404;
     }
     ```
     </details>

  2. <details>
         <summary>And add the following clause to enable routing all traffic (including WebSockets) to ogmios:</summary>

     ```nginx
     location ~* / {
         proxy_pass http://localhost:1337;
         proxy_http_version 1.1;
         proxy_set_header Upgrade $http_upgrade;
         proxy_set_header Connection "Upgrade";
         proxy_set_header Host $host;
     }
     ```
     </details>
            
<details>
  <summary> The final configuration should look like: <code>/etc/nginx/site-enabled/ogmios.dev</code></summary>

```nginx
server {
  server_name ogmios.dev;

  location ~* / {
    proxy_pass http://localhost:1337;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "Upgrade";
    proxy_set_header Host $host;
  }

  listen 443 ssl; # managed by Certbot
  ssl_certificate /etc/letsencrypt/live/ogmios.dev/fullchain.pem; # managed by Certbot
  ssl_certificate_key /etc/letsencrypt/live/ogmios.dev/privkey.pem; # managed by Certbot
  include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
  ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot
}

server {
  server_name ogmios.dev;
  listen 80;

  if ($host = ogmios.dev) {
      return 301 https://$host$request_uri;
  } # managed by Certbot

  return 404; # managed by Certbot
}
```
</details>
