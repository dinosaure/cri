## How to use the unikernel?

As a MirageOS project, you must install `opam` and `mirage` via:
```sh
$ sudo apt install opam
$ opam init
$ opam install mirage
```

Then, you must _pin_ `cri` as an available package into your OPAM environment:
```sh
$ git clone https://github.com/dinosaure/cri
$ cd cri
$ opam pin add -y .
```

Finally, you are able to compile the unikernel. For the unix target - the
simpler one, you can do:
```sh
$ cd unikernel
$ mirage configure -t unix
$ make depends
$ mirage build
$ ./logger --irc=ircs://irc.libera.chat:6697/ --remote=git@localhost:log.git \
    --ssh-seed=$SEED --channel=#mirage --nickname=noisy-bot --nameserver=8.8.8.8
```

We integrate many way to communicate with a Git repository and SSH is one of
them. You can look possibilities [here][ocaml-git-examples]. About SSH, you
must have the "seed" of the RSA key. It's available via [`awa-ssh`][awa]:
```sh
$ awa_gen_key
seed is M0XGwihC6RtGCzYptLcHuO+SV38TqBUbCMiCf898
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCmid+AGVt0mrVggEFkLAVS6rgWhhov+dw54Onm3L49BHYYPU4bz1Z4uIAmhGGvX/sfJ9KSYKr7PDncNi7EnvbeoGGBjNZM7GWKGji+jFZRwTWuamiJM/jL7NyRXb75bmOB1NT9NO42m0Z6BYnriOzm7dDl+3Hh3AqLpfwy2mg/6dlApcVgbGZTxpneY/vwMajtCikEXTyRmaXx0J2ceGCPt+i0R5MMGnEwPMmFsZF/J3juTp2dm/KG5eTcKOdXAs/WjwIv5W3vvDjwJUx5oxGugsUfgn+nHnX9EbqM4OeOlHwlG7l4Gp82PNRdhPFai1isFSDYFrXr7B5ZDnLWkXAx awa@awa.local
```

The seed is the first line and the public key is the second line. You just need
to save the public key into your GitHub account or as an allowed key for the
`git` Unix user.

## DNS nameserver

Due to the design of IRC, the choice of the DNS server can be important due to
the geographic position of your unikernel. It's why this parameter is required.
Note that the DNS server used to communicate with the Git repository **is not**
the same. The given nameserver is only used by the IRC client.

## Deploy on Gcloud

It's possible to deploy the unikernel to `gcloud`. This is a mini-tutorial to
deploy `cri-logger` on `gcloud`. Assume that you have a Gcloud project and the
`gcloud` command-line available. You must have a GitHub repository and we will
try to use SSH to let the unikernel to "push" logs on it.

You must create an SSH key via `awa-ssh`:
```sh
$ awa_gen_key
seed is <SEED>
ssh-rsa ... awa@awa.local
```

The first line is the seed to regenerate the private RSA key. The second line
is the public key which must be added to your GitHub account as an allowed SSH key.
You must keep the SSH seed to configure the unikernel then:
```sh
$ cd cri/unikernel
$ mirage configure -t virtio --dhcp true --hostname cri \
  --irc ircs://irc.libera.chat:6697/ \
  --remote git@github.com:user/repository \
  --nickname my-noisy-bot \
  --channel="##mirage" \
  --tick 86400 \
  --nameserver 9.9.9.9 \
  --ssh-seed <SEED>
```

You can change any of these parameters. `tick` is how long we log the given channel
- for instance, 1 day. `remote` is your GitHub repository and you can change
`nickname` and `channel as you want. You must copy the same `<SEED>` given by
`awa_gen_key`. Finally, you must choose the DNS server depending on the geographic
position of your unikernel (your `gcloud` "region").

Now, you are able to compile the unikernel if you correctly `pin` `cri` with `opam`:
```sh
$ make depends
$ mirage build
$ solo5-virtio-mkimage -f tar logger.tar.gz logger.virtio
```

We can start to deploy the unikernel then:
```sh
$ gsutil mb gs://cri-logger
$ gsutil cp logger.tar.gz gs://cri-logger
$ gcloud compute images create cri-logger --source-uri gs://cri-logger/logger.tar.gz
$ gcloud compute addresses create cri-logger --region europe-west1
```

You must take the IP address given by `gcloud` to be able then to create an instance:
```sh
$ gcloud compute instances create cri-logger \
  --image cri-logger \
  --address <IP> \
  --zone europe-west1-b \
  --machine-type f1-micro \
```

Et voilà! You will see in few second the bot on your channel and, in one day, the
unikernel will push a new file on your GitHub repository.

## Bugs, bad state & improvements

The unikernel is experimental and several issues exists if you give wrong
arguments (such as a bad IRC address or a bad SSH key). In some situation, we
can lost some messages. Indeed, `cri` does not implement the full version of
the IRC protocol.

[ocaml-git-examples]: https://github.com/mirage/ocaml-git/tree/master/unikernel/empty-commit
[awa]: https://github.com/mirage/awa-ssh
