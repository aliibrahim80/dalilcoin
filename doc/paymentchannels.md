# Payment Channels

Dalilcoin has the infrastructure to support bidirectional payment
channels ([Poon Dryja 2016]), namely, multisig and hashed timelock
contracts (htlc).  Details are below.

## Multisig

Multisig addresses are p2sh addresses spendable by m signatures
corresponding to m-of-n public keys.  For payment channels, only
2-of-2 multisig addresses are used and these multisig addresses are
created using `createchannel` (described below). General multisig
addresses can be created by `createmultisig` or `addmultisig` (which
adds the p2sh address and redeem script to the wallet).

To create a multisig address, we need the n pubkeys. The easiest way
to obtain a pubkey is to use `getaddressinfo` for an address in the user's wallet.
For example, three different nodes with three different wallets could
use an address in each wallet:

* User 1:

```
getaddressinfo DioyDNnmm42tf5H5u8UyEhSiAC6oEa6nbC
{"address":"p2pkh","pubkey":"03ffcaf23fd3dafe8fa4af133d29c7abda46443330c5c8382379c2113e473524ba"}
```

* User 2:

```
getaddressinfo DnesC5PG9R2gpfZLw7JjGrQK5jzN2vSELt
{"address":"p2pkh","pubkey":"02d345d8ff566e74d4c83232d49b2376e653d43ffb4b245bd1f69ca60d850c5f53"}
```

* User 3:

```
getaddressinfo DajzxEvV64gYzK29N4VsXZy9dFLNtKuwVh
{"address":"p2pkh","pubkey":"020f82abeff1195f22e1fea514be2b622a994fe65bfc05a31cc1c9344b8b9fdd91"}
```

Since the format for pubkeys is the same as in Bitcoin, a user could
also obtain the pubkey corresponding to a Bitcoin private key using
Bitcoin supporting software.  (Bitcoin private keys can be used to
sign Dalilcoin txs.)

After obtaining the 3 pubkeys any node can create a 2-of-3 multisig
address using the pubkeys. This is done as follows:

```
createmultisig 2 '["03ffcaf23fd3dafe8fa4af133d29c7abda46443330c5c8382379c2113e473524ba","02d345d8ff566e74d4c83232d49b2376e653d43ffb4b245bd1f69ca60d850c5f53","020f82abeff1195f22e1fea514be2b622a994fe65bfc05a31cc1c9344b8b9fdd91"]'

P2sh address: dNLSodJo6SqSU6Bd3BxBE8PbP8z4Fq12Nv
Redeem script: 522103ffcaf23fd3dafe8fa4af133d29c7abda46443330c5c8382379c2113e473524ba2102d345d8ff566e74d4c83232d49b2376e653d43ffb4b245bd1f69ca60d850c5f5321020f82abeff1195f22e1fea514be2b622a994fe65bfc05a31cc1c9344b8b9fdd9153ae
```

After creating the p2sh address, we might import it into the wallet
using `importp2sh` with the redeem script as follows:

```
importp2sh 522103ffcaf23fd3dafe8fa4af133d29c7abda46443330c5c8382379c2113e473524ba2102d345d8ff566e74d4c83232d49b2376e653d43ffb4b245bd1f69ca60d850c5f5321020f82abeff1195f22e1fea514be2b622a994fe65bfc05a31cc1c9344b8b9fdd9153ae
Imported p2sh address: dNLSodJo6SqSU6Bd3BxBE8PbP8z4Fq12Nv
```

The two steps can be combined using `addmultisig`. If the address is imported
into the wallet, the node can recover the redeem script from the address:

```
getaddressinfo dNLSodJo6SqSU6Bd3BxBE8PbP8z4Fq12Nv
{"address":"p2sh","script":"522103ffcaf23fd3dafe8fa4af133d29c7abda46443330c5c8382379c2113e473524ba2102d345d8ff566e74d4c83232d49b2376e653d43ffb4b245bd1f69ca60d850c5f5321020f82abeff1195f22e1fea514be2b622a994fe65bfc05a31cc1c9344b8b9fdd9153ae"}
```

Redeem scripts can also be explicitly given to commands like `signtx`
in case they are not in the wallet.

The multisig address dNLSodJo6SqSU6Bd3BxBE8PbP8z4Fq12Nv can be funded
using commands like `sendtoaddress`, `createtx` or `creategeneraltx`.
After funding the address, if the address is in the wallet
`printassets` can be used to see the assets at the address.
Here we explicitly give the ledger root for the Block 1296
since the example asset has already been spent as of Block 1337.

```
printassets df3dd32739ae04f41fbaa95ef4d01b1b2796900c87b4abcc57c7085eb47614c0
...
dNLSodJo6SqSU6Bd3BxBE8PbP8z4Fq12Nv:
67ba36dc568d98df19e6054db2de7b2c4e658c34c7de69475a098005f2330d0a: (id ab48141c22e280f07d02f888c119962bdca43011838775a997228a04b0ecd6be) [1295] Currency 0.02 fraenks (2000000000 cants)
...
```

Suppose we then wish to spend this asset. We can create the tx spending it as follows:

```
createtx '[{"dNLSodJo6SqSU6Bd3BxBE8PbP8z4Fq12Nv":"ab48141c22e280f07d02f888c119962bdca43011838775a997228a04b0ecd6be"}]' '[{"addr":"Dkyz9Q8X9Dxba2HLtfi1GyzBGAoBEw4nmN","val":0.0199}]'

0b73c62d959082aac065f316276eea96f1a359435c45a2e010110784ef13c0470cceb05ce1268589183cac4bbd1451248065b7f695da85f91e2736ae994116dc96f2f372904437da3e00000000b0e3ec0704
```

This unsigned tx then needs to be signed by at least two of the
private keys.  Suppose *privkey1* is the private key for
DioyDNnmm42tf5H5u8UyEhSiAC6oEa6nbC, *privkey2* is the private key for
DnesC5PG9R2gpfZLw7JjGrQK5jzN2vSELt and *privkey3* is the private key
for DajzxEvV64gYzK29N4VsXZy9dFLNtKuwVh.  Suppose neither the redeem
script nor any of the privkeys are in the wallet. We can sign with the
first privkey by giving *privkey1* and the redeem script explicitly to
`signtx`.

```
signtx 0b73c62d959082aac065f316276eea96f1a359435c45a2e010110784ef13c0470cceb05ce1268589183cac4bbd1451248065b7f695da85f91e2736ae994116dc96f2f372904437da3e00000000b0e3ec0704 '["<privkey1>"]' '["522103ffcaf23fd3dafe8fa4af133d29c7abda46443330c5c8382379c2113e473524ba2102d345d8ff566e74d4c83232d49b2376e653d43ffb4b245bd1f69ca60d850c5f5321020f82abeff1195f22e1fea514be2b622a994fe65bfc05a31cc1c9344b8b9fdd9153ae"]'

0b73c62d959082aac065f316276eea96f1a359435c45a2e010110784ef13c0470cceb05ce1268589183cac4bbd1451248065b7f695da85f91e2736ae994116dc96f2f372904437da3e00000000b0e3ec07b40605a87932c3f41eaf5fceed97a56cca4a11e20d681cf34a83c8496b7c3d7ef14b9ce97f27ef4e98e552c050bd9f7d0fff637e98f4d9f6b6cefabef2f10d37caa449f9a3729714fb73c4cf34add49003ff2b97ff73ba76ff8f49bf9ed833e5f1d5da8d129d09b338729c23f30a8fe8f3684d92ba430a9c5e14fb7fab6ee952472e5326f5cd23ed9a9f4afde7fea524b746b7cfd974c3c20cbf4e0d29f0a1e0d5efe367fc2a32bcffd2147daf14ab32f353f35bf9171ccd3178729a4b177fee1e79aa2b00
Partially signed.
```

The partially signed tx can then be given to the controller of the third key
who can complete the signature. The redeem script is already part of the
partial signature and need not be given again.

```
signtx 0b73c62d959082aac065f316276eea96f1a359435c45a2e010110784ef13c0470cceb05ce1268589183cac4bbd1451248065b7f695da85f91e2736ae994116dc96f2f372904437da3e00000000b0e3ec07b40605a87932c3f41eaf5fceed97a56cca4a11e20d681cf34a83c8496b7c3d7ef14b9ce97f27ef4e98e552c050bd9f7d0fff637e98f4d9f6b6cefabef2f10d37caa449f9a3729714fb73c4cf34add49003ff2b97ff73ba76ff8f49bf9ed833e5f1d5da8d129d09b338729c23f30a8fe8f3684d92ba430a9c5e14fb7fab6ee952472e5326f5cd23ed9a9f4afde7fea524b746b7cfd974c3c20cbf4e0d29f0a1e0d5efe367fc2a32bcffd2147daf14ab32f353f35bf9171ccd3178729a4b177fee1e79aa2b00 '["<privkey3>"]'

0b73c62d959082aac065f316276eea96f1a359435c45a2e010110784ef13c0470cceb05ce1268589183cac4bbd1451248065b7f695da85f91e2736ae994116dc96f2f372904437da3e00000000b0e3ec07b40605a87932c3f41eaf5fceed97a56cca4a11e20d681cf34a83c8496b7c3d7ef14b9ce97f27ef4e98e552c050bd9f7d0fff637e98f4d9f6b6cefabef2f10d37caa449f9a3729714fb73c41f14a0fba2e4cfca2cbc90a7e7ca6b4796773ab460dfea37593ba6ae77f741951f692e8c3bfc7a42e9220db31cd950aa6df21ae9925c9a31e77aab4ac11e76da5eb5c7b3b34547ce5e783bd3b452430efcaf5cfecfe9dafd3f26fd7a62cf94c7576b374a7426cce2c8718ecc2b3ca2cfa33549ea0e29707a51ecffadbaa54b1db94c99d4378fb46b7e2af59ffb9792dc1add3e67d30d0b33fc3a35a4c0878257bf8f9ff1abc8f0fe4b53f4bd52accacc4fcd6fe55f7034c7e0c9692e5dfcb97be4a9ae00
Completely signed.
```

## Hashed timelock contracts

The other kind of p2sh address used in payment channels are hashed
timelock contracts (htlc). As with multisig addresses, more general
htlc addresses are supported than are required for payment channels.
The command `createhtlc` is used to create a htlc address.  By default
`createhtlc` creates a p2sh address payable after an absolute lock
time (using the opcode OP_CLTV, an opcode analogous the Bitcoin script
op code [BIP0065]), but can create an address payable after relative
lock time (using the opcode OP_CSV, again analogous to the Bitcoin
script op code [BIP0068, BIP0112, BIP0113]) if the keyword 'relative'
is given after the timelock argument. As with Bitcoin, absolute lock
time refers to the block height if it is less than 500000000 and
refers to unix time otherwise. In contrast to Bitcoin, relative lock
time is always measured in number of blocks since the asset was
confirmed and relative lock times must be less than 500000000. Payment
channels are assumed to use a relative lock time of 28 blocks (1 week
at average block times).

We start by considering an example of an htlc with relative lock time
of 8 blocks.  In this case we explicitly give the secret
23fd3dafe8faffcaf4af133d29c7abda46443330c5c8382379c2113e473524ba. A
secret must be 32 bytes given in hex. If the secret is omitted then a
random 32 bytes will be generated and used.

```
createhtlc DioyDNnmm42tf5H5u8UyEhSiAC6oEa6nbC DnesC5PG9R2gpfZLw7JjGrQK5jzN2vSELt 8 relative 23fd3dafe8faffcaf4af133d29c7abda46443330c5c8382379c2113e473524ba

P2sh address: dSVykGFhdXaqnmqxLbB1bFoBh6J316v3tA
Redeem script: 6382012088a824bfed797c96c9e08a8e88baa788db4efa88db4efaf30ca1f7fd326bae941e029daeff45cc8876a9149d36c2ee3c3c2764abf3c62aa9c6c78c588df750670408000000b27576a914c75e7e60fe39a3ce68865161a02a9645d07482ac6888ac
Secret: 23fd3dafe8faffcaf4af133d29c7abda46443330c5c8382379c2113e473524ba
Hash of secret: bfed797c96c9e08a8e88baa788db4efaf30ca1f7fd326bae941e029daeff45cc
```

We could import the p2sh address into the wallet using `importp2sh` with the redeem script.

The redeem script gives two ways to spend an asset controlled by
dSVykGFhdXaqnmqxLbB1bFoBh6J316v3tA. The IF branch is spendable by
a signature from DioyDNnmm42tf5H5u8UyEhSiAC6oEa6nbC using the secret
23fd3dafe8faffcaf4af133d29c7abda46443330c5c8382379c2113e473524ba.
The ELSE branch is spendable by DnesC5PG9R2gpfZLw7JjGrQK5jzN2vSELt
8 blocks after the asset is confirmed.

The very attentive reader will notice something unusual about the
redeem script. The SHA256 hash of the secret is the 32 bytes

bfed797c96c9e08a8e88baa788db4efaf30ca1f7fd326bae941e029daeff45cc

In order to check a given secret has this hash, the script pushes
the following 36 bytes onto the stack for comparison with the output
of OP_SHA256:

bfed797c96c9e08a8e88baa788db4efa88db4efaf30ca1f7fd326bae941e029daeff45cc

This is the hash of the secret with 4 bytes in the middle repeated.
The Dalilcoin implementation of the Bitcoin scripting language contained
a bug that causes OP_SHA256 and OP_HASH256 to output the result with
4 bytes in the middle repeated. Since a hard fork would be required to
fix this bug, it is easier to work around it by repeating the 4 bytes
in the htlc redeem script.

A currency asset with 0.01 fraenks with assetid
1e2d960567df83faa8c4de1625eb121606586d5fbc42c9cf54c604646484a272 was
created at address dSVykGFhdXaqnmqxLbB1bFoBh6J316v3tA by a tx in Block
1320. This was then spent in the next block using the IF branch with
the secret. For the sake of the example the output was spent to the
same address, but leaving 0.001 as a tx fee.

```
createtx '[{"dSVykGFhdXaqnmqxLbB1bFoBh6J316v3tA":"1e2d960567df83faa8c4de1625eb121606586d5fbc42c9cf54c604646484a272"}]' '[{"addr":"dSVykGFhdXaqnmqxLbB1bFoBh6J316v3tA","val":0.009}]'

7be4639600eb8c509e51fda405a55755728b5eccf068b12c38fb1ed44725f6b6285997b030c06afbe2154a7ea632262023231495b3473e6609b0ce08e519d54f5a507a5525b7e8c50c00000000a8214d0700
```

Let us first see what happens if we try to sign with *privkey2*,
the private key for the second address DnesC5PG9R2gpfZLw7JjGrQK5jzN2vSELt.
This corresponds to using the ELSE branch. Signing was actually
successful immediately (without waiting the 8 blocks):

```
signtx 7be4639600eb8c509e51fda405a55755728b5eccf068b12c38fb1ed44725f6b6285997b030c06afbe2154a7ea632262023231495b3473e6609b0ce08e519d54f5a507a5525b7e8c50c00000000a8214d0700 '["<privkey2>"]' '["6382012088a824bfed797c96c9e08a8e88baa788db4efa88db4efaf30ca1f7fd326bae941e029daeff45cc8876a9149d36c2ee3c3c2764abf3c62aa9c6c78c588df750670408000000b27576a914c75e7e60fe39a3ce68865161a02a9645d07482ac6888ac"]'

7be4639600eb8c509e51fda405a55755728b5eccf068b12c38fb1ed44725f6b6285997b030c06afbe2154a7ea632262023231495b3473e6609b0ce08e519d54f5a507a5525b7e8c50c00000000a8214d07b00605f8df637997338fdb944f54a77fabef49d6af789165eada91a19a1d7afef050a695d72fe5bad6b0e5aa495b97675e5b27e38c092ba6fce938f7d3fde85926964f3e384ac9d5215e671c52e0f4a2d8ff5b754b973a729932a96f1e69d7fc54ea3ff72f25b935ba7dcea61b1666f8752a40a665c70a0e0812316a92bfdbe7e56b393978c58e11eb3e8d78bb53fd88b73bd53f9f61e8fbfd65ae754dd9a3c0ceaeff17658ed86e6a8a9d6d0a77cf93e749b2abe71b5799daf871c6581bdf877a962042800001caae6b3735c5e35efd82f59f73b473b486a38605add27251e87405b3468b9815
Completely signed.
```

However `validatetx` indicated that the tx is not yet valid:

```
validatetx 7be4639600eb8c509e51fda405a55755728b5eccf068b12c38fb1ed44725f6b6285997b030c06afbe2154a7ea632262023231495b3473e6609b0ce08e519d54f5a507a5525b7e8c50c00000000a8214d07b00605f8df637997338fdb944f54a77fabef49d6af789165eada91a19a1d7afef050a695d72fe5bad6b0e5aa495b97675e5b27e38c092ba6fce938f7d3fde85926964f3e384ac9d5215e671c52e0f4a2d8ff5b754b973a729932a96f1e69d7fc54ea3ff72f25b935ba7dcea61b1666f8752a40a665c70a0e0812316a92bfdbe7e56b393978c58e11eb3e8d78bb53fd88b73bd53f9f61e8fbfd65ae754dd9a3c0ceaeff17658ed86e6a8a9d6d0a77cf93e749b2abe71b5799daf871c6581bdf877a962042800001caae6b3735c5e35efd82f59f73b473b486a38605add27251e87405b3468b9815

Tx is not valid until block height 1328
Tx is valid and has id a884a4b27eba9d05098f103d52cd14db53a659f168652778257a61c9590f2339
Tx is supported by the current ledger and has fee 0.001 fraenks (above minrelayfee 0.0000312 fraenks)
```

Likewise, `sendtx` would refuse to send the tx until Block 1327 had been staked,
and peers would not accept it if it were sent before Block 1327.

Now we show how to sign using *privkey1*, the key for the IF branch, along with
the secret. In this case the secret is given in a json array of secrets after
the json array of redeem scripts. Currently there is no wallet support for
storing secrets and so these must be given to `signtx` explicitly.

```
signtx 7be4639600eb8c509e51fda405a55755728b5eccf068b12c38fb1ed44725f6b6285997b030c06afbe2154a7ea632262023231495b3473e6609b0ce08e519d54f5a507a5525b7e8c50c00000000a8214d0700 '["<privkey1>"]' '["6382012088a824bfed797c96c9e08a8e88baa788db4efa88db4efaf30ca1f7fd326bae941e029daeff45cc8876a9149d36c2ee3c3c2764abf3c62aa9c6c78c588df750670408000000b27576a914c75e7e60fe39a3ce68865161a02a9645d07482ac6888ac"]' '["23fd3dafe8faffcaf4af133d29c7abda46443330c5c8382379c2113e473524ba"]'

7be4639600eb8c509e51fda405a55755728b5eccf068b12c38fb1ed44725f6b6285997b030c06afbe2154a7ea632262023231495b3473e6609b0ce08e519d54f5a507a5525b7e8c50c00000000a8214d07b00605b89123e4d02bd91aeccbfa7c6aa9222ba24c3bbe7dd1f0d7ff0b75ecd87076fc7a777267ecf66761be72d12faf5afee140f3cd3f8b055df63a79ab3f9f3f3c2af1bf75f425f1fe1d1a72e07fe5f27f4ed7eeff31e9d7137ba63cbe5abb51a2336116478e73645ee1117d1ead495237c891fd7bbe46afffbf72faaf27f64c797cb576a34467c22c8e1ce7c8bcc223fa3c5a93a4eec04ccb8e151c102462d4247fb7cfcbd77272f08a1d23d67d1af176a7fa116f77aa7f3ec3d0f7fbcb5ceb9ab247819d5dff2fca1cb1ddd4143bdb14ee9e27cf936457cf37ae32b5f1e38cb136be0ff52c4184000102945dd76e6a8ac7bdfa05eb3fe768e7680d470d0b5aa5e5a2d0e90a668d16312b00
Completely signed.
```

`validatetx` could be used to confirm the signed tx is valid and
`sendtx` could be used to send it.  In fact the tx was sent and
confirmed in Block 1322. The newly created asset with 0.009 fraenks
has asset id
4192aab859159361174cef16e0f04198c83bbb70e6e463f66facc1666e78c9e6.
Since 8 blocks have passed, a tx spending the asset could be signed
with *privkey2* (without the secret) and the tx would be valid
immediately.

Using absolute lock time is essentially the same except the keyword
`relative` must be omitted from `createhtlc`. One also has the option
of using unix time instead of Dalilcoin block height.
In case unix time is used, a spending tx using the ELSE branch will
not be seen as valid until after the first Dalilcoin block with a later
time stamp has been staked. (In principle the spending tx could be
included in that block, but it will not be propagated until after
such a block has been validated.)

## Creating a payment channel

We now use multisig and htlc to create bidirectional payment channels.
The `createchannel` command can be used to obtain the initial about
the channel including a 2-of-2 multisig address (the *fund address*)
and redeem script, an unsigned funding tx and two asset ids (*fund
asset ids*) that will be held at the fund address if the funding tx is
signed and confirmed.

Let Alice and Bob be the two parties. Suppose Alice controls pubkey
031e722e4d5829ca85c2c916863df0b104c21d586bb2cf8a28d0f7060fc5978266
with private key *AlicePrivKey*
and Bob controls pubkey 03c04d0d26c5a3dd58f3ee09a14025d8b1c9fdd152346d3427add82500204c66fa
with private key *BobPrivKey*.
Alice's corresponding address is DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur
and Bob's corresponding address is Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61.
Suppose also that Alice controls a currency asset
with asset id
3389718876e1dcb69664327195d0e475a681d9a30925a76de9b044b974148056
with more than 0.6 fraenks
and Bob controls a curency asset with asset id
aa9d9467022fd5521564948e91e6a90e3c1bcb57f00ed29ced747161df29d190
with more than 0.4 fraenks.
A payment channel with Alice controling 0.6 fraenks
and Bob controlling 0.4 fraenks can be initialized as follows:

```
createchannel 031e722e4d5829ca85c2c916863df0b104c21d586bb2cf8a28d0f7060fc5978266 03c04d0d26c5a3dd58f3ee09a14025d8b1c9fdd152346d3427add82500204c66fa 3389718876e1dcb69664327195d0e475a681d9a30925a76de9b044b974148056 aa9d9467022fd5521564948e91e6a90e3c1bcb57f00ed29ced747161df29d190 0.6 0.4

Funding tx: 710d93f177bd471f31950c75abd9f510c7bf95449e498c43b40be7b6b5249389ab8426af330dcc1e4d28396d4b8725caa5a300b4ca565bc83128b6574dd967ea539214e5198adf51be6a27e599c04bb5540519a563a479aa03cfc6f215bc8334673b5d5cd8774a34a47142d8ac1c0b8814bf8206a17bda36b3310dd14e0200004003fe1116c038216c568e05448a5f4183d03d6d9bd9988668270100002001ea051220ae6132feaef7e823a692a16e35bb1ee2f8b792c800002080a3c19b0b98adb69063506caf9ab2cfd4a72429ca3314bfa37c000010e011a60e0404
Fund 2-of-2 address: dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH
Redeem script: 5221031e722e4d5829ca85c2c916863df0b104c21d586bb2cf8a28d0f7060fc59782662103c04d0d26c5a3dd58f3ee09a14025d8b1c9fdd152346d3427add82500204c66fa52ae
Fund asset id 1: 50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d
Fund asset id 2: b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8
```

The fundingtx spends Alice's current asset and Bob's current asset together to
create two fund assets (which will have the two fund asset ids) which together
add to 0.6+0.4 = 1 fraenk, and will return Alice's change to her address
and Bob's change to his address.

Both Alice and Bob must sign the funding tx before it can be confirmed.
Before doing so, both Alice and Bob must create and sign initial commitment txs.

Alice creates an initial commitment tx which Bob must sign and give back.
Alice first creates the htlc: `createhtlc <Bob> <Alice> 28 relative`.
This address will be spendable by Bob if Bob has the secret
and will be spendable by Alice 28 blocks after confirmation.

```
createhtlc Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur 28 relative

P2sh address: das7ySGhRBNGSVsAepmAuWV6gGkAUfUG5V
Redeem script: 6382012088a8248ad7d92719348ef501cd5aedd92067d1d92067d1ca6ea3c1fbb4f62a60c058c380e7563f8876a9145b6d21c7a0d85e35659fa94f49529467287e47f967041c000000b27576a914ae6132feaef7e823a692a16e35bb1ee2f8b792c86888ac
Secret: dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5
Hash of secret: 8ad7d92719348ef501cd5aedd92067d1ca6ea3c1fbb4f62a60c058c380e7563f
```

Here the secret is Alice's first secret.

Alice now creates her first commitment tx.  Loosely speaking, the
commitment tx spends the two (hypothetical) fund assets to send 0.5999
fraenks (0.6 minus a txfee) to her htlc address and 0.3999 fraenks
(0.4 minus a txfee) to Bob's p2pkh address.  To be more precise, the
commitment tx will spend the two fund assets from the fund asset
address to create two new currency assets still held at the fund
address.  However, the two new currency assets will be spendable by
different "lock addresses". The 0.5999 asset will be spendable by
Alice's htlc address and the 0.3999 asset will be spendable by Bob's
p2pkh address.  There is a technical reason for this approach.
Dalilcoin addresses are not allowed to hold more than 32 assets.
Because of this, a commitment tx might be made invalid by a third
party who fills the intended recipient address with 32 assets.  To
avoid this attack, we create two fund assets for the payment channel
held at the multisig fund address (taking up 2 of the potential 32
assets) and always have commitment txs spend the 2 assets and create 2
new assets at the same address (ensuring that the commitment txs will
never result in more than 32 assets being held at an address).

```
createtx '[{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d"},{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8"}]' '[{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.5999,"lockheight":0,"lockaddr":"das7ySGhRBNGSVsAepmAuWV6gGkAUfUG5V"},{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.3999,"lockheight":0,"lockaddr":"Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61"}]'

e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf30101
```

Alpha sends this unsigned commitment tx along with the hash of her secret to Bob.
Bob can verify it is correct using the command `verifycommitmenttx`

```
verifycommitmenttx DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH 50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8 0.5999 0.3999 8ad7d92719348ef501cd5aedd92067d1ca6ea3c1fbb4f62a60c058c380e7563f e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf30101

Valid commitment tx for DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur
```

Since this is a valid commitment tx for Alice, Bob signs it and returns it to Alice.
Bob cannot use `signtx` for this purpose since the assets being spent are not in the ledger.
However, the command `simplesigntx` can be used to sign it, since it makes certain simplifying
assumptions about assets that are not in the ledger.

```
simplesigntx e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf30101 '["<BobPrivKey>"]' '["5221031e722e4d5829ca85c2c916863df0b104c21d586bb2cf8a28d0f7060fc59782662103c04d0d26c5a3dd58f3ee09a14025d8b1c9fdd152346d3427add82500204c66fa52ae"]'

e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf301ad41016a74ca54a3eca9693b0ebdcff9b07aca5bd9f61f9b9cbb4ff14eaf17064bb0a5e4f17f0f4a6cf853e57ac52c35024f18b3f341f1254b0bc4badc26da8747eb57b598d1b3c7def0491e951a72a047b92e9b624da9bcb0f0e4160df7841f9ba0f08e58d7ca7eae1825f4fb061f16bf2cd86cc881c09b3634597c7477acf3dd270c0db424f6d8c9fb47974ab32dcd93adb197040892a959fd525d1d00
```

Bob then passes this back to Alice who checks if it is the same
commitment tx and if it is signed by Bob. There is no Dalilcoin
command for checking this partial signature, but Alice can check Bob's
signature by temporarily signing the tx with *AlicePrivKey* and
ensuring that the resulting tx is completely signed.

The procedure is then repeated interchanging the roles of Alice and Bob.

Bob creates an htlc and initial commitment tx.

```
createhtlc DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 28 relative

P2sh address: dNf9xoeUQxBGwuEohHuvCvjPycjB6k3jgf
Redeem script: 6382012088a824f21a8a0ac0fb5939be60e1abc3bcc81cc3bcc81c3149e57c6a1f3c8010522abe79dbd7808876a914ae6132feaef7e823a692a16e35bb1ee2f8b792c867041c000000b27576a9145b6d21c7a0d85e35659fa94f49529467287e47f96888ac
Secret: 682ce3627c72a6ddd8cc9e38b8e0c935bd7727ad3c6bd601f275ccfb198dc797
Hash of secret: f21a8a0ac0fb5939be60e1abc3bcc81c3149e57c6a1f3c8010522abe79dbd780

createtx '[{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d"},{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8"}]' '[{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.5999,"lockheight":0,"lockaddr":"DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur"},{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.3999,"lockheight":0,"lockaddr":"dNf9xoeUQxBGwuEohHuvCvjPycjB6k3jgf"}]'

e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ee61a26e3ef7a8f3e622a19ea56b3eb218e7f2b890c00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29dbc0c0bb23a8942c0feaeb0b94accbf24e89556281f0000000000000000000000129e2cf30101
```

Bob sends this tx to Alice along with the hash of his secret. She verifies it and signs it
and sends the partially signed tx back to Bob, who doublechecks the result.

Once both parties have partially signed commitment txs, they both sign the funding tx
and send it to be confirmed. In this example, the funding tx was confirmed
at height 1335, creating two fund assets.

```
printassets
...
dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH:
29e3d2d1113887fefce150d9eecdc885fbb5ffe78f99e9bca3802b8d89fefd19: (id b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8) [1335] Currency 0.4 fraenks (40000000000 cants)
39609328f47bf300d2c2a38e1e54e40f97df611703e70ab7cc74bc6631c8d417: (id 50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d) [1335] Currency 0.6 fraenks (60000000000 cants)
...
```

The payment channel is open as long as these assets remain unspent.
The only way to spend either of the assets is with a tx signed by
both Alice and Bob, and initially the only way of obtaining
such a tx is by having one party sign a commitment tx already signed
by the other party. According to the commitment txs, the current balance
of the channel is:

* Alice: 0.5999

* Bob: 0.3999

Note: If only one party is funding the channel, then assume that party
is Alice and use the command `createchannelonefunder`.

## Updating a payment channel

Suppose Alice decides to send 0.3 fraenks to Bob along the payment
channel. Assume the current balance is still the initial balance
reflected by the initial commitment txs:

* Alice: 0.5999

* Bob: 0.3999

Alice and Bob follow a protocol to exchange new commitment txs
and revoke previous commitment txs to reflect a new balance:

* Alice: 0.2999

* Bob: 0.6999

We walk through a 4 step protocol to make this update.

1. Alice creates a new commitment tx which is given to Bob, Bob signs
and gives back to Alice.

First Alice creates a new htlc with a new secret.

```
createhtlc Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur 28 relative

P2sh address: dWuXU9sF4FsmLMvEhWE5gqsDbmeFFCJL4y
Redeem script: 6382012088a8241ef19d58acbcbc0e20a18b6c60801413608014137992334b7a9d7baa6f829483b481fdd78876a9145b6d21c7a0d85e35659fa94f49529467287e47f967041c000000b27576a914ae6132feaef7e823a692a16e35bb1ee2f8b792c86888ac
Secret: 306b2293df0f1d958b2f31f843d4a7d6eb97d1ffc0d6171151fe730ac900a2a0
Hash of secret: 1ef19d58acbcbc0e20a18b6c608014137992334b7a9d7baa6f829483b481fdd7
```

Alice creates a new commitment tx which would create two new assets
held at the fund address with 0.2999 controlled by Alice's htlc
address and 0.6999 controlled by Bob's p2pkh address.

```
createtx '[{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d"},{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8"}]' '[{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.2999,"lockheight":0,"lockaddr":"dWuXU9sF4FsmLMvEhWE5gqsDbmeFFCJL4y"},{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.6999,"lockheight":0,"lockaddr":"Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61"}]'

e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14efe1b9d95502aaa91ab986e436885eebe889af40b09000000000000000000000006fb8b1580e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f00000000000000000000002096744b0101
```

Alice passes this unsigned tx to Bob and the hash of the new
secret. Bob verifies the commitment tx:

```
verifycommitmenttx DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH 50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8 0.2999 0.6999 1ef19d58acbcbc0e20a18b6c608014137992334b7a9d7baa6f829483b481fdd7 e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14efe1b9d95502aaa91ab986e436885eebe889af40b09000000000000000000000006fb8b1580e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f00000000000000000000002096744b0101

Valid commitment tx for DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur
```

Bob then signs it and sends the partially signed commitment tx back to
Alice.  Bob can either use `simplesigntx` or `signtx` for this.

2. Bob creates a new commitment tx which is given to Alice, Alice
signs and gives back to Bob.

First Bob creates a new htlc address with a new secret.

```
createhtlc DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 28 relative

P2sh address: dM7FUJ6XcYmdTND3gBsQ847CkUAQ6FTSbn
Redeem script: 6382012088a824ba42fcfdcaf007c0c17f485fd5f1879fd5f1879f1bdc16264fd1f04a0a495454c43daa728876a914ae6132feaef7e823a692a16e35bb1ee2f8b792c867041c000000b27576a9145b6d21c7a0d85e35659fa94f49529467287e47f96888ac
Secret: 7a9997061ef8a8f7683b3bcacb160751ce1e1ab04e3fb25f7dcbc8ec6ee4af9a
Hash of secret: ba42fcfdcaf007c0c17f485fd5f1879f1bdc16264fd1f04a0a495454c43daa72
```

Bob creates a new commitment tx which would create two new assets held
at the fund address with 0.2999 controlled by Alice's p2pkh address
and 0.6999 controlled by Bob's new htlc address.

```
createtx '[{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d"},{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8"}]' '[{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.2999,"lockheight":0,"lockaddr":"DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur"},{"addr":"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH","val":0.6999,"lockheight":0,"lockaddr":"dM7FUJ6XcYmdTND3gBsQ847CkUAQ6FTSbn"}]'

e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ee61a26e3ef7a8f3e622a19ea56b3eb218e7f2b890c000000000000000000000006fb8b1580e384b059391610297e050d42f7b46d66631aa29d9cea4a17e769fb313c74f4bab2d6ff516fb7cc4f0d00000000000000000000002096744b0101
```

Bob passes this new unsigned commitment tx along with the hash of the new secret to 
Alice. Alice verifies the new commitment:

```
verifycommitmenttx DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH 50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8 0.2999 0.6999 ba42fcfdcaf007c0c17f485fd5f1879f1bdc16264fd1f04a0a495454c43daa72 e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ee61a26e3ef7a8f3e622a19ea56b3eb218e7f2b890c000000000000000000000006fb8b1580e384b059391610297e050d42f7b46d66631aa29d9cea4a17e769fb313c74f4bab2d6ff516fb7cc4f0d00000000000000000000002096744b0101

Valid commitment tx for Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61
```

She then signs it and returns it to Bob.

3. Alice gives her previous secret to Bob, ensuring she will not use
the previous commitment tx.

Specifically Alice gives Bob the secret
dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5.
Bob can verify this is the secret easily as follows:

```
hashsecret dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5

8ad7d92719348ef501cd5aedd92067d1ca6ea3c1fbb4f62a60c058c380e7563f
```

Using this secret, Bob could spend the output intended for Alice if
she sends her prevous commitment tx.

4. Bob gives his previous secret to Alice, ensuring he will not use
the previous commitment tx.

Specifically, Bob sends Alice 682ce3627c72a6ddd8cc9e38b8e0c935bd7727ad3c6bd601f275ccfb198dc797
which she verifies:

```
hashsecret 682ce3627c72a6ddd8cc9e38b8e0c935bd7727ad3c6bd601f275ccfb198dc797

f21a8a0ac0fb5939be60e1abc3bcc81c3149e57c6a1f3c8010522abe79dbd780
```

At this point the only commitment txs that can be used without
penalty are the most recent ones.

Of course, Bob could also send fraenks to Alice in an analogous
way by reversing the roles of Alice and Bob above.

## Closing a payment channel

A payment channel can be closed by either party at any time without
penalty by sending the most recent commitment tx.  The party that
sends the commitment tx will need to wait 28 blocks to retrieve the
funds from the asset controlled by the htlc address.

Alternatively, the two parties can always sign a new tx spending both
the fund assets with the latest balances directly to p2pkh addresses
controlled by the two parties.

In this example suppose Alice tries to take back the 0.3 fraenks by
signing and publishing the original commitment tx.  She completes
the signature for the original commitment tx and sends it.

```
signtx e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf301ad41016a74ca54a3eca9693b0ebdcff9b07aca5bd9f61f9b9cbb4ff14eaf17064bb0a5e4f17f0f4a6cf853e57ac52c35024f18b3f341f1254b0bc4badc26da8747eb57b598d1b3c7def0491e951a72a047b92e9b624da9bcb0f0e4160df7841f9ba0f08e58d7ca7eae1825f4fb061f16bf2cd86cc881c09b3634597c7477acf3dd270c0db424f6d8c9fb47974ab32dcd93adb197040892a959fd525d1d00 ...

e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf301ad41013a46495923e9da46c11ad59f707758a70afb63464c9cbe69cfff719bf4adfbb7f7913da543f6a812f7ece52689a2dc9c15b5cab4918d27afcb9d36dbf87917fa36ad3035f0a288890705a8d129538db2a7a6ed38f43ee7c3ea296f65db7f6c72ee3ec53bbd5e182cc19692c7ff3d28b1e14f95eb15b3d4083c61ccce07c5972c2d10eb729b681f1ead5fd56246cf1e7bc32779546ac8811ee5ba6c8a35a5f2c2c2935b34dc137e6c82c23b625d2bfbb96294d0ef1b7c58fcb260b32107026fdad064f1d1ddb1ce779f3034d092d86327ef1f5d2acdb6344fb6c65e122048a666f54b75b5060548383edbd4d91927043818a97dd93ea3d69dec7b78f5cc15a57646ca3be5d79bb673b317597169d2f0180d9b2c0c3bb7d5cbd3c5628eec3fa6f2e729bbda8c8a55b7f1fe41efbf151b14a046a74c35ca9e9ab6e3d0fb9c0faba7bc956dffb1c9b9fb14eff47a61b0045b4a1efff7a0c4863f55ae57cc5223f084313b1f145fb2b440accb6da27d78b47e558b193d7bec0d9fe451a921077a94ebb229d694ca0b0b4f6ed1704ff8b1090aef8875adece78a5142bf6ff061f1cb82cd861c08bc694393c54777c73adf7dc2d0404b628f9dbc7f74a934dbd23cd91a7b498020999ad52fd515

sendtx e384b059391610297e050d42f7b46d66631aa29d841a50993bf957f9b70105d1d55066af616cd5099e2ffb440c5474cfb018b1e91c2784cdcab18048f12b6810baa76d331bd310ed242da0b30c0fb405918711b7bf2c36ccee2407d728acc175eddffee892417039be7142d8ac1c0b8814bf8206a17bda36b3310dd14ebede8316a0384efac818a6f68366c17e27560a020a00000000000000000000000df7aec180e384b059391610297e050d42f7b46d66631aa29d6cab2de41814dbaba6ec33f529498af20cc5ef281f0000000000000000000000129e2cf301ad41013a46495923e9da46c11ad59f707758a70afb63464c9cbe69cfff719bf4adfbb7f7913da543f6a812f7ece52689a2dc9c15b5cab4918d27afcb9d36dbf87917fa36ad3035f0a288890705a8d129538db2a7a6ed38f43ee7c3ea296f65db7f6c72ee3ec53bbd5e182cc19692c7ff3d28b1e14f95eb15b3d4083c61ccce07c5972c2d10eb729b681f1ead5fd56246cf1e7bc32779546ac8811ee5ba6c8a35a5f2c2c2935b34dc137e6c82c23b625d2bfbb96294d0ef1b7c58fcb260b32107026fdad064f1d1ddb1ce779f3034d092d86327ef1f5d2acdb6344fb6c65e122048a666f54b75b5060548383edbd4d91927043818a97dd93ea3d69dec7b78f5cc15a57646ca3be5d79bb673b317597169d2f0180d9b2c0c3bb7d5cbd3c5628eec3fa6f2e729bbda8c8a55b7f1fe41efbf151b14a046a74c35ca9e9ab6e3d0fb9c0faba7bc956dffb1c9b9fb14eff47a61b0045b4a1efff7a0c4863f55ae57cc5223f084313b1f145fb2b440accb6da27d78b47e558b193d7bec0d9fe451a921077a94ebb229d694ca0b0b4f6ed1704ff8b1090aef8875adece78a5142bf6ff061f1cb82cd861c08bc694393c54777c73adf7dc2d0404b628f9dbc7f74a934dbd23cd91a7b498020999ad52fd515

69b0d70e3131d9e7396a1317315dd92f16205ae24ea2c0288a888bf8535fe6d4
```

The tx 69b0d70e3131d9e7396a1317315dd92f16205ae24ea2c0288a888bf8535fe6d4
was confirmed in Block 1344. Afterwards, the assets held at the fund
address dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH were:

```
dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH:
1a3daf5e44b7e66184f0a122ba37f7cd4a704dc9b46f7df300f19c33bea46bc0: (id e6cf7e1745c9a09b3d418d0cea49a6e70403e3bc5367d6ec7467068bff94752b) [1344] Currency 0.3999 fraenks (39990000000 cants)
33d5dd4200172f674d69b38ef2c26f8139a5a04d5fa820bc2e696b39faec5414: (id 9e2eef7b27a0387f09cd5152f316798d514acbb6700c2643dfc96fc7d352e5fd) [1344] Currency 0.5999 fraenks (59990000000 cants)
```

The asset with 0.3999 is controlled by Bob:

```
query 1a3daf5e44b7e66184f0a122ba37f7cd4a704dc9b46f7df300f19c33bea46bc0

{"response":"known","dbdata":[{"type":"asset","assethash":"1a3daf5e44b7e66184f0a122ba37f7cd4a704dc9b46f7df300f19c33bea46bc0","assetid":"e6cf7e1745c9a09b3d418d0cea49a6e70403e3bc5367d6ec7467068bff94752b","bday":1344,"obligation":{"type":"obligation","lockaddress":"Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61","lockheight":0,"reward":false},"preasset":{"type":"preasset","preassettype":"currency","val":{"cants":39990000000,"fraenks":"0.3999"}}}]}
```

The asset with 0.5999 is controlled by Alice's first htlc address, the
one Bob for which the secret
dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5.

```
query 33d5dd4200172f674d69b38ef2c26f8139a5a04d5fa820bc2e696b39faec5414

{"response":"known","dbdata":[{"type":"asset","assethash":"33d5dd4200172f674d69b38ef2c26f8139a5a04d5fa820bc2e696b39faec5414","assetid":"9e2eef7b27a0387f09cd5152f316798d514acbb6700c2643dfc96fc7d352e5fd","bday":1344,"obligation":{"type":"obligation","lockaddress":"das7ySGhRBNGSVsAepmAuWV6gGkAUfUG5V","lockheight":0,"reward":false},"preasset":{"type":"preasset","preassettype":"currency","val":{"cants":59990000000,"fraenks":"0.5999"}}}]}
```

As a result Bob can spend both assets. He needs to spend the second
asset within 28 blocks since Alice would be able to spend it after 28
blocks.

```
createtx '[{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"e6cf7e1745c9a09b3d418d0cea49a6e70403e3bc5367d6ec7467068bff94752b"},{"dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH":"9e2eef7b27a0387f09cd5152f316798d514acbb6700c2643dfc96fc7d352e5fd"}]' '[{"addr":"Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61","val":0.9997}]'

e384b059391610297e050d42f7b46d66631aa29d347ff6bb284a06ddec096a64504f323d271818e79d3ab366a73b3358fca7ac5b192784cdcab18048f12b6810baa76d331bd310eda4a7cbfbde0928ce5f427394d4bc455e6394d2b22d1c83c9d077f2dbf1b45479bf6cb5851c83627bd5947da63e2549519ea1f81de5030000c085512b092000
```

To sign the tx, Bob needs the redeem script for Alice's first htlc
address.  Bob can easily regenerate this using the secret. (In fact,
the redeem script can be recovered with only the hash of the secret
and this is done internally by the `verifycommitmenttx` command. If a
user wanted the redeem script without the secret, the code could be
modified to print the redeem script when running
`verifycommitmenttx`.)

```
createhtlc Dcp7kSjoi2K6P51WcBLsxkev3bfw3vFN61 DkNjYGHA4YW4BuRYt7PtysjAjXM5tsbuur 28 relative dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5

P2sh address: das7ySGhRBNGSVsAepmAuWV6gGkAUfUG5V
Redeem script: 6382012088a8248ad7d92719348ef501cd5aedd92067d1d92067d1ca6ea3c1fbb4f62a60c058c380e7563f8876a9145b6d21c7a0d85e35659fa94f49529467287e47f967041c000000b27576a914ae6132feaef7e823a692a16e35bb1ee2f8b792c86888ac
Secret: dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5
Hash of secret: 8ad7d92719348ef501cd5aedd92067d1ca6ea3c1fbb4f62a60c058c380e7563f
```

Bob signs and sends the tx, taking all the funds.

```
signtx e384b059391610297e050d42f7b46d66631aa29d347ff6bb284a06ddec096a64504f323d271818e79d3ab366a73b3358fca7ac5b192784cdcab18048f12b6810baa76d331bd310eda4a7cbfbde0928ce5f427394d4bc455e6394d2b22d1c83c9d077f2dbf1b45479bf6cb5851c83627bd5947da63e2549519ea1f81de5030000c085512b092000 '["<BobPrivKey>"]' '["6382012088a8248ad7d92719348ef501cd5aedd92067d1d92067d1ca6ea3c1fbb4f62a60c058c380e7563f8876a9145b6d21c7a0d85e35659fa94f49529467287e47f967041c000000b27576a914ae6132feaef7e823a692a16e35bb1ee2f8b792c86888ac"]' '["dd522ff9de8d2e7ae0ec1b97ce0bd3d2052e24e927263c8bd157be1c20190cc5"]'

e384b059391610297e050d42f7b46d66631aa29d347ff6bb284a06ddec096a64504f323d271818e79d3ab366a73b3358fca7ac5b192784cdcab18048f12b6810baa76d331bd310eda4a7cbfbde0928ce5f427394d4bc455e6394d2b22d1c83c9d077f2dbf1b45479bf6cb5851c83627bd5947da63e2549519ea1f81de5030000c085512b09a051df8c0904a004bbf584a68d462aba3f3916bb042834c17d1eab7bb4d8a4a1093819e71e815a951fae5344ec507e3c229ba0d3bd465f80eec22959c2784fe21d6831758d0a0d59aa72ef6c1cb0bfdb5e60b5dd4fea396917e8b0fbfc1000fe86cb324327aa1016300a93a4bab4c1d95449bb424dbcc0f206ea9c5b8b47515152fa1a1420dcf6ea0d1a644f7420d5aaedb7fa450f33fd6989cb6522ce1e9562f8b9e617efd5195ca47e8dbb3dbe9d9afaafd9e3a45b2e1cb8bcee45d7bff937dccb7aa3fbbace83226e0bb47ac881c09b3634597c7477acf3dd270c0db424f6d8c9fb47974ab32dcd93adb197040892a959fd20bb4b7d99df7b63977ac1b3df78d9f9c2e9d20bba2499fea4499e8ba35ff5cd11644686c5a3322d3b56704090885193547c3dfbc98c341dd70fd85c6bfbec20cf46cf0ef26c74e56e4707df4fdbbe4ab0c0b10e077cdeea4fc4765353dcda36e471d0d8bdd62cfb39f5d3a452299f45e9f768feb3043902040850765dbba929ba0e2bd3bfebfbe8479a961cda6dcddd1ec5e3bf2d19395ac4ac00

sendtx e384b059391610297e050d42f7b46d66631aa29d347ff6bb284a06ddec096a64504f323d271818e79d3ab366a73b3358fca7ac5b192784cdcab18048f12b6810baa76d331bd310eda4a7cbfbde0928ce5f427394d4bc455e6394d2b22d1c83c9d077f2dbf1b45479bf6cb5851c83627bd5947da63e2549519ea1f81de5030000c085512b09a051df8c0904a004bbf584a68d462aba3f3916bb042834c17d1eab7bb4d8a4a1093819e71e815a951fae5344ec507e3c229ba0d3bd465f80eec22959c2784fe21d6831758d0a0d59aa72ef6c1cb0bfdb5e60b5dd4fea396917e8b0fbfc1000fe86cb324327aa1016300a93a4bab4c1d95449bb424dbcc0f206ea9c5b8b47515152fa1a1420dcf6ea0d1a644f7420d5aaedb7fa450f33fd6989cb6522ce1e9562f8b9e617efd5195ca47e8dbb3dbe9d9afaafd9e3a45b2e1cb8bcee45d7bff937dccb7aa3fbbace83226e0bb47ac881c09b3634597c7477acf3dd270c0db424f6d8c9fb47974ab32dcd93adb197040892a959fd20bb4b7d99df7b63977ac1b3df78d9f9c2e9d20bba2499fea4499e8ba35ff5cd11644686c5a3322d3b56704090885193547c3dfbc98c341dd70fd85c6bfbec20cf46cf0ef26c74e56e4707df4fdbbe4ab0c0b10e077cdeea4fc4765353dcda36e471d0d8bdd62cfb39f5d3a452299f45e9f768feb3043902040850765dbba929ba0e2bd3bfebfbe8479a961cda6dcddd1ec5e3bf2d19395ac4ac00

07e74c6e0d8340afb5ba3b99d7d9d03160e512935fd17b0d5acac12107f7ba15
```

This tx was confirmed in Block 1347.

## Layer 2

Creating, updating and closing payment channels by hand as above
can be done, but is tedious. The intention is that a second
process (implementing a "Layer 2" over Dalilcoin) would call
these commands and manage payment channels. In principle this
"Layer 2" could also combine payment channels into a lightning
network like the one currently active on the Bitcoin network (Poon Dryja 2016).

Many of the commands above can be given the keyword `json` as a last
argument and the output will be in json format.  A second process
could call the commands with this keyword when appropriate.

## References

[BIP0065] https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki

[BIP0068] https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki

[BIP0112] https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki

[BIP0113] https://github.com/bitcoin/bips/blob/master/bip-0113.mediawiki

[Poon Dryja 2016] Joseph Poon, Thaddeus Dryja. The Bitcoin Lightning
Network: Scalable Off-Chain Instant Payments
https://lightning.network/lightning-network-paper.pdf
