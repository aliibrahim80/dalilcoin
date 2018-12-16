# dalilcoin
A Formal Library as a Bitcoin Spin-Off

https://www.reddit.com/r/dalilcoin/

Dalilcoin is a fork of Qeditas (qeditas.org) which was designed
to use a blockchain to support a revival of the QED Project.

https://www.cs.ru.nl/~freek/qed/qed.html

Dalilcoin changes the consensus protocol of Qeditas by using Proof of
Burn to anchor Dalilcoin blocks into a Proof of Work blockchain
(Litecoin's).  In addition, the target block time has been slowed
considerably so that Dalilcoin is only expected to average 4 blocks
per day.  Much of the code is due to the Qeditas developers and the
(open source) copyright notices reflect this fact (and must continue
to do so).  Additional copyright notices for Dalilcoin developers have
been added to files changed after the Dalilcoin fork was created in
August 2017.

Dalilcoin is still under development. It can currently only
be used in linux using the command line.

* Installation

```
./configure
make
```

Instead of make, it is also possible to use the makebytecode script:

```
./configure
./makebytecode
```

The configure script can be given a number of parameters.

For example, the default data directory is .dalilcoin in the
user's home directory. This can be changed as follows:

```
./configure -datadir=<fullpathtodir>
```

The configure script will create the data directory if it does not already exist.

* Configuration file

For dalilcoin to run properly, it needs to communicate with a litecoin daemon.

First set up your litecoin.conf file (in .litecoin) to contain the following lines:

```
server=1
txindex=1
rpcuser=litecoinrpcusername
rpcpassword=replacewithrealpassword
rpcallowip=127.0.0.1
```

where of course `replacewithrealpassword` should be replaced with a
serious password (to protect litecoins in your local wallet).
You should put some litecoins in a segwit address in the local wallet.

Now create a a file `dalilcoin.conf` in your dalilcoin data directory.

```
ltcrpcuser=litecoinrpcusername
ltcrpcpass=replacewithrealpassword
ltcrpcport=9332
ltcaddress=yourltcsegwitaddress
```

There are many other configuration parameters you might want to set
in `dalilcoin.conf` (see `src/setconfig.ml`).  The ones above should suffice for dalilcoin
to interact with your litecoin node.

Here are a few examples of other configuration parameters.

If you want your node to listen for connections, give your IP and port
number by setting `ip=xx.xx.xx.xx` and `port=..`. The default port
number is 20805. There is no default IP address, and if none is given
then dalilcoin will not listen for incoming connections. You can have
dalilcoin listen for connections via a tor hidden service by setting
`onion=xxyouronionaddrxx.onion` `onionremoteport=..` and
`onionlocalport=..`.

Connections will only be created over tor (via socks proxies) if
`socks=4` is included in the configuration file.

After putting the dalilcoin/bin/ directory into your PATH,
dalilcoin can be run as follows:

```
dalilcoin
```

This will likely take some time to sync with the ltc node. During this time it is
searching through ltc blocks (after early 2018) for dalilcoin burn transactions
and using this information to determine the candidates for the current dalilcoin blockchain
(from a tree of blocks, including some of which may be invalid or orphaned).

After the initialization, you can issue commands via a console
interface.  These commands may take minutes to execute, so patience is
required.

The command `getpeerinfo` can be used to determine if your dalilcoin node is connected to
others.

```
> getpeerinfo
...
```

The command `getinfo` gives more information about the current state, including the
current best known blocks.

```
> getinfo
...
```

At first getinfo (and other commands) are likely to report that the node is
out of sync, as it takes some time to request and process the dalilcoin
blocks generated so far. When this report is seen, simply wait a while
and try again. Sometimes it helps to exit the node and restart.

```
> exit
```

A full list of commands is available via

```
> help
```

and the help command can be used to ask for information about specific commands.

Dalilcoin can also be run as a daemon and commands issued via a command line interface:

```
dalilcoin -daemon
```

```
dalilcoincli "getinfo"
```

* Obtaining the Initial Ledger Tree

Before running dalilcoin for the first time you have the option
of initializing the database with the initial ledger tree.
The initial ledger tree contains the initial distribution
of Dalilcoin assets and has hash root
d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed.

The full tree is available as the file db.tgz
(900MB) at:
https://mega.nz/#!waQE1DiC!yRo9vTYPK9CZsfOxT-6eJ7vtl3WLeIMqK4LAcA2ASKc

The sha256 hash of this file is
1920e33fdaf3749d6cce55ab0150faf961ef22c5057c92e082c3f6209fb335d5

After downloading the file cd to your Dalilcoin data directory.
Most likely this mean:

cd ~/.dalilcoin

Move the downloaded file to here and untar it. For example:

tar xzvf db.tgz

It will create a db subdirectory with all the necessary information.

If you have already run dalilcoin and already have the db subdirectory,
you should delete the db subdirectory before untar'ing db.tgz.

An alternative to downloading the db.tgz file is to use the command
requestfullledger to try to obtain the full ledger (piece by piece)
from peer nodes on the network. The requestfullledger command is very
slow and should be expected to run for several hours. The command
requestfullledger without an argument will use the block chain to
determine the most recent ledger root and try to find the full ledger
for this root.  Alternatively you can give the ledgerroot as an argument.
For example this call will try to obtain the full initial ledger:

requestfullledger d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed

* Importing Watch Addresses and Viewing Balances

Here is an example of how to view assets from the initial distribution.
This assumes you have a version of the initial ledger tree (see above).

We first import 3 p2pkh addresses and one p2sh address. We give them
as Bitcoin addresses and Dalilcoin prints the corresponding Dalilcoin address.
Dalilcoin p2pkh addresses start with D. Dalilcoin p2sh addresses start with d.

Here is an example:

```
dalilcoin

> importwatchbtcaddr 14M2d3UDXkkTL8AJmTUifUmAEwYo67cW2Q
Importing as Dalilcoin address DXpj9Qi9YM7cgZUzXTobhNCYkaY346HF8q
> importwatchbtcaddr 1LvNDhCXmiWwQ3yeukjMLZYgW7HT9wCMru
Importing as Dalilcoin address DpQ4k4STnJt6kVJLfm4ENSz51kGh8PsQVg
> importwatchbtcaddr 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo
Importing as Dalilcoin address DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy
> importwatchbtcaddr 37GxXLE4tiFEKwsLzNGFdHyjVfUEbj6wt2
Importing as Dalilcoin address dK1EDZZZF3dhqnii52aLqx7bzYNR3hFA5R
```

We next ask Dalilcoin to print the assets held at the wallet addresses
in the current ledger. At the moment (Dec 2018) the ledger is the one
after the block at height 580.

```
> printassets
Assets in ledger with root af6b3b954c20df2a8a7d8ce969671a0aa0af46908d1b4f8a19915eabc896a440:
Controlled p2pkh assets:
Possibly controlled p2sh assets:
Assets via endorsement:
Watched assets:
dK1EDZZZF3dhqnii52aLqx7bzYNR3hFA5R:
0a50e9aaae7517139d3c5c1453adae6499c70218b4ef4695579a953a38753eba: (id 000000000000000000000000ab6f7c6f2d94d3f7a36f39c64b46f4f6d5b492d0) [0] Currency 0.0015 fraenks (150000000 cants)
DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy:
5f3d6ade600b330ba405413935f32cb07dfe904ee52c894c015b8cb27ff702c4: (id 0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824) [0] Currency 0.0015 fraenks (150000000 cants)
DpQ4k4STnJt6kVJLfm4ENSz51kGh8PsQVg:
5f6eea1d442bdc4683aab077382b3aeff14247117dc383dc4b3f07dc721f1081: (id 00000000000000000000000037cfcd67a77ded709ff0b03c1d80ed5fbed8b33f) [0] Currency 0.0015 fraenks (150000000 cants)
DXpj9Qi9YM7cgZUzXTobhNCYkaY346HF8q:
737b615b1608bb483754b4dddc8e1aee692d89d51bd86491cd4c1d69b9bcd2f4: (id 0000000000000000000000000d5edd430a4ffe63fa96dd5c189989bd39b628cb) [0] Currency 0.1 fraenks (10000000000 cants)
Total p2pkh: 0.0000000000 fraenks
Total p2sh: 0.0000000000 fraenks
Total via endorsement: 0.0000000000 fraenks
Total watched: 0.1045 fraenks
```

For each of the imported addresses, there is a currency asset.

By Bitcoin Block 350,000

14M2d3UDXkkTL8AJmTUifUmAEwYo67cW2Q had a balance of 0.1 bitcoins (10000000 satoshis)

Hence the corresponding Dalilcoin address DXpj9Qi9YM7cgZUzXTobhNCYkaY346HF8q
has 0.1 fraenks (10000000000 cants). The number of cants is 1000 times more
than the number of satoshis since Dalilcoin has three extra digits of precision.
(The particular addresses used here came from the Qeditas README
and still contain their airdropped dalilcoin fraenks as of Dec 2018.)

Similarly, by Bitcoin Block 350,000 the addresses
1LvNDhCXmiWwQ3yeukjMLZYgW7HT9wCMru, 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo
and 37GxXLE4tiFEKwsLzNGFdHyjVfUEbj6wt2 had balances of 0.0015 bitcoins
(150000 satoshis).  Consequently, the corresponding Dalilcoin addresseses
have 0.0015 fraenks (150000000 cants) each.

If the node does not have the full ledger tree, Dalilcoin may warn you that some data is missing.
If data is missing and there are connections to peers, the node will try to request the missing data.

* Endorsements

You do not need to import your bitcoin private key to claim your part
of the airdrop.  Claims can be made using bitcoin signed
endorsements. Here is an example:

In the snapshot the Bitcoin address 1Ez6BBUGNrNqahWqF7kqzBp88fkBKfoRmj
had 1.01 bitcoins.  The corresponding Dalilcoin address is
DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf, a fact that can be determined
using the btcdaliladdr command:

```
> btctodaliladdr 1Ez6BBUGNrNqahWqF7kqzBp88fkBKfoRmj
Dalilcoin address DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf corresponds to Bitcoin address 1Ez6BBUGNrNqahWqF7kqzBp88fkBKfoRmj
```

This means that if the Dalilcoin wallet has the private key for the
Bitcoin address 1Ez6BBUGNrNqahWqF7kqzBp88fkBKfoRmj, which is the same
as the private key for the Dalilcoin address
DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf, then the Dalilcoin node can spend
the 1.01 fraenks. Endorsements give a way to spend the 1.01 fraenks
without importing this private key.

Suppose we have a Dalilcoin wallet that includes a private key for the
Dalilcoin address Dr44Xv5NJySSEQ3AfrnDio3HnTWHsrxbya. A Bitcoin
wallet can be used to sign the endorsement message

```
endorse Dr44Xv5NJySSEQ3AfrnDio3HnTWHsrxbya
```

with the key for the address 1Ez6BBUGNrNqahWqF7kqzBp88fkBKfoRmj.

This endorsement can then be imported into the Dalilcoin wallet using
the importendorsement command:

```
importendorsement DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf Dr44Xv5NJySSEQ3AfrnDio3HnTWHsrxbya HPcwtjEomAo6+RW0CGB1yp41fthk+d154Irnkfz9rC47CCIm2Ff5rb7nzDBkmo3X27Hu2XLJaiewzt7BVpOIIRU=
```

This endorsement allows the private key for
Dr44Xv5NJySSEQ3AfrnDio3HnTWHsrxbya to sign Dalilcoin transactions that
would otherwise need to be signed by the private key for
DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf.

The printassets command shows which assets are controlled, including
those controlled by endorsements. Note that one can find the asset
id for each of these assets, which is needed when creating
a transaction with createtx or creategeneraltx.

The commands createtx, signtx and sendtx can be used to spend the
1.01. Here is an example of how the 1.01 fraenks from
DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf could have been split among three
balances: 0.2 at address DhffKwWrRo2F9Y9C4AGcN5aruMvjo5mjmx locked
until block 500, 0.3 at address DYZRqxUh9NM88ZFB7TR8rvCoeppk6CQzr8
locked until block 100 and 0.5 fraenks unlocked at address
Dann5dycvyH7iVfcQvLsANGKghGBkcPQSF, leaving 0.01 fraenks as a
transaction fee. (The unlocked 0.5 fraenks can be spent anytime.  The
locked balances cannot be spent until the right block height is
reached. Locked rewards have more coinage for staking.)  The signtx
command uses the endorsement above and private key for
Dr44Xv5NJySSEQ3AfrnDio3HnTWHsrxbya to sign the transaction.

```
createtx '[{"DiTnhYiCPSjzw8qX185j25FWeJjRL9fErf":"0000000000000000000000003c21ff93ccfb652f62cb181121e942af592b0ab4"}]' '[{"addr":"DhffKwWrRo2F9Y9C4AGcN5aruMvjo5mjmx","val":0.2,"lock":500},{"addr":"DYZRqxUh9NM88ZFB7TR8rvCoeppk6CQzr8","val":0.3,"lock":1000},{"addr":"Dann5dycvyH7iVfcQvLsANGKghGBkcPQSF","val":0.5}]'
<hexofunsignedtx>
signtx <hexofunsignedtx>
<hexofsignedtx>
sendtx <hexofsignedtx>
```

* Checking Balances of Addresses

The query command can be used to obtain information about addresses or
hash values.  In particular using query with an address (Bitcoin or
Dalilcoin) gives a list of the assets at the address. If a Bitcoin
address is given, then the corresponding Dalilcoin address will be
given. The output is given in json format. Here is an example:

```
query 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo
{"response":"bitcoin address","daliladdress":"DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy","info":{"ledgerroot":"af6b3b954c20df2a8a7d8ce969671a0aa0af46908d1b4f8a19915eabc896a440","block":{"block":"5b47277f4383c6543b32978fa3998228b1f9aa482e47788bcfb5242f51094176","height":580,"ltcblock":"818c59d812a79623746806f4904dc3d87879eb76020a06c385394d1429387b0a","ltcburntx":"4461e47cfa8a7c1a02b718ff055a1dc9abb7bfda90cc250abcf554f8bedc2987","ltcmedtm":0,"ltcburned":0},"address":"DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy","total":0.0015,"contents":"0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824 [0] Currency 0.0015 fraenks; coinage 163350000000\n","currentassets":[{"type":"asset","assethash":"5f3d6ade600b330ba405413935f32cb07dfe904ee52c894c015b8cb27ff702c4","assetid":"0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824","bday":0,"preasset":{"type":"preasset","preassettype":"currency","val":{"cants":150000000,"fraenks":"0.0015"}}}]}}
```

The result indicates 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo is a Bitcoin
address with corresponding Dalilcoin address
DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy. The "info" part includes an array
of "currentassets". In this case there is one asset, a currency asset
with value 0.0015 fraenks.

Here is the same example giving the Dalilcoin address to the query command:

```
query DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy
{"response":"daliladdress","info":{"ledgerroot":"af6b3b954c20df2a8a7d8ce969671a0aa0af46908d1b4f8a19915eabc896a440","block":{"block":"5b47277f4383c6543b32978fa3998228b1f9aa482e47788bcfb5242f51094176","height":580,"ltcblock":"818c59d812a79623746806f4904dc3d87879eb76020a06c385394d1429387b0a","ltcburntx":"4461e47cfa8a7c1a02b718ff055a1dc9abb7bfda90cc250abcf554f8bedc2987","ltcmedtm":0,"ltcburned":0},"address":"DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy","total":0.0015,"contents":"0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824 [0] Currency 0.0015 fraenks; coinage 163350000000\n","currentassets":[{"type":"asset","assethash":"5f3d6ade600b330ba405413935f32cb07dfe904ee52c894c015b8cb27ff702c4","assetid":"0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824","bday":0,"preasset":{"type":"preasset","preassettype":"currency","val":{"cants":150000000,"fraenks":"0.0015"}}}]}}
```

* Staking

The node will attempt to stake (minting blocks for a reward) if
`staking=1` is included. Other important configuration parameters for
staking include `maxburn`, `maxburnrate` and `mintimebetweenburns`
which control the amount of litecoin you are willing to burn to stake
a block. By default no litecoins will be burned.  The parameter
`ltctxfee` determines the (fixed) transaction fee that will be used
for burn transactions, denominated in litoshis.  The default is 1,000
litoshis (0.00001 ltc). The command `nextstakingchances` will indicate
the next opportunities the node has to stake, possibly by burning
extra ltc.  The command `extraburn` can be used to dynamically
temporarily increase the number of litecoins you are willing to burn
in order to stake.

When private keys are imported into the dalilcoin wallet, the key can
be marked as staking or nonstaking. Only addresses corresponding to
staking keys will be used to stake.  The commands `stakewith` and
`donotstakewith` can be used to move addresses between these two
classifications. You should not be staking with the address if you
want to be able to spend assets from an address.

For more information about how to stake, see the file doc/howtostake.md.

