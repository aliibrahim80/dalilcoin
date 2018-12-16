* Overview of the consensus algorithm for dalilcoin:

The consensus algorithm of dalilcoin is a combination of
Proof-of-Stake (POS) and Proof-of-Burn (POB). For each dalilcoin
currency asset in a user's staking wallet at each second of time,
there is a chance that the asset can stake at that time.  The asset
can be made more likely to stake by burning a certain amount of
litecoin.

When a node stakes a dalilcoin block, the dalilcoin node first
constructs the block and computes its id (the hash of the header), and
then publishes the id in a litecoin transaction, along with a
reference to the previous litecoin burn tx. More precisely a litecoin
tx is created where the txid begins the two bytes 4461 (in hex) and
the first output of the litecoin tx is an OP_RETURN with the following
information:

<previous dalilcoin block (32 bytes)> <previous litecoin burn tx (32 bytes)> <optional nonce bytes>

The OP_RETURN may burn a nonzero amount of litecoins. It is easier to
stake with a dalilcoin asset if more litecoins are burned in the
corresponding OP_RETURN. In order to make the litecoin txid start with
4461, some extra nonce bytes may be included in the OP_RETURN after
the data above.  This extra data is ignored by dalilcoin.  In order to
ensure the litecoin txid begins with 4461 when confirmed, all utxos
spent in the transaction should be segwit utxos (as otherwise third
parties could malleate the transaction making it an invalid dalilcoin
burn simply due to its txid). Another condition to be a valid
burn tx is for the timestamp of the dalilcoin block (determined by
the second when the asset is allowed to stake) must be no larger
than the median time of the litecoin block in which the corresponding
burn tx was confirmed.

Since staked nodes are published as litecoin transactions, there is no
possibility of nodes staking in secret.  This prevents long range
attacks possible in pure POS.

After the litecoin tx confirms, the new dalilcoin block is published
to the dalilcoin peers. The priority of dalilcoin blocks is determined
by the order of corresponding confirmed litecoin txs.  If multiple
litecoin txs corresponding to multiple dalilcoin blocks are confirmed
in the same litecoin block, then these are multiple equally possible
dalilcoin chain tips. The most likely possibility in this case is that
one of these chain tips will be built upon and have its corresponding
litecoin tx confirmed before any other.  Recent dalilcoin blocks (up
to the past week) can be orphaned, but only if no one builds on top of
one of the current chain tips. Any chain that builds on top of the
current chain tip is automatically the best chain tip.  If no
dalilcoin blocks (with corresponding litecoin txs) are published within
a week, then the dalilcoin block chain has reached a terminal state
and can only be rescued via a hard fork.

* How to set up Dalilcoin staking in 11 steps:

1. Locally run a litecoin (LTC) node with txindex=1 and RPC support.
An easy way to do this is use litecoind -daemon where your
litecoin.conf file contains lines like the following:

```
server=1
txindex=1
rpcuser=litecoinrpcusername
rpcpassword=replacewithrealpassword
rpcallowip=127.0.0.1
```

2. Create ltc segwit address and send some litecoin to it. Even if you will
only only burn 0 litecoins, you will need at least one segwit utxo in order
to create the burn txs and pay litecoin tx fees. In recent versions of
litecoin, the following command (using either litecoin-cli or the console of
litecoin-qt) can be used to obtain a p2sh-segwit address. It is possible
a bech32 address would also work, but this has not been tested.

```
getnewaddress "" "p2sh-segwit"
```

We will call this address "yourltcsegwitaddress" below. You should send
some litecoins to this address. A small amount (0.01 litecoins) will be
sufficient if you do not plan to burn a significant amount of litecoins
to create burn txs for dalilcoin.

3. Compile and prepare dalilcoin.

First you will need some dependencies like ocaml, make and curl.

sudo apt-get install build-essential ocaml curl

Once the dependencies are installed, compiling dalilcoin
usually involves executing two commands in the dalilcoin directory:

```
./configure
make
```

One of the things the configure script does is create a .dalilcoin
subdirectory of your home directory (if one does not exist).
You should create a dalilcoin.conf file including lines like the following:

```
ltcrpcuser=litecoinrpcusername
ltcrpcpass=replacewithrealpassword
ltcrpcport=9332
ltcaddress=yourltcsegwitaddress
```

4. Download the initial database for the airdrop. The file db.tgz is available here:

```
https://mega.nz/#!waQE1DiC!yRo9vTYPK9CZsfOxT-6eJ7vtl3WLeIMqK4LAcA2ASKc
```

After downloading this file, place it in your .dalilcoin directory
and extract it there:

```
tar xzf db.tgz
```

5. Start dalilcoin, connect to the network. Dalilcoin can be run as a
single process with a console interface (dalilcoin) or as a daemon
(dalilcoin -daemon) where interaction is then done via a command line
interface (dalilcoincli). We will use the console interface here:

dalilcoin

```
Initializing the database...Initialized.
Initializing theory and signature trees.
Initializing blocktree
...
>
```

This will spend some time communicating with the local litecoin node
and dalilcoin peers to determine the current state (including
requesting and validating the dalilcoin blockchain so far).

Sometimes before dalilcoin has synced, it will give the message
"No blocks were created in the past week. Dalilcoin has reached terminal status."
Ignore this message if your node needs to sync.

6. Create some dalilcoin addresses. You can either create dalilcoin
addresses with the command 'newaddress' which will generate random
keys and addresses (but *only* if your node has the database and is
fully synced), or you can generate bitcoin keys and addresses
independently (possibly using an HD wallet with which you are already
familiar) and import these keys into dalilcoin.  The second
possibility is the one described here.

Suppose you have 3 fresh bitcoin private keys with corresponding
addresses:

```
KwNZQrNR1jRVX1XGAp1gCKXLei5pWiiZ9jLei7WXzunXopcaqoiy
1Kv7atVx1oVtJmbeQQM8S7ypyPG6HpzUtn
L3EuULFidmPo86L7ay6BTfv5uaEmki4UkV5tq6cqd8CeLTfL6uDY
1Mx9taDisSYuwwxjsaGJkjdAgXmu3EDJYy
L5cY4N9MqshnroBDCP8Lcpyg8roYVZB1jAyh37jaEkrZ4nBkfvVA
1EQfiC3TR5hyovjsGJZKyk8X5a5cbomyGB
```

You can import these into your dalilcoin wallet as follows:

```
> importbtcprivkey KwNZQrNR1jRVX1XGAp1gCKXLei5pWiiZ9jLei7WXzunXopcaqoiy
Imported key for address DoPp7Fjt2Ps3fCvLAQg1U1RDV2FLJxbE1q
> importbtcprivkey L3EuULFidmPo86L7ay6BTfv5uaEmki4UkV5tq6cqd8CeLTfL6uDY
Imported key for address DqRrQwTet2v5JPHRdabBnd4ZCAm91YUhAB
> importbtcprivkey L5cY4N9MqshnroBDCP8Lcpyg8roYVZB1jAyh37jaEkrZ4nBkfvVA
Imported key for address DhtNEZHPRg59AN4Z2JtD1dZubD4riLH2yo
```

You can use printassets to verify that your dalilcoin node currently has
no assets. If your node does not have the full ledger information
(is not fully synced), then it may request some ledger information from
peers. In this case, you may need to call printassets more than once
to get the full results.

7. Obtain some dalilcoin, possibly splitting and locking it (see
createsplitlocktx below) for staking.  If you are buying or otherwise
obtaining some new dalilcoin, then simply send it to an address in
your wallet (from the previous step).  If you have some dalilcoin from
the airdrop in a p2pkh address, you can use a signed endorsement to
have it controlled by a key in your wallet.  (You can also directly
import the bitcoin private key from the airdrop address, though the
endorsement mechanism means you do not need to trust the dalilcoin
code with a bitcoin private key that once held bitcoins.)

Consider the address DoPp7Fjt2Ps3fCvLAQg1U1RDV2FLJxbE1q whose key was
imported into the dalilcoin wallet in the previous step.
If you have a key to a bitcoin address from the airdrop, you can
use a bitcoin wallet to sign a message of the form

```
endorse DoPp7Fjt2Ps3fCvLAQg1U1RDV2FLJxbE1q
```

with the key for the airdrop bitcoin address. The airdrop bitcoin address
has a corresponding dalilcoin address which you can find using the command
btctodaliladdr. For example,

```
> btctodaliladdr 1Mx9taDisSYuwwxjsaGJkjdAgXmu3EDJYy
Dalilcoin address DqRrQwTet2v5JPHRdabBnd4ZCAm91YUhAB corresponds to Bitcoin address 1Mx9taDisSYuwwxjsaGJkjdAgXmu3EDJYy
```

Once you have the signed endorsement message, you can import this into your wallet
using the importendorsement command:

```
> importendorsement <dalilcoin version of bitcoin airdrop address> <local dalilcoin address> <signature of endorsement message>
...
```

If this was successful, you can now stake with or spend the airdrop coins.

8. Prepare your wallet and decide how to treat dalilcoin rewards.

The address where dalilcoin places rewards is configurable. In some
circumstances when dalilcoin stakes it will place the rewards in the
same address as the staked asset.  Reusing the same address is not
good practice for a few reasons. In general address reuse is a bad
idea. In dalilcoin it is especially a bad idea since each address can
hold at most 32 assets held in a single address at a time. If you try
to stake with an address that already holds 32 assets, staking will
fail.

It is recommended to put each staking reward in a new address and to
have the reward asset locked by an obligation address whose key is
offline. If you do not wish to stake with rewards, then you can also
send the reward to an address whose key is held offline. Each of these
options requires some manual preparation.

In order to support the various options mentioned, keys and addresses
are given classifications in the dalilcoin wallet. Suppose we have
generated 4 bitcoin keys and addresses:

```
L4Nj1PNc562vPK4Hgd6kbHCEG7ajv3C6JueCWAdXHTzbX91Kt7ue
16qwhnJxKtd3kzSzbNWiMVrjKP8LHKk26j
L3D8jwnpq9Ehiuy1SSGJFfBXAbHA17jM2kPtJJShQAmvRvFef466
16ummZ6eyo3kCspGZT5PTg6y1S6TFr7Twk
KyxrTXuTbx8z1WQNptpkrqQbmGXSo9BVfrgKmMkaSRJ3zZRm3kp1
1BoJCvHwwrBGRp3ja6NE4KcDxULPx1wC6U
L5iRtzW8u1aeVej5WNRqWgZtbWGGBfoy2MnCjAZPDfzKEfJGecqF
18RavyJuTRLZ8ksMRFrc4TD3UP474GH3Nx
```

We can import the keys either as "staking" (the default),
"nonstaking", "staking_fresh" or "nonstaking_fresh" as follows:

```
> importbtcprivkey L4Nj1PNc562vPK4Hgd6kbHCEG7ajv3C6JueCWAdXHTzbX91Kt7ue staking
Imported key for address DaKeE9YtLUzD7RmgMNqbPPJ7q27aM9nVHf
> importbtcprivkey L3D8jwnpq9Ehiuy1SSGJFfBXAbHA17jM2kPtJJShQAmvRvFef466 nonstaking
Imported key for address DaPUHvLazPQuZK8xKTQGVZYMX55hKaKMRC
> importbtcprivkey KyxrTXuTbx8z1WQNptpkrqQbmGXSo9BVfrgKmMkaSRJ3zZRm3kp1 staking_fresh
Imported key for address DfGzjHXsxSYRnFNRL6h76D3cU7KdysTXyx
> importbtcprivkey L5iRtzW8u1aeVej5WNRqWgZtbWGGBfoy2MnCjAZPDfzKEfJGecqF nonstaking_fresh
Imported key for address DbuHTLYqU1hiVCC3BGBV6LeRz23M4agNYB
```

If you plan to stake long term, it makes sense to import thousands of
keys into the staking_fresh and/or nonstaking_fresh categories so that
the dalilcoin node always has quick access to fresh addresses.  You
can always move addresses to the "staking" category using the
stakewith command:

```
> stakewith DaPUHvLazPQuZK8xKTQGVZYMX55hKaKMRC
```

You can also import addresses without importing their keys.
Suppose we have generated 2 bitcoin keys and addresses offline:

```
Ky9oJ3SFL2x6iHQ9y7pKhzm8vuWGfKmr4VqGy7xv45UrXmu7oTcW
18BEdi2w8JwCGs21WkuMVezR6sg4LUMF85
KyqvHthE3Zfs9NaKqr95QTByC6NXDkLe1ZoF5avde4dMdTB96PSc
12UkB1aQKfEwFwBbqufac3N4ToueHdinuG
```

We can import the addresses with importwatchbtcaddr and classify them
to have an offlinekey (offlinekey) and possibly be fresh with an
offline key (offlinekey_fresh):

```
> importwatchbtcaddr 18BEdi2w8JwCGs21WkuMVezR6sg4LUMF85 offlinekey
Importing as Dalilcoin address DbewA5Gs8uJMdJLhGmEEXYRocWfJNQyqL3
> importwatchbtcaddr 12UkB1aQKfEwFwBbqufac3N4ToueHdinuG offlinekey_fresh
Importing as Dalilcoin address DVxShNpLLFc6cNWHbuzTdvoSySttRJacLQ
```

Again, if you plan to stake long term and send rewards to addresses
with an offline key, it makes sense to prepare by importing thousands
of addresses in the category offlinekey_fresh.

When a staking dalilcoin node chooses where to send a reward, it uses
three boolean configuration parameters offlinestakerewardsdest
(default 0), stakewithrewards (default 0) and
generatenewrewardaddresses (default 1).

If offlinestakerewardsdest is set to 1 either by having the line

```
offlinestakerewardsdest=1
```

in dalilcoin.conf or by having -offlinestakerewardsdest on the command
line, then dalilcoin will check if there is a watched address in the
wallet classified as "offlinekey_fresh." If there is one, the address
is used and reclassified in the wallet as "offlinekey."

If offlinestakerewardsdest is 0 (the default) and generatenewrewardaddresses
is 1 (the default), then dalilcoin tries to obtain a fresh address to
hold the reward. The address chosen depends on the value of
stakewithrewards.  If stakewithrewards is 0 (the default) and there is
an address in the wallet classified as "nonstaking_fresh" (e.g.,
DbuHTLYqU1hiVCC3BGBV6LeRz23M4agNYB above), then this address is used
to hold the reward and its classification is changed to "nonstaking."
By default your node will not stake with these assets (once they
mature) unless you explicitly change the address's classification
using the stakewith command. If stakewithrewards is 1 and there is an
address in the wallet classified as "staking_fresh" (e.g.,
DfGzjHXsxSYRnFNRL6h76D3cU7KdysTXyx), then this address is used to hold
the reward and its classification will be changed to "staking." In
this case once the reward asset has matured (after 32 blocks) the node
will begin trying to stake with the reward asset. If dalilcoin cannot
find an appropriate fresh address in the wallet, it will attempt to
randomly generate a new key and corresponding address.

The value of stakewithrewards can be set to 1 by adding the line

```
stakewithrewards=1
```

or including -stakingwithrewards on the command line when dalilcoin is started.

The value of generatenewrewardaddresses can be set to 0 by having the line

```
generatenewrewardaddresses=0
```

in the dalilcoin.conf file or having -generatenewrewardaddresses=0 on the command line.

In every case, the default fallback position is to use the address
with the staked asset to hold the reward asset (unless that address
holds 32 assets, in which the chance to stake will be missed).

If you would like to lock the rewards with an offline address,
it is enough to set offlinestakerewardslock to an appropriate
dalilcoin address. This could be done by putting

```
offlinestakerewardslock=DbewA5Gs8uJMdJLhGmEEXYRocWfJNQyqL3
```

in dalilcoin.conf or start dalilcoin with a command line option
-offlinestakerewardslock=DbewA5Gs8uJMdJLhGmEEXYRocWfJNQyqL3.  Of
course, DbewA5Gs8uJMdJLhGmEEXYRocWfJNQyqL3 should be replaced by a
dalilcoin address for which you have the key (offline).

9. Decide whether to stake immediately or lock assets for longer term staking.

The ability of an asset to stake depends on its age and whether it is
locked.

All airdrop assets are unlocked and start with their maximum age, so
it makes more sense to stake with an airdropped asset than spend it.
You can still spend the airdrop asset after staking with it.

All other unlocked assets age slowly and reach their maximum age in
about 4 months.  Reward assets are always locked for 512 blocks (about
4 months) and become mature for staking after 32 blocks.  Once a
reward asset is mature it ages slowly and reaches its maximum age in
about 4 months, just like an unlocked asset.  Other assets intended
for staking should be locked until a certain block height.  All locked
assets have maximum age until the lock height has been reached.  After
the lock height has past, the asset goes back to being new and ages
very slowly reaching the maximum age again in about 12 years.  Of
course, the intention is that after the lock height has past, the
owner of the asset should either spend it or lock it again.
Locked assets cannot be spent until their lock height is reached.

The command createsplitlocktx can be used to spend an asset to be several
assets locked until a certain height.

```
createsplitlocktx <current address> <assetid> <number of outputs> <lockheight> <fee> [<new holding address> [<new obligation address> [<ledger root>]]]
```

An advantage of having more than one locked asset is simply that
the same asset cannot stake two blocks in a row. Thus having
multiple locked assets means there will always be an asset available for staking.

After creating the tx, you can use signtx and sendtx to sign and send the tx.
You can also use validatetx to check that the tx is well-formed.

10. Start dalilcoin with staking enabled. This can be accomplished by
including -staking on the command line or putting

```
staking=1
```

in the dalilcoin.conf file.

The debug.log file will give you information about the staking process.
For example, how many assets are being staked and when the next possibility
to stake will occur unless a new block is staked before then.

11. The command nextstakingchances will tell you next chances you have
to stake, with certain ltc burn requirements.

```
> nextstakingchances
...
```

The command extraburn can be used to temporarily allow the node to
burn a given amount of ltc in order to stake.

```
> extraburn 0.0001
```

You can generally set the burn parameters maxburn (an integer, using
litoshis as the unit) and maxburnrate (an integer, using litoshis as
the unit) to automatically control how much litecoin you are willing
to burn in order to stake.  Also, the parameter ltctxfee sets the
number of litoshis to use as a litcoin tx fee. The default is 10000
(0.0001 ltc), though recent changes to litecoin's policies
seem to make 1000 (0.00001 ltc) sufficient.


