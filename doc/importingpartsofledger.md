# The ledger

Dalilcoin stores the current assets in a large Merkle tree called a
*ctree*. This Merkle tree is a binary tree with a depth of 162, and so
each leaf corresponds to a sequence of 162 bits.  The first two bits
determine the kind of address:

00: p2pkh (pay to public key hash)
01: p2sh (pay to script hash)
10: term (where information about terms and propositions are stored)
11: publication (where publications with formal mathematics are stored)

The remaining 160 bits are determined what is traditionally considered
an address in cryptocurrencies like Bitcoin or Litecoin.

The 162 bits can be thought of as a sequence of 18 9-bit numbers
(0-511).  A *ctree element* is a tree with depth at most 9 where at
level 9 there are hashes (CHash) refering to the Merkle root of the
subtree.  An exception is when some level of the tree only has one
leaf.  In such cases the ctree is CLeaf(bl,hl) where bl gives the
remaining bits and hl gives the contents of the address. In a ctree
element, CLeaf must have the form CLeaf(bl,NehHash(h,l)). This h is
the hash root of an "hcons element" and abbreviates the list of assets
stored at the address and l is an integer indicating how many assets
are on the list.

An *hcons element* is a pair (ah,hr) where ah is the hash of an asset
(which should not be confused with the asset id). hr is either None if
there are no more assets on the list or (k,l) where k is the hash root
of another hcons element and l is an integer indicating how many
assets are on the list.

The full information about a subtree can be given as a sequence of
ctree elements, hcons elements and assets.  These are
stored in the databases DbCTreeElt, DbHConsElt and DbAsset.

## Obtaining the ledger

A database with the initial ledger is available as the file db.tgz
(900MB) at:

https://mega.nz/#!waQE1DiC!yRo9vTYPK9CZsfOxT-6eJ7vtl3WLeIMqK4LAcA2ASKc

A database with the full ledger (and history) up to Block 600 (the end
of 2018) is available as the file db2018.tgz (1GB) at:

https://mega.nz/#!Ab4yTaYb!PvJshyDf7nsXh_ejSAVDGLRCLytgR8griEFNLV1EAWI

There are also 383 smaller files that can be downloaded as needed
available in the folder:

https://mega.nz/#F!oXBmwSRT!JbB4-EHbkz87RuOUMcK9Nw

These files are named dcd6_i where i ranges over 0 to 511,
with some values between 256 and 511 omitted.

Each of these files is the result of exporting parts of the
ledger tree at Block 1337 (August 2019) with ledger root

dcd667f29a315104d4860c5be7122ad1699a1388e34219d1a7b57803accf2abb

The files where i ranges from 0 to 127 correspond to the subtrees
containing p2pkh addresses. This is where most of the assets are since
they have assets airdropped from Bitcoin.

The files where i ranges from 128 to 255 correspond to subtrees
containing p2sh addresses.  Most of these assets are also the result
of the airdrop, but there were significantly fewer funded p2sh
addresses.

The files where i ranges from 256 to 383 correspond to subtrees
containing term addresses (e.g., who owns a term or proposition and by
extension which propositions have been proven).  In two cases (i=262
and i=350) there are not yet enough assets to justify the formation of
a subtree as an element and so no corresponding file was given. The
relevant term addresses will be in the top ctree element and this is
already included in every file.

As of August 2019 there have been very few formal mathematics publications
and so there is only one subtree element with i ranging from 384
to 511, namely, 420.

## Importing parts of a ledger

Suppose we are interested in importing the part of the ledger
containing the p2sh address dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH. We can
find out the general location of this address using the
addresslocation command.

```addresslocation dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH
206:16
```

This indicates the address is in the 206th subelement and the 16th subsubelement.

We can download dcd6_206 from the mega folder above. We can then
import the ctree to the local database as follows:

```importctreeelts "<fullpath>/dcd6_206" dcd667f29a315104d4860c5be7122ad1699a1388e34219d1a7b57803accf2abb
```

After doing this, we can check the assets in the address.  For
example, we can import the address as a watch address in the wallet
and use printassets.

```importwatchaddr dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH
printassets dcd667f29a315104d4860c5be7122ad1699a1388e34219d1a7b57803accf2abb
...
Watched assets:
dTeUT6q7o9JM9Z15qTLn41N37iKu9qGydH:
29e3d2d1113887fefce150d9eecdc885fbb5ffe78f99e9bca3802b8d89fefd19: (id b480ce323cd016441e46dcfeb2d830bb931c5ca3b006d7b57ffba34b06c1e5f8) [1335] Currency 0.4 fraenks (40000000000 cants)
39609328f47bf300d2c2a38e1e54e40f97df611703e70ab7cc74bc6631c8d417: (id 50032a7327ff2aff36a020ba1acaec358cad3ac1f3659f88818aee191623369d) [1335] Currency 0.6 fraenks (60000000000 cants)
...
```

We could further download and import other parts of the ledger as desired.

After importing new parts of the ledger, it is a good idea to
reprocess the block chain since Block 1337 in order to generate new
ctree elements corresponding to the changes in ledger.

```reprocessblockchain 1337
```

## Exporting new subtrees of ledgers

Operators of full nodes may want to occasionally generate new files
for subtrees to help new nodes initialize.
The command `exportctreeelts` can be used for this purpose:

```exportctreeelts "<fullpath>/<fileprefix>_<i>" <ledgerroot> <i>
```

Or a file for a small subsubtree can be generated

```exportctreeelts "<fullpath>/<fileprefix>_<i>_<j>" <ledgerroot> <i> <j>
```

The command `getledgerroot` or `getinfo` can be used to determine the ledger root
of the most recent validated block.

