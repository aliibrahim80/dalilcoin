# Publishing formal mathematics

The *raison d'être* of Dalilcoin is the support of formalized
mathematics. The first formalized mathematics documents were published
into the Dalilcoin chain after the July 2019 hard fork.  Here we
summarize those publications and walk through how new users can create
and publish more. Some unproven propositions currently have bounties
and the first person to prove the proposition (or its negation) will
be able to claim the bounty.

## Three kinds of publications

Dalilcoin supports three kinds of publications: theories, signatures
and documents.  Theories declare certain primitives to have certain
types and declare certain axioms to hold in the theory.  Every
signature and document is relative to a theory (possibly the empty
theory which need not be published).  A signature declares certain
objects (of a given type) defined in a previous document, certain
definitions and certain known propositions so that these are
accessible to signatures and documents that import the signature.
Documents make definitions and prove theorems.  Documents are expected
to be the main form of publication.  When a document is published, the
publisher declares owners of new objects (determined by new
definitions) and owners of new propositions (determined by proofs of
previously unproven propositions). These ownership assets are stored
in term addresses in the ledger.  When checking correctness of new
documents, we need access to the theory and any signatures imported by
the document, but we do not need previous documents -- it is enough to
inspect the term addresses in the ledger to know if a definition was
previously made or if a proposition was previously proven.

Publication of new theories and signatures is expected to be rare,
especially since checking a new signature or document requires access
to all theories and signatures published before.  In order to
discourage publishing too many theories and signatures, txs that
publish a theory or signature must burn a significant amount of
fraenks. Specifically, 0.021 fraenks must be burned for each byte
consumed by the serialization of a theory or signature. Since there
will never be more than 21 million fraenks, no more than 1GB of
theories and signatures (combined) may be published. It is likely that
less than 10 million fraenks will ever exist, leading to an even
greater restriction.

## Publication stages

In terms of Dalilcoin assets, publications are assets that fall into
one of the three preasset cases of TheoryPublication, SignaPublication
or DocPublication (see the `assets` module).  In order to support
creating the txs that create such assets, there are commands
`readdraft`, `addnonce`, `addpublisher`, `commitdraft` and
`publishdraft`. Each of these commands operate on text files
specifying the content of a draft publication. The command
`commitdraft` is used when a draft is ready to be published with no
further changes and `publishdraft` is used to publish after the
commitment has matured (after a day on average).

### Writing a draft

A draft file is either a theory file, a signature file or a document
file and this is indicated by the first line of the file which must
either be

* Theory

* Signature Empty

* Signature *theoryid*

* Document Empty

* Document *theoryid*

The *theoryid* is a 32 byte hash value given in hex and identifies a
previously published theory. Alternatively, "Empty" indicates that the
signature or document is in the empty theory. The empty theory
corresponds to intuitionistic higher-order logic with no
extensionality principles.

The rest of the file is a list of items.

#### Items for a theory file include:

* Base *string*

* Prim *string* : *type*

* Def *string* : *type* := *term*

* Axiom *string* : *term*

Types can refer to a base type i for each i=0,1,2,...  and each Base
item gives a local name to the next type.  Likewise, terms have a
primitive constant i for each i=0,1,2,...  and each Prim item gives a
local name and a global type for the primitive. In the empty theory,
no primitive constant is well-typed. In a published theory only
finitely many primitive constants will be well-typed.  Definitions
give abbreviations for terms written after the definition and axioms
declare that certain propositions are proven by in theory in advance.

#### Items for a signature file include:

* Include *signatureid* [ *string* * ] [ *string* * ]

* Base *string*

* Let *string* : *type* := *term*

* Def *string* : *type* := *term*

* Param *objectid* *string* : *type*

* Known *string* : *term*

An Include item indicates that the signature with the given id should
be included before checking the rest of the signature, along with any
signatures included by that signature. Likewise, when this new
signature is included all included signatures will be exported. The [
- ] delimited list of strings give local names to the objects imported
and known propositions imported.

Base items are used to give local names to base types of the theory,
in order.  These items have no actual effect on the signature created
by the draft.

Let items give local names to certain typed terms to ease writing
later terms.  These items have no actual effect on the signature
created by the draft.

Def items export a definition transparently so that publications that
import the signature can refer to the definition and consider it
automatically expanded away for the purposes of proof checking.

Param items use a string (associated with a type) to refer to some
object defined previously in the theory. This is a way of exporting a
defined object opaquely -- without the ability to use its definition
during proof checking.

Known items give a local name to a proposition already proven in the
theory.

#### Items for a document file include:

All the items allowed in the signature file and

* Thm *string* : *term* := *proof*

* Conj *string* : *term*

A Thm item is used to prove new propositions within the theory.  A
Conj item declares a proposition to be a conjecture.  Such a
declaration is useful if one wants to put bounties on a proposition.

### Additional information in a draft

Every draft must also include a "nonce" and a "publisher address"
before the author can commit or publish the draft. The purpose of the
nonce is to ensure that the hash of the draft reveals no information
about the draft.  The publisher address is a p2pkh (or p2sh) address
that must sign the tx that publishes the draft. The reason the
publisher must sign this tx is because the tx that publishes a draft
will often also create ownership assets for new objects and new
propositions. It is vital that the ownership details be determined by
the author of the document in a way that cannot be changed by third
parties before the draft is published in a block.

The command `addnonce` can be used to generate a random nonce and at
it to the file.

The command `addpublisher` can be used to add a publisher address to
the file.

Since theories have new axioms, they will declare owners of the
corresponding propositions. In addition, documents often make new
definitions and prove new theorems. In theory and document files there
may be additional items of the form:

* NewOwner *string* *payaddr*

* NewRights *string* *payaddr* *fraenks*

* NewRights *string* *payaddr* None

A *payaddr* is either a p2pkh or p2sh address.

In each such item the *string* refers to the local name of the
definition, theorem or axiom. The local name is unrelated to where the
ownership assets will be stored in the ledger. The ownership assets
are stored at term addresses that are uniquely determined by the
Merkle root of the internal nameless representation of the relevant
terms.

NewRights indicates conditions under which future documents can use
the object (as an opaque Param import) or the proposition (as a Known
import). If 0 fraenks are indicated, then the object or proposition
will be free for other to use, or even to include in a signature.  If
a positive number of fraenks are indicated, then rights need to be
purchased (by paying to the given *payaddr*) to use the object or
proposition.  Those rights can be purchased in advance (creating
*rights* assets) or purchased on-the-fly by the tx that publishes the
document.  If "None" is given, then no one can use the object or
proposition without redefining the object or reproving the
proposition.

By default, the new owner of each new object or new proposition will
be the publisher address of the draft. In addition, by default, no
rights will be required for others to use the object or proposition.
If an author wishes to use something other than the default, the
author must add the NewOwner and NewRights items to the file by hand.
The `readdraft` command will tell the user which new objects and
propositions will be given owners and suggest adding NewOwner and
NewRights items for these.

```
readdraft *draftfile*
```

Ownership assets can be inputs and outputs of txs and so the rights
policy can always be changed by the owner. In addition, ownership can
be transferred by such txs.

If a conjecture is declared in a document, a Bounty item can be given.
This bounty will be funded by the tx that publishes the document.

* Bounty *string* *fraenks*

Bounties can also be added to proposition addresses later in order to
encourage work on resolving the conjecture.

### Committing to a draft

Once a draft is ready to be published, the publisher (i.e., the
controller of the publisher address) must first publish a
"commitment".  This is a Marker asset whose sole purpose is to
indicate that the contents of the draft, along with the nonce and
publisher address, were fixed as of the time of the commitment. The
command `commitdraft` is used to create the tx to publish the
commitment. This new tx is put into a file. Creating this tx requires
the wallet to have some currency asset sufficient to cover the tx fee.

```
commitdraft <draftfile> <committxfile>
```

The command `validatetxfile` can be used to ensure that the tx created
is valid and fully signed.

```
validatetxfile <committxfile>
```

If it is not fully signed, then `signtxfile` can be used (possibly
with extra information like private keys) to create a file with the
signed tx.

```
signtxfile <txfilein> <txfileout>
```

Once the tx is valid and fully signed, `sendtxfile` can be used to
send it to peers.

```
sendtxfile <txfile>
```

### Publishing a draft

After a commitment has been confirmed for 4 blocks (a day on average),
then the draft can be published. The publication tx will spend the
commitment Marker asset and the Marker asset can only be spent if the
data in the tx matches the commitment.  This prevents a third party
from seeing the published but unconfirmed draft and simply changing
the publisher address and confirmed their malleated publication first.

The draft is published by calling publishdraft to form the publication
tx in a new file.

```
publishdraft <draftfile> <publishtxfile>
```

Once the tx is created, `validatetxfile` and `sendtxfile` (and
possibly `signtxfile`) can be used to send the tx file to peers.

Publishing a document requires the wallet to have an asset sufficient
to cover the tx fee. This fee will be higher than the fee for most txs
since documents will tend to have more data than simple currency txs.
Publishing a theory or signature further requires the tx to burn a
significant amount of fraenks, and the wallet must have a single asset
sufficient to cover this. The command `sendtoaddress` can be used to
consolidate several currency assets into one asset in advance if
required.

Currently blocks are limited to 500M, so drafts larger than this
cannot be published. In addition, if a document creates many new
objects and propositions, then the tx to publish the document may be
too big since it must include new ownership assets. In general,
authors should exercise good judgment about the sizes of drafts before
trying to publish them.

## Types, terms and proofs

The ocaml types for types, terms and proofs are found in the `logic`
module and are named `stp`, `trm` and `pf`.  In the text files these
are given in a simple prefix notation, using local names to stand in
for constants, variables, definitions and known propositions. While
these can be written by hand, the intention is that they are forms
that various interactive theorem provers could produce automatically.

### Types

Types are simple types extended by (explicit) prefix polymorphism.
They are specified in text as follows:

* Prop

This is the type of propositions.

* TpArr *type* *type*

This is the usual arrow type from a domain type to a codomain type.

* TpAll A *type*

This forms the polymorphic type by binding the type variable A.

* *string*

Here *string* must be the name of a type variable in context or the
name of a base type.

### Terms

Terms are simply typed lambda terms. All terms must be given in
beta-eta normal form in drafts. Here are the forms of terms given in
the text format for drafts:

* Ap *term* *term*

Apply a term to a term. The first term must have a function type and
the second term must have the domain type of the function type.

* Lam *string* *type* *term*

This forms the lambda-abstraction binding the named variable in the
term, where the variable is assigned the given type in the body.

* Imp *term* *term*

Both terms should be propositions (terms of type Prop) and the
resulting term is the implication.

* All *string* *type* *term*

The term must be a proposition and the result is the universally
quantified proposition given by quantifying over the given variable
(ranging over the given type).

* TTpAp *term* *type*

This applies a polymorphic term to the given type.

* TTpLam *string* *term*

This lambda binds a type variable in a term in order to make a
polymorphic definition.

* TTpAll *string* *term*

This universally quantifies over a type variable to form a polymorphic
proposition.

* Prim *int*

This refers to a specific primitive constant term by giving an
integer. Prim *int* usually only occurs in a "Let" declarations to
give more readable names to the primitives.

* *string*

Here the string is either an typed object named already (either as a
Let, Param or as a definition) or is the name of a local variable.

### Proofs

Proofs are Curry-Howard style proof terms, and so are also a kind of
lambda term. Here are the forms of proofs given in the text format in
drafts:

* PrAp *proof* *proof*

Apply a proof to another proof. The first proof must prove an
implication and the second proof must prove the antecedent of the
implication.

* TmAp *proof* *term*

Apply a proof to a term. The proof must prove a universally quantified
proposition and the term must have the type of the quantified
variable.

* TpAp *proof* *type*

Apply a proof to a type. The proof must prove a polymorphic
proposition.

* PrLa *string* *term* *proof*

Form a lambda abstraction to prove an implication where the body of
the lambda is a proof of the conclusion where the string locally
refers to the antecedent as a hypothesis.

* TmLa *string* *type* *proof*

Form a lambda abstraction to prove a universal proposition
(quantifying over the given type) where the body of the lambda is a
proof of the body of the universal quantifier and the string gives a
local name for the variable of the given type.

* TpLa *string* *proof*

Form a lambda abstraction proving a polymorphic proposition
quantifying by abstracting over the given type variable.

* *string*

The string must refer to something known (either an imported known or
a previously proven proposition) or to a local hypothesis.

## The first publications
