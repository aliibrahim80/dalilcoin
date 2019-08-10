# Publishing formal mathematics

The *raison d'être* of Dalilcoin is the support of formalized
mathematics. The first formalized mathematics publications were
confirmed into the Dalilcoin chain after the July 2019 hard fork.
Here we describe how publications can be prepared for publication and
summarize the publications so far.  Some unproven propositions
currently have bounties. In each case the first person to prove the
proposition (or its negation) will be able to claim the corresponding
bounty.

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
declare that certain propositions are proven by in the theory in
advance.

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
signature is included all included signatures will be exported. The 
bracket delimited list of strings give local names to the objects imported
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

A NewRights item indicates conditions under which future documents can
use the object (as an opaque Param import) or the proposition (as a
Known import). If 0 fraenks are indicated, then the object or
proposition will be free for other to use, or even to include in a
signature (effectively making them freely available to everyone
forever).  If a positive number of fraenks are indicated, then rights
need to be purchased (by paying to the given *payaddr*) to use the
object or proposition.  Those rights can be purchased in advance
(creating *rights* assets) or purchased on-the-fly by the tx that
publishes the document.  If "None" is given, then no one can use the
object or proposition without redefining the object or reproving the
proposition in each document where it is needed.

By default, the new owner of each new object or new proposition will
be the publisher address of the draft. In addition, by default, no
rights will be required for others to use the object or proposition.
If an author wishes to use something other than the default, the
author must add the NewOwner and NewRights items to the file by hand.
The `readdraft` command will tell the user which new objects and
propositions will be given owners and suggest adding NewOwner and
NewRights items for these.

```
readdraft <draftfile>
```

Ownership assets can be inputs and outputs of txs and so the rights
policy can always be changed by the owner. In addition, ownership can
be transferred by such txs.

If a conjecture is declared in a document, a Bounty item can be given.
This bounty will be funded by the tx that publishes the document.

* Bounty *string* *fraenks* NoTimeout

* Bounty *string* *fraenks* *lockheight* *payaddr*

Bounties can also be added to proposition addresses later in order to
encourage work on resolving the conjecture.
Before the lock height (if one is given), the only way a bounty asset
can be spent (resulting in a currency asset) is by the owner of the
term address as either a proposition (meaning the conjecture was proven)
or as a negated proposition (meaning the negation of the conjecture was proven).
If a lock height is given, then after the block height passes, the controller
of the given address has the option of reclaiming the bounty by spending
it to a currency asset (without needing to resolve the conjecture).

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

Currently blocks are limited to 500K, so drafts larger than this
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

* TpAll *string* *type*

This forms the polymorphic type by binding the type variable
named by the string.

* *string*

Here *string* must be the name of a type variable in context or the
name of a base type.

### Terms

Terms are simply typed lambda terms. Here are the forms of terms given
in the text format for drafts:

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

Here the string is either an typed object named already (imported from
a signature or declared using Let, Param or Def) or is the name of a
local variable.

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
a theorem from earlier in the document) or to a local hypothesis.

## The first publications

As of August 2019 there have been 10 publications: one theory, two
signatures and seven documents.  The source draft files can be found
in the doc/pubsrc subdirectory. We briefly review these documents in
the order in which they were published, summarizing their contents and
explaining how Dalilcoin commands can be used to create and check
drafts, commit to drafts and publish drafts.

The one theory was directly ported from the foundation file
distributed with
[QeditasEgal](https://github.com/input-output-hk/qeditas-egal).  The
remaining documents and signatures are all within this theory. Most of
the initial documents were ported from selected parts of some of the
formalizations from QeditasEgal. The rest of the documents
introduce some definitions from category theory and make
a number of conjectures about specific categories.

### Theory tgtsd

tgtsd is a theory publication, as indicted by the first line of the file:

```
Theory
```

In general a theory publication specifies the types of some primitives and lists some axioms.
Later documents and signatures are always within a theory, referenced by its theory id.

The specific theory **tgtsd** gives primitives and axioms for the
higher-order set theory distributed with Qeditas-Egal.

The second line of **tgtsd** states that the first base type is called
**set** within the file.

```
Base set
```

Before the primitives and axioms are given basic definitions like like
**not**, **and**, **or**, **iff** (if and only if), **eq** (equality) and **ex** (existential
quantifiers) are defined.

The first axiom says that two propositions *A* and *B* are equal if
both imply each other.

```
Axiom prop_ext : All A Prop All B Prop Imp Ap Ap iff A B Ap Ap TTpAp eq Prop A B
```

A similar axiom says two functions are equal if they give the same
result on all inputs.

The first primitive is a polymorphic choice operator **Eps**
and a corresponding axiom states that **Eps** *P* satisfies *P*
of *P x* holds for some *x*.

```
Prim Eps : TpAll A TpArr TpArr A Prop A
Axiom EpsR : TTpAll A All P TpArr A Prop All x A Imp Ap P x Ap P Ap TTpAp Eps A Lam x A Ap P x
```

The rest of the draft file presents set theory proper.
There is a primitive membership relation locally referred to as **In**.

```
Prim In : TpArr set TpArr set Prop
```

The remaining primitives are set constructors
locally referred to as **Empty**, **Union**, **Power**, **Repl**
and **UnivOf** along with axioms specifying when a set is
a member of a set constructed using one of these primitives.

The file already has a nonce and publisher added using `addnonce` and `addpublisher`.

The theory file was checked to be correctly formed using `readdraft`:

```
readdraft tgtsd

Theory is correct and has id 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de and address PEczb7vcjrc4n7krn9Vmk2BMR1j2z7jvhD.
9.114 fraenks must be burned to publish the theory.
Nonce: 21d9d6871999da6903b83acf7c26b6ecb92b2b4b9b39c170661ebe67dada7dcc
Publisher address: Df9i59wmWtXriTJ7aoottJJ8xBTnS7FNnX
Marker Address: PKLwvPisgLoEv2NSJKsCYEPfdZ4UJf4Xg6
...
```

Note that 9.114 fraenks needed to be burned to publish the theory.
The theory id 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
is used to indicate when a signature or document is in this theory.
Since the theory has already been published, the theory command can
be used to obtain information about it.

```
theory 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de

Theory 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de 7 Prims 13 Axioms:
Ids and Types of Prims:
...
Ids of Axioms:
...
```

When a theory is published, owners are given to the propositions in
the theory.  The `readtheory` command also informs the author about
this in advance, in case the author wishes to include NewOwner and
NewRights items.  There are actually two kinds of ownership: ownership
of the "pure" proposition (shared throughout all theories) and
ownership within the new theory.

```
readdraft tgtsd

...
Pure proposition 'UnivOf_Min' has no owner.
You will be declared as the owner when the document is published with the following details:
New ownership: publisher address.
 (This can be changed prior to publication with NewOwner <defname> <payaddress>.)
Rights policy: free to use
 (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)
Proposition 'UnivOf_Min' in theory has no owner.
You will be declared as the owner when the document is published with the following details:
New ownership: publisher address.
 (This can be changed prior to publication with NewOwner <defname> <payaddress>.)
Rights policy: free to use
 (This can be changed prior to publication with NewRights <defname> <payaddress> [Free|None|<fraenks>].)
...
```

The defaults were used in the **tgtsd** file, meaning the publisher address
Df9i59wmWtXriTJ7aoottJJ8xBTnS7FNnX was given as owner and no special rights
are required to use the axioms in documents.

The commitment tx was formed as follows:

```
commitdraft "tgtsd" "tgtsdcommit"

Completely signed.
The commitment transaction (to publish the marker) was created.
To inspect it:
> decodetxfile tgtsdcommit
To validate it:
> validatetxfile tgtsdcommit
To send it:
> sendtxfile tgtsdcommit
```

The new file tgtsdcommit contained a small tx to publish a commitment
Marker asset at publication address PKLwvPisgLoEv2NSJKsCYEPfdZ4UJf4Xg6.
This address is uniquely determined by the contents of the theory,
the nonce, and the publisher address. If any of these are changed,
the commitment cannot be used to publish the modified version.

After the commitment tx has confirmed, the commitment must age for 4 blocks
(a day, on average) before it can be spent. The only way a commitment marker
can be spent is by a tx creating the publication. If the marker has not sufficiently
aged, `publishdraft` will warn the user and refuse to create the publication tx:

```
publishdraft "tgtsd" "tgtsdpublish"

The commitment will mature after 2 more blocks.
The draft can only be published after the commitment matures.
```

After the commitment matured, `publishdraft` created the tx.  The
theory was published in Block 1223
(562491e89946b1cf744e1acb87fff80c4a718b29716c4946560de7af0c6dc1ff) in
the tx
f9cdfbcae9449fbca9822a3303d578988342fbac7d4656b775c8064bff80114f

### Documents firstdoc and firstdocp1

The first document intended to be published is in the file
**firstdoc**.
It turned out that the tx to publish this document was too
large. Instead the first half of the document was
used to create **firstdocp1** which was published first.
After this was published (assigning owners to many new objects
and propositions), the tx to publish **firstdoc** was
small enough to be published.

The first line of both files indicate that it is a document
in the theory with the theory id just published.

```
Document 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
```

The first half of **firstdoc** (and all of **firstdocp1**)
defines some basic logical operators (some of which were
defined in the theory, although definitions in the theory
cannot be exported and so must be repeated in a document).
In addition a number of basic theorems are proven.
For example, **and** is defined and a basic theorem **andI**
is proven:

```
Def and : TpArr Prop TpArr Prop Prop
 := Lam A Prop Lam B Prop All P Prop Imp Imp A Imp B P P
Thm andI : All A Prop All B Prop Imp A Imp B Ap Ap and A B
 := TmLa A Prop TmLa B Prop PrLa a A PrLa b B TmLa P Prop PrLa H Imp A Imp B P PrAp PrAp H a b
```

Intuitively **andI** gives a way to combine a proof of a proposition
*A* and a proof of a proposition *B* to obtain a proof of the
conjunction of *A* and *B*.

After the basic logical operations and their properties, some
properties of binary relations over a type variable are defined and a
number of properties of partial equivalence relations (pers) are
proven.

```
Def symmetric : TpAll A TpArr TpArr A TpArr A Prop Prop
 := TTpLam A Lam R TpArr A TpArr A Prop All x A All y A Imp Ap Ap R x y Ap Ap R y x
Def transitive : TpAll A TpArr TpArr A TpArr A Prop Prop
 := TTpLam A Lam R TpArr A TpArr A Prop All x A All y A All z A Imp Ap Ap R x y Imp Ap Ap R y z Ap Ap R x z
Def per : TpAll A TpArr TpArr A TpArr A Prop Prop
 := TTpLam A Lam R TpArr A TpArr A Prop Ap Ap and Ap TTpAp symmetric A R Ap TTpAp transitive A R
...
Thm per_ref1 : TTpAll A All R TpArr A TpArr A Prop Imp Ap TTpAp per A R All x1 A All x2 A Imp Ap Ap R x1 x2 Ap Ap R x1 x1
 := TpLa A TmLa H1 TpArr A TpArr A Prop PrLa x1 Ap TTpAp per A H1 PrAp TmAp x1 All x A All y A Imp Ap Ap H1 x y Ap Ap H1 x x PrLa x2 Ap TTpAp symmetric A H1 PrLa x3 Ap TTpAp transitive A H1 TmLa H2 A TmLa H3 A PrLa x4 Ap Ap H1 H2 H3 PrAp PrLa x5 Ap Ap H1 H3 H2 PrAp PrAp TmAp TmAp TmAp x3 H2 H3 H2 x4 x5 PrAp TmAp TmAp x2 H2 H3 x4
...
```

Next the choice operator is given. In the theory this was
declared as the first (zeroth) primitive `Eps`.
In the local file we only know this is `Prim 0`.
In order to give a more readable name, we use a Let declaration:

```
Let Eps : TpAll A TpArr TpArr A Prop A := Prim 0
```

After this we can refer to this primitive simply as `Eps`.
The Let declaration is purely for readability and convenience.
The declarations are expanded away before the document is
stored. The choice axiom is imported with a Known definition
(which is allowed since the theory proposition has an owner):

```
Known EpsR : TTpAll A All P TpArr A Prop All x A Imp Ap P x Ap P Ap TTpAp Eps A Lam x A Ap P x
```

After this a slight variant is proven which makes use of the existential quantifier.

```
Thm EpsR2 : TTpAll A All P TpArr A Prop Imp Ap TTpAp ex A Lam x A Ap P x Ap P Ap TTpAp Eps A Lam x A Ap P x
 := TpLa A TmLa P TpArr A Prop PrLa u Ap TTpAp ex A P PrAp TmAp u Ap P Ap TTpAp Eps A Lam x A Ap P x TmLa x A PrLa v Ap P x PrAp TmAp TmAp TpAp EpsR A P x v
```

Excluded middle is proven using choice:

```
Thm classic : All P Prop Ap Ap or P Ap not P
  := ...
```

The proof term is long and omitted here.

An if-then-else operator **If** is defined and some properties of it are proven.

Next a **canonical_elt** operation is defined.
This takes a binary relation over a type and returns
a function from the type to itself
by choosing some *y* related to the input *x*.

```
Def canonical_elt : TpAll A TpArr TpArr A TpArr A Prop TpArr A A
 := TTpLam A Lam R TpArr A TpArr A Prop Lam x A Ap TTpAp Eps A Lam y A Ap Ap R x y
```

If the relation is an equivalence relation, then **canonical_elt** will
assign a unique equivalent representative for every input.
This also works for pers as long as one gives an input in the domain of the per.

Canonical elements are used to define quotients.

A particularly interesting equivalence is defined as **MetaFuncEquiv**.

```
Def MetaFuncEquiv : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A B TpArr TpArr A B Prop
 := TTpLam A TTpLam B Lam P TpArr A Prop Lam f TpArr A B Lam g TpArr A B All x A Imp Ap P x Ap Ap TTpAp eq B Ap f x Ap g x
```

Given two types *A* and *B* and a predicate *P* over the type *A*,
two meta-functions from *A* to *B* are equivalent if they agree on
elements of type *A* satisfying *P*.

(We say *meta-functions* to distinguish these from set theoretic functions
which would be of type **set**.)

We can now define an operator **MetaFuncC** that selects a canonical
function from *A* to *B* among those that agree on a predicate *P*.

```
Def MetaFuncC : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A B TpArr A B
 := TTpLam A TTpLam B Lam P TpArr A Prop Lam f TpArr A B Ap Ap TTpAp canonical_elt TpArr A B Ap TTpAp TTpAp MetaFuncEquiv A B P f
```

This operation will be particularly useful when defining specific categories.

The file **firstdoc** continues beyond this point and covers some basic set theory.

The remaining primitives are locally named using Let declarations.
The axioms are imported using Known declarations.
A number of basic set theoretic facts are proven.
As a sample, here is a short segment involving power sets:

```
Let Power : TpArr set set
 := Prim 4
Known PowerEq : All X set All Y set Ap Ap iff Ap Ap In Y Ap Power X Ap Ap Subq Y X
...
Thm PowerI : All X set All Y set Imp Ap Ap Subq Y X Ap Ap In Y Ap Power X
 := TmLa X set TmLa Y set PrAp TmAp TmAp TmAp PowerEq X Y Imp Ap Ap Subq Y X Ap Ap In Y Ap Power X PrLa _ Imp Ap Ap In Y Ap Power X Ap Ap Subq Y X PrLa H Imp Ap Ap Subq Y X Ap Ap In Y Ap Power X H
Thm Empty_In_Power : All X set Ap Ap In Empty Ap Power X
 := TmLa X set PrAp TmAp TmAp PowerI X Empty TmAp Subq_Empty X
```

Using **If** and **Repl**, an operator **Sep** is defined
and corresponds to forming a set by separating the elements
of a set *X* satisfying a predicate *P*.

Binary intersections and unions are defined, as singletons
and unordered pairs. In addition a **SetAdjoin** operation
is defined taking *X* and *y* to a set unioning *X* with singleton *y*.
This can be used to define successor ordinals.

The command `readdraft` was used to check the correctness of the document
and report which objects and propositions are used (and whether they would
need rights for use) and
which objects and propositions are created (and so need to be given owners).

```
readdraft firstdoc

Document is correct and has id 3441458eaedbe167a7f1236a0a7d3e6ac636e1350f272e9a784b815c21a34b0f and address PFdMgWPHo2541Dc9AoQ8g9yxY3BuoVkK18.
...
Document uses 12 props:
 Theory Prop 'prop_ext' df00c473fbc37da25c57ab730d639e65df1060549e7cc7e23a5856fd68f3c387 (TLVxLQ6eG4RRRkwMRdAkPqpChGMsQcP5J4) Owner Df9i59wmWtXriTJ7aoottJJ8xBTnS7FNnX: free to use
...
Document creates 30 objects:
Pure object 'False' has no owner.
...
Document creates 111 props:
Pure proposition 'FalseE' has no owner.
...
```

In this case the defaults were not generally
used for ownership and rights.
Consider the following items for `False`:

```
NewOwner False DosfWPhS8UC2VCWiExKWxgBWKdqMHnJfNS
NewRights False DosfWPhS8UC2VCWiExKWxgBWKdqMHnJfNS None
```

This declares DosfWPhS8UC2VCWiExKWxgBWKdqMHnJfNS as the owner of **False**
and declares that no one has the right to import **False**.
Due to this rights policy, no one can import **False** opaquely with a Param item.
In this case (as is the case with many logical operators) proofs
tend to be simpler if they make use of the definitions (since the
definitions are essentially the elimination principle).
If someone is using **False** in a document, the rights policy forces
them to include the definition of **False** (using it transparently).

The same policy would make sense for **and**, but in this case
a less strict rights policy was chosen:

```
NewOwner and DqSi6e31EEMnTdmLrhLGpA72JieQjwubhG
NewRights and DqSi6e31EEMnTdmLrhLGpA72JieQjwubhG 1
```

This rights policy allows authors to import **and** opaquely
with a Param item, but only if they pay 1 fraenk to
DqSi6e31EEMnTdmLrhLGpA72JieQjwubhG for the right to make
this use of **and**.
In practice, this should never happen since
the first signature (below) will include definitions
like **False** and **and** so authors can freely make
use of them by importing the signature.

After making these declarations, commands were used to
commit to the draft and publish the commitment.

```
commitdraft firstdoc firstdoccommit
...
validatetxfile firstdoccommit
...
sendtxfile firstdoccommit
```

After the commitment matured, `publishdraft` attempted to create a tx:

```
publishdraft firstdoc firstdocpublish
...
```

However the tx created was too large to fit into a block (> 500K).
This is in spite of the fact that the document itself is smaller than 500K.
The problem is that so many new ownership assets needed to be created.

To get around this, the first part of the document was used to
create a different document to publish first.

```
readdraft firstdocp1

Document is correct and has id 2c6f0d06f347aa14cc13003affe0c47f3cd8e045ed6b20595f74ad91ba0912bc and address PTwkyYmCk9P5jdnrpVjwJkeeugjGhgGq17.
...
Document creates 18 objects:
...
Document creates 78 props:
...

commitdraft firstdocp1 firstdocp1commit
...

sendtxfile firstdocp1commit
```

Once this commitment matured, publishdraft could be used to create the publication tx.

```
publishdraft firstdocp1 firstdocp1publish

Document is correct and has id 2c6f0d06f347aa14cc13003affe0c47f3cd8e045ed6b20595f74ad91ba0912bc and address PTwkyYmCk9P5jdnrpVjwJkeeugjGhgGq17.
...
```

In this case the tx only needed to give ownership assets to the objects
and propositions created in the first half, making the tx small enough to be
sent and confirmed.

```
sendtxfile firstdocp1publish

a6c5a5122c246daaa4f3ab51953f8beaac2b59f3afc8c2419a37fa217649a3c1
```

After firstdocp1 was published (in Block 1238), the original firstdoc
could be published even without changes (using the original
commitment).  The publication tx was small enough to be confirmed
since it only had to assign owners to the objects and propositions
created in the second half of the document.

```
publishdraft firstdoc firstdocpublish

Document is correct and has id 3441458eaedbe167a7f1236a0a7d3e6ac636e1350f272e9a784b815c21a34b0f and address PFdMgWPHo2541Dc9AoQ8g9yxY3BuoVkK18.
Completely signed.
The transaction to publish the document was created.
...
```

```
sendtxfile firstdocpublish

8e909e2caf8934f0e5cbd28bfba0a858a91a4a27ef5f1f13c7b85250562f9dc8
```

The document was published in Block 1240.

### Signature firstsig

Once the first two documents were published, the first signature
was published. The signature makes a number of objects available
either transparently (as Def items) or opaquely (as Param items)
and makes a number of propositions available (as Known items).

The first line of the file indicates the file is a signature
in the theory identified by the given theory id:

```
Signature 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
```

Signatures cannot create any new objects or propositions,
they can only make them easily available for future signatures
and documents.

An example of a definition made available transparently is **and**:

```
Def and : TpArr Prop TpArr Prop Prop
 := Lam A Prop Lam B Prop All P Prop Imp Imp A Imp B P P
```

Furthermore the **andI** proposition proven in the document is made available:

```
Known andI : All A Prop All B Prop Imp A Imp B Ap Ap and A B
```

Every document importing this signature will be able to use **and**
to form terms and use **andI** to form proofs.
(The actual names are not part of the signature. When signatures
are imported in a draft signature or document, the author chooses
names for everything imported.)

An example of an object included opaquely is **If**:

```
Param 3b77aee5210fc256802a1d8b8fe0782e99240b9de9d82216a6bb45fca0642a12 If : TpAll A TpArr Prop TpArr A TpArr A A
```

The object id 3b77aee5210fc256802a1d8b8fe0782e99240b9de9d82216a6bb45fca0642a12
was reported by `readdraft` when the relevant document was being prepared.
It will also be reported applying `readdraft` to the already published document
or a new document with the same definition.

Two known facts about **If** are included:

```
Known If_0 : TTpAll A All p Prop All x A All y A Imp Ap not p Ap Ap TTpAp eq A Ap Ap Ap TTpAp If A p x y y
Known If_1 : TTpAll A All p Prop All x A All y A Imp p Ap Ap TTpAp eq A Ap Ap Ap TTpAp If A p x y x
```

These two facts essentially confirm that **If** behaves as an if-then-else operator.
After importing the signature, these will be the only facts about **If** available to use.

Another object included opaquely is **MetaFuncC**:

```
Param a18909d50552b83e53167b2f7dfb6eeefed5e175e767bd2c614dcf92b8ce45bb MetaFuncC : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A B TpArr A B
```

A number of basic facts about this object are included:

```
Known MetaFuncC_rel : TTpAll A TTpAll B All P TpArr A Prop All f TpArr A B Ap Ap Ap TTpAp TTpAp MetaFuncEquiv A B P f Ap Ap TTpAp TTpAp MetaFuncC A B P f
Known MetaFuncC_eq : TTpAll A TTpAll B All P TpArr A Prop All f TpArr A B All g TpArr A B Imp Ap Ap Ap TTpAp TTpAp MetaFuncEquiv A B P f g Ap Ap TTpAp eq TpArr A B Ap Ap TTpAp TTpAp MetaFuncC A B P f Ap Ap TTpAp TTpAp MetaFuncC A B P g
Known MetaFuncC_idem : TTpAll A TTpAll B All P TpArr A Prop All f TpArr A B Ap Ap TTpAp eq TpArr A B Ap Ap TTpAp TTpAp MetaFuncC A B P f Ap Ap TTpAp TTpAp MetaFuncC A B P Ap Ap TTpAp TTpAp MetaFuncC A B P f
```

Next a number of the basic set theoretic objects and propositions
from the previous document are exported.

Calling `readdraft` on the file verifies the correctness
and provides information about how many fraenks must be
burned to publish the signature:

```
readdraft firstsig

Signature is correct and has id 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7 and address PEouWaiP4aR7PaSM6MGAR9xsh7hYTq2qfr.
151.851 fraenks must be burned to publish signature.
```

Again, 0.021 fraenks must be burned per byte and so we can infer the
serialization of this signature is 7231 bytes. During the process of
creating the signature, the high burn cost (which was originally
higher) led to consideration of the objects and propositions most
important to include.  In the end what was included are the objects
and propositions that seemed the most important for authors to be able
to easily reuse.

Using `commitdraft` the signature was committed and
then using `publishdraft` the signature was published.
The publication tx burned roughly 152 fraenks.

The reported theory id 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7
is used to include the signature in future signatures and documents.
Since the signature has already been published,
the `signature` command can be used to print information about the signature.

```
signature 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7

Signature 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7 in Theory 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
0 Imported Signatures 27 Objects 59 Knowns:
Imports:
Objects:
43f34d6a2314b56cb12bf5cf84f271f3f02a3e68417b09404cc73152523dbfa0
...
Knowns:
9a6fd4292d63e7be0caf7c85f8d8c56e3be3c23ff46c2001263cd459da21e6ed
...
```

When importing the signature we will need to give local names to the
27 objects and 59 known propositions.  In practice we will always give
the same names, but authors are free to use different names since the
names are all replaced (by global hashval ids) before publication.

### Document categoryandfunctordefs1

The next document made a number of basic definitions from Category
Theory. While some of this was also taken from QeditasEgal
formalizations, the contents were significantly extended to include
functors, natural transformations, adjunctions and monads.
From this point on the Dalilcoin publications begin to differ
significantly from the QeditasEgal formalizations.

As usual, the first line indicates the file specifies a document in the usual theory.

```
Document 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
```

The next lines include the signature just published using an
Include item along with names for the imported objects and known
propositions from the signature. (The order of the names is important.)

```
Include 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7
  [ False True not and or iff eq ex symmetric transitive per If MetaFuncEquiv MetaFuncC MetaFuncQ Subq Sep binintersect UPair Sing binunion SetAdjoin TransSet Union_closed Power_closed Repl_closed ZF_closed ]
  [ TrueI andI orIL orIR eqI eq_sym eq_trans f_equal exI prop_ext2 func_ext per_tra per_stra1 per_ref1 per_ref2 EpsR classic NNPP If_0 If_1 MetaFuncEquiv_ref MetaFuncEquiv_sym MetaFuncEquiv_tra MetaFuncEquiv_per MetaFuncC_rel MetaFuncC_eq MetaFuncC_idem MetaFuncQ_prop2 set_ext In_ind EmptyE UnionE2 UnionI PowerE PowerI ReplE2 ReplI SepI SepE SepE1 SepE2 binintersectI binintersectE binintersectE1 binintersectE2 UPairE UPairI1 UPairI2 SingI SingE binunionI1 binunionI2 binunionE ZF_closed_I ZF_closed_E UnivOf_In UnivOf_TransSet UnivOf_ZF_closed UnivOf_Min ]
```

As usual, we have one Base item so we can refer to the first base type as **set**.

```
Base set
```

Next we list previously proven propositions as known
that were not in the signature but will be used in the document.
These are all propositions for working with several conjunctions.

```
Known and3I : All P1 Prop All P2 Prop All P3 Prop Imp P1 Imp P2 Imp P3 Ap Ap and Ap Ap and P1 P2 P3
Known and3E : All P1 Prop All P2 Prop All P3 Prop Imp Ap Ap and Ap Ap and P1 P2 P3 All p Prop Imp Imp P1 Imp P2 Imp P3 p p
Known and4I : All P1 Prop All P2 Prop All P3 Prop All P4 Prop Imp P1 Imp P2 Imp P3 Imp P4 Ap Ap and Ap Ap and Ap Ap and P1 P2 P3 P4
Known and4E : All P1 Prop All P2 Prop All P3 Prop All P4 Prop Imp Ap Ap and Ap Ap and Ap Ap and P1 P2 P3 P4 All p Prop Imp Imp P1 Imp P2 Imp P3 Imp P4 p p
Known and5I : All P1 Prop All P2 Prop All P3 Prop All P4 Prop All P5 Prop Imp P1 Imp P2 Imp P3 Imp P4 Imp P5 Ap Ap and Ap Ap and Ap Ap and Ap Ap and P1 P2 P3 P4 P5
Known and5E : All P1 Prop All P2 Prop All P3 Prop All P4 Prop All P5 Prop Imp Ap Ap and Ap Ap and Ap Ap and Ap Ap and P1 P2 P3 P4 P5 All p Prop Imp Imp P1 Imp P2 Imp P3 Imp P4 Imp P5 p p
```

#### Definition of a metacategory

The next five definitions are polymorphic relations
**idT**, **compT**, **idL**, **idR** and **compAssoc**.
Each of these depend on two types *A* and *B*,
a predicate *Obj* over *A* recognizing objects (of type *A*) of a proposed category,
a relation *Hom* over *A*, *A* and *B* intended to recognize arrows (of type *B*)
from one object to another in a proposed category,
a function *id* from *A* to *B* intended to give the identity arrow for an object
and
a function *comp* from *A* to *A* to *A* to *B* to *B* to *B*
intended to give the composition of two arrows (relative to the appropriate objects).
The meaning of the definitions can be stated informally as follows:

* **idT** *A* *B* *Obj* *Hom* *id* *comp* holds if *Obj* *X* implies *Hom* *X* *X* (*id* *X*)
for all *X* of type *A*.

```
Def idT : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B Prop
 := TTpLam A TTpLam B Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B All X A Imp Ap Obj X Ap Ap Ap Hom X X Ap id X
```

* **compT** *A* *B* *Obj* *Hom* *id* *comp* holds if 
*Obj* *X*,
*Obj* *Y*,
*Obj* *Z*,
*Hom* *X* *Y* *f*
and
*Hom* *Y* *Z* *g*
imply *Hom* *X* *Z* (*comp* *X* *Y* *Z* *g* *f*)
for all
*X*, *Y* and *Z* of type *A*
and all *f* and *g* of type *B*.

```
Def compT : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B Prop
 := TTpLam A TTpLam B Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B All X A All Y A All Z A All f B All g B Imp Ap Obj X Imp Ap Obj Y Imp Ap Obj Z Imp Ap Ap Ap Hom X Y f Imp Ap Ap Ap Hom Y Z g Ap Ap Ap Hom X Z Ap Ap Ap Ap Ap comp X Y Z g f
```

* **idL** *A* *B* *Obj* *Hom* *id* *comp* holds if 
*Obj* *X*,
*Obj* *Y*
and
*Hom* *X* *Y* *f*
imply
*comp* *X* *X* *Y* *f* (*id* *X*) = *f*
for all *X* and *Y* of type *A*
and all *f* of type *B*.

```
Def idL : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B Prop
 := TTpLam A TTpLam B Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B All X A All Y A All f B Imp Ap Obj X Imp Ap Obj Y Imp Ap Ap Ap Hom X Y f Ap Ap TTpAp eq B Ap Ap Ap Ap Ap comp X X Y f Ap id X f
```


* **idR** *A* *B* *Obj* *Hom* *id* *comp* holds if 
*Obj* *X*,
*Obj* *Y*
and
*Hom* *X* *Y* *f*
imply
*comp* *X* *Y* *Y* (*id* *Y*) *f* = *f*
for all *X* and *Y* of type *A*
and all *f* of type *B*.

```
Def idR : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B Prop
 := TTpLam A TTpLam B Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B All X A All Y A All f B Imp Ap Obj X Imp Ap Obj Y Imp Ap Ap Ap Hom X Y f Ap Ap TTpAp eq B Ap Ap Ap Ap Ap comp X Y Y Ap id Y f f
```

* **compAssoc** *A* *B* *Obj* *Hom* *id* *comp* holds if
*Obj* *X*,
*Obj* *Y*,
*Obj* *Z*,
*Obj* *W*,
*Hom* *X* *Y* *f*,
*Hom* *Y* *Z* *g*
and
*Hom* *Z* *W* *h*,
imply
*comp* *X* *Y* *W* (*comp* *Y* *Z* *W* *h* *g*) *f*
=
*comp* *X* *Z* *W* *h* (*comp* *X* *Y* *Z* *g* *f*)
for all *X*, *Y*, *Z* and *W* of type *A*
and all *f*, *g* and *h* of type *B*.

```
Def compAssoc : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B Prop
 := TTpLam A TTpLam B Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B All X A All Y A All Z A All W A All f B All g B All h B Imp Ap Obj X Imp Ap Obj Y Imp Ap Obj Z Imp Ap Obj W Imp Ap Ap Ap Hom X Y f Imp Ap Ap Ap Hom Y Z g Imp Ap Ap Ap Hom Z W h Ap Ap TTpAp eq B Ap Ap Ap Ap Ap comp X Y W Ap Ap Ap Ap Ap comp Y Z W h g f Ap Ap Ap Ap Ap comp X Z W h Ap Ap Ap Ap Ap comp X Y Z g f
```

We can now define what a metacategory is by conjoining the definitions above:

```
Def MetaCat : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B Prop
 := TTpLam A TTpLam B Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B Ap Ap and Ap Ap and Ap Ap and Ap Ap Ap Ap TTpAp TTpAp idT A B Obj Hom id comp Ap Ap Ap Ap TTpAp TTpAp compT A B Obj Hom id comp Ap Ap and Ap Ap Ap Ap TTpAp TTpAp idL A B Obj Hom id comp Ap Ap Ap Ap TTpAp TTpAp idR A B Obj Hom id comp Ap Ap Ap Ap TTpAp TTpAp compAssoc A B Obj Hom id comp
```

In order to ease the process of proving something does or does not form a metacategory,
we prove two theorems that can be used instead of the definition of **MetaCat**.
This will make it easier to import **MetaCat** as an opaque object later.
**MetaCatI** allows us to infer something is a metacategory by proving the five properties.
**MetaCatE** allows us to assume the five properties as hypotheses whenever
we know we have a metacategory.

```
Thm MetaCatI : TTpAll A TTpAll B All Obj TpArr A Prop All Hom TpArr A TpArr A TpArr B Prop All id TpArr A B All comp TpArr A TpArr A TpArr A TpArr B TpArr B B Imp Ap Ap Ap Ap TTpAp TTpAp idT A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp compT A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp idL A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp idR A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp compAssoc A B Obj Hom id comp Ap Ap Ap Ap TTpAp TTpAp MetaCat A B Obj Hom id comp
 := ...
Thm MetaCatE : TTpAll A TTpAll B All Obj TpArr A Prop All Hom TpArr A TpArr A TpArr B Prop All id TpArr A B All comp TpArr A TpArr A TpArr A TpArr B TpArr B B Imp Ap Ap Ap Ap TTpAp TTpAp MetaCat A B Obj Hom id comp All p Prop Imp Imp Ap Ap Ap Ap TTpAp TTpAp idT A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp compT A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp idL A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp idR A B Obj Hom id comp Imp Ap Ap Ap Ap TTpAp TTpAp compAssoc A B Obj Hom id comp p p
 := ...
```

The proofs are straightforward and omitted here.

#### Definition of some limits, colimits and other constructions

Relative to a metacategory we next define a number of
predicates recognizing limits, colimits and other well-known
category theoretic constructions.

* **terminal_p** recognizes when an object and function from objects to arrows (giving the
unique arrow)
specifies a terminal object.

* **initial_p** is dual to terminal and recognizes initial objects.

* **product_p** recognizes when an object (along with two arrows
and a function giving appropriate arrows) give a product of two objects.

* **product_constr_p** recognizes when certain functions give a general construction of
products for every two objects of the metacategory.

* **coproduct_p** is dual to **product_p** and recognizes when an object (along with two arrows
and a function giving appropriate arrows) give a coproduct of two objects.

* **coproduct_constr_p** recognizes when certain functions give a general construction of
coproducts for every two objects of the metacategory.

* **equalizer_p** recognizes equalizers for specific arrows *f* and *g* in some *Hom* *X* *Y*
and **equalizer_constr_p** recognizes constructions that give equalizers when
given appropriate *X*, *Y*, *f* and *g* as input.

* **coequalizer_p** and **coequalizer_constr_p** are dual.

* **pullback_p** recognizes specific pullbacks and **pullback_constr_p** recognizes general constructions of pullbacks.

* **pushout_p** and **pushout_constr_p** are dual.

* **exponent_p** recognizes exponents and **product_exponent_constr_p** recognizes general constructions of products and exponents together (since exponents depend on products).

* **monic** recognizes when an arrow is monic.

* **subobject_classifier_p** recognizes a subobject classifier.

* **nno_p** recognizes a natural number object.

Most of the conjectures given in later documents conjecture
the existence of these constructions.
In some cases the conjecture should be provable and in other
cases the negation should be provable.
For example, all the constructions can be realized in the
metacategory of sets, but the metacategory of hereditarily
finite sets will not have a natural number object.

#### Functors

We next define **MetaFunctor** to recognize morphisms from 
one metacategory to another. This depends on four type variables:
two for each category.

* **MetaFunctor** *A* *B* *C* *D* *Obj* *Hom* *id* *comp* *Obj'* *Hom'* *id'* *comp'* *Obj'* *F0* *F1*
holds if *F0* (a function from *A* to *C*) maps from *Obj* to *Obj'*,
*F1* (a function from *A*, *A* and *B* to *D*) maps from *Hom* *X* *Y* *f*
to *Hom'* (*F0* *X*) (*F0* *Y*) (*F1* *X* *Y* *f*),
*F1* maps identity arrows to identity arrows
and *F1* respects composition.

```
Def MetaFunctor : TpAll A TpAll B TpAll C TpAll D TpArr TpArr A Prop TpArr TpArr A TpArr A TpArr B Prop TpArr TpArr A B TpArr TpArr A TpArr A TpArr A TpArr B TpArr B B TpArr TpArr C Prop TpArr TpArr C TpArr C TpArr D Prop TpArr TpArr C D TpArr TpArr C TpArr C TpArr C TpArr D TpArr D D TpArr TpArr A C TpArr TpArr A TpArr A TpArr B D Prop
 := TTpLam A TTpLam B TTpLam C TTpLam D Lam Obj TpArr A Prop Lam Hom TpArr A TpArr A TpArr B Prop Lam id TpArr A B Lam comp TpArr A TpArr A TpArr A TpArr B TpArr B B Lam Obj' TpArr C Prop Lam Hom' TpArr C TpArr C TpArr D Prop Lam id' TpArr C D Lam comp' TpArr C TpArr C TpArr C TpArr D TpArr D D Lam F0 TpArr A C Lam F1 TpArr A TpArr A TpArr B D Ap Ap and Ap Ap and Ap Ap and All X A Imp Ap Obj X Ap Obj' Ap F0 X All X A All Y A All f B Imp Ap Obj X Imp Ap Obj Y Imp Ap Ap Ap Hom X Y f Ap Ap Ap Hom' Ap F0 X Ap F0 Y Ap Ap Ap F1 X Y f All X A Imp Ap Obj X Ap Ap TTpAp eq D Ap Ap Ap F1 X X Ap id X Ap id' Ap F0 X All X A All Y A All Z A All f B All g B Imp Ap Obj X Imp Ap Obj Y Imp Ap Obj Z Imp Ap Ap Ap Hom X Y f Imp Ap Ap Ap Hom Y Z g Ap Ap TTpAp eq D Ap Ap Ap Ap Ap comp' Ap F0 X Ap F0 Y Ap F0 Z Ap Ap Ap F1 Y Z g Ap Ap Ap F1 X Y f Ap Ap Ap F1 X Z Ap Ap Ap Ap Ap comp X Y Z g f
```

We again prove straightforward theorems **MetaFunctorI** and **MetaFunctorE** that can
be used instead of the definition.

Later in the document we prove a result **MetaIdFunctorThm**
that essentially says the identity functor is a metafunctor.
More precisely the object mapping given by sending *X* to *X*
(at type *A*)
and arrow mapping given by sending *X*, *Y* and *f* to *f* (at type *B*)
forms a metafunctor.

We also prove **MetaCompFunctorThm**
essentially proving the composition of two metafunctors yields a metafunctor.

The definition **MetaFunctor_strict** is a conjunction of three propositions:
the two alleged metacategories are really both metacategories
and the **MetaFunctor** property holds.

#### Natural transformations

A meta-natural transformation is a mapping between two functors.

* **MetaNatTrans** *A* *B* *C* *D* *Obj* *Hom* *id* *comp* *Obj'* *Hom'* *id'* *comp'* *F0* *F1* *G0* *G1* *eta* holds if
*eta* (type from *A* to *D*)
maps from *Obj* *X* to *Hom* (*F0* *X*) (*G0* *X*) (*eta* *X*)
and
satisfies the equation
*comp'* (*F0* *X*) (*G0* *X*) (*G0* *Y*) (*G1* *X* *Y* *f*) (*eta* *X*)
=
*comp'* (*F0* *X*) (*F0* *Y*) (*G0* *Y*) (*eta* *Y*) (*F1* *X* *Y* *f*)
when appropriate.

Straightforward theorems **MetaNatTransI** and **MetaNatTransE**
are proven that can 
be used instead of the definition.

We also define **MetaNatTrans_strict** which conjoins several propositions:
the two alleged metacategories are really metacategories,
the two alleged metafunctors are really metafunctors
and finally **MetaNatTrans** holds.

#### Monads

* **MetaMonad** *A* *B* *Obj* *Hom* *id* *comp* *T0* *T1* *eta* *mu*
holds if *comp* ... (*mu* *X*) (*T1* ... (*mu* *X*)) = *comp* ... (*mu* *X*) (*mu* (*T0* *X*)),
*comp* ... (*mu* *X*) (*eta* (*T0* *X*)) = *id* (*T0* *X*)
and
*comp* ... (*mu* *X*) (*T1* ... (*eta* *X*)) = *id* (*T0* *X*)
when appropriate.

* **MetaMonad_strict** adds the properties that we have a metacategory, a metafunctor and *mu* is a meta-natural transformation.

#### MetaAdjunction

* **MetaAdjunction** *A* *B* *C* *D* *Obj* *Hom* *id* *comp* *Obj'* *Hom'* *id'* *comp'* *F0* *F1* *G0* *G1* *eta* *eps*
holds if
*comp'* ... (*eps* (*F0* *X*)) (*F1* ... (*eta* *X*)) = *id'* (*F0* *X*)
and
*comp'* ... (*G1* ... (*eps* *Y*)) (*eta* (*G0* *Y*)) = *id* (*G0* *Y*)
when appropriate.

* **MetaAdjunction_strict** adds the properties that we really have metacategories, metafunctors and meta-natural transformations.

* **MetaAdjunctionMonad** proves that adjoint metafunctors compose to give a metamonad.

* **MetaAdjunctionMonad_strict** also proves adjoint metafunctors compose to give a metamonad,
but has a simpler statement by using the strict forms.
It is essentially one implication of the form

**MetaAdjunction_strict** *A* *B* *C* *D* *Obj* *Hom* *id* *comp* *Obj'* *Hom'* *id'* *comp'* *F0* *F1* *G0* *G1* *eta* *eps*

implies

**MetaAdjunctionMonad_strict** *A* *B* *Obj* *Hom* *id* *comp* *H0* *H1* *eta* *mu*

where *H0* is formed by composing *G0* and *F0*,
*H1* is formed by essentially composing *G1* and *F1*
and *mu* is formed by essentially composing *G1* and *eps*.

This completes the document. It was published in the usual way,
giving ownership to various addresses, but leaving all rights free for all to use.

### Signature categoryandfunctordefs1sig

We next turn to the second signature which will make several basic category theoretic
definitions and results easy to import and use.

The first lines indicate the file gives a signature
and imports the previous signature. (Note that importing signatures
is transitive, so that documents that import this signature
will also import the previous signature.)

```
Signature 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
Include 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7
  [ False True not and or iff eq ex symmetric transitive per If MetaFuncEquiv MetaFuncC MetaFuncQ Subq Sep binintersect UPair Sing binunion SetAdjoin TransSet Union_closed Power_closed Repl_closed ZF_closed ]
  [ TrueI andI orIL orIR eqI eq_sym eq_trans f_equal exI prop_ext2 func_ext per_tra per_stra1 per_ref1 per_ref2 EpsR classic NNPP If_0 If_1 MetaFuncEquiv_ref MetaFuncEquiv_sym MetaFuncEquiv_tra MetaFuncEquiv_per MetaFuncC_rel MetaFuncC_eq MetaFuncC_idem MetaFuncQ_prop2 set_ext In_ind EmptyE UnionE2 UnionI PowerE PowerI ReplE2 ReplI SepI SepE SepE1 SepE2 binintersectI binintersectE binintersectE1 binintersectE2 UPairE UPairI1 UPairI2 SingI SingE binunionI1 binunionI2 binunionE ZF_closed_I ZF_closed_E UnivOf_In UnivOf_TransSet UnivOf_ZF_closed UnivOf_Min ]
```

The first five definitions from the previous document are given here transparently:
**idT**, **compT**, **idL**, **idR** and **compAssoc**.

The following objects defined in the previous document are given opaquely:
**MetaCat**, **MetaFunctor**, **MetaNatTrans**, **MetaFunctor_strict**,
**MetaNatTrans_strict**, **MetaMonad**, **MetaMonad_strict**, **MetaAdjunction**
and **MetaAdjunction_strict**.

The corresponding **...I** and **...E** propositions are given as known
so that one can make inferences with the opaque objects.

In addition, the propositions **MetaIdFunctorThm** and **MetaCompFunctorThm**
are exported as known.

Since this is a signature, fraenks had to be burned to publish it.
The `readdraft` command reports that roughly 161 fraenks needed to be burned.

```
readdraft categoryandfunctordefs1sig 

Signature is correct and has id ddfef2844accc6c45198be2644ce01c5acc8a400b7da2a41e1f599947f259157 and address PFCgZ2LzrnSWZ23DrNwk4DYFncnVmDrUfg.
160.587 fraenks must be burned to publish signature.
Signature imports 1 signatures:
 2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7
Signature exports 41 objects:
...
Signature exports 79 props:
...
```

The signature was committed and published as usual, burning the 160.587 fraenks.
Since the signature has been published,
the information can be retrieved by the `signature` command using
its signature id (reported above).

```
signature ddfef2844accc6c45198be2644ce01c5acc8a400b7da2a41e1f599947f259157

Signature ddfef2844accc6c45198be2644ce01c5acc8a400b7da2a41e1f599947f259157 in Theory 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
1 Imported Signatures 14 Objects 20 Knowns:
Imports:
2bfbea57a7e8f69fa2ff5a02e5e2e71b61fe3d858457f954b6b18448642b46d7
Objects:
...
Knowns:
...
```

### Document categoryconjs1

We now turn to the first document declaring some conjectures.
The document begins by declaring the theory it is in
and importing the new signature (which imports both signatures).

```
Document 82c7571f411491f3a88406467944703766cd46c98aeeeb6b17b2cc095f1954de
Include ddfef2844accc6c45198be2644ce01c5acc8a400b7da2a41e1f599947f259157
  [ False True not and or iff eq ex symmetric transitive per If MetaFuncEquiv MetaFuncC MetaFuncQ Subq Sep binintersect UPair Sing binunion SetAdjoin TransSet Union_closed Power_closed Repl_closed ZF_closed idT compT idL idR compAssoc MetaCat MetaFunctor MetaNatTrans MetaFunctor_strict MetaNatTrans_strict MetaMonad MetaMonad_strict MetaAdjunction MetaAdjunction_strict ]
  [ TrueI andI orIL orIR eqI eq_sym eq_trans f_equal exI prop_ext2 func_ext per_tra per_stra1 per_ref1 per_ref2 EpsR classic NNPP If_0 If_1 MetaFuncEquiv_ref MetaFuncEquiv_sym MetaFuncEquiv_tra MetaFuncEquiv_per MetaFuncC_rel MetaFuncC_eq MetaFuncC_idem MetaFuncQ_prop2 set_ext In_ind EmptyE UnionE2 UnionI PowerE PowerI ReplE2 ReplI SepI SepE SepE1 SepE2 binintersectI binintersectE binintersectE1 binintersectE2 UPairE UPairI1 UPairI2 SingI SingE binunionI1 binunionI2 binunionE ZF_closed_I ZF_closed_E UnivOf_In UnivOf_TransSet UnivOf_ZF_closed UnivOf_Min MetaCatI MetaCatE MetaFunctorI MetaFunctorE MetaNatTransI MetaNatTransE MetaIdFunctorThm MetaCompFunctorThm MetaFunctor_strict_I MetaFunctor_strict_E MetaNatTrans_strict_I MetaNatTrans_strict_E MetaMonadI MetaMonadE MetaMonad_strict_I MetaMonad_strict_E MetaAdjunctionI MetaAdjunctionE MetaAdjunction_strict_I MetaAdjunction_strict_E ]
```

We then make a few declarations to allow ourselves to talk specifically about
the base type of sets and primitives in the set theory.

```
Base set
Let In : TpArr set TpArr set Prop
 := Prim 1
Let Empty : set
 := Prim 2
Let Union : TpArr set set
 := Prim 3
Let UnivOf : TpArr set set
 := Prim 6
```

We then repeat the definitions of **terminal_p**, **initial_p**, etc.,
originally defined in the categoryandfunctordefs1 document.  Note that
these definitions were not included in the signature due to the high
burn cost that would have been required. Every document using them
must include them (unless someone else includes them in a future
signature).

We then make a few definitions that will allow us to
easily specify many concrete metacategories.

* **CF** takes a set *X* and a metafunction *f* from sets to sets
and returns a canonical representative *f'* from sets to sets
that agrees with *f* when restricted to inputs from the set *X*.

```
Def CF : TpArr set TpArr TpArr set set TpArr set set
 := Lam X set Lam f TpArr set set Ap Ap TTpAp TTpAp MetaFuncC set set Lam x set Ap Ap In x X f
```

* **CFHom** *A* *R* *U* *X* *Y* *f* (where *A* is a type variable,
 *R* is a relation on two members of *A* and a metafunction from sets to sets,
 *U* is a function from *A* to sets,
 *X* and *Y* are of type *A*,
 and *f* is a metafunction from sets to sets)
holds if *R* *X* *Y* *f* holds
and *f* is the canonical representative of metafunctions
relative to the input set *U* *X*.

Our proposed metacategories in this document (and the remaining
documents) will use a variety of types *A* for objects, but will
always use the type of metafunctions from sets to sets as arrows.  We
will also always have a function *U* taking an object to an underlying
set.  The relation *R* will give the restrictions on function
including properties the function should have (e.g., being a
homomorphism of some kind) for them to qualify as arrows of the
metacategory.  Since **CFHom** requires the metafunction to be
canonical, there will be at most one representative of each intended
function.  It will always be necessary to check that *R* respects the
**MetaFuncEquiv** equivalence relation, but in all our examples this
will be clear.

#### Metacategory of sets

Our first example is the category Set of sets, specified as follows:

* Let *A* be set.

* Let *R* *X* *Y* *f* hold if *f* maps member of the set *X* to members of the set *Y*.

* Let *U* *X* be *X*.

* Define **SetHom** to be **CFHom** set *R* *U*.

* Let *Obj* *X* always be true.

* Let *Hom* be **SetHom**.

* Let *id* *X* be the result of applying **CF** *X* to the identity metafunction.

* Let *comp* *X* *Y* *Z* *g* *f* be the result of applying **CF** *X* to the composition of the metafunctions *g* and *f*.

We can now make our first conjecture, that the metacategory of all sets is really
a metacategory:

* **MetaCatSet** : **MetaCat** set (set to set) *Obj* *Hom* *id* *comp*.

```
Conj MetaCatSet : Ap Ap Ap Ap TTpAp TTpAp MetaCat set TpArr set set Lam x9 set True SetHom Lam x7 set Ap Ap CF x7 Lam x8 set x8 Lam x1 set Lam x2 set Lam x3 set Lam x4 TpArr set set Lam x5 TpArr set set Ap Ap CF x1 Lam x6 set Ap x4 Ap x5 x6
```

We then make several conjectures that certain constructions are possible in this
conjectured metacategory. For example, we conjecture there are terminal and initial objects.

```
Conj MetaCatSet_terminal : Ap TTpAp ex set Lam one set Ap TTpAp ex TpArr set TpArr set set Lam uniq TpArr set TpArr set set Ap Ap Ap Ap Ap Ap TTpAp TTpAp terminal_p set TpArr set set Lam x9 set True SetHom Lam x7 set Ap Ap CF x7 Lam x8 set x8 Lam x1 set Lam x2 set Lam x3 set Lam x4 TpArr set set Lam x5 TpArr set set Ap Ap CF x1 Lam x6 set Ap x4 Ap x5 x6 one uniq
Conj MetaCatSet_initial : Ap TTpAp ex set Lam zer set Ap TTpAp ex TpArr set TpArr set set Lam uniq TpArr set TpArr set set Ap Ap Ap Ap Ap Ap TTpAp TTpAp initial_p set TpArr set set Lam x9 set True SetHom Lam x7 set Ap Ap CF x7 Lam x8 set x8 Lam x1 set Lam x2 set Lam x3 set Lam x4 TpArr set set Lam x5 TpArr set set Ap Ap CF x1 Lam x6 set Ap x4 Ap x5 x6 zer uniq
```

In this case, just to give a simple example of a conjecture that is false,
we included a conjecture that there is a set which acts as both an initial and a terminal object.

```
Conj MetaCatSet_initial_and_terminal : Ap TTpAp ex set Lam X set Ap TTpAp ex TpArr set TpArr set set Lam uniq0 TpArr set TpArr set set Ap TTpAp ex TpArr set TpArr set set Lam uniq1 TpArr set TpArr set set Ap Ap and Ap Ap Ap Ap Ap Ap TTpAp TTpAp initial_p set TpArr set set Lam x18 set True SetHom Lam x16 set Ap Ap CF x16 Lam x17 set x17 Lam x10 set Lam x11 set Lam x12 set Lam x13 TpArr set set Lam x14 TpArr set set Ap Ap CF x10 Lam x15 set Ap x13 Ap x14 x15 X uniq0 Ap Ap Ap Ap Ap Ap TTpAp TTpAp terminal_p set TpArr set set Lam x9 set True SetHom Lam x7 set Ap Ap CF x7 Lam x8 set x8 Lam x1 set Lam x2 set Lam x3 set Lam x4 TpArr set set Lam x5 TpArr set set Ap Ap CF x1 Lam x6 set Ap x4 Ap x5 x6 X uniq1
```

In the next document we disprove this conjecture and show how to collect the
corresponding bounty (that was placed when this document was published).

#### Metacategory of hereditarily finite sets

Applying **UnivOf** to the empty set yields the set of all hereditarily finite sets.
This should also form a metacategory HFSet which can be specified simply by modifying
*Obj* above as follows:

* *Obj* *X* holds if *X* is in **UnivOf** **Empty**.

We then declare the conjecture that this does form a metacategory
and that all the constructions defined above are possible
(except the example conjecture about one object being both initial and terminal,
which will not be repeated below).
Each of these conjectures should be provable except one:
the metacategory does not have a natural number object.
Proving there is no natural number object would be required
to prove the negation of the conjecture and collect the corresponding bounty.

We also conjecture that the obvious forgetful functor
from HFSet to Set is a functor.

#### Metacategory of small sets

We next modify *Obj* again to give the metacategory SmallSet of small sets.

* Let *Obj* *X* hold if *X* is in **UnivOf** (**UnivOf** **Empty**).

We again make the same conjectures as before for this modified
metacategory.  As before, we conjecture the forgetful functor from
SmallSet to Set is a functor.

#### Metacategory of pointed sets

We now make a significant modification to the metacategory we
consider.  We want to consider a category PtdSet of pointed sets.  A
"pointed set" is a set *X* with a specific element *x* of *X*.  We
essentially need an object to be a pair of sets satisfying the
property that the second is an element of the first.

In general in simple type theory if we want to consider a pair of objects of
the same type *A*, we can use the type ((*A* to *A* to *A*) to *A*)
as the type of pairs. The first projection from such pairs to *A* is given
by applying the pair to *P1* where *P1* *x* *y* is *x*.
The second projection from such pairs to *A* is given by applying the pair
to *P2* where *P2* *x* *y* is *y*. Given particular *x* and *y* of type *A*,
the pair of *x* and *y* is formed by the function taking *u* (of type *A* to *A* to *A*)
to *u* *x* *y*.

In this case our type of objects will be ((set to set to set) to set).

* Let *U* *X* be the result of taking the first projection of *X* (yielding a set, as desired).

* Let *p1* *X* be the result of taking the second projection of *X*.

* Let *R* *X* *Y* *f* hold if *f* maps *U* *X* into *U* *Y* and *f* (*p1* *X*) = *p1* *Y*.

* Let *Obj* *X* hold if *p1* *X* is in *U* *X*.

* Let *Hom* be **CFHom** *R* *U*.

* Let *id* *X* be the result of applying **CF** (*U* *X*) to the identity metafunction.

* Let *comp* *X* *Y* *Z* *g* *f* be the result of applying **CF** (*U* *X*) to the composition of the metafunctions *g* and *f*.

We then conjecture this specifies a metacategory and
all the usual constructions can be done.

In addition we add three more conjectures:

* There is a forgetful functor from PtdSet to Set given by the *U* function on objects
and the identity (essentially) on arrows.

* There is a functor from Set to PtdSet giving a left adjoint to the forgetful functor.

* There is a functor from Set to PtdSet giving a right adjoint to the forgetful functor.

#### Metacategory of topological spaces and cts functions

Using the same technique as before to represent objects as pairs of
sets, the metacategory Top of topological spaces is defined by taking *A*
and *U* as above and taking the second component of the pair as being
a collection of open subsets of the first component.  The *Obj*
predicate requires the second component (the set of open sets) to
satisfy the usual conditions relative to the first component (*U*
*X*): every open set must be a subset of *U* *X*, the union of every
collection of open sets must be open and intersection of every pair of
open sets must be open.  The restriction *R* on functions requires the
functions to map from *U* *X* to *U* *Y* and to be continuous.

We then conjecture this forms a metacategory and all the usual
constructions can be done.  We again conjecture left and right
metaadjoints of the forgetful metafunctor to Set.

#### Metacategory of pointed topological spaces and cts functions

Finally, we form a metacategory PtdTop by changing *A* to be ((set to set
to set to set) to set) in order to represent objects as triples of
sets.  These triples of sets correspond to an underlying set (given by
*U*), a collection of open subsets, and a distinguished point.  Arrows
are continuous functions sending the distinguished point in the source
topological space to the distinguished point in the target topological
space.

As in the previous cases and in all future cases in this initial set
of conjectures, we conjecture this gives a category with all the
usual constructs and we conjecture a left and right adjoint of the
forgetful functor.

On each conjectures in the document we place a bounty of 40 fraenks.
This is done by putting a Bounty item in the draft.

```
Bounty MetaCatSet 40 NoTimeout
```

The NoTimeout flag indicates that there is no possibility of
the bounty being redeemed in the future without the conjecture
being resolved. The only ways someone can collect the bounty
is by either proving the conjecture or proving its negation.

A total of 84 bounties worth 40 fraenks each were placed
when the document was published.
Three of these were claimed by resolving three of the conjectures
in the next document to provide an example
of how to claim bounties, leaving 81 bounties worth 40 fraenks each
(3240 fraenks).

### Document categoryconjs1withsomepfs

The next document resolves three of the conjectures from the previous document
and collects the bounties.

The document includes the signatures as the previous document did.
One extra result is imported as a Known item, namely that the empty set
is always in the power set of *X*:

```
Known Empty_In_Power : All X set Ap Ap In Empty Ap Power X
```

The definition of terminal and initial objects are repeated (since there
were not in the signature), as are a number of other definitions.
It is interesting to note that althought `MetaFuncQ` was imported
from first the signature, it was imported opaquely. By repeating
the definition in the document, it becomes transparent.

```
Def MetaFuncQ : TpAll A TpAll B TpArr TpArr A Prop TpArr TpArr A B Prop
 := TTpLam A TTpLam B Lam P TpArr A Prop Ap TTpAp quotient TpArr A B Ap TTpAp TTpAp MetaFuncEquiv A B P
```

A number of theorems are proven, leading to two proofs of conjectures from the previous document.

* Set does form a metacategory:

```
Thm MetaCatSet : Ap Ap Ap Ap TTpAp TTpAp MetaCat set TpArr set set Lam x264 set True SetHom Lam x262 set Ap Ap CF x262 Lam x263 set x263 Lam x256 set Lam x257 set Lam x258 set Lam x259 TpArr set set Lam x260 TpArr set set Ap Ap CF x256 Lam x261 set Ap x259 Ap x260 x261
 := ...
```

* There is an initial object in Set (given by the empty set).

```
Thm MetaCatSet_initial : Ap TTpAp ex set Lam x45 set Ap TTpAp ex TpArr set TpArr set set Lam x46 TpArr set TpArr set set Ap Ap Ap Ap Ap Ap TTpAp TTpAp initial_p set TpArr set set Lam x55 set True SetHom Lam x53 set Ap Ap CF x53 Lam x54 set x54 Lam x47 set Lam x48 set Lam x49 set Lam x50 TpArr set set Lam x51 TpArr set set Ap Ap CF x47 Lam x52 set Ap x50 Ap x51 x52 x45 x46
 := PrAp TmAp TmAp TpAp exI set ... Empty ...
```

In addition we prove the negation of the conjecture that some set acts
both an initial and terminal object.

```
Thm not_MetaCatSet_initial_and_terminal : Ap not Ap TTpAp ex set Lam x125 set Ap TTpAp ex TpArr set TpArr set set Lam x126 TpArr set TpArr set set Ap TTpAp ex TpArr set TpArr set set Lam x127 TpArr set TpArr set set Ap Ap and Ap Ap Ap Ap Ap Ap TTpAp TTpAp initial_p set TpArr set set Lam x145 set True SetHom Lam x143 set Ap Ap CF x143 Lam x144 set x144 Lam x137 set Lam x138 set Lam x139 set Lam x140 TpArr set set Lam x141 TpArr set set Ap Ap CF x137 Lam x142 set Ap x140 Ap x141 x142 x125 x126 Ap Ap Ap Ap Ap Ap TTpAp TTpAp terminal_p set TpArr set set Lam x136 set True SetHom Lam x134 set Ap Ap CF x134 Lam x135 set x135 Lam x128 set Lam x129 set Lam x130 set Lam x131 TpArr set set Lam x132 TpArr set set Ap Ap CF x128 Lam x133 set Ap x131 Ap x132 x133 x125 x127
 := ...
```

New owners are declared for these (and other) propositions proven in the document:

```
NewOwner MetaCatSet DZBzYg3QAdcnnnShdN5y5UZGmb7DNs3rXX
NewOwner MetaCatSet_initial Dg9AdUPMoCVKjp1unZo8L7mRS2U4hsdbJc
NewOwner not_MetaCatSet_initial_and_terminal DaTeFDnw4kcLdxvKaeoPpGENw8XbiRXQPA
```

As a result, these addresses will be marked as the owners
of the term addresses corresponding to the three propositions.
In the first two cases, these addresses will each hold
a *Bounty* asset of 40 fraenks.
Since the last proposition has the form of a negation,
`DaTeFDnw4kcLdxvKaeoPpGENw8XbiRXQPA` will also control
an *OwnsNegProp* asset at the term address for the body of the
negation, where another *Bounty* asset of 40 fraenks is stored.
An *OwnsNegProp* asset can only be used for collecting bounties.

After the document was published, the author could
use these assets (signing with the private keys corresponding to
the three ownership addresses) to collect the three bounties.
A simple command for collecting bounties is `collectbounties`.

```
collectbounties DgWBaE2wsz4aS3yUEGtPXeRWQCud2MtvZJ collectbountiestx1

Transaction created to claim 119.9997416 fraenks from bounties.
...
```

This created a tx in the file collectbountiestx1 spending the three
bounties controlled by the wallet and sending the contents (as a
*Currency* asset) to the given address
DgWBaE2wsz4aS3yUEGtPXeRWQCud2MtvZJ.  This tx was then sent.

```
sendtxfile collectbountiestx1
cf2eb823bf436c87f6e2d43176f703640abc4225cf76c991b5379ec51a201270
```

The tx was confirmed in Block 1275
(33630536973c4722f660f605df3b61c50a44561ef8c0733ef246cee6ba88da29).

### Document categoryconjs2

In this document we define three categories
and make the usual conjectures.

* Mon: Category of monoids and homomorphisms.

* Grp: Category of groups and homomorphisms.

* AbGrp: Category of abelian groups and homomorphisms.

In each of these cases, the type *A* of objects
is a tuple of operations (set to set to set).
The underlying set of the algebraic structure is given
by taking the first component and applying it to the empty set twice.
Another component is applied to the empty set twice to obtain the identity
element and another component gives the binary operation.
In the case of groups, a fourth component is applied to the empty
set once to obtain a unary inverse operation.

45 bounties worth 75 fraenks each were published with the document for
a total of 3375 fraenks.  These can be collected by resolving each
conjecture.

### Document categoryconjs3

The last document published in July 2019 defined four more categories
and making more conjectures as usual. Every category has
some variant of a ring structure as objects, and tuples of
(set to set to set) are used to represent the components.
The four categories are:

* Rng: Rings (without multiplicative identity)

* CRng: Commutative rings (without multiplicative identity)

* RngWId: Rings (with multiplicative identity)

* CRngWId: Commutative rings (with multiplicative identity)

60 bounties worth 50 fraenks each were published with the document for
a total of 3000 fraenks.  These can be collected by resolving each
conjecture.

## Table of initial bounties on conjectures

We end with a table summarizing the 9615 fraenks worth of bounties
published with the initial documents in July 2019.

Each conjecture corresponds to a category
and for most categories we include the following conjectures:

* C: The structure is a category.

* T: There is a terminal object.

* I: There is an initial object.

* P: Products exist.

* CP: Coproducts exist.

* E: Equalizers exist.

* CE: Coequalizers exist.

* PL: Pullbacks exist.

* PS: Pushouts exist.

* EX: Exponents exist.

* S: There is a subobject classifier.

* N: There is a natural number object.

* F: The forgetful functor is a functor.

* LA: There is a left adjoint to the forgetful functor.

* RA: There is a right adjoint to the forgetful functor.

```
         C  T  I  P  CP E  CE PL PS EX S  N  F  LA RA
Set         40    40 40 40 40 40 40 40 40 40
HFSet    40 40 40 40 40 40 40 40 40 40 40 40 40
SmallSet 40 40 40 40 40 40 40 40 40 40 40 40 40
PtdSet   40 40 40 40 40 40 40 40 40 40 40 40 40 40 40
Top      40 40 40 40 40 40 40 40 40 40 40 40 40 40 40
PtdTop   40 40 40 40 40 40 40 40 40 40 40 40 40 40 40
Mon      75 75 75 75 75 75 75 75 75 75 75 75 75 75 75
Grp      75 75 75 75 75 75 75 75 75 75 75 75 75 75 75
AbGrp    75 75 75 75 75 75 75 75 75 75 75 75 75 75 75
Rng      50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
CRng     50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
RngWId   50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
CRngWId  50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
```

## References

[QeditasEgal] https://github.com/input-output-hk/qeditas-egal
