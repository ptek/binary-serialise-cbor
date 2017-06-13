-- |
-- Module      : Data.Binary.Serialise.CBOR.Tutorial
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lorem ipsum...
--
module Data.Binary.Serialise.CBOR.Tutorial
  ( -- * Introduction
    -- $introduction

    -- ** The CBOR format
    -- $cborformat

    -- ** The 'Serialise' class
    -- $serialise

    -- ** Encoding terms
    -- $encoding

    -- ** Decoding terms
    -- $decoding
  ) where

import Data.Binary.Serialise.CBOR

{- $introduction

@binary-serialise-cbor@ is a library for the serialisation of Haskell values.

As in modern modern serialisation libraries, @binary-serialise-cbor@ offers
instance derivation via GHC's 'GHC.Generic' mechanism,

> import qualified Data.Binary.Serialise.CBOR as CBOR
> import qualified Data.ByteString.Lazy as BSL
>
> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed :: Int }
>             deriving (Generic)
>
> instance CBOR.Serialise Animal
>
> fredTheFrog :: Animal
> fredTheFrog = HoppingAnimal "Fred" 4
>
> main = BSL.writeFile "hi" (CBOR.serialise fredTheFrog)

We can then later read Fred,

> main = do
>     fred <- CBOR.deserialise <$> BSL.readFile "hi"
>     print fred

-}

{- $cborformat

@binary-serialise-cbor@ uses the Concise Binary Object Representation, CBOR
(IETF RFC 7049), as its serialised representation. This encoding is efficient in
both encoding\/decoding complexity as well as space, and is generally
machine-independent.

The CBOR data model resembles that of JSON, having arrays, key/value maps,
integers, floating point numbers, and a variety of string types. In addition,
CBOR allows items to be /tagged/ with a number which describes the type of data
that follows. This can be used both to identify which data constructor of a type
an encoding represents, as well as representing different versions of the same
constructor.

*** A note on interoperability

@binary-serialise-cbor@ is intended primarily as a /serialisation/ library for
Haskell values. That is, a means of stably storing Haskell values for later
reading by @binary-serialise-cbor@. While it uses the CBOR encoding format, the
library doesn't /not/ primarily aim to facilitate serialisation and
deserialisation across different CBOR implementations.

If you want to use @binary-serialise-cbor@ to serialise\/deserialise values
for\/from another CBOR implementation (either in Haskell or another language),
you should keep a few things in mind,

1. The 'Serialise' instances for some "basic" Haskell types (e.g. 'Maybe',
   'Data.ByteString.ByteString') don't carry a tag, in contrast to common
   convention. This is an intentional design decision to minimize encoding size.

2. The library reserves the right to change encodings in
   non-backwards-compatible ways across super-major versions.

3. While the library tries to use standard encodings in its instances wherever possible,
   these instances aren't guaranteed to implement all valid variants of the
   encodings in the specification. For instance, the 'UTCTime' instance only
   implements a small subset of the encodings described by the Extended Date
   RFC.

-}


{- $encoding

During encoding, abstract CBOR resentations are represented by the
'Data.Binary.Serialise.CBOR.Encoding.Tokens' type. Such a representation can be
efficiently built using the 'Data.Binary.Serialise.CBOR.Encoding.Encoding'
'Monoid'.

For instance, to implement an encoder for the @Animal@ type above we might write,

> encodeAnimal :: Animal -> Encoding
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeTag 0 <> encodeString name <> encodeInt height
> encodeAnimal (WalkingAnimal name speed) =
>     encodeListLen 3 <> encodeTag 1 <> encodeString name <> encodeInt speed

Here we see that each encoding begins with a /length/, describing how many
values belonging to our @Animal@ will follow. We then encode a /tag/, which
identifies which constructor. We then encode the fields.

-}

{- $decoding

Decoding CBOR representations to Haskell values is done in the 'Decoder'
'Monad'. We can write a 'Decoder' for the @Animal@ type defined above as
follows,

> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeTag
>     case (len, tag) of
>       (3, 0) -> HoppingAnimal <$> decodeString <*> decodeInt
>       (3, 1) -> WalkingAnimal <$> decodeString <*> decodeInt
>       _      -> fail "invalid Animal encoding"

Note that we scrutinize both the length and the tag to determine which encoding
we are looking at. This allows us to, for instance, add fields without changing
using a new tag by simply appending new fields to the end of the encoding and
changing the length appropriately.

-}


{- $serialise

As mentioned in the introduction, @binary-serialise-cbor@ provides a 'Serialise'
class for convenient access to serialisers and deserialisers,

> -- Can be derived via 'Generic'
> class Serialise a where
>     encode  :: a -> Encoding
>     decode  :: Decoder s a
>
>     -- These have defaults and can be ignored
>     encodeList :: [a] -> Encoding
>     decodeList :: Decoder s [a]

Note the existance of the 'encodeList' and 'decodeList' methods; these allow for
efficient handling of cases like 'String'.

We can write an instance for 'Serialise' for @Animal@ using the manually-written
decoders and encoders from above,

> instance Serialise Animal where
>     encode = encodeAnimal
>     decode = decodeAnimal

This would be equivalent to using the generically derived instance,

> instance Serialise Animal

-}
