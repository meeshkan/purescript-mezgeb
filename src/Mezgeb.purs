module Mezgeb where

import Data.Symbol (class IsSymbol)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy)

class Lacks :: forall k1 k2. Row k1 -> Row k2 -> Constraint
class Lacks r0 r1

instance mezgebLacksAll :: (RowList.RowToList r0 r0', Lacks' r0' r0 r1) => Lacks r0 r1
class Lacks' :: forall k1 k2 k3. RowList.RowList k1 -> Row k2 -> Row k3 -> Constraint
class Lacks' r0' r0 r1

instance mezgebLacks'Cons :: (Lacks' r0'' r0 r1, Row.Lacks sym r1) => Lacks' (RowList.Cons sym ignore r0'') r0 r1
instance mezgebLacks'Nil :: Lacks' RowList.Nil r0 r1

class Cons :: forall k1 k2. Row k1 -> Row k2 -> Constraint
class Cons r0 r1

instance mezgebConsAll :: (RowList.RowToList r0 r0', Cons' r0' r0 r1) => Cons r0 r1
class Cons' :: forall k1 k2 k3. RowList.RowList k1 -> Row k2 -> Row k3 -> Constraint
class Cons' r0' r0 r1

instance mezgebCons'Cons :: (Cons' r0'' r0 r1, Row.Cons sym a ignore r1) => Cons' (RowList.Cons sym a r0'') r0 r1
instance mezgebCons'Nil :: Cons' RowList.Nil r0 r1

data MezgebGet
  = MezgebGet

instance mezgebGet ::
  (IsSymbol l, Row.Cons l x i' i, Row.Lacks l o', Row.Cons l x o' o) =>
  FoldingWithIndex MezgebGet (Proxy l) ({ | i } /\ { | o' }) ignore ({ | i } /\ { | o }) where
  foldingWithIndex MezgebGet prop (i /\ o') _ = i /\ Record.insert prop (Record.get prop i) o'

get ::
  forall i r o.
  HFoldlWithIndex MezgebGet ({ | i } /\ {}) { | r } ({ | i } /\ { | o }) =>
  { | r } -> { | i } -> { | o }
get r i = snd (hfoldlWithIndex MezgebGet (i /\ {}) r)

data MezgebSet
  = MezgebSet

instance mezgebSet ::
  (IsSymbol l, Row.Cons l a r i, Row.Cons l b r o) =>
  FoldingWithIndex MezgebSet (Proxy l) { | i } b { | o } where
  foldingWithIndex MezgebSet prop i b = Record.set prop b i

set ::
  forall i r o.
  HFoldlWithIndex MezgebSet { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
set r i = hfoldlWithIndex MezgebSet i r

data MezgebModify
  = MezgebModify

instance mezgebModify ::
  (IsSymbol l, Row.Cons l a r i, Row.Cons l b r o) =>
  FoldingWithIndex MezgebModify (Proxy l) { | i } (a -> b) { | o } where
  foldingWithIndex MezgebModify prop i ab = Record.modify prop ab i

modify ::
  forall i r o.
  HFoldlWithIndex MezgebModify { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
modify r i = hfoldlWithIndex MezgebModify i r

data MezgebInsert
  = MezgebInsert

instance mezgebInsert ::
  (IsSymbol l, Row.Lacks l i, Row.Cons l a i o) =>
  FoldingWithIndex MezgebInsert (Proxy l) { | i } a { | o } where
  foldingWithIndex MezgebInsert prop i a = Record.insert prop a i

insert ::
  forall i r o.
  HFoldlWithIndex MezgebInsert { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
insert r i = hfoldlWithIndex MezgebInsert i r

data MezgebDelete
  = MezgebDelete

instance mezgebDelete ::
  (IsSymbol l, Row.Lacks l o, Row.Cons l x o i) =>
  FoldingWithIndex MezgebDelete (Proxy l) { | i } ignore { | o } where
  foldingWithIndex MezgebDelete prop i _ = Record.delete prop i

delete ::
  forall i r o.
  HFoldlWithIndex MezgebDelete { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
delete r i = hfoldlWithIndex MezgebDelete i r
