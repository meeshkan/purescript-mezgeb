module Megzeb where

import Data.Symbol (class IsSymbol)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Lacks, class Cons)
import Record as Record
import Type.Proxy (Proxy)

data MegzebGet
  = MegzebGet

instance mezgebGet ::
  (IsSymbol l, Cons l x i' i, Lacks l o', Cons l x o' o) =>
  FoldingWithIndex MegzebGet (Proxy l) ({ | i } /\ { | o' }) ignore ({ | i } /\ { | o }) where
  foldingWithIndex MegzebGet prop (i /\ o') _ = i /\ Record.insert prop (Record.get prop i) o'

get ::
  forall i r o.
  HFoldlWithIndex MegzebGet ({ | i } /\ {}) { | r } ({ | i } /\ { | o }) =>
  { | r } -> { | i } -> { | o }
get r i = snd (hfoldlWithIndex MegzebGet (i /\ {}) r)

data MegzebSet
  = MegzebSet

instance mezgebSet ::
  (IsSymbol l, Cons l a r i, Cons l b r o) =>
  FoldingWithIndex MegzebSet (Proxy l) { | i } b { | o } where
  foldingWithIndex MegzebSet prop i b = Record.set prop b i

set ::
  forall i r o.
  HFoldlWithIndex MegzebSet { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
set r i = hfoldlWithIndex MegzebSet i r

data MegzebModify
  = MegzebModify

instance mezgebModify ::
  (IsSymbol l, Cons l a r i, Cons l b r o) =>
  FoldingWithIndex MegzebModify (Proxy l) { | i } (a -> b) { | o } where
  foldingWithIndex MegzebModify prop i ab = Record.modify prop ab i

modify ::
  forall i r o.
  HFoldlWithIndex MegzebModify { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
modify r i = hfoldlWithIndex MegzebModify i r

data MegzebInsert
  = MegzebInsert

instance mezgebInsert ::
  (IsSymbol l, Lacks l i, Cons l a i o) =>
  FoldingWithIndex MegzebInsert (Proxy l) { | i } a { | o } where
  foldingWithIndex MegzebInsert prop i a = Record.insert prop a i

insert ::
  forall i r o.
  HFoldlWithIndex MegzebInsert { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
insert r i = hfoldlWithIndex MegzebInsert i r

data MegzebDelete
  = MegzebDelete

instance mezgebDelete ::
  (IsSymbol l, Lacks l o, Cons l x o i) =>
  FoldingWithIndex MegzebDelete (Proxy l) { | i } ignore { | o } where
  foldingWithIndex MegzebDelete prop i _ = Record.delete prop i

delete ::
  forall i r o.
  HFoldlWithIndex MegzebDelete { | i } { | r } { | o } =>
  { | r } -> { | i } -> { | o }
delete r i = hfoldlWithIndex MegzebDelete i r
